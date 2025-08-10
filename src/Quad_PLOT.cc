/*
    This file is part of GNU APL, a free implementation of the
    ISO/IEC Standard 13751, "Programming Language APL, Extended"

    Copyright © 2018-2020  Dr. Jürgen Sauermann

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/** @file
*/

#include <errno.h>
#include <signal.h>

#include <iostream>
#include <iomanip>

#include "Common.hh"

// always require libX11
//
# if defined( HAVE_LIBX11 )
#  define MISSING1 ""
# else
#  define MISSING_LIBS 1
#  define MISSING1 " libX11.so"
#endif       // HAVE_LIBX11

#if apl_GTK3                            // ======== GTK based ⎕PLOT ========

# if defined( HAVE_LIBGTK_3 )
#  define MISSING2 ""
# else
#  define MISSING_LIBS 1
#  define MISSING2 " libgtk-3.so"
#endif       // HAVE_LIBGTK_3_0

# if defined( HAVE_LIBGDK_3 )
#  define MISSING3 ""
# else
#  define MISSING_LIBS 1
#  define MISSING3 " libgdk-3.so"
#endif       // HAVE_LIBGTK_3_0

# if defined( HAVE_LIBCAIRO )
#  define MISSING4 ""
# else
#  define MISSING_LIBS 1
#  define MISSING4 " libcairo.so"
#endif       // HAVE_LIBGTK_3_0

#else    // don't apl_GTK3:             // ======== XCB based ⎕PLOT ========

# if defined( HAVE_LIBXCB )
#  define MISSING2 ""
# else
#  define MISSING_LIBS 1
#  define MISSING2 " libxcb.so"
#endif       // HAVE_LIBXCB

# if defined( HAVE_LIBX11_XCB )
#  define MISSING3 ""
# else
#  define MISSING_LIBS 1
#  define MISSING3 " libX11-xcb.so"
#endif       // HAVE_LIBX11_XCB

# if ! defined( HAVE_XCB_XCB_H )
#  define MISSING_LIBS 1
#  define MISSING4 " xcb/xcb.h"
# else
#  define MISSING4 ""
# endif      // HAVE_XCB_XCB_H

#endif   // apl_GTK3                    // ======== GTK vs. XCB ========

#include "Avec.hh"
#include "Common.hh"
#include "Quad_PLOT.hh"

using namespace std;

Quad_PLOT  Quad_PLOT::fun;

vector<Quad_PLOT::PLOT_context *> Quad_PLOT::all_PLOT_windows;

Quad_PLOT::Handle Quad_PLOT::next_handle = 0;

sem_t __all_PLOT_windows_sema;
sem_t * Quad_PLOT::all_PLOT_windows_sema = &__all_PLOT_windows_sema;

sem_t __expose_sema;
sem_t * Quad_PLOT::expose_sema = &__expose_sema;

int Quad_PLOT::verbosity = 0;

#if defined(MISSING_LIBS)
//----------------------------------------------------------------------------
Quad_PLOT::Quad_PLOT()
  : QuadFunction(TOK_Quad_PLOT)
{
   verbosity = 0;
}
//----------------------------------------------------------------------------
Quad_PLOT::~Quad_PLOT()
{
}
//----------------------------------------------------------------------------
Token
Quad_PLOT::eval_B(Value_P B) const
{
   MORE_ERROR() <<
      "⎕PLOT is not available because some of its build prerequisites "
      "(in particular\n" MISSING1 MISSING2 MISSING3 MISSING4
      ") were either missing,\n" 
      " or were explicitly disabled in ./configure.";

   SYNTAX_ERROR;
   return Token();
}
//----------------------------------------------------------------------------
Token
Quad_PLOT::eval_AB(Value_P A, Value_P B) const
{
   return eval_B(B);
}
//----------------------------------------------------------------------------

#else   // not defined(WHY_NOT)

# include <X11/Xlib.h>
# include <X11/Xutil.h>
# include <xcb/xcb.h>
# include <xcb/xproto.h>

# include "ComplexCell.hh"
# include "FloatCell.hh"
# include "Security.hh"
# include "Workspace.hh"

// UTF8 support for XCB windows needs additional libraries that may
// not be present. You can disable that with: XCB_WINDOWS_WITH_UTF8_CAPTIONS 0
// or maybe CXXFLAGS=-D XCB_WINDOWS_WITH_UTF8_CAPTIONS=0 ./configure...
//
# ifndef XCB_WINDOWS_WITH_UTF8_CAPTIONS
#  define XCB_WINDOWS_WITH_UTF8_CAPTIONS 0
# endif

# if XCB_WINDOWS_WITH_UTF8_CAPTIONS
#  include <X11/Xutil.h>
#  include <X11/Xlib.h>
#  include <X11/Xlib-xcb.h>
# endif

# include <stdio.h>
# include <math.h>
# include <stdlib.h>
# include <string.h>
# include <xcb/xcb.h>
# include <xcb/xproto.h>

# include <iostream>
# include <iomanip>

using namespace std;

#include "Plot_data.hh"
#include "Plot_line_properties.hh"
#include "Plot_window_properties.hh"

//============================================================================

Quad_PLOT::Quad_PLOT()
  : QuadFunction(TOK_Quad_PLOT)
{
   __sem_init(all_PLOT_windows_sema, 0, 1);
   __sem_init(expose_sema, 1, 0);
   verbosity = 0;
}
//----------------------------------------------------------------------------
Quad_PLOT::~Quad_PLOT()
{
   __sem_destroy(all_PLOT_windows_sema);
}
//----------------------------------------------------------------------------
Token
Quad_PLOT::eval_AB(Value_P A, Value_P B) const
{
   CHECK_SECURITY(disable_Quad_PLOT);

   if (B->get_rank() > 3)        RANK_ERROR;
   if (B->element_count() < 2)   LENGTH_ERROR;

   // plot window with default attributes
   //
Plot_data * data = setup_data(*B);
   if (data == 0)   DOMAIN_ERROR;

Plot_window_properties * w_props = new Plot_window_properties(data, verbosity);
   if (w_props == 0)
      {
        delete data;
        WS_FULL;
      }

   Log(LOG_Quad_PLOT)
     CERR << "wprops = " << w_props << " created." << endl;

   // from here on 'data' is owned by 'w_props' (whose destructor
   // will delete it).
   //
   if (const ErrorCode ec = parse_attributes(*A, w_props))
      {
        delete w_props;
        throw_apl_error(ec, LOC);
      }
   if (w_props->update(verbosity))   { delete w_props;   DOMAIN_ERROR; }

   // do_plot_data takes ownership of w_props and will delete w_props
   //
const APL_Integer Z = do_plot_data(w_props, data);
   return Token(TOK_APL_VALUE1, IntScalar(Z, LOC));
}
//----------------------------------------------------------------------------
ErrorCode
Quad_PLOT::parse_attributes(const Value & A, Plot_window_properties * w_props)
{
   if (A.is_member())   // new-style attributes
      {
        loop(row, A.get_rows())
            {
              const Cell & att_name = A.get_cravel(2*row);
              if (att_name.is_pointer_cell())   // used entry in A
                 {
                   const UCS_string ucs = att_name.get_pointer_value()
                                                 ->get_UCS_ravel();
                   if (const char * error = w_props->set_attribute(ucs,
                                                  A.get_cravel(2*row + 1)))
                      {
                        MORE_ERROR() << error << " in ⎕PLOT attribute ." << ucs;
                        return E_DOMAIN_ERROR;
                      }
                 }
            }
        return E_NO_ERROR;
      }

   // old-style attributes
   //
   if (A.get_rank() > 1)   { delete w_props;   RANK_ERROR; }

const ShapeItem len_A = A.element_count();
   if (len_A < 1)   return E_LENGTH_ERROR;

const APL_Integer qio = Workspace::get_IO();

   loop(a, len_A)
       {
         const Cell & cell_A = A.get_cravel(a);
         if (!cell_A.is_pointer_cell())
            {
               MORE_ERROR() << "A[" << (a + qio)
                            << "] is not a string in A ⎕PLOT B";
               return E_DOMAIN_ERROR;
            }

         const Value * attr = cell_A.get_pointer_value().get();
         if (!attr->is_char_string())
            {
               MORE_ERROR() << "A[" << (a + qio)
                            << "] is not a string in A ⎕PLOT B";
               return E_DOMAIN_ERROR;
            }

         UCS_string ucs = attr->get_UCS_ravel();
         ucs.remove_leading_and_trailing_whitespaces();
         if (ucs.size() == 0)               continue;
         if (Avec::is_comment(ucs[0]))      continue;
         UTF8_string utf(ucs);
         if (const char * error = w_props->set_attribute(utf.c_str()))
            {
              MORE_ERROR() << error << " in ⎕PLOT attribute '" << ucs << "'";
              return E_DOMAIN_ERROR;
            }
       }

   return E_NO_ERROR;
}
//----------------------------------------------------------------------------
Token
Quad_PLOT::eval_B(Value_P B) const
{
   CHECK_SECURITY(disable_Quad_PLOT);

   if (B->get_rank() == 0 && !B->get_cfirst().is_pointer_cell())
      {
        // scalar (integer) argument: window control and logging
        //
        const APL_Integer B0 = B->get_cscalar().get_int_value();
        Value_P Z = window_control(B0);
        return Token(TOK_APL_VALUE1, Z);
      }

   if (B->get_rank() == 1 && B->element_count() == 0)
      {
        help();
        return Token(TOK_APL_VALUE1, Idx0(LOC));
      }

   if (B->get_rank() > 3)   RANK_ERROR;

   // plot window with default attributes
   //
Plot_data * data = setup_data(*B);
   if (data == 0)   DOMAIN_ERROR;

Plot_window_properties * w_props = new Plot_window_properties(data, verbosity);
   if (w_props == 0)
      {
        delete data;
         WS_FULL;
      }

const APL_Integer Z = do_plot_data(w_props, data);
   return Token(TOK_APL_VALUE1, IntScalar(Z, LOC));
}
//----------------------------------------------------------------------------
Value_P
Quad_PLOT::window_control(APL_Integer B0) const
{
   // scalar (integer) argument: plot window control
   //
   if (B0 == 0)                 // reset plot verbosity
      {
        verbosity = 0;
        CERR << "⎕PLOT verbosity turned off" << endl;
        return Idx0(LOC);
      }

   if (B0 == -1)                // enable SHOW_EVENTS
      {
        verbosity |= SHOW_EVENTS;
        CERR << "⎕PLOT will show X events " << endl;
        return Idx0(LOC);
      }

   if (B0 == -2)                // enable SHOW_DATA
      {
        verbosity |= SHOW_DATA;
        CERR << "⎕PLOT will  show APL data (and debug infos)" << endl;
        return Idx0(LOC);
      }

   if (B0 == -3)   // close all ⎕PLOT windows, return their handles
      {
        Value_P Z = window_control(-6);   // get all open handles, see below

         loop(h, all_PLOT_windows.size())
            {
              all_PLOT_windows[h]->plot_stop();
            }

         all_PLOT_windows.clear();
         next_handle = 0;
         Z->check_value(LOC);
         return Z;
       }

    if (B0 == -4)                // enable SHOW_DRAW
       {
         verbosity |= SHOW_DRAW;
         CERR << "⎕PLOT will  show rendering details " << endl;
         return Idx0(LOC);
       }

    if (B0 == -6)                // (sorted) list of handles
       {
         Value_P Z(all_PLOT_windows.size(), LOC);
         for (int offset = 0; Z->more(); offset += 64)
             {
               uint64_t bits = 0;
               loop(h, all_PLOT_windows.size())
                   {
                     const int handle = all_PLOT_windows[h]->handle;
                     const int bit = handle - offset;
                     if (bit >= 0 && bit < 64)   bits |= 1ULL << bit;
                   }

               loop(bit, 64)
                   {
                     if (bits & 1ULL << bit)   Z->next_ravel_Int(offset+bit);
                   }
             }
         Z->check_value(LOC);
         return Z;
       }

   // close one plot window...
   //
bool found = false;

   sem_wait(all_PLOT_windows_sema);
       loop(w, Quad_PLOT::all_PLOT_windows.size())
           {
             if (all_PLOT_windows[w]->handle != B0)   continue;

             all_PLOT_windows[w]->plot_stop();
             all_PLOT_windows[w] = all_PLOT_windows.back();
             all_PLOT_windows.pop_back();
             found = true;
             break;
           }
   sem_post(all_PLOT_windows_sema);

   if (all_PLOT_windows.size() == 0)   next_handle = 0;   // restart numbering

   return IntScalar(found ? B0 : 0, LOC);
}
//----------------------------------------------------------------------------
Plot_data *
Quad_PLOT::setup_data(const Value & B)
{
   /** check data. We expect B to be either:

       1a. a numeric vector (for a single plot line), or
       1b. a numeric matrix (for one plot line for each row of the matrix), or
       2a. a scalar of a nested numeric vector (for a single plot line), or
       2b. a vector of nested numeric vectors (for one plot line per item)
       3.  a 3-dimensional real vector for surface plots
    **/

const Value * pB = &B;
   if (B.is_scalar())   // case 2a. → 1a. by disclosing scalar B
      {
        if (!B.get_cfirst().is_pointer_cell())   DOMAIN_ERROR;
        pB = B.get_cfirst().get_pointer_value().get();   // B ← ⊃ B
      }

   if (pB->get_rank() == 3)                   return setup_data_3D(*pB);
   if (!pB->get_cfirst().is_pointer_cell())   return setup_data_2D(*pB);
   return setup_data_2D_2b(*pB);
}
//----------------------------------------------------------------------------
Plot_data *
Quad_PLOT::setup_data_2D(const Value & B)
{
   /** initialize the data for a 2D plot. B is the right argument
       of ⎕PLOT B or A ⎕PLOT B and contains the plot coordinates of
       the plot points as simple numeric values. Every B[line;] is one
       plot line.

       A complex number xJy means 2D plot coordinates (x, y) while a
       real number x means 2D plot coordinates (N, y) where N is the
       position of x in B[d;];

       The third dimension Z is set to Y although not used.
    **/

const ShapeItem cols_B = B.get_cols();
const ShapeItem rows_B = B.get_rows();
const ShapeItem len_B = rows_B * cols_B;

   // all items of B shall be simple numbers (integer, real, or complex).
   loop(b, len_B)
       {
         if (!B.get_cravel(b).is_numeric())   return 0;
       }

   // split B into X=real B, Y=imag Y

double * X = new double[3*len_B];
double * Y = X + len_B;
double * Z = Y + len_B;
   if (!X)   WS_FULL;

const APL_Integer qio = Workspace::get_IO();
   loop(b, len_B)
       {
         const Cell & cB = B.get_cravel(b);
         if (cB.is_complex_cell())   // (x, y)
            {
              X[b] = cB.get_real_value();
              Z[b] = Y[b] = cB.get_imag_value();
            }
         else                        // (N, y)
            {
              X[b] = qio + b % cols_B;
              Z[b] = Y[b] = cB.get_real_value();
            }
       }

Plot_data * data = new Plot_data(rows_B);
   loop(r, rows_B)
       {
         const double * pX = X + r*cols_B;
         const double * pY = Y + r*cols_B;
         const double * pZ = Z + r*cols_B;
         const Plot_data_row * pdr = new Plot_data_row(pX, pY, pZ, r, cols_B);
         data->add_row(pdr);
       }

   return data;
}
//----------------------------------------------------------------------------
Plot_data *
Quad_PLOT::setup_data_2D_2b(const Value & B)
{
   /** initialize the data for a 2D plot. B is the right argument
       of ⎕PLOT B or A ⎕PLOT B and contains the plot lines as nested
       values. Every ⊃B[line] is one plot line.

       The third dimension Z is set to Y although not used.
    **/

ShapeItem data_points = 0;
   if (B.get_rank() > 1)   RANK_ERROR;
const ShapeItem rows = B.element_count();   // number of plot rows
   loop(r, rows)
       {
         const Value * vrow = B.get_cravel(r).get_pointer_value().get();
         if (vrow->get_rank() > 1)   RANK_ERROR;
         const ShapeItem row_len = vrow->element_count();
         data_points += row_len;
         loop(rb, row_len)
             {
               if (!vrow->get_cravel(rb).is_numeric())   DOMAIN_ERROR;
             }
       }

double * X = new double[3*data_points];
double * Y = X + data_points;
double * Z = Y + data_points;
   if (!X)   WS_FULL;

ShapeItem idx = 0;
const APL_Integer qio = Workspace::get_IO();

   loop(r, rows)
       {
         const Value * vrow = B.get_cravel(r).get_pointer_value().get();
         loop(v, vrow->element_count())
             {
               const Cell & cB = vrow->get_cravel(v);
               if (cB.is_complex_cell())
                  {
                    X[idx] = cB.get_real_value();
                    Z[idx] = Y[idx] = cB.get_imag_value();
                  }
               else
                  {
                    X[idx] = qio + v;
                    Z[idx] = Y[idx] = cB.get_real_value();
                  }
               ++idx;
             }
       }

Plot_data * data = new Plot_data(rows);
   idx = 0;
   loop(r, rows)
       {
         const double * pX = X + idx;
         const double * pY = Y + idx;
         const double * pZ = Z + idx;
         const Value * vrow = B.get_cravel(r).get_pointer_value().get();
         const ShapeItem row_len = vrow->element_count();
         const Plot_data_row * pdr = new Plot_data_row(pX, pY, pZ, r,
                                                            row_len);
         data->add_row(pdr);
         idx += row_len;
       }

   return data;
}
//----------------------------------------------------------------------------
Plot_data *
Quad_PLOT::setup_data_3D(const Value & B)
{
   /** initialize the data for a 3D (surface-) plot. B is the right argument
       of ⎕PLOT B or A ⎕PLOT B and can be:

       1. A single nested array Y. In this case the X and Z coordintes
          are derived from ⍴Y and run from X=0...cols and from Z=0 to rows.
          The double * data contains only the values from Y (height).

          The double * Y contains the values from for Y (height):

           Y
           ↓
          ┌───────────────┐
          │ Y-coordinates │
          └───────────────┘
          │← data_points →│


       2. Two nested arrays X and Y. In this case the Z coordinate is derived
          from ⍴Y and run from Z=0 to rows.

          The double * X contains the values from X, followed by the values
          for Y (height):

           X               Y               Z
           ↓               ↓              
          ┌───────────────┬───────────────┐
          │ X-coordinates │ Y-coordinates │
          └───────────────┴───────────────┘
          │← data_points →│← data_points →│


       3. B← (⊂X) (⊂Y) (⊂Z). This indicates a 3D aka, surface plot.
          All coordinates come from B and the double * X contains
          Y and Z like this:

           X               Y               Z
           ↓               ↓               ↓
          ┌───────────────┬───────────────┬───────────────┐
          │ X-coordinates │ Y-coordinates │ Z-coordinates │
          └───────────────┴───────────────┴───────────────┘
          │← data_points →│← data_points →│← data_points →│

    **/

const ShapeItem planes = B.get_shape_item(0);   // (X), Y, and (Z)
const ShapeItem rows   = B.get_shape_item(1);
const ShapeItem cols   = B.get_shape_item(2);

   if (planes < 1 || planes > 3)    LENGTH_ERROR;
   if (rows < 1)                    LENGTH_ERROR;
   if (cols < 1)                    LENGTH_ERROR;

Plot_data * data = 0;

const ShapeItem data_points = rows * cols;

   if (planes == 3)   // X, Y, and Z
      {
        double * X = new double[3*data_points];
        double * Y = X + data_points;
        double * Z = Y + data_points;
        if (!X)   WS_FULL;

        data = new Plot_data(rows);
        loop(r, rows)
            {
              loop(c, cols)
                  {
                    const ShapeItem p = c + r*cols;
                    const Cell & cX = B.get_cravel(p);
                    const Cell & cY = B.get_cravel(p +   data_points);
                    const Cell & cZ = B.get_cravel(p + 2*data_points);

                    if (!(cX.is_integer_cell() ||
                          cX.is_real_cell()))   DOMAIN_ERROR;
                    if (!(cY.is_integer_cell() ||
                          cY.is_real_cell()))   DOMAIN_ERROR;
                    if (!(cZ.is_integer_cell() ||
                          cZ.is_real_cell()))   DOMAIN_ERROR;

                    X[p] = cX.get_real_value();
                    Y[p] = cY.get_real_value();
                    Z[p] = cZ.get_real_value();
                  }
                   const double * pX = X + r*cols;
                   const double * pY = Y + r*cols;
                   const double * pZ = Z + r*cols;
                   const Plot_data_row * pdr = new Plot_data_row(pX, pY, pZ,
                                                                 r, cols);
                   data->add_row(pdr);
            }
      }
   else if (planes == 2)   // X and Y, but no Z
      {
        double * X = new double[2*data_points];
        double * Y = X + data_points;
        if (!X)   WS_FULL;

        data = new Plot_data(rows);
        loop(r, rows)
            {
              loop(c, cols)
                  {
                    const ShapeItem p = c + r*cols;
                    const Cell & cX = B.get_cravel(p);
                    const Cell & cY = B.get_cravel(p + data_points);

                    if (!(cX.is_integer_cell() ||
                    cX.is_real_cell()))   DOMAIN_ERROR;
                    if (!(cY.is_integer_cell() ||
                               cY.is_real_cell()))   DOMAIN_ERROR;

                    X[p] = cX.get_real_value();
                    Y[p] = cY.get_real_value();
                  }
              const double * pX = X + r*cols;
              const double * pY = Y + r*cols;
              const Plot_data_row * pdr = new Plot_data_row(pX, pY, 0, r, cols);
              data->add_row(pdr);
            }
      }
   else                    // Y, but no X and no Z
      {
        double * Y = new double[data_points];
        if (!Y)   WS_FULL;

        data = new Plot_data(rows);
        loop(r, rows)
            {
              loop(c, cols)
                  {
                    const ShapeItem p = c + r*cols;
                    const Cell & cY = B.get_cravel(p);

                    if (!(cY.is_integer_cell() ||
                          cY.is_real_cell()))   DOMAIN_ERROR;

                    Y[p] = cY.get_real_value();
                  }
              const double * pY = Y + r*cols;
              const Plot_data_row * pdr = new Plot_data_row(0, pY, 0, r, cols);
              data->add_row(pdr);
            }
      }

   data->surface = true;
   return data;
}
//----------------------------------------------------------------------------
APL_Integer
Quad_PLOT::do_plot_data(Plot_window_properties * w_props,
                        const Plot_data * data)
{
   w_props->set_verbosity(verbosity);
   verbosity > 0 && w_props->print(CERR);

   // check (possibly again) for empty plot ranges which could be caused
   // by bad plot data but also by bad window properties.
   //
   if (w_props->get_min_X() >= w_props->get_max_X())
      {
        MORE_ERROR() << "empty X range in A ⎕PLOT B";
        DOMAIN_ERROR;
      }
   if (w_props->get_min_Y() >= w_props->get_max_Y())
      {
        MORE_ERROR() << "empty Y range in A ⎕PLOT B";
        DOMAIN_ERROR;
      }

   if (w_props->get_min_Z() >= w_props->get_max_Z())
      {
        MORE_ERROR() << "empty Z range in A ⎕PLOT B";
        DOMAIN_ERROR;
      }

const APL_Integer Z = ++next_handle;
   sem_wait(all_PLOT_windows_sema);
      {
#if apl_GTK3   // GTK

   plot_main_GTK(w_props, Z);   // pushes a GTK_context into all_PLOT_windows.

#else          // XCB

pthread_t th;
   pthread_create(&th, 0, plot_main_XCB, w_props);   // pushes a XCB_context

#endif
      }
   sem_post(all_PLOT_windows_sema);

   sem_wait(expose_sema);   // blocks until window shown

   if (w_props->get_with_border())
      {
        // the user wants window borders, which requires a delay (of 100 ms,
        // see WM_timeout() in Event_handler 'Draw' in Plot_gtk.cc). We add
        // twice that timeout at ⎕PLOT level so that the next ⎕PLOT (if any)
        // does not interfer with the saving of the current plot window.
        //
        usleep(2000 * SAVE_BORDER_DELAY_ms);
      }

   return Z;
}
//----------------------------------------------------------------------------
static UCS_string
fill_14(const std::string & name)
{
UTF8_string name_utf(name.c_str());
UCS_string ret(name_utf);
   while (ret.size() < 14)   ret += UNI_SPACE;
   return ret;
}

void
Quad_PLOT::help()
{
   CERR <<
"\n"
"   ⎕PLOT Usage:\n"
"\n"
"   ⎕PLOT B     with ⍴⍴B > 0: plot B with default attribute values\n"
"   ⎕PLOT B     with integer scalar B: special ⎕PLOT functions\n"
"   A ⎕PLOT B   plot B with attribute overrides specified by A\n"
"\n"
"   A is a nested vector of strings.\n"
"   Each string A[i] has the form \"Attribute: Value\"\n"
"   Colors are specified either as #RGB or as #RRGGBB or as RR GG BB)\n"
"\n"
"   The attributes understood by ⎕PLOT and their default values are:\n"
"\n"
"   1. Global (plot window) Attributes:\n"
"\n";

   CERR << left;

# define gdef(ty,  na,  val, descr)                                        \
   CERR << setw(20) << #na ":  " << fill_14(Plot_data::ty ## _to_str(val)) \
        << " (" << descr << ")" << endl;
# include "Quad_PLOT.def"

   CERR <<
"\n"
"color_level-P:      (none)         "
                    "(color gradient at P% (surface plots only))\n"
"\n"
"   2. Local (plot line N) Attributes:\n"
"\n";

# define ldef(ty,  na,  val, descr)             \
   CERR << setw(20) << #na "-N:  " << setw(14) \
        << Plot_data::ty ## _to_str(val) << " (" << descr << ")" << endl;
# include "Quad_PLOT.def"

   CERR << right;
}
//----------------------------------------------------------------------------
Quad_PLOT::Handle
Quad_PLOT::PLOT_context::remove_handle(Handle handle)
{
// CERR << "remove_handle(" << handle << ")" << endl;

   loop(h, all_PLOT_windows.size())
      {
        if (all_PLOT_windows[h]->handle == handle)
           {
             all_PLOT_windows[h] = all_PLOT_windows.back();
             all_PLOT_windows.pop_back();
             return handle;
           }
      }

   return 0;
}
//----------------------------------------------------------------------------
#endif // defined(WHY_NOT)

