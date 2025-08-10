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

/// \file

#include <errno.h>

#include "Common.hh"
#if HAVE_LOCALE_H
#include <locale.h>
#endif

#if HAVE_LIBX11 && HAVE_LIBXCB && HAVE_LIBX11_XCB && HAVE_X11_XLIB_XCB_H

# include <X11/Xlib.h>
# include <X11/Xutil.h>
# include <xcb/xcb.h>
# include <xcb/xproto.h>

# include "ComplexCell.hh"
# include "FloatCell.hh"
# include "Quad_PLOT.hh"
# include "Workspace.hh"

// UTF8 support for XCB windows needs additional libraries that may
// not be present. You can enable that with: XCB_WINDOWS_WITH_UTF8_CAPTIONS 1
// in this file or better:
//
//  CXXFLAGS=-D XCB_WINDOWS_WITH_UTF8_CAPTIONS=1 ./configure. The
//
// default is disabled.
//
# ifndef XCB_WINDOWS_WITH_UTF8_CAPTIONS
   /// undefined → 1
#  define XCB_WINDOWS_WITH_UTF8_CAPTIONS 1
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

# include "Plot_data.hh"
# include "Plot_line_properties.hh"

# include "Plot_window_properties.hh"

/// two int16_t
#define I1616(x, y) int16_t(x), int16_t(y)
/// the number of items in \b x
#define ITEMS(x) sizeof(x)/sizeof(*x), x

/// 2 byte little endian
#define LE2(x) uint8_t((x)), uint8_t((x) >> 8)

/// 4 byte little endian
#define LE4(x) LE2((x)), LE2((x >> 16))

// ===========================================================================
/// Some xcb IDs (returned from the X server)
class XCB_context : public Quad_PLOT::PLOT_context
{
public:
   /// constructor
   XCB_context(const Plot_window_properties & props, Quad_PLOT::Handle h)
   : Quad_PLOT::PLOT_context(h),
     thread(pthread_self()),
     caption(0),
     w_props(props),
     window_goon(true),
     file_saved(false)
   {}

   /// destructor
   virtual ~XCB_context()
      {
        free(const_cast<char *>(caption));
        delete &w_props;
      }

   /// overloaded PLOT_context::plot_stop()
   virtual void plot_stop()
      {
        window_goon = false;
      }

   /// return the thread that handles this plott window
   virtual pthread_t get_thread() const
      { return thread; }

   /// save plot window to a bmp (!) file
   void save_file(const char * outfile);

   /// the pthread running the X event loop for the plot window
   pthread_t thread;

   /// window caption
   const char * caption;

   /// the window properties (as choosen by the user)
   const Plot_window_properties & w_props;

   xcb_connection_t * conn;       ///< the connection to the X server
   Display *          display;    ///< the open display
   xcb_window_t       window;     ///< the plot window
   xcb_screen_t       * screen;   ///< the screen containing the window
   xcb_gcontext_t     fill;       ///< the curren fill style
   xcb_gcontext_t     line;       ///< the curren line style
   xcb_gcontext_t     point;      ///< the curren point style
   xcb_gcontext_t     text;       ///< the curren text style
   xcb_font_t         font;       ///< the curren font

   /// false if the window was closed from APL
   bool window_goon;

   /// plot window was written to an output file
   bool file_saved;
};
//----------------------------------------------------------------------------
/// the supposed width and the height of a string (in pixels)
struct string_width_height
{
   /// constructor: ask X-server for the size (in pixels) of \b string
   string_width_height(const XCB_context & pctx, const char * string);

   /// the width of the string (pixels)
   Pixel_X width;

   /// the height of the string (pixels)
   Pixel_Y height;
};
//----------------------------------------------------------------------------
string_width_height::string_width_height(const XCB_context & pctx,
                                         const char * string)
{
const size_t string_length = strlen(string);
xcb_char2b_t xcb_str[string_length + 10];

   loop(s, string_length)
       {
         xcb_str[s].byte1 = 0;
         xcb_str[s].byte2 = string[s];
       }

const xcb_query_text_extents_cookie_t cookie =
   xcb_query_text_extents(pctx.conn, pctx.font, string_length, xcb_str);

   if ( xcb_query_text_extents_reply_t * reply =
        xcb_query_text_extents_reply(pctx.conn, cookie, 0))
      {
        width  = reply->overall_width;
        height = reply->font_ascent + reply->font_descent;
        // origin = reply->font_ascent;
        free(reply);
      }
   else
      {
        puts("XCB ERROR: xcb_query_text_extents_reply() failed.");
        width  = 24;
        height = 13;
      }
}
//----------------------------------------------------------------------------
/// test a returned cookie for errors (and print \b errMessage if any).
void
testCookie(xcb_void_cookie_t cookie, xcb_connection_t * conn,
           const char * errMessage)
{
   if (xcb_generic_error_t * error = xcb_request_check(conn, cookie))
      {
        CERR <<"FATA X ERROR: " << errMessage << " : "
             << error->error_code << endl;
        free(error);
        xcb_disconnect(conn);
        exit (-1);
      }
}
//----------------------------------------------------------------------------
/// return a xcb_gcontext_t for font \b font_name
xcb_gcontext_t
setup_font_gc(XCB_context & pctx, const char * font_name, Color canvas_color)
{
   /* get font */

   testCookie(xcb_open_font_checked(pctx.conn, pctx.font, strlen(font_name),
                                    font_name), pctx.conn, "can't open font");

   // create graphics context
xcb_gcontext_t gc = xcb_generate_id(pctx.conn);
uint32_t mask     = XCB_GC_FOREGROUND | XCB_GC_BACKGROUND | XCB_GC_FONT;
uint32_t values[3] = { 0x00FFFFFF & ~canvas_color, canvas_color, pctx.font };
   testCookie(xcb_create_gc_checked(pctx.conn, gc, pctx.window, mask, values),
              pctx.conn, "can't create font gc");
   return gc;
}
//----------------------------------------------------------------------------
# define XFT   0   /**< draw text with Xdt */
# define PANGO 0   /**< draw text with pango */
# if XFT
#  include <X11/Xft/Xft.h>
# endif

/// draw text \b label at position \b xy
void
draw_text(const XCB_context & pctx, const char * label, const Pixel_XY & xy)
{
# if XFT

const int x = xy.x;
const int y = xy.y;

const Colormap colormap = XDefaultColormap(pctx.display, /*screen*/ 0);
Visual * visual = DefaultVisual(pctx.display, /*screen*/ 0);
XftDraw * xftd = XftDrawCreate(pctx.display, pctx.window, visual, colormap);

static XftFont * font = 0;
static XftColor black, white, red;
   if (font == 0)   // first time through
      {
      //const char * font_name = "Arial-10";
        const char * font_name = "Courier-10";

        // alpha = 0 is fully transparent, 0xFFFF is fully opaque
        //                                  red   green    blue   alpha
        const XRenderColor rgb_black = {      0,      0,      0, 0xFFFF };
        const XRenderColor rgb_white = { 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF };
        const XRenderColor rgb_red   = { 0xFFFF, 0x8000, 0x8000, 0xFFFF };

       XftColorAllocValue(pctx.display, visual, colormap, &rgb_white, &white);
        XftColorAllocValue(pctx.display, visual, colormap, &rgb_black, &black);
        XftColorAllocValue(pctx.display, visual, colormap, &rgb_red,   &red);

        font = XftFontOpenName(pctx.display, /*screen*/ 0, font_name);
      }

   Assert(font);

   XftDrawRect(xftd, &red, x, y - 10, 40, 20);

// XGlyphInfo extents;
//    XftTextExtentsUtf8(pctx.display, font,
//                      (const FcChar8 *)label, strlen(label), &extents);
   XftDrawStringUtf8(xftd, &black, font,
                     x, y,
             //      x + (cellwidth - extents.xOff) / 2, y + font->ascent,
                     (const FcChar8 *)label, strlen(label));

   xcb_flush(pctx.conn);

# elif PANGO

cairo_surface_t * surface =·
cairo_t cr = cairo_create(surface);

# else   // works reliably

   // draw the text
   //
xcb_void_cookie_t textCookie = xcb_image_text_8_checked(pctx.conn,
                                                        strlen(label),
                                                        pctx.window, pctx.text,
                                                        xy.x, xy.y, label);

   testCookie(textCookie, pctx.conn,
              "xcb_image_text_8_checked() failed.");
# endif
}
//----------------------------------------------------------------------------
/// return the text of a grid tick with value \b val
char *
format_tick(double val)
{
static char cc[80];
char * cp = cc;
const char * end = cc + sizeof(cc);
   if (val == 0)
      {
        *cp++ = '0';
        *cp++ = 0;
        return cc;
      }

   if (val < 0)
      {
        *cp++ = '-';
        val = -val;
      }

const char * unit = 0;
   if (val >= 1e3)
      {
        if      (val >= 1e15)   ;
        else if (val >= 1e12)   { unit = "T";   val *= 1e-12; }
        else if (val >= 1e9)    { unit = "G";   val *= 1e-9;  }
        else if (val >= 1e6)    { unit = "M";   val *= 1e-6;  }
        else                    { unit = "k";   val *= 1e-3;  }
      }
   else
      {

        if (val >= 1e0)    { unit = "";                  }
        else if (val >= 1e-3)   { unit = "m";   val *= 1e3;   }
        else if (val >= 1e-6)   { unit = "μ";   val *= 1e6;   }
        else if (val >= 1e-9)   { unit = "n";   val *= 1e9;   }
        else if (val >= 1e-12)  { unit = "p";   val *= 1e12;  }
      }

   if (unit == 0)   // very large or very small
      {
        snprintf(cp, end - cc, "%.2E", val);
        NULL_TERMINATE(cc)
        return cc;
      }

   // at this point: 1 ≤ val < 1000
   //
   if      (val < 10)    snprintf(cp, end - cc, "%.2f%s", val, unit);
   else if (val < 100)   snprintf(cp, end - cc, "%.1f%s", val, unit);
   else                  snprintf(cp, end - cc, "%.0f%s", val, unit);
   NULL_TERMINATE(cc)
   return cc;
}
//----------------------------------------------------------------------------
/// draw a line from P0 to P1
void
draw_line(const XCB_context & pctx, xcb_gcontext_t gc,
          const Pixel_XY & P0, const Pixel_XY & P1)
{
const xcb_segment_t segment = { I1616(P0.x, P0.y), I1616(P1.x, P1.y) };
   xcb_poly_segment(pctx.conn, pctx.window, gc, 1, &segment);
}
//----------------------------------------------------------------------------
/// swap pixels at P0 (with value Y0) and P1 (with value Y1) and 
inline void
pv_swap(Pixel_XY & P0, double & Y0, Pixel_XY & P1, double & Y1)
{
const double   Y = Y0;   Y0 = Y1;   Y1 = Y;
const Pixel_XY P = P0;   P0 = P1;   P1 = P;
}
//----------------------------------------------------------------------------
inline double
intersection(double Ax, double Ay, double Bx, double By,
             double Cx, double Cy, double Dx, double Dy)
{
   /** given 4 points A=(Ax, Ay), B=(Bx, By), C=(Cx, Cy), and D=(Dx, Dy)
       in the plane, return the factor beta so that the intersection point
       M=(Mx, My) of the lines A-C and B-D is given by

       M = (Mx, My) == A + alpha*(D-A) == B + beta*(C - B). Note that:

       A
        \   B
         \ /
          M
         / \
        /   \
       C     D
    Ax     + alpha*(Dx-Ax) == Bx + beta*(Cx-Bx)
    Ay     + alpha*(Dy-Ay) == By + beta*(Cy-By)

   **/

   /* first, translate A, B, and D by -C, so that C=(0, 0):

       A
        \   B
         \ /
          M
         / \
        /   \
       0     D
   */
   Ax -= Cx;   Bx -= Cx;   Dx -= Cx;
   Ay -= Cy;   By -= Cy;   Dy -= Cy;

   /* Thus:

   (Ax-Bx) + alpha*(Dx-Ax) == beta*(-Bx)
   (Ay-By) + alpha*(Dy-Ay) == beta*(-By)

   (Ax-Bx)*(Dy-Ay) + alpha*(Dx-Ax)*(Dy-Ay) == beta*(-Bx)*(Dy-Ay)
   (Ay-By)*(Dx-Ax) + alpha*(Dy-Ay)*(Dx-Ax) == beta*(-By)*(Dx-Ax)
   =======================================    ==================
   (Ax-Bx)*(Dy-Ay) - (Ay-By)*(Dx-Ax) ==  beta*((-Bx)*(Dy-Ay) - (-By)*(Dx-Ax))
   **/

const double numer = (Ax-Bx)*(Dy-Ay) - (Ay-By)*(Dx-Ax);   // left side
const double denom = ((-Bx)*(Dy-Ay) - (-By)*(Dx-Ax));     // right factor
   if (denom == 0.0)
      {
        Ax += Cx;   Bx += Cx;   Dx += Cx;
        Ay += Cy;   By += Cy;   Dy += Cy;
        MORE_ERROR() << "⎕PLOT: one of the X-Z squares has bad coordinates:"
                     << "P1=(" << Ax << ":" << Ay << "), "
                        "P2=(" << Bx << ":" << By << "), "
                        "P3=(" << Cx << ":" << Cy << ") "
                        "P4=(" << Dx << ":" << Dy << ")";
        DOMAIN_ERROR;
      }

   return /* beta == */ numer/denom;
}
//----------------------------------------------------------------------------
/// set the an attribute of a xcb_gcontext_t
inline void
set_GC_attr(const XCB_context & pctx, xcb_gcontext_t gc,
            xcb_gc_t mask, uint32_t value)
{
   xcb_change_gc(pctx.conn, gc, mask, &value);
}
//----------------------------------------------------------------------------
/// set the foreground color of a xcb_gcontext_t
inline void
set_GC_foreground(const XCB_context & pctx, xcb_gcontext_t gc, Color color)
{
   set_GC_attr(pctx, gc, XCB_GC_FOREGROUND, color);
}
//----------------------------------------------------------------------------
/// set the line width of a xcb_gcontext_t
inline void
set_GC_line_width(const XCB_context & pctx, xcb_gcontext_t gc, int width)
{
   set_GC_attr(pctx, gc, XCB_GC_LINE_WIDTH, width);
}
//----------------------------------------------------------------------------
/// draw a triangular area of a 3D plot
void
draw_triangle(const XCB_context & pctx, int verbosity,
              Pixel_XY P0, double H0, Pixel_XY P1, Pixel_XY P2, double H12)
{
   // draw a triangle with P1 and P2 at the same level H12
   //
   Assert(H0 >= 0.0);    Assert(H0 <= 1.0);
   Assert(H12 >= 0.0);   Assert(H12 <= 1.0);

   if (verbosity & SHOW_DRAW)
      CERR <<   " ∆2: P0(" << P0.x << ":" << P0.y << ") @H0=" << H0
           << "     P1(" << P1.x << ":" << P1.y << ") @H12=" << H12
           << "     P2(" << P2.x << ":" << P2.y << ") @H12=" << H12 << endl;

const double d1 = P0.distance2(P1);
const double d2 = P0.distance2(P2);
const int steps = sqrt(d1 > d2 ? d1 : d2);
const double dH = (H12 - H0) / steps;
   loop(s, steps)
       {
         const double alpha = H0 + s*dH;
         const double beta  = 1.0 * s/ steps;
         const uint32_t color = pctx.w_props.get_color(alpha);
         set_GC_foreground(pctx, pctx.fill, color);

         const Pixel_XY P0_P1(P0.x + beta * (P1.x - P0.x),
                              P0.y + beta * (P1.y - P0.y));
         const Pixel_XY P0_P2(P0.x + beta * (P2.x - P0.x),
                              P0.y + beta * (P2.y - P0.y));
         draw_line(pctx, pctx.fill, P0_P1, P0_P2);
       }
}
//----------------------------------------------------------------------------
/// draw a triangular plot point (∆ or ∇)
void
draw_triangle(const XCB_context & pctx, int verbosity, Pixel_XY P0, double H0,
              Pixel_XY P1, double H1, Pixel_XY P2, double H2)
{
   // draw a triangle with P0, P1 and P2 at levels H0, H1, and H2

   Assert(H0 >= 0);   Assert(H0 <= 1.0);
   Assert(H1 >= 0);   Assert(H1 <= 1.0);
   Assert(H2 >= 0);   Assert(H2 <= 1.0);

   if (verbosity & SHOW_DRAW)
      CERR << "\n∆1: P0(" << P0.x << ":" << P0.y << ")@H=" << H0
           << "      P1(" << P1.x << ":" << P1.y << ")@H=" << H1
           << "      P2(" << P2.x << ":" << P2.y << ")@H=" << H2 << endl;

   if (H0 < H1)   pv_swap(P0, H0, P1, H1);   // then H0 >= H1
   if (H0 < H2)   pv_swap(P0, H0, P2, H2);   // then H0 >= H2
   if (H1 < H2)   pv_swap(P1, H1, P2, H2);   // then H1 >= H2

   // here H0 >= H1 >= H2
   //
   Assert(H0 >= H1);
   Assert(H1 >= H2);

   if (H0 == H1)   // P0 and P1 have the same height
      {
        draw_triangle(pctx, verbosity, P2, H2, P0, P1, H0);
      }
   else if (H1 == H2)   // P1 and P2 have the same height
      {
        draw_triangle(pctx, verbosity, P0, H0, P1, P2, H1);
      }
   else            // P2 lies below P1
      {
        const double alpha = (H0 - H1) / (H0 - H2);   // alpha → 1 as P1 → P2
        Assert(alpha >= 0.0);
        Assert(alpha <= 1.0);

        // compute the point P on P0-P2 that has the heigth H1 (of P1)
        const Pixel_XY P(P0.x + alpha*(P2.x - P0.x),
                         P0.y + alpha*(P2.y - P0.y));
        draw_triangle(pctx, verbosity, P0, H0, P1, P, H1);
        draw_triangle(pctx, verbosity, P2, H2, P1, P, H1);
      }
}
//----------------------------------------------------------------------------
/// draw a plot point (representing one data value) at position \b xy
void
draw_point(const XCB_context & pctx, const Pixel_XY & xy, Color canvas_color,
           const Plot_line_properties & l_props)
{
const Pixel_X point_size  = l_props.get_point_size();   // outer point diameter
const Pixel_X point_size2 = l_props.get_point_size2();  // inner point diameter
const int16_t half = point_size >> 1;                   // outer point radius
const int point_style = l_props.get_point_style();
const Color point_color = l_props.get_point_color();

   // set outer point color for all point styles (and maybe inner point colors
   // later
   //
   set_GC_foreground(pctx, pctx.point, point_color);
   if (point_style == 1)   // circle
      {
        xcb_arc_t arc = { I1616(xy.x - half, xy.y - half),   // positions
                          point_size, point_size,            // diameters
                          0, 360 << 6 };                     // angles
        xcb_poly_fill_arc(pctx.conn, pctx.window, pctx.point, 1, &arc);

        if (point_size2)   // hollow
           {
             const int16_t half2 = point_size2 >> 1;
             xcb_arc_t arc2 = { I1616(xy.x - half2, xy.y - half2),   // pos
                                point_size2, point_size2,            // dia
                                0, int16_t(360 << 6) };              // ang
             set_GC_foreground(pctx, pctx.point, canvas_color);
             xcb_poly_fill_arc(pctx.conn, pctx.window, pctx.point, 1, &arc2);
             set_GC_foreground(pctx, pctx.point, point_color);
           }
      }
   else if (point_style == 2)   // triangle ∆
      {
        const int16_t center_y = xy.y - 2*point_size/3;
        const int16_t hypo_y = half*1.732;
        const xcb_point_t points[3]   = { { int16_t(xy.x), center_y  },
                                          { half, hypo_y },
                                          { int16_t(-2*half), 0   } };
        xcb_fill_poly(pctx.conn, pctx.window, pctx.point,
                      XCB_POLY_SHAPE_CONVEX,
                      XCB_COORD_MODE_PREVIOUS, ITEMS(points));
        if (point_size2)   // hollow
           {
             const int16_t half2 = point_size2 >> 1;
             const int16_t center2_y = xy.y - 2*point_size2/3;
             const int16_t hypo2_y = half2*1.732;
             const xcb_point_t points2[3]  = {
                 { int16_t(xy.x), center2_y   },
                 { int16_t(half2), hypo2_y },
                 { int16_t(-2*half2), 0    } };
             set_GC_foreground(pctx, pctx.point, canvas_color);
             xcb_fill_poly(pctx.conn, pctx.window, pctx.point,
                           XCB_POLY_SHAPE_CONVEX,
                           XCB_COORD_MODE_PREVIOUS, ITEMS(points2));
             set_GC_foreground(pctx, pctx.point, point_color);
           }
      }
   else if (point_style == 3)   // triangle ∇
      {
        const int16_t center_y = xy.y + 2*point_size/3;
        const int16_t hypo_y = - half*1.732;
        const xcb_point_t points[3]   = {
              { int16_t(xy.x), center_y  },
              { int16_t(half), hypo_y },
              { int16_t(-2*half), 0   } };
        xcb_fill_poly(pctx.conn, pctx.window, pctx.point,
                      XCB_POLY_SHAPE_CONVEX,
                      XCB_COORD_MODE_PREVIOUS, ITEMS(points));
        if (point_size2)   // hollow
           {
             const int16_t half2 = point_size2 >> 1;
             const int16_t center2_y = xy.y + 2*point_size2/3;
             const int16_t hypo2_y = - half2*1.732;
             const xcb_point_t points2[3]  = {
                 { int16_t(xy.x), center2_y   },
                 { int16_t(half2), hypo2_y },
                 { int16_t(-2*half2), 0    } };
             set_GC_foreground(pctx, pctx.point, canvas_color);
             xcb_fill_poly(pctx.conn, pctx.window, pctx.point,
                           XCB_POLY_SHAPE_CONVEX,
                           XCB_COORD_MODE_PREVIOUS, ITEMS(points2));
             set_GC_foreground(pctx, pctx.point, point_color);
           }
      }
   else if (point_style == 4)   // caro
      {
        const xcb_point_t points[4] =           {
              { int16_t(xy.x), int16_t(xy.y - half) },
              {  half,  half },
              { int16_t(-half),  half },
              { int16_t(-half), int16_t(-half) } };
        xcb_fill_poly(pctx.conn, pctx.window, pctx.point,
                      XCB_POLY_SHAPE_CONVEX,
                      XCB_COORD_MODE_PREVIOUS, ITEMS(points));
        if (point_size2)   // hollow
           {
             const int16_t half2 = point_size2 >> 1;
             const xcb_point_t points2[4] =      {
                   { I1616(xy.x, xy.y - half2) },
                   { I1616( half2,  half2)     },
                   { I1616(-half2,  half2)     },
                   { I1616(-half2, -half2)     } };
             set_GC_foreground(pctx, pctx.point, canvas_color);
             xcb_fill_poly(pctx.conn, pctx.window, pctx.point,
                           XCB_POLY_SHAPE_CONVEX,
                           XCB_COORD_MODE_PREVIOUS,
                           ITEMS(points2));
             set_GC_foreground(pctx, pctx.point, point_color);
           }
      }
   else if (point_style == 5)   // square
      {
        const xcb_point_t points[4] =   {
              { I1616(xy.x - half, xy.y - half) },
              { I1616(point_size, 0)  },
              { I1616(0, point_size)  },
              { I1616(-point_size, 0) } };
        xcb_fill_poly(pctx.conn, pctx.window, pctx.point,
                      XCB_POLY_SHAPE_CONVEX,
                      XCB_COORD_MODE_PREVIOUS, ITEMS(points));
        if (point_size2)   // hollow
           {
             const int16_t half2 = point_size2 >> 1;
             const xcb_point_t points2[4] =
                 { { int16_t(xy.x - half2), int16_t(xy.y - half2) },
                   { int16_t(point_size2), 0  },
                   { 0, int16_t(point_size2)  },
                   { int16_t(-point_size2), 0 } };
             set_GC_foreground(pctx, pctx.point, canvas_color);
             xcb_fill_poly(pctx.conn, pctx.window, pctx.point,
                           XCB_POLY_SHAPE_CONVEX,
                           XCB_COORD_MODE_PREVIOUS, ITEMS(points2));
             set_GC_foreground(pctx, pctx.point, point_color);
           }
      }

   // otherwise: draw no point
}
//----------------------------------------------------------------------------
/// draw the legend
void
draw_legend(const XCB_context & pctx, const Plot_window_properties & w_props)
{
Plot_line_properties const * const * l_props = w_props.get_line_properties();
const int line_count = w_props.get_line_count();

   // estimate the box size from the length of the longest legend string
   //
const char * longest_legend = "";
   for (int l = 0; l < line_count; ++l)
       {
         const char * lstr = l_props[l]->get_legend_name().c_str();
         if (strlen(longest_legend) < strlen(lstr))   longest_legend = lstr;
       }

string_width_height longest_size(pctx, longest_legend);

const Color canvas_color = w_props.get_canvas_color();

   // every legend looks like:   ---o---  legend_name
   //                            |  |  |  |
   //                           x0 x1 x2 xt
   //
const int x0 = w_props.get_legend_X();               // left of    --o--
const int x1 = x0 + 30;                              // point o in --o--
const int x2 = x1 + 30;                              // end of     --o--
const int xt = x2 + 10;                              // text after --o--

const int y0 = w_props.get_legend_Y();               // y of first legends
const int dy = w_props.get_legend_dY();

   // draw legend background
   {
     set_GC_foreground(pctx, pctx.fill, canvas_color);
     xcb_rectangle_t rect = { int16_t(x0 - 10),
                              int16_t(y0 -  8),
                              uint16_t(20 + xt + longest_size.width - x0),
                              uint16_t(dy*line_count) };
     xcb_poly_fill_rectangle(pctx.conn, pctx.window, pctx.fill, 1, &rect);
     xcb_flush(pctx.conn);
   }

   for (int l = 0; l_props[l]; ++l)
       {
         const Plot_line_properties & lp = *l_props[l];
         const Pixel_Y y1 = y0 + l*dy;

         set_GC_foreground(pctx, pctx.line, lp.get_line_color());
         set_GC_line_width(pctx, pctx.line, lp.get_line_width());
         draw_line(pctx, pctx.line, Pixel_XY(x0, y1), Pixel_XY(x2, y1));
         draw_text(pctx, lp.get_legend_name().c_str(), Pixel_XY(xt, y1 + 5));

         set_GC_foreground(pctx, pctx.point, canvas_color);
         set_GC_line_width(pctx, pctx.point, lp.get_point_size());
         draw_point(pctx, Pixel_XY(x1, y1), canvas_color, *l_props[l]);
       }
     xcb_flush(pctx.conn);
}
//----------------------------------------------------------------------------
/// draw the (vertical) grid lines that start at the X axis
void
draw_X_grid(const XCB_context & pctx, const Plot_window_properties & w_props,
            bool surface)
{
   set_GC_foreground(pctx, pctx.line, w_props.get_gridX_color());
   set_GC_line_width(pctx, pctx.line, w_props.get_gridX_line_width());

const Pixel_Y py0 = w_props.valY2pixel(0);
const double dy = w_props.get_max_Y() - w_props.get_min_Y();
const Pixel_Y py1 = w_props.valY2pixel(dy);
   for (int ix = 0; ix <= w_props.get_gridX_last(); ++ix)
       {
         const double v = w_props.get_min_X() +
                          ix*w_props.get_value_per_tile_X();
         const int px0 = w_props.valX2pixel(v - w_props.get_min_X())
                      + w_props.get_origin_X();
         if (ix == 0 || ix == w_props.get_gridX_last() ||
             w_props.get_gridX_style() == 1)
            {
              draw_line(pctx, pctx.line, Pixel_XY(px0, py0),
                                         Pixel_XY(px0, py1));
            }
         else if (w_props.get_gridX_style() == 2)
            {
              draw_line(pctx, pctx.line, Pixel_XY(px0, py0 - 5),
                                         Pixel_XY(px0, py0 + 5));
            }

         char * cc = format_tick(v);
         string_width_height wh(pctx, cc);
        if (surface)
           {
              const Pixel_X px2 = px0 - w_props.get_origin_X();
              const Pixel_Y py2 = py0 + w_props.get_origin_Y();
              draw_line(pctx, pctx.line, Pixel_XY(px0, py0),
                                         Pixel_XY(px2, py2));

              draw_text(pctx, cc,
                        Pixel_XY(px0 - wh.width/2 - w_props.get_origin_X(),
                                 py0 + wh.height + 3+ w_props.get_origin_Y()));
           }
        else
           {
             draw_text(pctx, cc,
                       Pixel_XY(px0 - wh.width/2, py0 + wh.height + 3));
           }
       }
}
//----------------------------------------------------------------------------
/// draw the (horizontal) grid lines that start at the Y axis
void
draw_Y_grid(const XCB_context & pctx, const Plot_window_properties & w_props,
            bool surface)
{
   set_GC_foreground(pctx, pctx.line, w_props.get_gridY_color());
   set_GC_line_width(pctx, pctx.line, w_props.get_gridY_line_width());

const Pixel_X px0 = w_props.valX2pixel(0) + w_props.get_origin_X();
const double dx = w_props.get_max_X() - w_props.get_min_X();
const Pixel_X px1 = w_props.valX2pixel(dx) + w_props.get_origin_X();
   for (int iy = 0; iy <= w_props.get_gridY_last(); ++iy)
       {
         const double v = w_props.get_min_Y() +
                          iy*w_props.get_value_per_tile_Y();
         const Pixel_Y py0 = w_props.valY2pixel(v - w_props.get_min_Y());
         if (iy == 0 || iy == w_props.get_gridY_last() ||
               w_props.get_gridY_style() == 1)
            {
              draw_line(pctx, pctx.line, Pixel_XY(px0, py0),
                                         Pixel_XY(px1, py0));
            }
         else if (w_props.get_gridY_style() == 2)
            {
              draw_line(pctx, pctx.line, Pixel_XY(px0 - 5, py0),
                                         Pixel_XY(px0 + 5, py0));
            }

        char * cc = format_tick(v);
        string_width_height wh(pctx, cc);
        if (surface)
           {
              const Pixel_X px2 = px0 - w_props.get_origin_X();
              const Pixel_Y py2 = py0 + w_props.get_origin_Y();
              draw_line(pctx, pctx.line, Pixel_XY(px0, py0),
                                         Pixel_XY(px2, py2));

             draw_text(pctx, cc,
                       Pixel_XY(px0 - wh.width - 5 - w_props.get_origin_X(),
                             py0 + wh.height/2 - 1 + w_props.get_origin_Y()));
           }
        else
           {
             draw_text(pctx, cc,
                       Pixel_XY(px0 - wh.width - 5,
                                py0 + wh.height/2 - 1));
           }
       }

   if (!surface)   return;
}
//----------------------------------------------------------------------------
/// draw the grid lines that start at the Z axis
void
draw_Z_grid(const XCB_context & pctx, const Plot_window_properties & w_props)
{
   set_GC_foreground(pctx, pctx.line, w_props.get_gridZ_color());
   set_GC_line_width(pctx, pctx.line, w_props.get_gridZ_line_width());

const int iz_max = w_props.get_gridZ_last();
const Pixel_X len_Zx = w_props.get_origin_X();
const Pixel_Y len_Zy = w_props.get_origin_Y();
const Pixel_XY orig = w_props.valXYZ2pixelXY(w_props.get_min_X(),
                                             w_props.get_min_Y(),
                                             w_props.get_min_Z());
const Pixel_X len_X = w_props.valX2pixel(w_props.get_max_X())
                    - w_props.valX2pixel(w_props.get_min_X());
const Pixel_Y len_Y = w_props.valY2pixel(w_props.get_max_Y())
                    - w_props.valY2pixel(w_props.get_min_Y());
   for (int iz = 1; iz <= iz_max; ++iz)
       {
         const double v = w_props.get_min_Z()
                        + iz*w_props.get_value_per_tile_Z();
         const Pixel_X px0 = orig.x - iz * len_Zx / iz_max;
         const Pixel_Y py0 = orig.y + iz * len_Zy / iz_max;
         const Pixel_X px1 = px0 + len_X;
         draw_line(pctx, pctx.line, Pixel_XY(px0, py0),
                                    Pixel_XY(px1,  py0));
         draw_line(pctx, pctx.line, Pixel_XY(px0, py0),
                                    Pixel_XY(px0,  py0 + len_Y));

        char * cc = format_tick(v);
        string_width_height wh(pctx, cc);

        draw_text(pctx, cc, Pixel_XY(px1 + 10, py0 + wh.height/2 - 1));
       }
}
//----------------------------------------------------------------------------
/// draw the plot lines
void
draw_plot_lines(const XCB_context & pctx,
                const Plot_window_properties & w_props, const Plot_data & data)
{
const Color canvas_color = w_props.get_canvas_color();
Plot_line_properties const * const * l_props = w_props.get_line_properties();

   loop(l, data.get_row_count())
       {
         const Plot_line_properties & lp = *l_props[l];
         set_GC_foreground(pctx, pctx.line, lp.get_line_color());
         set_GC_line_width(pctx, pctx.line, lp.get_line_width());

         // draw lines between points
         //
         Pixel_XY last(0, 0);
         loop(n, data[l].get_N())
             {
               double vx, vy;   data.get_XY(vx, vy, l, n);
               const Pixel_XY P(w_props.valX2pixel(vx - w_props.get_min_X())
                                + w_props.get_origin_X(),
                                w_props.valY2pixel(vy - w_props.get_min_Y()));
               if (n)   draw_line(pctx, pctx.line, last, P);
               last = P;
             }

         // draw points...
         //
         set_GC_foreground(pctx, pctx.point, lp.get_point_color());
         set_GC_line_width(pctx, pctx.point, lp.get_line_width());
         loop(n, data[l].get_N())
             {
               double vx, vy;   data.get_XY(vx, vy, l, n);
               const Pixel_X px = w_props.valX2pixel(vx - w_props.get_min_X())
                                + w_props.get_origin_X();
               const Pixel_Y py = w_props.valY2pixel(vy - w_props.get_min_Y());
               draw_point(pctx, Pixel_XY(px, py), canvas_color, *l_props[l]);
             }
       }
}
//----------------------------------------------------------------------------
/// draw the surface plot lines
void
draw_surface_lines(const XCB_context & pctx,
                   const Plot_window_properties & w_props,
                   const Plot_data & data)
{
const Color canvas_color = w_props.get_canvas_color();
Plot_line_properties const * const * l_props = w_props.get_line_properties();
const Plot_line_properties & lp0 = *l_props[0];

   // 1. setup graphic contexts
   //
   set_GC_foreground(pctx, pctx.line, lp0.get_line_color());
   set_GC_line_width(pctx, pctx.line, lp0.get_line_width());

   set_GC_foreground(pctx, pctx.point, lp0.get_point_color());
   set_GC_line_width(pctx, pctx.point, lp0.get_point_size());

   // 2. draw areas between plot lines...
   //
const double Hmin = w_props.get_min_Y();
const double dH = w_props.get_max_Y() - Hmin;   // 100%
const int verbosity = w_props.get_verbosity();

   loop(row, data.get_row_count() - 1)
   loop(col, data[row].get_N() - 1)
       {
         if (verbosity & SHOW_DATA)
            CERR << "B[" << row << ";" << col << "]"
                 << " X=" << data.get_X(row, col)
                 << " Y=" << data.get_Y(row, col)
                 << " Z=" << data.get_Z(row, col) << endl;

         if (!w_props.get_gradient().size())   continue; // no gradient
//       if (row != 3)   continue;   // show only given row
//       if (col != 0)   continue;   // show only given column

         const double   X0 = data.get_X(row, col);
         const double   Y0 = data.get_Y(row, col);
         const double   Z0 = data.get_Z(row, col);
         const double   H0 = (Y0 - Hmin)/dH;
         const Pixel_XY P0 = w_props.valXYZ2pixelXY(X0, Y0, Z0);

         const double   X1 = data.get_X(row, col + 1);
         const double   Y1 = data.get_Y(row, col + 1);
         const double   Z1 = data.get_Z(row, col + 1);
         const double   H1 = (Y1 - Hmin)/dH;
         const Pixel_XY P1 = w_props.valXYZ2pixelXY(X1, Y1, Z1);

         const double   X2 = data.get_X(row + 1, col);
         const double   Y2 = data.get_Y(row + 1, col);
         const double   Z2 = data.get_Z(row + 1, col);
         const double   H2 = (Y2 - Hmin)/dH;
         const Pixel_XY P2 = w_props.valXYZ2pixelXY(X2, Y2, Z2);

         const double   X3 = data.get_X(row + 1, col + 1);
         const double   Y3 = data.get_Y(row + 1, col + 1);
         const double   Z3 = data.get_Z(row + 1, col + 1);
         const double   H3 = (Y3 - Hmin)/dH;
         const Pixel_XY P3 = w_props.valXYZ2pixelXY(X3, Y3, Z3);


         /* the surface has 4 points P0, P1, P2, and P3, but they are not
            necessarily coplanar. We could draw the surface in 2 ways:

            A. find the intersection PM of P0-P3 and P1-P2 on the bottom
               plane Y=0, interpolate the respective heights, and draw
               4 triangles from PM to each of the four edges of the square

            B. fold the surface at its shorter edge into 2 triangles that
               have the edge in common. (triangles are always coplanar).

            Option B seems to look better.
         */

#if 0
         /* Option A...

            interpolate middle point PM with height HM and split the
            square P0-P1-P2-P3 into 4 triangles with common point PM
            and edges P0-P1, P1-P3, P3-P2, and P2-P0 respectively.
         */
         const double beta = intersection(X0,Z0, X1,Z1, X2,Z2, X3,Z3);
         const double   XM = X1 + beta*(X2 - X1);
         const double   YM = Y1 + beta*(Y2 - Y1);
         const double   ZM = Z1 + beta*(Z2 - Z1);
         const double   HM = (YM - Hmin)/dH;
         const Pixel_XY PM = w_props.valXYZ2pixelXY(XM, YM, ZM);

         draw_triangle(pctx, verbosity, PM, HM, P0, H0, P1, H1);
         draw_triangle(pctx, verbosity, PM, HM, P1, H1, P3, H3);
         draw_triangle(pctx, verbosity, PM, HM, P3, H3, P2, H2);
         draw_triangle(pctx, verbosity, PM, HM, P2, H2, P0, H0);
#else
         /* Option B...

            fold surface-square along its shorter edge
         */

         if (P0.distance2(P3) < P1.distance2(P2))   // P0-P3 is shorter
            {
              draw_triangle(pctx, verbosity, P1, H1, P0, H0, P3, H3);
              draw_triangle(pctx, verbosity, P2, H2, P0, H0, P3, H3);
            }
         else                                       // P1-P2 is shorter
            {
              draw_triangle(pctx, verbosity, P0, H0, P1, H1, P2, H2);
              draw_triangle(pctx, verbosity, P3, H3, P1, H1, P2, H2);
            }
#endif
       }

   // 3. draw lines between plot points
   //
   loop(row, data.get_row_count() - 1)
   loop(col, data[row].get_N() - 1)
       {
         if (verbosity & SHOW_DATA)
            CERR << "data[" << row << "," << col << "]"
                 << " X=" << data.get_X(row, col)
                 << " Y=" << data.get_Y(row, col)
                 << " Z=" << data.get_Z(row, col) << endl;

         const double X0 = data.get_X(row, col);
         const double Y0 = data.get_Y(row, col);
         const double Z0 = data.get_Z(row, col);
         const Pixel_XY P0 = w_props.valXYZ2pixelXY(X0, Y0, Z0);

         const double X1 = data.get_X(row, col + 1);
         const double Y1 = data.get_Y(row, col + 1);
         const double Z1 = data.get_Z(row, col + 1);
         const Pixel_XY P1 = w_props.valXYZ2pixelXY(X1, Y1, Z1);

         const double X2 = data.get_X(row + 1, col);
         const double Y2 = data.get_Y(row + 1, col);
         const double Z2 = data.get_Z(row + 1, col);
         const Pixel_XY P2 = w_props.valXYZ2pixelXY(X2, Y2, Z2);

         const double X3 = data.get_X(row + 1, col + 1);
         const double Y3 = data.get_Y(row + 1, col + 1);
         const double Z3 = data.get_Z(row + 1, col + 1);
         const Pixel_XY P3 = w_props.valXYZ2pixelXY(X3, Y3, Z3);

#if 0
         // debug: plot points as vertical lines
         //
         const Pixel_XY xy0 = w_props.valXYZ2pixelXY(data.get_X(row, col),
                                                     w_props.get_min_Y(),
                                                     data.get_Z(row, col));
         draw_line(pctx, pctx.line, xy0, P0);
         continue;
#endif
         draw_line(pctx, pctx.line, P0, P1);
         if (row == (data.get_row_count() - 2))   // last row
            draw_line(pctx, pctx.line, P2, P3);

         draw_line(pctx, pctx.line, P0, P2);
         if (col == (data[row].get_N() - 2))   // last column
            draw_line(pctx, pctx.line, P1, P3);
       }

   // 4. draw plot points
   //
   loop(row, data.get_row_count())
   loop(col, data[row].get_N())
       {
         const Pixel_XY P0 = w_props.valXYZ2pixelXY(data.get_X(row, col),
                                                    data.get_Y(row, col),
                                                    data.get_Z(row, col));
         draw_point(pctx, P0, canvas_color, *l_props[0]);
       }
}
//----------------------------------------------------------------------------
/// draw the content of a plot window
void
do_plot(const XCB_context & pctx,
        const Plot_window_properties & w_props, const Plot_data & data)
{
   // draw the canvas background
   //
const Color canvas_color = w_props.get_canvas_color();
   {
     set_GC_attr(pctx, pctx. line, XCB_GC_FOREGROUND, canvas_color);
     xcb_rectangle_t rect = { 0, 0, w_props.get_window_width(),
                                    w_props.get_window_height() };
     xcb_poly_fill_rectangle(pctx.conn, pctx.window, pctx.line, 1, &rect);
   }

const bool surface = data.is_surface_plot();   // 2D or 3D plot ?

   // draw grid lines...
   //
   draw_Y_grid(pctx, w_props, surface);
   draw_X_grid(pctx, w_props, surface);
   if (surface)   draw_Z_grid(pctx, w_props);

   // draw plot lines...
   //
   if  (surface)   draw_surface_lines(pctx, w_props, data);
   else            draw_plot_lines(pctx, w_props, data);

   // draw legend...
   //
   draw_legend(pctx, w_props);
}
//----------------------------------------------------------------------------
/// obtain an ID for \b name
xcb_atom_t
get_atom_ID(xcb_connection_t * conn, int only_existing, const char * name)
{
xcb_intern_atom_cookie_t cookie = xcb_intern_atom(conn, only_existing,
                                                  strlen(name), name);
xcb_intern_atom_reply_t & reply = *xcb_intern_atom_reply(conn, cookie, 0);
   return reply.atom;
}
//----------------------------------------------------------------------------
/// save plot window to file named \b outfile
void
XCB_context::save_file(const char * outfile)
{
   // save the file only after the first EXPOSE
   //
   if (file_saved)   return;
   file_saved = true;

   // figure position and size...
   //
int x, y;   // position of the plot window in its parent
   {
     Window child;
     XTranslateCoordinates(display, window, screen->root,
                           0, 0, &x, &y, &child );

     /*
       XWindowAttributes xwa;
       XGetWindowAttributes(pctx.display, pctx.window, &xwa);
       CERR << "POS: " <<  (x - xwa.x) << ":" << (y - xwa.y) << endl;
     */
   }

Window geo_root;
int geo_x, geo_y;
unsigned int geo_w, geo_h;
unsigned int geo_border_w, geo_depth;
   if (XGetGeometry(display, window,
                    &geo_root, &geo_x, &geo_y, &geo_w, &geo_h,
                    &geo_border_w, &geo_depth))   // success
      {
        /*
        CERR <<                                             endl
             << "  POS:    "    << geo_x << ":" << geo_y << endl
             << "  SIZE: "      << geo_w << "x" << geo_h << endl
             << "  bw:   "      << geo_border_w          << endl
             << "  bits/pixel=" << geo_depth             << endl;
         */
      }
   else                                           // error
      {
        CERR << "  XGetGeometry() failed" << endl;
        MORE_ERROR() << "XGetGeometry() failed in save_file()";
        DOMAIN_ERROR;
      }

   // Note: the point geo_x:geo_y seems to be the top-left corner of the
   // plotarea.
   // Therefore we assume that the window border has a thickness of geo_x
   // left, right, and below the plotarea and a thickness of geo_y above
   // (caused by the window caption etc.).
   //
   // To save the window including its borders we have to start at point
   // (x - geo_x):(y - geo_y) and add the borders to width:height.
   //
const unsigned int width  = geo_x + geo_w + geo_x;
const unsigned int height = geo_y + geo_h + geo_x;
const unsigned long plane_mask = AllPlanes;
const int format = XYPixmap;
const int screen = XDefaultScreen(display);
XImage * image = XGetImage(display, XRootWindow(display, screen),
                            x - geo_x, y - geo_y,
                            width, height, plane_mask, format);

   if (image == 0)
      {
        MORE_ERROR() << "XGetImage() failed in save_file()";
        DOMAIN_ERROR;
      }

   // write .bmp file...
   //
UTF8_string outfile_utf8(outfile);
   if (!outfile_utf8.ends_with(".bmp"))   outfile_utf8.append_ASCII(".bmp");

   // writing the file takes a while, so let the user know.
   //
   CERR << "writing ⎕PLOT output file " << outfile_utf8 << "..." << endl;

   errno = 0;
FILE * bmp = fopen(outfile_utf8.c_str(), "w");
   if (bmp == 0)
      {
        CERR << "open " << outfile << ": " << strerror(errno) << endl;
        MORE_ERROR() << "save_file() failed: cannot open output file "
                     << outfile_utf8;
        DOMAIN_ERROR;
      }

   // compute some .bmp related sizes...
   //
const int pixel_count = width * height;

   enum
      {
        file_header_size = 14,
        dib_header_size  = 40,
        pixel_offset     = file_header_size + dib_header_size
      };

const int part_DWORD = (width*3) & 3;        // bytes in final scanline DWORD
const int pad_DWORD  = 3 & (4-part_DWORD);   // pad bytes per scanline
const int pad_bytes = height*pad_DWORD;
const int file_size = file_header_size
                    + dib_header_size
                    + 3*pixel_count
                    + pad_bytes;

int file_bytes = 0;
   {
     const uint8_t file_header[file_header_size] =
        {
          uint8_t('B'),
          uint8_t('M'),
          LE4(file_size),
          LE4(0),              // reserved
          LE4(pixel_offset),   // offset
       };
     file_bytes += fwrite(file_header, 1, sizeof(file_header), bmp);
   };

   {
     const uint8_t dib_header[dib_header_size] =
        {
          LE4(dib_header_size),
          LE4(width),
          LE4(height),
          LE2(1),    // number of planes
          LE2(24),   // bits per pixel
          LE4(0),    // no compression
          LE4(0),    // image size (0 if no compression)
          LE4(0),    // Xpixels per meter
          LE4(0),    // Ypixels per meter
          LE4(0),    // colors used
          LE4(0),    // importantcolors
        };
     file_bytes += fwrite(dib_header, 1, sizeof(dib_header), bmp);
   }

const Colormap cmap = XDefaultColormap(display, screen);
   for (int y = height - 1; y >= 0; --y)
       {
         uint8_t scanline[3*width + 10];
         uint8_t * s = scanline;
         for (unsigned int x = 0; x < width; ++x)
             {
               XColor c;   c.pixel = XGetPixel(image, x, y);
               XQueryColor(display, cmap, &c);
               *s++ = uint8_t(c.blue /256);
               *s++ = uint8_t(c.green/256);
               *s++ = uint8_t(c.red  /256);
             }
          while ((s - scanline) & 3)   *s++ = 0;   // scanline padding
          file_bytes += fwrite(scanline, 1, s - scanline, bmp);
       }

   XFree(image);
   fclose(bmp);
   CERR << "output file " << outfile_utf8 << " written" << endl;
}
//----------------------------------------------------------------------------
void *
Quad_PLOT::plot_main_XCB(void * vp_props)
{
   setlocale(LC_ALL, "C");   // needed for portable snprintf()

Plot_window_properties & w_props =
      *reinterpret_cast<Plot_window_properties *>(vp_props);
const char * outfile = strdup(w_props.get_output_filename().c_str());

const Quad_PLOT::Handle handle = Quad_PLOT::next_handle;
XCB_context pctx(w_props, handle);
   Quad_PLOT::all_PLOT_windows.push_back(&pctx);

const int off = 50*handle;

const Plot_data & data = w_props.get_plot_data();

   // open a connection to the X server
   //
   pctx.display = XOpenDisplay(0);
# if XCB_WINDOWS_WITH_UTF8_CAPTIONS
   pctx.conn = XGetXCBConnection(pctx.display);

# else   // not XCB_WINDOWS_WITH_UTF8_CAPTIONS
   pctx.conn = xcb_connect(0, 0);
# endif  // XCB_WINDOWS_WITH_UTF8_CAPTIONS

   if (pctx.conn == 0 || xcb_connection_has_error(pctx.conn))
      {
        xcb_disconnect(pctx.conn);
        MORE_ERROR() << "could not connect to the X-server ";
        DOMAIN_ERROR;
      }

   // get the first screen
   //
const xcb_setup_t * setup = xcb_get_setup(pctx.conn);
   pctx.screen = xcb_setup_roots_iterator(setup).data;

   // create a window
   //
   pctx.window = xcb_generate_id(pctx.conn);
   enum { mask = XCB_CW_BACK_PIXEL | XCB_CW_EVENT_MASK};
   const uint32_t values[] = { w_props.get_canvas_color(),
                               XCB_EVENT_MASK_EXPOSURE
                             | XCB_EVENT_MASK_PROPERTY_CHANGE
                             | XCB_EVENT_MASK_STRUCTURE_NOTIFY };

     xcb_create_window(pctx.conn,                       // connection
                       XCB_COPY_FROM_PARENT,            // depth
                       pctx.window,                     // window Id
                       pctx.screen->root,               // parent window
                       w_props.get_pw_pos_X() + off,    // X position on screen
                       w_props.get_pw_pos_Y() + off,    // Y position on screen
                       w_props.get_window_width(),
                       w_props.get_window_height(),
                       w_props.get_border_width(),
                       XCB_WINDOW_CLASS_INPUT_OUTPUT,   // class
                       pctx.screen->root_visual,        // visual
                       mask, values);                   // mask and values

   // create some graphic contexts
   //
   pctx.fill  = xcb_generate_id(pctx.conn);
   pctx.line  = xcb_generate_id(pctx.conn);
   pctx.point = xcb_generate_id(pctx.conn);
   pctx.font  = xcb_generate_id(pctx.conn);
   pctx.text = setup_font_gc(pctx, "fixed", w_props.get_canvas_color());

   {
     enum { mask1 = XCB_GC_GRAPHICS_EXPOSURES,

            mask2 = XCB_GC_LINE_WIDTH
                  | XCB_GC_GRAPHICS_EXPOSURES
          };
     const uint32_t values1[] = { 0    // XCB_GC_GRAPHICS_EXPOSURES
                                };

     const uint32_t values2[] = { 2,   // XCB_GC_LINE_WIDTH
                                  0    // XCB_GC_GRAPHICS_EXPOSURES
                                };

     xcb_create_gc(pctx.conn, pctx.point,pctx.screen->root, mask1, values1);
     xcb_create_gc(pctx.conn, pctx.line, pctx.screen->root, mask1, values1);
     xcb_create_gc(pctx.conn, pctx.fill, pctx.screen->root, mask2, values2);
   }

   if (w_props.get_user_caption())
      {
        pctx.caption = strdup(w_props.get_caption().c_str());
      }
   else   // default caption: ⎕PLOT
      {
       char cc[50];
       SPRINTF(cc, "%s %d", w_props.get_caption().c_str(), handle);
       pctx.caption = strdup(cc);
      }

   {
# if XCB_WINDOWS_WITH_UTF8_CAPTIONS

// size (and position) hints for the window manager
XSizeHints size_hints;
   memset(&size_hints, 0, sizeof(XSizeHints));
   size_hints.flags       = USPosition | PPosition;
   size_hints.x           = w_props.get_pw_pos_X();
   size_hints.base_width  = w_props.get_pw_pos_X();
   size_hints.y           = w_props.get_pw_pos_Y();
   size_hints.base_height = w_props.get_pw_pos_Y();

   Xutf8SetWMProperties(pctx.display, pctx.window, pctx.caption,
                        "icon", 0, 0, &size_hints, 0, 0);

# else   // not XCB_WINDOWS_WITH_UTF8_CAPTIONS

   xcb_change_property(pctx.conn, XCB_PROP_MODE_REPLACE, pctx.window,
                       XCB_ATOM_WM_NAME, XCB_ATOM_STRING, 8,
                       strlen(pctx.caption), pctx.caption);

# endif  // XCB_WINDOWS_WITH_UTF8_CAPTIONS

   }

   // tell the window manager that we will handle WM_DELETE_WINDOW events...
   //
const xcb_atom_t window_deleted = get_atom_ID(pctx.conn, 0, "WM_DELETE_WINDOW");
const xcb_atom_t WM_protocols   = get_atom_ID(pctx.conn, 1, "WM_PROTOCOLS");
   xcb_change_property(pctx.conn, XCB_PROP_MODE_REPLACE, pctx.window,
                        WM_protocols, XCB_ATOM_ATOM,
                        /* bits */ 32, 1, &window_deleted);

   // remember who has the focus before mapping a new window
   //
const xcb_get_input_focus_reply_t * focusReply =
   xcb_get_input_focus_reply(pctx.conn, xcb_get_input_focus(pctx.conn), 0);

   // map the window on the screen and flush
   //
   xcb_map_window(pctx.conn, pctx.window);
   xcb_flush(pctx.conn);

   // X main loop
   //
   while (pctx.window_goon)
      {
        union
           {
             void                         * vp;
             xcb_generic_event_t          * generic_event_t;
             xcb_request_error_t          * request_error_t;
             xcb_configure_notify_event_t * configure_notify_event_t;
             xcb_property_notify_event_t  * property_notify_event_t;
             xcb_client_message_event_t   * client_message_event_t;
           } event;
        event.generic_event_t = xcb_poll_for_event(pctx.conn);
        if (event.vp == 0)   // nothing happened
           {
             if (pctx.window_goon)   usleep(200000);
             continue;
           }

        switch(event.generic_event_t->response_type & ~0x80)
           {
             case 0:   // error
                  {
                    const xcb_request_error_t * e = event.request_error_t;
                    const int error_code = event.request_error_t->error_code;
                    CERR << "\n*** X11 ERROR " << error_code             << endl
                         << "    major_opcode: " << int(e->major_opcode) << endl
                         << "    minor_opcode: " << int(e->minor_opcode) << endl
                         << "    bad_value:    " << HEX(e->bad_value) << endl
                         << endl;
                  }
                     break;

             case XCB_EXPOSE:             // 12
                  if (w_props.get_verbosity() & SHOW_EVENTS)
                     CERR << "\n*** XCB_EXPOSE " << endl;
                  do_plot(pctx, w_props, data);
                  xcb_flush(pctx.conn);

                  if (outfile && *outfile)
                     {
                       pctx.save_file(outfile);
                       if (w_props.get_auto_close())
                          {
                            // unleash the APL interpreter
                            sem_post(Quad_PLOT::expose_sema);
                            pctx.window_goon = false;
                            continue;
                          }
                     }

                  if (focusReply)
                     {
                       /* this is the first XCB_EXPOSE event (since we clear
                          focusReply below). X has moved the focus away from
                          our caller (the window that is running the GNU APL
                          interpreter) to the newly created ⎕PLOT window.

                          This focus change is rather annoying, and to fix it
                          we move the focus back to the APL interpreter

                          Return the focus to the window from which we have
                          stolen it.
                        */
                       xcb_set_input_focus(pctx.conn, XCB_INPUT_FOCUS_PARENT,
                                           focusReply->focus, XCB_CURRENT_TIME);
                       focusReply = 0;
                    }
                  xcb_flush(pctx.conn);
                  sem_post(Quad_PLOT::expose_sema);   // unleash the interpreter
                  break;

             case XCB_UNMAP_NOTIFY:       // 18
                  if (w_props.get_verbosity() & SHOW_EVENTS)
                     CERR << "\n*** XCB_UNMAP_NOTIFY ***\n";
                  break;

             case XCB_MAP_NOTIFY:         // 19
                  if (w_props.get_verbosity() & SHOW_EVENTS)
                     CERR << "\n*** XCB_MAP_NOTIFY ***\n";
                  break;

             case XCB_REPARENT_NOTIFY:    // 21
                  break;

             case XCB_CONFIGURE_NOTIFY:   // 22
                  if (focusReply)   break;   // not yet exposed

                  // XCB_EXPOSE has occurred.
                  {
                    const xcb_configure_notify_event_t * notify =
                          event.configure_notify_event_t;
                    w_props.set_window_size(notify->width, notify->height);
                  }
             break;

             case XCB_PROPERTY_NOTIFY:     // 28
                  {
                    const xcb_property_notify_event_t * notify =
                          event.property_notify_event_t;
                    if (w_props.get_verbosity() & SHOW_EVENTS)
                       CERR << "\n*** XCB_PROPERTY_NOTIFY"
                               " atom=" << int(notify->atom) <<
                               ", state=" << int(notify->state) << " ***\n";
                  }
             break;

             case XCB_CLIENT_MESSAGE:     // 33
                  if (event.client_message_event_t->data.data32[0]
                      == window_deleted)
                     {
                       // CERR << "Killed!" << endl;
                       free(event.vp);

                       // make this thread a zombie
                       //
                       sem_wait(Quad_PLOT::all_PLOT_windows_sema);
                          const int count = Quad_PLOT::all_PLOT_windows.size();
                          const pthread_t thread = pthread_self();
                          loop(pt, count)
                              if (all_PLOT_windows[pt]->get_thread() == thread)
                                 {
                                   Quad_PLOT::all_PLOT_windows[pt] =
                                      Quad_PLOT::all_PLOT_windows[count - 1];
                                   Quad_PLOT::all_PLOT_windows.pop_back();
                                   break;
                                 }
                       sem_post(Quad_PLOT::all_PLOT_windows_sema);

                       pctx.window_goon = false;   // break the outer for ()
                       continue;   // for ()
                     }
                  break;   // switch()

             default:
                if (w_props.get_verbosity() & SHOW_EVENTS)
                   CERR << "unexpected/unsupported event type "
                        << int(event.generic_event_t->response_type)
                         << " (ignored)" << endl;
           }

        free(event.vp);
      }

   // at this point the plot window was closed

   // remove the APL handle for this window
   //
   Quad_PLOT::PLOT_context::remove_handle(pctx.handle);
   if (Quad_PLOT::all_PLOT_windows.size() == 0)   Quad_PLOT::next_handle = 0;

   // free the text_gc
   //
   testCookie(xcb_free_gc(pctx.conn, pctx.text), pctx.conn, "can't free gc");

   // close font
   {
     xcb_void_cookie_t cookie = xcb_close_font_checked(pctx.conn, pctx.font);
     testCookie(cookie, pctx.conn, "can't close font");
   }

   xcb_disconnect(pctx.conn);
   free(const_cast<char *>(outfile));

   return 0;
}
// ===========================================================================

#endif // HAVE_LIBX11 && HAVE_LIBXCB && HAVE_LIBX11_XCB && HAVE_X11_XLIB_XCB_H

