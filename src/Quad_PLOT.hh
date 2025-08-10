/*
    This file is part of GNU APL, a free implementation of the
    ISO/IEC Standard 13751, "Programming Language APL, Extended"

    Copyright © 2018-2019  Dr. Jürgen Sauermann

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

#ifndef __Quad_PLOT_DEFINED__
#define __Quad_PLOT_DEFINED__

#include <math.h>
#include <pthread.h>
#include <semaphore.h>

#include "QuadFunction.hh"
#include "Value.hh"

class Plot_window_properties;
class Plot_data;

/// ⎕PLOT verbosity bitmap
enum
{
   SHOW_EVENTS = 1,   ///< show X events
   SHOW_DATA   = 2,   ///< show APL data
   SHOW_DRAW   = 4,   ///< show draw details
};

/// a shrt delay needed when the user wants window borders to be saved
//  into a .png file
enum { SAVE_BORDER_DELAY_ms = 100 };

/// The class implementing ⎕PLOT
class Quad_PLOT : public QuadFunction
{
public:
   /// Constructor.
   Quad_PLOT();

   static Quad_PLOT  fun;          ///< Built-in function.

   /// a semaphore blocking until the plot window has been EXPOSED
   static sem_t * expose_sema;

   /// a semaphore protecting vector all_PLOT_windows
   static sem_t * all_PLOT_windows_sema;

   /// a small number that identifies a window
   typedef int Handle;

   /// the next handle (-1)
   static Handle next_handle;

   /// the GUI-independent part of ⎕PLOT (at APL level).
   class PLOT_context
      {
        public:
           /// constructor
           PLOT_context(Handle h)
           : handle(h)
           {}

           /// return the per-window thread (for GUIs that have one, i.e. XCB).
           virtual pthread_t get_thread() const
              { return 0; }

           /// close the plot window in the GUI
           virtual void plot_stop() = 0;

           /// destructor (shall clean up the GUI-dependent context)
           virtual ~PLOT_context() {}

           /// z unique number that identifies \b this PLOT_context (in
           /// \b all_PLOT_windows).
           const Handle handle;

           /// close \b hnadle from all_PLOT_windows
           static Handle remove_handle(Handle handle);   // GTK only
      };

   /// all open ⎕PLOT windows.
   static basic_string<PLOT_context *> all_PLOT_windows;

   /// the GTK window that handles one plot window. Always declared here
   /// (to make doxygen happy, but only implemented if apl_GTK3
   static void plot_main_GTK(void * vp_props, Handle handle);

   /// the pthread that handles all XCB plot windows. Always declared here
   /// (to make doxygen happy, but only implemented if NOT apl_GTK3
   static void * plot_main_XCB(void * vp_props);

   static int get_verbosity()
      { return verbosity; }

protected:
   /// Destructor.
   ~Quad_PLOT();

   /// overloaded Function::eval_AB()
   virtual Token eval_AB(Value_P A, Value_P B) const;

   /// overloaded Function::eval_B()
   virtual Token eval_B(Value_P B) const;

   /// control logging etc. of ⎕PLOT
   Value_P window_control(APL_Integer B) const;

   /// print attribute help text
   static void help();

   /// plot the data (creating a new plot window in X)
   static APL_Integer do_plot_data(Plot_window_properties * w_props,
                                   const Plot_data * data);

   /// close the plot window with \b handle (from APL)
   static Handle plot_stop_APL(Handle handle);

   /// initialize the data to be plotted
   static Plot_data * setup_data(const Value & B);

   /// initialize the data to be plotted for a 3D plot
   static Plot_data * setup_data_3D(const Value & B);

   /// initialize the data to be plotted for a 2D plot (except case 2b.)
   static Plot_data * setup_data_2D(const Value & B);

   /// initialize the data to be plotted for a 2D plot (case 2b.)
   static Plot_data * setup_data_2D_2b(const Value & B);

   /// parse the (all-optional) attributes in A
   static ErrorCode parse_attributes(const Value & A,
                                     Plot_window_properties * w_props);

   /// whether to print some debug info during plotting
   static int verbosity;
};

#endif // __Quad_PLOT_DEFINED__
