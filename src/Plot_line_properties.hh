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

#ifndef __PLOT_LINE_PROPERTIES_HH_DEFINED__
#define __PLOT_LINE_PROPERTIES_HH_DEFINED__

#include <iostream>
#include <stdint.h>
#include <string>
#include <Workspace.hh>

//=============================================================================
/// properties of a single plot line. The properties, their type, and their
/// default values are defined in file Quad_PLOT.def.

class Plot_line_properties
{
public:
   /** constructor. Set all property values to the defaults that are
       defined in Quad_PLOT.def, and then override those property that
       have different defaults for different lines.
   **/
   Plot_line_properties(int lnum) :
# define ldef(_ty,  na,  val, _descr) na(val),
# include "Quad_PLOT.def"
   line_number(lnum)
   {
     snprintf(legend_name_buffer, sizeof(legend_name_buffer),
              "Line-%d", int(lnum + Workspace::get_IO()));
     legend_name = legend_name_buffer;
   }

   // define the get_XXX() function for every attribute XXX that is
   // defined in Quad_PLOT.def...
   //
# define ldef(ty,  na,  _val, _descr)     \
   /** return the value of na **/         \
   ty get_ ## na() const   { return na; }
# include "Quad_PLOT.def"

   // define the set_XXX() function for every attribute XXX that is
   // defined in Quad_PLOT.def...
   //
# define ldef(ty,  na,  _val, _descr)     \
   /** set the  value of na **/           \
   void set_ ## na(ty val)   { na = val; }
# include "Quad_PLOT.def"

   /// print the line properties
   int print(std::ostream & out) const;

   /// return true if \b  matches \b attribute. For a match attribute is
   /// prefix, followed by optional whitespace, terminated by :, -, or space.
   static bool is_line_property(const char * prefix, const char * attribute);

protected:
# define ldef(ty,  na,  _val, descr) /** descr **/ ty na;
# include "Quad_PLOT.def"

  /// plot line number
  const int line_number;   // starting a 0 regardless of ⎕IO

  /// a buffer for creating a (default) legend name from a macro
  char legend_name_buffer[50];
};
//============================================================================

#endif // __PLOT_LINE_PROPERTIES_HH_DEFINED__

