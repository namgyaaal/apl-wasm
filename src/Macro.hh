/*
    This file is part of GNU APL, a free implementation of the
    ISO/IEC Standard 13751, "Programming Language APL, Extended"

    Copyright © 2008-2023  Dr. Jürgen Sauermann

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

#ifndef __MACRO_HH_DEFINED__
#define __MACRO_HH_DEFINED__

#include "UserFunction.hh"

//----------------------------------------------------------------------------
/// a system function or operator implemented as an internal defined function
class Macro : public UserFunction
{
public:
   /// the unique number of a macro
   enum Macro_num
      {
#define mac_def(name, _txt) MAC_ ## name,   ///< a Macro_num
#include "Macro.def"
        MAC_COUNT,
        MAC_NONE = -1
      };

   /// constructor
   Macro(Macro_num num, const UTF8_string & text);

   /// overloaded Function::is_macro()
   virtual bool is_macro() const   { return true; }

   /// overloaded UserFunction::get_macnum()
   virtual int get_macnum() const
      { return macro_number; }

   /// unmark all values used in the bodies of all macros
   static void unmark_all_macros();

   /// return the macro with \b macro_number num
   static Macro * get_macro(Macro_num num);

#define mac_def(name, _txt) static Macro name;   ///< a macro
#include "Macro.def"

   /// a vector of all macros
   static Macro * all_macros[MAC_COUNT];

protected:
   /// A (compile-time) unique number for this macro
   const Macro_num macro_number;

private:
   /// destructor (not supposed to be called)
   ~Macro();
};
//----------------------------------------------------------------------------

#endif // __MACRO_HH_DEFINED__

