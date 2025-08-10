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

#ifndef __ID_HH_DEFINED__
#define __ID_HH_DEFINED__

#include <iostream>

#include "IdEnums.hh"
#include "TokenEnums.hh"
#include "UTF8_string.hh"
#include "UCS_string.hh"

class Function;
class Symbol;
class UCS_string;

//----------------------------------------------------------------------------
/**
 An Identifier for each internal object (primitives, Quad-symbols, and more).
 The ID can be derived in different ways:

 1. from a (user0defined) name,       e.g.  ID_APL_VALUE or ID_CHARACTER
 2. from a distinguished var name,    e.g.  ID_Quad_AI or ID_Quad_AV
 3. from a distinguished fun name,    e.g.  ID_Quad_AT or ID_Quad_EM
 4. from a special token (apl symbol) e.g.  ID_L_PARENT1 or ID_ASSIGN

  This is controlled by 5 corresponding macros: pp() qv() qf() resp. st()
 */
/// Identifier of an internal object of the APL interpreter
class ID
{
public:
   /// return the printable name for id as UTF8 *
   static const UTF8 * get_name(Id id);

   /// return the printable name for id as UCS_string
   static UCS_string get_name_UCS(Id id);

   /// If \b id is the ID of primitive function, primitive operator, or
   /// quad function, then return a pointer to it. Otherwise return 0.
   static cFunction_P get_system_function(Id id);

   /// If \b tag is the tag of primitive function, primitive operator, or
   /// quad function, then return a pointer to it. Otherwise return 0.
   static cFunction_P get_system_function(TokenTag tag)
      { return get_system_function(Id(tag >> 16)); }

   /// If \b id is the ID of a quad variable, then return a pointer to its
   /// symbol. Otherwise return 0.
   static Symbol * get_system_variable(Id id);

   /// return the TokenTag for \b id
   static TokenTag get_token_tag(Id id);

   /// release UCS_strings with ID names
   static void cleanup();
};
//----------------------------------------------------------------------------

#include "TokenEnums.hh"

#endif // __ID_HH_DEFINED__

