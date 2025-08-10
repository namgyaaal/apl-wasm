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

#ifndef __NAMED_OBJECT_HH_DEFINED__
#define __NAMED_OBJECT_HH_DEFINED__

#include "Id.hh"
#include "Value.hh"

class Function;
class Symbol;
class UCS_string;
class Value;

//----------------------------------------------------------------------------
/// The possible values returned by \b ⎕NC.
enum NameClass
{
  NC_INVALID          =  0x0100,   ///< invalid name class.
  NC_UNUSED_USER_NAME =  0x0200,   ///< unused user name, not yet assigned
  NC_LABEL            =  0x0401,   ///< Label.
  NC_VARIABLE         =  0x0802,   ///< (assigned) variable.
  NC_FUNCTION         =  0x1003,   ///< (defined) function.
  NC_OPERATOR         =  0x2004,   ///< (defined) operator.
  NC_SYSTEM_VAR       =  0x4005,   ///< system variable.
  NC_SYSTEM_FUN       =  0x8006,   ///< system function.
  NC_case_mask        =  0x00FF,   ///< almost ⎕NC
  NC_bool_mask        =  0xFF00,   ///< for fast selection

  /// name of a function or operator
  NC_FUN_OPER         = (NC_FUNCTION | NC_OPERATOR) & NC_bool_mask,

   // NC class bitmaps...

  /// name allowed left of ←
  NC_left             = (NC_VARIABLE         |
                         NC_UNUSED_USER_NAME |
                         NC_SYSTEM_VAR       |
                         NC_INVALID          //  ⎕, ⍞, ⎕xx
                        ) & NC_bool_mask,

  /// name of a variable
  NC_variables        = (NC_VARIABLE | NC_SYSTEM_VAR)       & NC_bool_mask,

  /// name that has a value
  NC_values           = (NC_LABEL    | NC_variables)        & NC_bool_mask,

  /// name of a function
  NC_functions        = (NC_FUNCTION | NC_SYSTEM_FUN)       & NC_bool_mask,

  /// name whose class may be changed
  NC_unknown          = (NC_INVALID  | NC_UNUSED_USER_NAME) & NC_bool_mask,
};
//----------------------------------------------------------------------------
/**
 A named object is something with a name.
 The name can be user defined or system defined.

  User define names are handled by class Symbol, while
   system defined names are handled by class Id.

  User define names are used for (user-defined) variables, functions,
  or operators.

   System names are used by system variables and functions (⎕xx) and
   for primitive functions and operators.
 **/
/// A user-defined variable or function
class NamedObject
{
public:
   /// constructor from Id
   NamedObject(Id i)
   : id(i)
   {}

   /// return the name of the named object
   virtual UCS_string get_name() const
      { return ID::get_name_UCS(id); }

   /// return the function for this Id (if any) or 0 if this Id does
   /// (currently) represent a function.
   virtual const Function * get_function() const  { return 0; }

   /// return the symbol for this user defined symbol (if any) or 0 if this Id
   /// refers to a system name
   virtual Symbol * get_symbol()   { return 0; }

   /// return the symbol for this user defined symbol (if any) or 0 if this Id
   /// refers to a system name
   virtual const Symbol * get_symbol() const  { return 0; }

   /// return the Id of this object (ID_USER_SYMBOL for user defined objects)
   Id get_Id() const
      { return id; }

   /// return true, iff this object is user-defined
   bool is_user_defined() const
      { return id == ID_USER_SYMBOL; }

   /// Get current \b NameClass of \b this name.
   NameClass get_NC() const;

   /// the object's id
   const Id id;
};

#endif // __NAMED_OBJECT_HH_DEFINED__
