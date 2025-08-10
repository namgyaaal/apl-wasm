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

#ifndef __BIF_OPER2_POWER_HH_DEFINED__
#define __BIF_OPER2_POWER_HH_DEFINED__

#include "PrimitiveOperator.hh"

//----------------------------------------------------------------------------
/** Primitive operator ⍣ (power)
 */
/// The class implementing ⍣
class Bif_OPER2_POWER : public PrimitiveOperator
{
public:
   /// constructor
   Bif_OPER2_POWER() : PrimitiveOperator(TOK_OPER2_POWER) {}

   /// overloaded Function::eval_ALRB()
   virtual Token eval_ALRB(Value_P A, Token & LO, Token & RO, Value_P B) const;

   /// overloaded Function::eval_LRB()
   virtual Token eval_LRB(Token & LO, Token & RO, Value_P B) const;

   /// overloaded Function::may_push_SI()
   virtual bool may_push_SI() const
      { return false; }

   /// eval_ALRB() or eval_LRB() for numeric RO (aka. Form 1)
   static Token eval_form_1(Value_P A, Token & LO, Value_P N, Value_P B);

   /// eval_ALRB() or eval_LRB() for numeric RO (aka. Form 1)
   static Token eval_form_2(Value_P A, Token & LO, Token & RO, Value_P B);

   static Bif_OPER2_POWER  fun;      ///< Built-in function

protected:
};
//----------------------------------------------------------------------------

#endif // __BIF_OPER2_POWER_HH_DEFINED__
