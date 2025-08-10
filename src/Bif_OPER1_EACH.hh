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

#ifndef __BIF_OPER1_EACH_HH_DEFINED__
#define __BIF_OPER1_EACH_HH_DEFINED__

#include "PrimitiveOperator.hh"

//----------------------------------------------------------------------------
/** Primitive operator ¨ (each).
 */
/// The class implementing ¨
class Bif_OPER1_EACH : public PrimitiveOperator
{
public:
   /// Constructor.
   Bif_OPER1_EACH() : PrimitiveOperator(TOK_OPER1_EACH) {}

   /// Overloaded Function::eval_LB().
   virtual Token eval_LB(Token & LO, Value_P B) const
      { return do_eval_LB(LO, B); }

   /// Overloaded Function::eval_ALB().
   virtual Token eval_ALB(Value_P A, Token & LO, Value_P B) const;

   static Bif_OPER1_EACH  fun;      ///< Built-in function.

   /// implementation of eval_LB()
   static Token do_eval_LB(Token & LO, Value_P B);

protected:
   /// overloaded Function::may_push_SI()
   virtual bool may_push_SI() const
      { return false; }
};
//----------------------------------------------------------------------------
#endif // __BIF_OPER1_EACH_HH_DEFINED__
