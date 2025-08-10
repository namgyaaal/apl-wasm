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

#include "Bif_OPER1_COMMUTE.hh"
#include "DerivedFunction.hh"

Bif_OPER1_COMMUTE   Bif_OPER1_COMMUTE::fun;

//----------------------------------------------------------------------------
Token
Bif_OPER1_COMMUTE::eval_LB(Token & LO, Value_P B) const
{
   if (!LO.is_function())   SYNTAX_ERROR;
cFunction_P fun_LO = LO.get_function();

/*
   Under "normal" circumstances, eval_LB() is called with a dyadic function LO
   which is then called with value B as both left and right argument. I.e.

            LO ⍨ B      is:       B LO B   or
   e.g:     + ⍨ 1 2 3   is:   1 2 3 +  1 2 3

   Unfortunately a special case occurs when:

   1. LO is an operator, and
   2. LO accepts a value

   In that case one might expect (and e.g. Dyalog SPL does) that

   A  LO ⍨ B would be: B LO A because ⍨ should call LO with A and B exchanged.

   However, what actually happens is that A  LO ⍨ B is parsed as (parentheses
   added to show the binding strengths):

   ((A LO) ⍨) B

   The parser binds A to LO, creating a derived function, say A_LO.
   The parser then binds the derived function A_LO to operator ~ and
       creates another derived function, say A_LO_COMMUTE.
   Finally, A_LO_COMMUTE B is evaluated (monadically!) which brings us here.
   It should have come to Bif_OPER1_COMMUTE::eval_ALB() below, but it cannot
   see the left argument A anymore (which is hidden in the derived A_LO).

   In this special situation we dig into the (derived) argument LO (which is
    A_LO above) and split it into A and LO. LO is then called syadically with
    left (!) argument B and right (!) argument A.
*/

   if (fun_LO->is_derived())   // maybe special case
      {
        const DerivedFunction * A_LO =
                      reinterpret_cast<const DerivedFunction *>(fun_LO);
        Value_P    A = A_LO->get_bound_LO_value();
        if (+A)   // definitelt special case
           {
              cFunction_P LO = A_LO->get_OPER();
              return LO->eval_AB(B, A);
           }
      }

   // normal case
   //
   return fun_LO->eval_AB(B, B);
}
//----------------------------------------------------------------------------
Token
Bif_OPER1_COMMUTE::eval_LXB(Token & LO, Value_P X, Value_P B) const
{
   return LO.get_function()->eval_AXB(B, X, B);
}
//----------------------------------------------------------------------------
Token
Bif_OPER1_COMMUTE::eval_ALB(Value_P A, Token & LO, Value_P B) const
{
   return LO.get_function()->eval_AB(B, A);
}
//----------------------------------------------------------------------------
Token
Bif_OPER1_COMMUTE::eval_ALXB(Value_P A, Token & LO, Value_P X, Value_P B) const
{
   return LO.get_function()->eval_AXB(B, X, A);
}
//----------------------------------------------------------------------------
