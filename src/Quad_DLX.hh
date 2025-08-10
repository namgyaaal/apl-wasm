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

#ifndef __Quad_DLX_HH_DEFINED__
#define __Quad_DLX_HH_DEFINED__

#include "QuadFunction.hh"
#include "Value.hh"

//----------------------------------------------------------------------------
/**
   The system function ⎕DLX aka. Dancing Links or Algorithm X by D. Knuth 2000
 */
/// The class implementing ⎕DLX
class Quad_DLX : public QuadFunction
{
public:
   /// Constructor.
   Quad_DLX() : QuadFunction(TOK_Quad_DLX) {}

   static Quad_DLX  fun;          ///< Built-in function.

protected:
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const;

   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const;

   /// common part of eval_AB() and eval_B()
   static Token do_DLX(ShapeItem result_count, const Value & B);
};
//----------------------------------------------------------------------------

#endif // __Quad_DLX_HH_DEFINED__


