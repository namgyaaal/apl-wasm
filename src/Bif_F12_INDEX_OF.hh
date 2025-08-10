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

#ifndef __Bif_F12_INDEX_OF_HH_DEFINED__
#define __Bif_F12_INDEX_OF_HH_DEFINED__

#include "PrimitiveFunction.hh"

//----------------------------------------------------------------------------
/** System function index of (⍳) */
/// The class implementing ⍳
class Bif_F12_INDEX_OF : public NonscalarFunction
{
public:
   /// Constructor
   Bif_F12_INDEX_OF()
   : NonscalarFunction(TOK_F12_INDEX_OF)
   {}

   /// overloaded Function::eval_B()
   virtual Token eval_B(Value_P B) const;

   /// overloaded Function::eval_AB()
   virtual Token eval_AB(Value_P A, Value_P B) const;

   static Bif_F12_INDEX_OF  fun;   ///< Built-in function

protected:
   /// find Cell B in the ravel A (of length len_A). Return the position
   /// (< len_A) if found, or len_A if not.
   static ShapeItem find_B_in_A(const Cell * A, ShapeItem len_A,
                         const Cell & cell_B, double qct)
      {
        loop(a, len_A)   if (cell_B.equal(A[a], qct))   return a;   // found
        return len_A;                                               // not found
      }

   /// find Cell B in value A. Return the position (< len_A) if found,
   /// or len_A if not. Idx_A is ⍋A ⊣ ⎕IO←0.
   static ShapeItem find_B_in_sorted_A(const Value & A,
                                       const vector<ShapeItem> & Idx_A,
                                       const Cell & cell_B, double qct);

   /// compare function for Heapsort<ShapeItem>::search<const Cell &>
   static int bs_cmp(const Cell & cell, const ShapeItem & A, const void * ctx);
};
//----------------------------------------------------------------------------

#endif // __Bif_F12_INDEX_OF_HH_DEFINED__


