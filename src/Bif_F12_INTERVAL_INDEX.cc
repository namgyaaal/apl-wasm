/*
    This file is part of GNU APL, a free implementation of the
    ISO/IEC Standard 13751, "Programming Language APL, Extended"

    Copyright © 2020-2020  Dr. Jürgen Sauermann

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

#include "Bif_F12_INTERVAL_INDEX.hh"
#include "Workspace.hh"

Bif_F12_INTERVAL_INDEX Bif_F12_INTERVAL_INDEX::fun;    // ⍳

//============================================================================
/** return { (,⍵) / ,⍳⍴⍵ }. ⍸B is similar to ⍳B except that:
    * ⍸Z is a vector ⍴,B instead of an array with shape ⍴B, and'
    * the n_th index ist repeated (,B)[n] times instead of once (and
      therefore vanish if (,B)[n] is 0).

   (⍸S⍴1) ≡ ,⍳S  for all shapes S = ⍴ S1
 **/

Token
Bif_F12_INTERVAL_INDEX::eval_B(Value_P B) const
{
   // B must be boolean. Che that and count the number of 1s in B.
   //
const APL_Integer qio = Workspace::get_IO();
const ShapeItem ec_B = B->element_count();
ShapeItem count = 0;
   loop(b, ec_B)
       {
         const Cell & cell = B->get_cravel(b);
         if (!cell.is_near_int())
            {
              MORE_ERROR() << "non-integer item in the argument of monadic ⍸";
              DOMAIN_ERROR;
            }

          const APL_Integer Bi = B->get_cravel(b).get_near_int();
          if (Bi < 0)
            {
              MORE_ERROR() << "negative item in the argument of monadic ⍸";
              DOMAIN_ERROR;
            }

         count += Bi;
       }

Value_P Z(count, LOC);

const uRank rank = B->get_rank();
   loop(b, ec_B)
       {
         const Cell & cell = B->get_cravel(b);
         const APL_Integer Bi = cell.get_near_int();   // number of repetitions
         if (!Bi)   continue;   // nothing to do

        const Shape sh_b = B->get_shape().offset_to_index(b, qio);
        Assert(sh_b.get_rank() == rank);
         loop(rep, Bi)
             {
               if (rank <= 1)   // simple shape
                  {
                    Z->next_ravel_Int(b + qio);
                  }
               else             // nested shape
                  {
                     Value_P ZZ(rank, LOC);
                     loop(r, rank)
                         {
                           const ShapeItem sh_r = sh_b.get_shape_item(r);
                           ZZ->next_ravel_Int(sh_r);
                         }
                     ZZ->set_proto_Int();
                     ZZ->check_value(LOC);
                     Z->next_ravel_Pointer(ZZ.get());
                  }
             }
       }

   Z->set_proto_Int();
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
/// search elements of B in ntervals defined by A. ⍴Z is ⍴B, and elements of Z
/// are indices of A so that A[Z[N]] ≤ B[N] < A[Z[N] + 1]
Token
Bif_F12_INTERVAL_INDEX::eval_AB(Value_P A, Value_P B) const
{
  // A must be a non-empty sorted vector
  //
   if (A->get_rank() > 1)   RANK_ERROR;
const ShapeItem ec_A = A->element_count();
   if (ec_A < 1)
      {
        MORE_ERROR() << "the left argument of A ⍸ B is empty";
        LENGTH_ERROR;
      }

   for(ShapeItem a = 1; a < ec_A; ++a)
       {
         const Cell & c1 = A->get_cravel(a-1);
         const Cell & c2 = A->get_cravel(a);
         const Comp_result c1_c2 = c1.compare(c2);
         if (c1_c2 != COMP_LT)
            {
              MORE_ERROR() << "the left argument of A ⍸ B "
                              "is not sorted ascendingly";
              DOMAIN_ERROR;
            }
       }

   // from here on nothing can fail.
   //
Value_P Z(B->get_shape(), LOC);
const ShapeItem ec_B = B->element_count();
const Cell * cells_A = &A->get_cfirst();
const APL_Integer qio = Workspace::get_IO();

   loop(b, ec_B)
      {
        const ShapeItem z = find_range(B->get_cravel(b), cells_A, ec_A);
        Z->next_ravel_Int(z + qio);
      }

   Z->set_proto_Int();
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
ShapeItem
Bif_F12_INTERVAL_INDEX::find_range(const Cell & cell, const Cell * ranges,
                                   ShapeItem range_count)
{
   // first check if cell is below or above it
   //
   {
     const Comp_result c0 = cell.compare(ranges[0]);
     if (c0 == COMP_LT)   return -1;   // == 0 with ⎕IO = 1

     const Comp_result cN = cell.compare(ranges[range_count - 1]);
     if (cN != COMP_LT)   return range_count-1;
   }

   // divide and conquer the ranges
   //
ShapeItem ret = 0;
   while (range_count > 1)
         {
           const ShapeItem middle = range_count >> 1;
           const Comp_result cm = cell.compare(ranges[ret + middle]);
           if (cm == COMP_LT)   // cell is below the middle
              {
                range_count = middle;
              }
           else                 // cell is above the middle
              {
                ret         += middle;
                range_count -= middle;
              }
         }


   return ret;
}
//============================================================================

