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

#include "Bif_F12_PARTITION_PICK.hh"
#include "Bif_OPER1_EACH.hh"
#include "Bif_F12_TAKE_DROP.hh"
#include "Workspace.hh"

// primitive function instances
//
Bif_F12_TAKE      Bif_F12_TAKE     ::fun;    // ↑
Bif_F12_DROP      Bif_F12_DROP     ::fun;    // ↓

//============================================================================
Value_P
Bif_F12_TAKE::first(const Value & B)
{
   /*
      lrm p. 131: ⍴ Z ← ↑ B depends on the shape of the first item.

      IBM APL2:
          A.  ↑'DO' 'RE' 'MI'   is: 'DO' with shape 2
          B.  ↑⊂'DO'       also is: 'DO' with shape 2

      Therefore: S ≡ ↑S ≡ ↑⊃S for simple scalars S,
            but: V ≢ ↑V ≡ ⊃V[⎕IO]

      We handle the nested first_B case first because all others return scalars
    */
const Cell & first_B = B.get_cfirst();
   if (first_B.is_pointer_cell())   // first item is nested
      {
        Value_P sub = first_B.get_pointer_value();
        return CLONE_P(sub, LOC);
      }

   if (B.element_count() ||      // the normal case,         e.g. Z ← ↑ 1 2 3
       first_B.is_lval_cell())   // selective specification, e.g. (↑ B) ← V
      {
        Value_P Z(LOC);   // the (always) scalar result of ↑B
        Z->next_ravel_Cell(first_B);
        Z->check_value(LOC);
        return Z;
      }

   // ↑ '' or ↑ ⍬ : return prototype
   //
   if (first_B.is_character_cell())   return CharScalar(UNI_SPACE, LOC);
   else                               return IntScalar(0, LOC);
}
//----------------------------------------------------------------------------
Token
Bif_F12_TAKE::eval_AXB(Value_P A, Value_P X, Value_P B) const
{
   if (A->get_rank() > 1)   RANK_ERROR;
   if (X->get_rank() > 1)   RANK_ERROR;

const ShapeItem len_A = A->element_count();
const ShapeItem len_X = X->element_count();
   if (len_A != len_X)   LENGTH_ERROR;

   if (len_X == 0)   // no axes
      {
        Token result(TOK_APL_VALUE1, CLONE_P(B, LOC));
        return result;
      }

   // construct the left argument of Bif_F12_TAKE::fill(). Start with ⍴B and
   // then replace corresponding shape items with A[X[x]].
   //
const APL_Integer qio = Workspace::get_IO();
const AxesBitmap axes_X = X->to_bitmap("A ↑[X] B", B->get_rank());
Shape sh_take = B->get_shape();   // start with ⍴B
   loop(x, len_X)                 // for exery axis X[x] in X
      {
        const APL_Integer axis = X->get_cravel(x).get_near_int() - qio;
        const APL_Integer alen = A->get_cravel(x).get_near_int();
        sh_take.set_shape_item(axis, alen);
      }

   return Token(TOK_APL_VALUE1, do_take(sh_take, *B, axes_X));
}
//----------------------------------------------------------------------------
Token
Bif_F12_TAKE::eval_XB(Value_P X, Value_P B) const
{
   // ↑[X] B    ←→ ((⍴X)⍴1)↑[X] B   (GNU APL only)
   if (X->get_rank() > 1)   AXIS_ERROR;

const AxesBitmap axes_X = X->to_bitmap("↑[X] B", B->get_rank());

   // construct the left argument of Bif_F12_TAKE::fill(). Start with ⍴B and
   // then replace corresponding shape items with 1.
   //
Shape sh_take = B->get_shape();   // start with ⍴B
   loop(b, B->get_rank())         // for exery axis X[x] in X
       {
         if (axes_X & 1 << b) sh_take.set_shape_item(b, 1);
       }

   return Token(TOK_APL_VALUE1, do_take(sh_take, *B, axes_X));
}
//----------------------------------------------------------------------------
Token
Bif_F12_TAKE::eval_AB(Value_P A, Value_P B) const
{
Shape ravel_A1(*A, /* ⎕IO */ 0);   // checks 1 ≤ ⍴⍴A and ⍴A ≤ MAX_RANK

   if (B->is_scalar())
      {
        Shape shape_B1;
        loop(a, ravel_A1.get_rank())   shape_B1.add_shape_item(1);
        Value_P B1 = CLONE_P(B, LOC);   // so that we can set_shape()
        B1->set_shape(shape_B1);
        return Token(TOK_APL_VALUE1, do_take(ravel_A1, *B1, false));
      }
   else
      {
        if (ravel_A1.get_rank() != B->get_rank())   LENGTH_ERROR;
        return Token(TOK_APL_VALUE1, do_take(ravel_A1, *B, false));
      }
}
//----------------------------------------------------------------------------
Value_P
Bif_F12_TAKE::do_take(const Shape & ravel_A1, const Value & B,
                      AxesBitmap axes)
{
   // ravel_A1 can have negative items (for take from the end).
   //
Value_P Z(ravel_A1.abs(), LOC);

   if (ravel_A1.is_empty())   Z->set_default(B, LOC);
   else                       fill(ravel_A1, *Z, B, axes);
   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------
void
Bif_F12_TAKE::fill(const Shape & shape_Zi, Value & Z,
                   const Value & B, AxesBitmap axes)
{
   for (TakeDropIterator i(true, shape_Zi, B.get_shape()); i.has_more(); ++i)
       {
         const ShapeItem offset = i();
         if (offset != -1)                          // valid cell
            {
              Z.next_ravel_Cell(B.get_cravel(offset));
            }
         else if (axes)                             // overtake from axis
            {
              const ShapeItem offset = i.axis_proto(axes);
              Z.next_ravel_Proto(B.get_cravel(offset));
            }
         else                                       // overtake from ↑B
            {
              Z.next_ravel_Proto(B.get_cproto());
            }
       }
}
//============================================================================
Token
Bif_F12_DROP::eval_AB(Value_P A, Value_P B) const
{
   if (A->get_rank() > 1)   RANK_ERROR;

const Shape ravel_A(*A, /* ⎕IO */ 0);

   if (B->is_scalar())
      {
        /*
           A scalar B is taken as ((⍴⍴B)⍴1)⍴B.

           Z←A↓B with scalar B has an element count() of either 0 or 1:

           If all items of A are are 0 then nothing is dropped) and therefore
           the result is the scalar B.

            Otherwise at least one axis A[j] is ≠ 0 dropped and the result
            is empty.

           ⍴⍴Z is ⍴,A and ⍴Z[j] ←→  0 ⌈ (⍴B)[j] - ∣ A[j]
         */
        Shape shape_Z;
        loop(r, ravel_A.get_rank())
            {
               // (⍴Zi)[r] r'th either 1 (if nothing is dropped)
               // or else 1 (possibly overdrop).
               if (ravel_A.get_shape_item(r))   shape_Z.add_shape_item(0);
               else                             shape_Z.add_shape_item(1);
            }

        Value_P Z(shape_Z, LOC);

        Z->set_ravel_Cell(0, B->get_cfirst());
        if (shape_Z.get_volume() == 0)   Z->to_type(false);
        Z->check_value(LOC);
        return Token(TOK_APL_VALUE1, Z);
      }

   if (ravel_A.get_rank() != B->get_rank())   LENGTH_ERROR;

Shape shape_Z;
   loop(r, ravel_A.get_rank())
       {
         const ShapeItem sA = ravel_A.get_shape_item(r);    // A[r]
         const ShapeItem sB = B->get_shape_item(r);         // (⍴B[r]
         const ShapeItem pA = sA < 0 ? -sA : sA;            // ∣ A[r]
         if (pA >= sB)   shape_Z.add_shape_item(0);         // over-drop
         else            shape_Z.add_shape_item(sB - pA);   // normal drop
       }

Value_P Z(shape_Z, LOC);
   if (shape_Z.is_empty())   // empty Z, e.g. from overdrop
      {
        Value_P Z(shape_Z, LOC);
        Z->set_default(*B.get(), LOC);
        Z->check_value(LOC);
        return Token(TOK_APL_VALUE1, Z);
      }

   for (TakeDropIterator i(false, ravel_A, B->get_shape()); i.has_more(); ++i)
      {
        const ShapeItem offset = i();
        Z->next_ravel_Cell(B->get_cravel(offset));
      }

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Bif_F12_DROP::eval_AXB(Value_P A, Value_P X, Value_P B) const
{
   if (X->element_count() == 0)   // no axes
      {
        Value_P Z = CLONE_P(B, LOC);
        Token result(TOK_APL_VALUE1, Z);
        return result;
      }

   if (X->get_rank() > 1)    INDEX_ERROR;

const uint64_t len_X = X->element_count();
   if (len_X > MAX_RANK)     INDEX_ERROR;
   if (len_X == 0)           INDEX_ERROR;

   if (A->get_rank() > 1)    RANK_ERROR;

uint64_t len_A = A->element_count();
   if (len_A != len_X)   LENGTH_ERROR;

const APL_Integer qio = Workspace::get_IO();

   // init ravel_A = shape_B and seen.
   //
Shape ravel_A(B->get_shape());
bool seen[MAX_RANK];
   loop(r, B->get_rank())   seen[r] = false;

   loop(r, len_X)
       {
         const APL_Integer a = A->get_cravel(r).get_near_int();
         const APL_Integer x = X->get_cravel(r).get_near_int() - qio;

         if (x >= B->get_rank())   INDEX_ERROR;
         if (seen[x])              INDEX_ERROR;
         seen[x] = true;

         const ShapeItem amax = B->get_shape_item(x);
         if      (a >= amax)   ravel_A.set_shape_item(x, 0);
         else if (a >= 0)      ravel_A.set_shape_item(x, a - amax);
         else if (a > -amax)   ravel_A.set_shape_item(x, amax + a);
         else                  ravel_A.set_shape_item(x, 0);
       }

   return Token(TOK_APL_VALUE1,
                Bif_F12_TAKE::do_take(ravel_A, *B, 0));
}
//============================================================================
ShapeItem
TakeDropIterator::axis_proto(AxesBitmap axes) const
{
   /* compute the offset for the prototype of current_offset. The APL2
      language reference somehow suggests that for A↑[X] B the axes of B
      shall be enclosed (to give ⊂[X] B) and then the prototype of the
      enclosed sub-array shall be used as the fill item for overtake.

      This would mean that the offsets of the prototype is the sum of the
      weighted offsets of the axes in X. Interestingly we get the output
      shown in the language reference only if we use the sum of the weighted
      offsets of the axes NOT in X.
      0
    */
ShapeItem ret = 0;
   loop(a, ref_B.get_rank())
      {
        if (axes & 1 << a)   // axis a in X
           {
             const _ftwc & ftwc_a = ftwc[a];
             ret += ftwc_a.current * ftwc_a.weight;
           }
      }

   return ret;
}
//============================================================================

