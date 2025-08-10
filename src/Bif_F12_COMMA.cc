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

#include "Bif_F12_COMMA.hh"
#include "StateIndicator.hh"
#include "Workspace.hh"

Bif_F12_COMMA  Bif_F12_COMMA ::fun;    // ,
Bif_F12_COMMA1 Bif_F12_COMMA1::fun;    // ⍪

//============================================================================
Token
Bif_COMMA::ravel(const Shape & new_shape, Value_P B)
{
Value_P Z(new_shape, LOC);

const ShapeItem count = B->element_count();
   Assert(count == Z->element_count());

   loop(c, count)   Z->next_ravel_Cell(B->get_cravel(c));

   Z->set_default(*B.get(), LOC);
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Value_P
Bif_COMMA::prepend_scalar(const Cell & cell_A, uAxis axis, const Value & B)
{
   if (B.is_empty())
      {
        Shape shape_Z = B.get_shape();
        shape_Z.set_shape_item(axis, shape_Z.get_shape_item(axis) + 1);
        Value_P Z(shape_Z, LOC);
        if (Z->is_empty())
           {
              Z->set_default(cell_A, LOC);   // prototype
           }
        else
           {
             loop(z, Z->element_count())   Z->next_ravel_Cell(cell_A);
           }
        return Z;
      }

   if (B.is_scalar())
      {
        Value_P Z(2, LOC);
        Z->next_ravel_Cell(cell_A);
        Z->next_ravel_Cell(B.get_cfirst());
        Z->check_value(LOC);
        return Z;
      }

   if (axis >= B.get_rank())   INDEX_ERROR;

Shape shape_Z(B.get_shape());
   shape_Z.set_shape_item(axis, shape_Z.get_shape_item(axis) + 1);

Value_P Z(shape_Z, LOC);

const Shape3 shape_B3(B.get_shape(), axis);
   const ShapeItem slice_a = shape_B3.l();
   const ShapeItem slice_b = shape_B3.l() * B.get_shape_item(axis);

const Cell * cB = &B.get_cfirst();

   loop(hz, shape_B3.h())
       {
         loop(lz, slice_a)   Z->next_ravel_Cell(cell_A);

         Cell::copy(*Z.get(), cB, slice_b);
       }

   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
Bif_COMMA::append_scalar(const Value & A, uAxis axis, const Cell & cell_B)
{
   if (A.is_empty())
      {
        Shape shape_Z = A.get_shape();
        shape_Z.set_shape_item(axis, shape_Z.get_shape_item(axis) + 1);
        Value_P Z(shape_Z, LOC);
        if (Z->is_empty())
           {
              Z->set_default(cell_B, LOC);   // prototype
           }
        else
           {
             loop(z, Z->element_count())   Z->next_ravel_Cell(cell_B);
           }
        return Z;
      }
   // A.is_scalar() is handled by prepend_scalar()

   if (axis >= A.get_rank())   INDEX_ERROR;

Shape shape_Z(A.get_shape());
   shape_Z.set_shape_item(axis, shape_Z.get_shape_item(axis) + 1);

Value_P Z(shape_Z, LOC);

const Shape3 shape_A3(A.get_shape(), axis);
const ShapeItem slice_a = shape_A3.l() * A.get_shape_item(axis);
const ShapeItem slice_b = shape_A3.l();

const Cell * cA = &A.get_cfirst();

   loop(hz, shape_A3.h())
       {
         Cell::copy(*Z.get(), cA, slice_a);
         loop(lz, slice_b)   Z->next_ravel_Cell(cell_B);
       }

   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
Bif_COMMA::catenate(const Value & A, sAxis axis, const Value & B)
{
   // NOTE: the case A.is_scalar() && B.is_scalar() was supposedly ruled out
   //       before calling catenate()

   if (A.is_scalar())
      {
        const Cell & cell_A = A.get_cfirst();
        Value_P Z = prepend_scalar(cell_A, axis, B);
        Z->check_value(LOC);
        return Z;
      }

   if (B.is_scalar())
      {
        const Cell & cell_B = B.get_cfirst();
        Value_P Z = append_scalar(A, axis, cell_B);
        Z->check_value(LOC);
        return Z;
      }

   if ((A.get_rank() + 1) == B.get_rank())
      {
        Shape shape_Z;

        // check shape conformance.
        {
          ShapeItem ra = 0;
          loop(rb, B.get_rank())
             {
               if (rb != axis)
                  {
                    shape_Z.add_shape_item(B.get_shape_item(rb));
                    if (A.get_shape_item(ra) != B.get_shape_item(rb))
                       {
                         LENGTH_ERROR;
                       }
                    ++ra;
                  }
               else
                  {
                    shape_Z.add_shape_item(B.get_shape_item(rb) + 1);
                  }
             }
        }

        Value_P Z(shape_Z, LOC);

        Z->set_default(B, LOC);

        const Shape3 shape_B3(B.get_shape(), axis);
        const ShapeItem slice_a = shape_B3.l();
        const ShapeItem slice_b = shape_B3.l() * B.get_shape_item(axis);

        const Cell * cA = &A.get_cfirst();
        const Cell * cB = &B.get_cfirst();

        loop(hz, shape_B3.h())
            {
              Cell::copy(*Z.get(), cA, slice_a);
              Cell::copy(*Z.get(), cB, slice_b);
            }

        Z->check_value(LOC);
        return Z;
      }

   if (A.get_rank() == (B.get_rank() + 1))
      {
        // e.g.»        ∆∆∆ , 3
        //              ∆∆∆   4
        //
        Shape shape_Z;

        // check shape conformance. We step ranks ra and rb, except for the
        // axis where only ra is incremented.
        {
          uint32_t rb = 0;
          loop(ra, A.get_rank())
             {
               if (ra != axis)
                  {
                    if (A.get_shape_item(ra) != B.get_shape_item(rb))
                       LENGTH_ERROR;

                    shape_Z.add_shape_item(A.get_shape_item(ra));
                    ++rb;
                  }
               else
                  {
                    shape_Z.add_shape_item(A.get_shape_item(ra) + 1);
                  }
             }
        }

        Value_P Z(shape_Z, LOC);

        Z->set_default(B, LOC);

        const Shape3 shape_A3(A.get_shape(), axis);
        const ShapeItem slice_a = shape_A3.l() * A.get_shape_item(axis);
        const ShapeItem slice_b = shape_A3.l();

        const Cell * cA = &A.get_cfirst();
        const Cell * cB = &B.get_cfirst();

        loop (hz, shape_A3.h())
            {
              Cell::copy(*Z.get(), cA, slice_a);
              Cell::copy(*Z.get(), cB, slice_b);
            }

        Z->set_default(B, LOC);

        Z->check_value(LOC);
        return Z;
      }
   if (A.get_rank() != B.get_rank())   RANK_ERROR;

   // A and B have the same rank. The shapes need to agree, except for the
   // dimension corresponding to the axis.
   //
Shape shape_Z;

   loop(r, A.get_rank())
       {
         if (r != axis)
            {
              if (A.get_shape_item(r) != B.get_shape_item(r))
                 LENGTH_ERROR;

              shape_Z.add_shape_item(A.get_shape_item(r));
            }
         else
            {
              shape_Z.add_shape_item(A.get_shape_item(r) +
                                   + B.get_shape_item(r));
            }
       }

const Shape3 shape_A3(A.get_shape(), axis);

const Cell * cA = &A.get_cfirst();
const Cell * cB = &B.get_cfirst();
const ShapeItem slice_a = shape_A3.l() * A.get_shape_item(axis);
const ShapeItem slice_b = shape_A3.l() * B.get_shape_item(axis);

Value_P Z(shape_Z, LOC);
   loop(hz, shape_A3.h())
       {
         Cell::copy(*Z.get(), cA, slice_a);
         Cell::copy(*Z.get(), cB, slice_b);
       }

   Z->set_default(B, LOC);
   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
Bif_COMMA::laminate(const Value & A, sAxis axis, const Value & B)
{
   // shapes of A and B must be the same, unless one of them is a scalar.
   //
   if (!A.is_scalar() && !B.is_scalar())
      A.get_shape().check_same(B.get_shape(),
                                E_INDEX_ERROR, E_LENGTH_ERROR, LOC);

const Shape shape_Z = A.is_scalar() ? B.get_shape().insert_axis(axis, 2)
                                    : A.get_shape().insert_axis(axis, 2);

Value_P Z(shape_Z, LOC);

const Shape3 shape_Z3(shape_Z, axis);
   if (shape_Z3.m() != 2)   AXIS_ERROR;

const Cell * cA = &A.get_cfirst();
const Cell * cB = &B.get_cfirst();
   if (A.is_scalar())
      {
        if (B.is_scalar())
           {
             Z->next_ravel_Cell(*cA);
             Z->next_ravel_Cell(*cB);
           }
        else
           {
             loop(h, shape_Z3.h())
                 {
                   loop(l, shape_Z3.l())
                       Z->next_ravel_Cell(*cA);
                   Cell::copy(*Z.get(), cB, shape_Z3.l());
                }
           }
      }
   else
      {
        if (B.is_scalar())
           {
             loop(h, shape_Z3.h())
                 {
                   Cell::copy(*Z.get(), cA, shape_Z3.l());
                   loop(l, shape_Z3.l())   Z->next_ravel_Cell(*cB);
                }
           }
        else
           {
             loop(h, shape_Z3.h())
                 {
                   Cell::copy(*Z.get(), cA, shape_Z3.l());
                   Cell::copy(*Z.get(), cB, shape_Z3.l());
                }
           }
      }

   Z->set_default(B, LOC);

   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------
Token
Bif_COMMA::ravel_axis(Value_P X, Value_P B, uAxis axis)
{
const APL_Integer qio = Workspace::get_IO();

   if (!X)   SYNTAX_ERROR;

   // I must be a (integer or real) scalar or simple integer vector
   //
   if (X->get_rank() > 1)   INDEX_ERROR;

   // There are 3 variants, determined by I:
   //
   // 1) I is empty:             append a new last axis of length 1
   // 2) I is a fraction:        append a new axis before I
   // 3a) I is integer scalar:   return B
   // 3b) I is integer vector:   combine axes

   // case 1:   ,['']B or ,[⍳0]B : append new first or last axis of length 1.
   //
   if (X->element_count() == 0)
      {
        if (B->get_rank() == MAX_RANK)   AXIS_ERROR;

        const Shape shape_Z = B->get_shape().insert_axis(axis, 1);
        return ravel(shape_Z, B);
      }

   // case 2:   ,[x.y]B : insert axis before axis x+1
   //
   if (!X->get_cfirst().is_near_int())  // fraction: insert an axis
      {
        if (B->get_rank() == MAX_RANK)   INDEX_ERROR;

        const APL_Float new_axis = X->get_cfirst().get_real_value() - qio;
        sAxis axis = new_axis;   if (new_axis < 0.0)   axis = -1;
        const Shape shape_Z = B->get_shape().insert_axis(axis + 1, 1);
        return ravel(shape_Z, B);
      }
   // case 3a: ,[n]B : return B (combine single axis doesn't change anything)
   //
   if (X->is_scalar_or_len1_vector())   // single int: return B->
      {
        Token result(TOK_APL_VALUE1, CLONE_P(B, LOC));
        return result;
      }

   // case 3b: ,[n1 ... nk]B : combine axes.
   //
const Shape axes(*X, qio);

const ShapeItem from = axes.get_first_shape_item();
   if (from < 0)   AXIS_ERROR;

const ShapeItem to   = axes.get_last_shape_item();
   if (to >= B->get_rank())   AXIS_ERROR;

   // check that the axes are contiguous and compute the number of elements
   // in the combined axes
   //
ShapeItem count = 1;
   loop(a, axes.get_rank())
      {
        if (axes.get_shape_item(a) != (from + a))   AXIS_ERROR;
        count *= B->get_shape_item(from + a);
      }

Shape shape_Z;
   loop (r, from)   shape_Z.add_shape_item(B->get_shape_item(r));
   shape_Z.add_shape_item(count);
   for (uRank r = to + 1; r < B->get_rank(); ++r)
       shape_Z.add_shape_item(B->get_shape_item(r));

   return ravel(shape_Z, B);
}
//============================================================================
Token
Bif_F12_COMMA::eval_B(Value_P B) const
{
const Shape shape_Z(B->element_count());

   if (DO_RT_COMMA_B              &&
       B->get_owner_count() == 2  &&
       this == Workspace::SI_top()->get_prefix().get_monadic_fun())
      {
        Log(LOG_optimization)
           CERR << "optimizing ,B (len="
                << B->nz_element_count() << ")" << endl;

        OptmizationStatistics::count(OPTI_RT_COMMA_B);

        B->set_shape(shape_Z);
        return Token(TOK_APL_VALUE1, B);
      }

   return ravel(shape_Z, B);
}
//----------------------------------------------------------------------------
Token
Bif_F12_COMMA::eval_AB(Value_P A, Value_P B) const
{
  if (A->is_scalar() && B->is_scalar())
     {
       Value_P Z(2, LOC);
       Z->next_ravel_Cell(A->get_cscalar());
       Z->next_ravel_Cell(B->get_cscalar());
       Z->check_value(LOC);
       return Token(TOK_APL_VALUE1, Z);
     }

uRank max_rank = A->get_rank();
   if (max_rank < B->get_rank())  max_rank = B->get_rank();
   return Token(TOK_APL_VALUE1, catenate(*A, max_rank-1, *B));
}
//----------------------------------------------------------------------------
Value_P
Bif_COMMA::catenate_or_laminate(const Value & A, const Value & X,
                                const Value & B)
{
 if (A.is_scalar() && B.is_scalar())   RANK_ERROR;

   // catenate or laminate
   //
   if (!X.is_scalar_or_len1_vector())   AXIS_ERROR;

const Cell & cX = X.get_cfirst();
const APL_Integer qio = Workspace::get_IO();

   if (cX.is_near_int())   // catenate along existing axis
      {
        const sAxis axis = cX.get_checked_near_int() - qio;
        if (axis < 0)                                         AXIS_ERROR;
        if (uAxis(axis) >= A.get_rank() && uAxis(axis) >= B.get_rank())
           AXIS_ERROR;
        return catenate(A, axis, B);
      }

const APL_Float axis = cX.get_real_value() - qio;
   if (axis <= -1.0)   AXIS_ERROR;
   if (axis >= (A.get_rank() + 1.0) &&
       axis >= (B.get_rank() + 1.0))   AXIS_ERROR;
   return laminate(A, sAxis(axis + 1.0), B);
}
//============================================================================
Token
Bif_F12_COMMA1::eval_B(Value_P B) const
{
   // turn B into a matrix
   //
ShapeItem c1 = 1;   // assume B is scalar
ShapeItem c2 = 1;   // assume B is scalar;

   if (B->get_rank() >= 1)
      {
        c1 = B->get_shape_item(0);
        for (uRank r = 1; r < B->get_rank(); ++r)   c2 *= B->get_shape_item(r);
      }

Shape shape_Z(c1, c2);
   if (DO_RT_COMMA1_B            &&
       B->get_owner_count() == 2 &&
       this == Workspace::SI_top()->get_prefix().get_monadic_fun())
      {
        Log(LOG_optimization) CERR << "optimizing ,B" << endl;

        OptmizationStatistics::count(OPTI_RT_COMMA1_B);

        B->set_shape(shape_Z);
        return Token(TOK_APL_VALUE1, B);
      }
   return ravel(shape_Z, B);
}
//----------------------------------------------------------------------------
Token
Bif_F12_COMMA1::eval_AB(Value_P A, Value_P B) const
{
  if (A->is_scalar() && B->is_scalar())
     {
       Value_P Z(2, LOC);
       Z->next_ravel_Cell(A->get_cfirst());
       Z->next_ravel_Cell(B->get_cfirst());
       Z->check_value(LOC);
       return Token(TOK_APL_VALUE1, Z);
     }

   return Token(TOK_APL_VALUE1, catenate(*A, 0, *B));
}
//============================================================================

