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

#include <string.h>
#include <stdio.h>

#include "ArrayIterator.hh"
#include "Avec.hh"
#include "Bif_F12_COMMA.hh"
#include "Bif_F12_SORT.hh"
#include "Bif_F12_TAKE_DROP.hh"
#include "Bif_OPER1_EACH.hh"
#include "Bif_OPER1_REDUCE.hh"
#include "CharCell.hh"
#include "ComplexCell.hh"
#include "Command.hh"
#include "ConstCell_P.hh"
#include "FloatCell.hh"
#include "Id.hh"
#include "IndexExpr.hh"
#include "IndexIterator.hh"
#include "IntCell.hh"
#include "LvalCell.hh"
#include "Macro.hh"
#include "Output.hh"
#include "PointerCell.hh"
#include "PrimitiveFunction.hh"
#include "PrintOperator.hh"
#include "StateIndicator.hh"
#include "UserFunction.hh"
#include "Value.hh"
#include "Workspace.hh"

// primitive function instances
//
Bif_F12_DECODE    Bif_F12_DECODE   ::fun;    // ⊥
Bif_F12_ELEMENT   Bif_F12_ELEMENT  ::fun;    // ϵ
Bif_F12_ENCODE    Bif_F12_ENCODE   ::fun;    // ⊤
Bif_F12_EQUIV     Bif_F12_EQUIV    ::fun;    // ≡
Bif_F1_EXECUTE    Bif_F1_EXECUTE   ::fun;    // ⍎
Bif_F2_INDEX      Bif_F2_INDEX     ::fun;    // ⌷
Bif_F2_INTER      Bif_F2_INTER     ::fun;    // ∩
Bif_F2_LEFT       Bif_F2_LEFT      ::fun;    // ⊣
Bif_F12_NEQUIV    Bif_F12_NEQUIV   ::fun;    // ≢
Bif_F2_RIGHT      Bif_F2_RIGHT     ::fun;    // ⊢
Bif_F12_RHO       Bif_F12_RHO      ::fun;    // ⍴
Bif_F12_ROTATE    Bif_F12_ROTATE   ::fun;    // ⌽
Bif_F12_ROTATE1   Bif_F12_ROTATE1  ::fun;    // ⊖
Bif_F12_TRANSPOSE Bif_F12_TRANSPOSE::fun;    // ⍉
Bif_F12_UNION     Bif_F12_UNION    ::fun;    // ∪
Bif_F0_ZILDE      Bif_F0_ZILDE     ::fun;    // ⍬

int Bif_F1_EXECUTE::copy_pending = 0;

//----------------------------------------------------------------------------
Token
PrimitiveFunction::eval_fill_AB(Value_P A, Value_P B) const
{
   return eval_AB(A, B);
}
//----------------------------------------------------------------------------
Token
PrimitiveFunction::eval_fill_B(Value_P B) const
{
   return eval_B(B);
}
//----------------------------------------------------------------------------
ostream &
PrimitiveFunction::print(ostream & out) const
{
   return out << get_Id();
}
//----------------------------------------------------------------------------
void
PrimitiveFunction::print_properties(ostream & out, int indent) const
{
UCS_string ind(indent, UNI_SPACE);
   out << ind << "System Function ";
   print(out);
   out << endl;
}
//----------------------------------------------------------------------------
Token
NonscalarFunction_default_identity::eval_identity_fun(Value_P B,
                                                      sAxis axis) const
{
   // axis is already normalized to IO←0
   // return Z←,/B0 where (B0 , B) is B.

const Shape & shape_B = B->get_shape();
const sRank rank_B = shape_B.get_rank();
   if (rank_B < 1)       RANK_ERROR;   // identity restriction, lrm p. 212
   if (axis >= rank_B)   RANK_ERROR;

   // eval_identity_fun() is supposedly only called when the length of axis is 0
const ShapeItem axis_len = shape_B.get_shape_item(axis);
   Assert(axis_len == 0);

const Shape shape_Z = shape_B.without_axis(axis);

   /* the removal of the reduction axis must not create a non-empty result.

      In IBM APL2:

                   ┌───── reduction axis
                   │
            ⍴ ,/ 0 0⍴42   → 0
            ⍴ ,/ 0 3⍴42   → 0 (not happening here since axis_len == 0)
            ⍴ ,/ 3 0⍴42   → DOMAIN ERROR (volume would be 3)
            ⍴ ,/   0⍴42   → DOMAIN ERROR (volume would be 1)
      but:  ⍴ +/   0⍴42   → ⍬   and +/   0⍴42   → 0 (integer scalar 0)
            ⍴ ×/   0⍴42   → ⍬   and +/   0⍴42   → 1 (integer scalar 1)
    */
   if (shape_Z.get_rank() && shape_Z.get_volume() > 0)   DOMAIN_ERROR;

Value_P Z(shape_Z, LOC);
   Z->set_default(*B, LOC);
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//============================================================================
Token
Bif_F0_ZILDE::eval_() const
{
   return Token(TOK_APL_VALUE1, Idx0(LOC));
}
//============================================================================
Token
Bif_F12_RHO::eval_B(Value_P B) const
{
Value_P Z(B->get_rank(), LOC);

   loop(r, B->get_rank())   Z->next_ravel_Int(B->get_shape_item(r));

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Bif_F12_RHO::eval_AB(Value_P A, Value_P B) const
{
#ifdef cfg_PERFORMANCE_COUNTERS_WANTED
const uint64_t start_1 = cycle_counter();
#endif

const Shape shape_Z(*A, 0);

   // check that shape_Z is positive
   //
   loop(r, shape_Z.get_rank())
      {
        if (shape_Z.get_shape_item(r) < 0)   DOMAIN_ERROR;
      }

const ShapeItem len_Z = shape_Z.get_volume();

   if (DO_RT_A_RHO_B               &&
       len_Z <= B->element_count() &&   // 1.   Z is not longer than B
       B->get_owner_count() == 2   &&   // 2.   B is a temporary value
       this == Workspace::SI_top()->get_prefix().get_dyadic_fun())   // 3. below
      {
        /* Optimization of Z←A⍴B. At this point:

          1. Z is not longer than B, and
          2. B has only 2 owners:
             2a. the prefix (who will discard it after we return), and
             2b. our Value_P B.
          3. A⍴B was called from a reduction rule (Prefix::reduce_A_F_B())

           We will give up our ownership 2a. on return ret; below, and
           Prefix::reduce_A_F_B prefix will Prefix::pop_args_push_result()
           and hence give up its ownership 2b, causeing B to be erased.

           That means that B will no longer be used and that, instead of
           of copying B into a new Z and then erasing B, we can reshape B
           in place and return the reshaped B.

           return Token(TOK_APL_VALUE1, B); below will take ownership of B
           so that Prefix::reduce_A_F_B() won't erase B.
         */
        Log(LOG_optimization) CERR << "optimizing A⍴B" << endl;

        // release the no longer used cells of B after shape_Z.
        //
        const ShapeItem len_B = B->element_count();   // all Cells
        ShapeItem rest = len_Z;                       // Cells remaining
        if (rest == 0)   // Z is empty
           {
             rest = 1;
             if (B->get_cproto().is_pointer_cell())
                {
                  B->get_cproto().get_pointer_value()->to_type(false);
                }
             else
                {
                   B->get_wproto().init_type(B->get_cproto(), *B, LOC);
                }
           }

        // release the Cells after Z
        while (rest < len_B)   B->release(rest++, LOC);

        B->set_shape(shape_Z);

#ifdef cfg_PERFORMANCE_COUNTERS_WANTED
const uint64_t end_1 = cycle_counter();
   Performance::fs_F12_RHO_AB.add_sample(end_1 - start_1,
                                         B->nz_element_count());
#endif

        OptmizationStatistics::count(OPTI_RT_A_RHO_B);
        return Token(TOK_APL_VALUE1, B);
      }

#ifdef cfg_PERFORMANCE_COUNTERS_WANTED
Token ret = do_reshape(shape_Z, *B);
const uint64_t end_1 = cycle_counter();
   Performance::fs_F12_RHO_AB.add_sample(end_1 - start_1,
                                         shape_Z.get_volume());
   return ret;
#else
   return do_reshape(shape_Z, *B);
#endif
}
//----------------------------------------------------------------------------
Token
Bif_F12_RHO::do_reshape(const Shape & shape_Z, const Value & B)
{
const ShapeItem len_B = B.element_count();

Value_P Z(shape_Z, LOC);
const ShapeItem len_Z = Z->element_count();

   if (len_B == 0)   // empty B: use prototype of B for all cells of Z
      {
        loop(z, len_Z)   Z->next_ravel_Proto(B.get_cproto());
      }
   else
      {
        loop(z, len_Z)
          {
            Z->next_ravel_Cell(B.get_cravel(z % len_B));
          }
      }

   Z->set_default(B, LOC);
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//============================================================================
Token
Bif_ROTATE::reverse(Value_P B, sAxis axis)
{
   if (B->is_scalar())
      {
        Token result(TOK_APL_VALUE1, CLONE_P(B, LOC));
        return result;
      }

const Shape3 shape_B3(B->get_shape(), axis);

Value_P Z(B->get_shape(), LOC);

   loop(h, shape_B3.h())
       {
         // plane h is the first element in B[h;;]
         //
         const ShapeItem plane_h = h * shape_B3.m() * shape_B3.l();
         loop(m, shape_B3.m())
             {
               // col_m is the source column in B for Z[h;m;0]
               const ShapeItem col_m = plane_h
                                     + (shape_B3.l() * (shape_B3.m() - m - 1));
               loop(l, shape_B3.l())
                   {
                     Z->next_ravel_Cell(B->get_cravel(col_m + l));
                   }
             }
       }

   Z->set_default(*B.get(), LOC);
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Bif_ROTATE::rotate(Value_P A, Value_P B, sAxis axis)
{
int32_t gsh = 0;   // global shift (scalar A); 0 means local shift (A) used.

const Shape3 shape_B3(B->get_shape(), axis);
const Shape shape_A2(shape_B3.h(), shape_B3.l());

   if (A->is_scalar_or_len1_vector())
      {
        gsh = A->get_cfirst().get_near_int();
        if (gsh == 0)   // nothing to do.
           {
             Token result(TOK_APL_VALUE1, CLONE_P(B, LOC));
             return result;
           }
      }
   else   // otherwise shape A must be shape B with 'axis' removed.
      {
        A->get_shape().check_same(B->get_shape().without_axis(axis),
                                 E_RANK_ERROR, E_LENGTH_ERROR, LOC);
      }


Value_P Z(B->get_shape(), LOC);

   loop(h, shape_B3.h())
   loop(m, shape_B3.m())
   loop(l, shape_B3.l())
       {
         ShapeItem src = gsh;
         if (!src)   src = A->get_cravel(l + h*shape_B3.l()).get_near_int();
         src += shape_B3.m() + m;
         while (src < 0)               src += shape_B3.m();
         while (src >= shape_B3.m())   src -= shape_B3.m();
         Z->next_ravel_Cell(B->get_cravel(shape_B3.hml(h, src, l)));
       }

   Z->set_default(*B.get(), LOC);

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Bif_F12_ROTATE::eval_XB(Value_P X, Value_P B) const
{
const sAxis axis = Value::get_single_axis(X.get(), B->get_rank());
   return reverse(B, axis);
}
//----------------------------------------------------------------------------
Token
Bif_F12_ROTATE::eval_AXB(Value_P A, Value_P X, Value_P B) const
{
const sAxis axis = Value::get_single_axis(X.get(), B->get_rank());
   return rotate(A, B, axis);
}
//----------------------------------------------------------------------------
Token
Bif_F12_ROTATE1::eval_XB(Value_P X, Value_P B) const
{
const sAxis axis = Value::get_single_axis(X.get(), B->get_rank());
   return reverse(B, axis);
}
//----------------------------------------------------------------------------
Token
Bif_F12_ROTATE1::eval_AXB(Value_P A, Value_P X, Value_P B) const
{
const sAxis axis = Value::get_single_axis(X.get(), B->get_rank());
   return rotate(A, B, axis);
}
//----------------------------------------------------------------------------
Token
Bif_F12_TRANSPOSE::do_eval_B(const Value * B)
{
Shape shape_A;

   // monadic transpose is A⍉B with A = ... 4 3 2 1 0
   //
   loop(r, B->get_rank())   shape_A.add_shape_item(B->get_rank() - r - 1);

Value_P Z = transpose(shape_A, B);
   Z->set_default(*B, LOC);
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Bif_F12_TRANSPOSE::eval_AB(Value_P A, Value_P B) const
{
   // A should be a scalar or vector.
   //
   if (A->get_rank() > 1)
      {
        MORE_ERROR() << "A⍉B: A is not a vector or scalar.";
        RANK_ERROR;
      }

const Shape shape_A(*A, Workspace::get_IO());   // rank(shape_A) = length(A)
   if (shape_A.get_rank() != B->get_rank())
      {
        MORE_ERROR() << "A⍉B: ⍴A is " << shape_A.get_rank()
                     << ", but ⍴⍴B is " << B->get_rank()
                     << " (i.e. ≠ ⍴A)";
        LENGTH_ERROR;
      }

   if (B->is_scalar())   // B is a scalar (so A should be empty)
      {
        Value_P Z = CLONE_P(B, LOC);
        Z->check_value(LOC);
        return Token(TOK_APL_VALUE1, Z);
      }

   // shape_A is normalized to ⎕IO←- and shall only contain valid axes of B.
   loop(r, shape_A.get_rank())
      {
        if (shape_A.get_shape_item(r) < 0)                DOMAIN_ERROR;
        if (shape_A.get_shape_item(r) >= B->get_rank())   DOMAIN_ERROR;
      }

Value_P Z = shape_A.get_rank() == B->get_rank() && shape_A.is_permutation()
          ? transpose(shape_A, B.get())
          : transpose_diag(shape_A, B.get());

   Z->set_default(*B.get(), LOC);

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Value_P
Bif_F12_TRANSPOSE::transpose(const Shape & sh_A, const Value * B)
{
   // some frequent and simple to optimize cases beforehand...
   //
   if (sh_A.get_rank() <= 2)   // transpose matrix or vectors
      {
        if (sh_A.get_rank() <= 1)   // scalar or vector B:
           {
              return CLONE(B, LOC);
           }

        // 2-dimensional matrix (probably the most frequent case).
        //
        if (sh_A.get_shape_item(0) == 0 &&
            sh_A.get_shape_item(1) == 1)   return CLONE(B, LOC);   // identity

        const ShapeItem rows_B = B->get_shape_item(0);
        const ShapeItem cols_B = B->get_shape_item(1);
        const Shape shape_Z(cols_B, rows_B);
        Value_P Z(shape_Z, LOC);
        loop(rZ, cols_B)   // the rows of B are columns of Z
        loop(cZ, rows_B)   // the columns of B are rows of Z
            Z->next_ravel_Cell(B->get_cravel(rZ + cZ*cols_B));
        Z->check_value(LOC);
        return Z;
      }

   /*
      Z←A ⍉ B could be reasonably implemented in 2 ways

      1. loop over (source) B and ArrayIterator for (destination) Z, or
      2. ArrayIterator for (source) B and loop over (destination) Z.

      sh_A specifies an axis permutation in the "forward" direction, i.e.

      B[i] → Z[A[i]]   for all indices i.

      which implies alternative 1. above. However, Z->check_value() below
      takes O(length(Z)) in alternative 1. above but only O(2) in
      alternative 2. above because then next_ravel_Cell() can be used.

      For alternative 2. we used the inverse mapping:

      Z[i] ← B[A⁻¹[i]]   for all indices i.

      which is achieved by using the inverse permutation for sh_A (which
      exchanges the source and the destination).

      The caller is supposed to normalized sh_A to to ⎕IO←0, therefore
      the shape items of sh_A are the axes 0, 1, ... (in some order).
    */

const Shape   shape_inv_A = inverse_permutation(sh_A);
const Shape & shape_B     = B->get_shape();
const Shape   shape_Z     = permute(shape_B, shape_inv_A);

Value_P Z(shape_Z, LOC);

   if (shape_Z.is_empty())
      {
         Z->set_default(*B, LOC);
         return Z;
      }

   for (ArrayIterator b(shape_Z, sh_A); b.has_more(); ++b)
       {
         Z->next_ravel_Cell(B->get_cravel(b.get_ravel_offset()));
       }

   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
Bif_F12_TRANSPOSE::transpose_diag(const Shape & sh_A, const Value * B)
{
   // A⍉B with repeated items in A. The caller has normalized sh_A to ⎕IO←0.

   // 1. compute rank_Z ← 1 + ⌈/sh_A.
   //
ShapeItem rank_Z = 0;
const Shape & sh_B = B->get_shape();
   loop(a, sh_A.get_rank())
       {
         const ShapeItem s_A = sh_A.get_shape_item(a);
         if (rank_Z < s_A)   rank_Z = s_A;
       }
   ++rank_Z;

   // compute the B-weights of the untransposed B axes
   //
const Shape weights_B = sh_B.get_weights();

   // compute the B-weights of the transposed B axes. transpose_diag() is
   // called rarely, and if so with small ranks. We can therefore afford
   // a rank_Z² algorithm. We also create shape_Z as we go.
   //
   // A unit step in Z[z] is a weight_Z[z] step in B, where weight_Z is the
   // sum of weights that map to z.
   //
Shape shape_Z;
ShapeItem weight_Z[rank_Z];
   loop(z, rank_Z)
       {
         weight_Z[z] = 0;
         ShapeItem min_len_B = LARGE_INT;
         loop(a, sh_A.get_rank())
             {
               if (z == sh_A.get_shape_item(a))   // B-axis a maps to Z-axis z
                  {
                    const ShapeItem len_b = sh_B.get_shape_item(a);
                    if (min_len_B > len_b)   min_len_B = len_b;
                    weight_Z[z] += weights_B.get_shape_item(a);
                  }
             }

         if (weight_Z[z] == 0)
            {
              /* weight_Z[z] == 0 is rather unlikely and may have 2 reasons:

                 1. none of the A items is z (which raises DOMAIN ERROR), or
                 2. all B axes for z are 0.(which continues below).
               */
              bool z_in_A = false;
              loop(a, sh_A.get_rank())
                  {
                    if (sh_A.get_shape_item(a) == z)   // hence not case 1.
                       {
                         z_in_A = true;
                         break;   // no need to search further
                       }
                  }

              if (!z_in_A)   // z is missing in A
                 {
                   const APL_Integer qio = Workspace::get_IO();
                   MORE_ERROR() << "A⍉B: axis " << (qio + z)
                                << " is missing in A; "
                                   "A should contain all integers " << qio
                                << "..." << (qio + rank_Z) << ".";
                   DOMAIN_ERROR;
                 }
            }
         shape_Z.add_shape_item(min_len_B);
       }

Value_P Z(shape_Z, LOC);
   if (Z->is_empty())
      {
         Z->set_default(*B, LOC);
        return Z;
      }

   for (ArrayIterator iZ(shape_Z); iZ.has_more(); ++iZ)
       {
         const Cell * cB = &B->get_cfirst();
         loop(z, rank_Z)   cB += iZ.get_shape_offset(z) * weight_Z[z];
         Z->next_ravel_Cell(*cB);
       }

   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------
Shape
Bif_F12_TRANSPOSE::inverse_permutation(const Shape & perm)
{
   // perm is a permutation of ⎕IO + 0, 1, ...
   // return the inverse permutaion of perm.

ShapeItem rho[MAX_RANK];

   // 1. set all items to -1.
   //
   loop(r, perm.get_rank())   rho[r] = -1;

   // 2. set all items to the shape items of perm
   //
   loop(a, perm.get_rank())
       {
         const ShapeItem ax = perm.get_shape_item(a);
         if (ax < 0)
            {
              UCS_string & more = MORE_ERROR();
              more << "Axis " << ax << " is < ⎕IO (="
                   << Workspace::get_IO() << ") in permutation";
              loop(a, perm.get_rank())
                  more << " " << (Workspace::get_IO() + perm.get_shape_item(a));
              MORE_ERROR() = more;
              AXIS_ERROR;
            }

         if (ax >= perm.get_rank())
            {
              UCS_string & more = MORE_ERROR();
              more << "Axis " << ax << " exceeds rank "
                   << perm.get_rank() << " of shape in permutation";
              loop(a, perm.get_rank())
                  more << " " << (Workspace::get_IO() + perm.get_shape_item(a));
              MORE_ERROR() = more;
              AXIS_ERROR;
            }
         if (rho[ax] != -1)
            {
              UCS_string & more = MORE_ERROR();
              more << "Duplictate Axis " << ax << " in permutation";
              loop(a, perm.get_rank())
                  more << " " << (Workspace::get_IO() + perm.get_shape_item(a));
              MORE_ERROR() = more;
              AXIS_ERROR;
            }

         // everything OK
         //
         rho[ax] = a;
       }

   return Shape(perm.get_rank(), rho);
}
//----------------------------------------------------------------------------
Shape
Bif_F12_TRANSPOSE::permute(const Shape & sh, const Shape & perm)
{
   /* permute sh according to perm.

      perm is a shape specifying a permutation like this:

      0 → perm[0]
      1 → perm[1]
      ...

      the result ret is the permuted shape sh:

     ret[0] = sh[perm[0]]
     ret[1] = sh[perm[1]]
     ...

    */
Shape ret;

   loop(r, perm.get_rank())
      {
        ret.add_shape_item(sh.get_shape_item(perm.get_shape_item(r)));
      }

   return ret;
}
//============================================================================
Token
Bif_F12_DECODE::eval_AB(Value_P A, Value_P B) const
{
   // ρZ  is: (¯1↓ρA),1↓ρB
   // ρρZ is: (0⌈¯1+ρρA) + (0⌈¯1+ρρB)
   //
const Shape shape_A1 = A->get_shape().without_last_axis();
const Shape shape_B1 = B->get_shape().without_first_axis();

const ShapeItem l_len_A = A->get_rank() ? A->get_last_shape_item() : 1;
const ShapeItem h_len_B = B->get_rank() ? B->get_shape_item(0)     : 1;

const ShapeItem h_len_A = shape_A1.get_volume();
const ShapeItem l_len_B = shape_B1.get_volume();

   if (l_len_A == 0 || h_len_B == 0)   // empty result
      {
        const Shape shape_Z(shape_A1 + shape_B1);
        Value_P Z(shape_Z, LOC);
        Z->check_value(LOC);
        return Token(TOK_APL_VALUE1, Z);
      }

   if ((l_len_A != 1) &&       // cannot scalar-extend A, and
       (h_len_B != 1) &&       // cannot scalar-extend B, and
       (l_len_A != h_len_B))   // the lengths of A and B differ
       LENGTH_ERROR;

const Shape shape_Z = shape_A1 + shape_B1;

Value_P Z(shape_Z, LOC);

const Cell * cA = &A->get_cfirst();

   loop(h, h_len_A)
       {
         // cA ... cA + len_A are used. See if they are complex.
         //
         bool complex_A = false;
         bool integer_A = true;
         loop(aa, l_len_A)
             {
                if (!cA[aa].is_near_real())
                   {
                     complex_A = true;
                     integer_A = false;
                     break;
                   }

                if (!cA[aa].is_near_int())   integer_A = false;
             }

         loop(l, l_len_B)
             {
                // cB, cB + l_len_B, ... are used. See if they are complex
                //
                bool complex_B = false;
                bool integer_B = true;
                loop(bb, h_len_B)
                    {
                      if (!B->get_cravel(l + bb*l_len_B).is_near_real())
                         {
                           complex_B = true;
                           integer_B = false;
                           break;
                         }

                      if (!B->get_cravel(l + bb*l_len_B).is_near_int())
                         integer_B = false;
                    }

               const Cell * cB = &B->get_cravel(l);
               if (integer_A && integer_B)
                  {
                    const bool overflow = decode_int(*Z, l_len_A, cA,
                                                     h_len_B, cB, l_len_B);
                     if (!overflow)   continue;

                     // otherwise: compute as float
                  }

               if (complex_A || complex_B)
                  decode_complex(*Z, l_len_A, cA,
                                             h_len_B, cB, l_len_B);
               else
                  decode_real(*Z, l_len_A, cA,
                                          h_len_B, cB, l_len_B);
             }
         cA += l_len_A;
       }

   Z->set_default(*B.get(), LOC);
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
bool
Bif_F12_DECODE::decode_int(Value & Z, ShapeItem len_A, const Cell * cA,
                       ShapeItem len_B, const Cell * cB, ShapeItem dB)
{
   // decode_int() can easily produce an integer overflow. We keep track
   // of that by also computing the final result as double and return
   // true if an integer overflow occurs. The caller shall then call
   // decode_real() instead.
   //
const ShapeItem dec_A = len_A == 1 ? 0 : 1;
const ShapeItem dec_B = len_B == 1 ? 0 : dB;
const ShapeItem len = dec_A ? len_A : len_B;

   cA += dec_A*len_A;    // let cA point past the lowest weight item in A
   cB += dec_B*len_B;    // let cB point past the lowest weight item in B

APL_Integer value = 0;
APL_Float value_f = 0.0;

APL_Integer weight = 1;
APL_Float weight_f = 1.0;

   loop(l, len)
      {
        cA -= dec_A;
        cB -= dec_B;

        if (weight_f > LARGE_INT)   return true;
        if (weight_f < SMALL_INT)   return true;

        const APL_Integer vB = cB[0].get_near_int();
        value   = value   + weight   * vB;
        value_f = value_f + weight_f * vB;
        if (value_f > LARGE_INT)   return true;
        if (value_f < SMALL_INT)   return true;

        weight   = weight   * cA[0].get_near_int();
        weight_f = weight_f * cA[0].get_near_int();
      }

   Z.next_ravel_Int(value);

   return false;   // no overflow
}
//----------------------------------------------------------------------------
void
Bif_F12_DECODE::decode_real(Value & Z, ShapeItem len_A, const Cell * cA,
                       ShapeItem len_B, const Cell * cB, ShapeItem dB)
{
const ShapeItem dec_A = len_A == 1 ? 0 : 1;
const ShapeItem dec_B = len_B == 1 ? 0 : dB;
const ShapeItem len = dec_A ? len_A : len_B;

   cA += dec_A*len_A;    // let cA point past the lowest weight item in A
   cB += dec_B*len_B;    // let cB point past the lowest weight item in B

APL_Float accu   = 0.0;
APL_Float weight = 1.0;
   loop(l, len)
      {
        cA -= dec_A;
        cB -= dec_B;
        accu += weight * cB->get_real_value();
        weight *= cA->get_real_value();
      }

   Z.next_ravel_Number(accu);
}
//----------------------------------------------------------------------------
void
Bif_F12_DECODE::decode_complex(Value & Z, ShapeItem len_A, const Cell * cA,
                       ShapeItem len_B, const Cell * cB, ShapeItem dB)
{
const ShapeItem dec_A = len_A == 1 ? 0 : 1;
const ShapeItem dec_B = len_B == 1 ? 0 : dB;
const ShapeItem len = dec_A ? len_A : len_B;

   cA += dec_A*len_A;    // let cA point past the lowest weight item in A
   cB += dec_B*len_B;    // let cB point past the lowest weight item in B

APL_Complex accu(0.0, 0.0);
APL_Complex weight(1.0, 0.0);
   loop(l, len)
      {
        cA -= dec_A;
        cB -= dec_B;
        accu += weight*cB->get_complex_value();
        weight *= cA->get_complex_value();
      }

   Z.next_ravel_Number(accu);
}
//----------------------------------------------------------------------------
Token
Bif_F12_ENCODE::eval_AB(Value_P A, Value_P B) const
{
   // A⊤B: every number in B is represented in a number system with
   // radix A. ⍴Z ←→ (⍴A, ⍴B) and Z[...;...b...] = A⊤B[...b...]

   if (A->is_scalar())   return Bif_F12_STILE::fun.eval_AB(A, B);

const ShapeItem ec_A = A->element_count();
const ShapeItem ec_B = B->element_count();
const Shape shape_Z = A->get_shape() + B->get_shape();
Value_P Z(shape_Z, LOC);

   if (ec_A == 0 || ec_B == 0)   // empty A or B
      {
        Z->check_value(LOC);
        return Token(TOK_APL_VALUE1, Z);
      }

const ShapeItem aL = A->get_shape_item(0);    // first (LSB) dimension of A
const ShapeItem aH = ec_A/aL;                 // remaining (MSB) dimensions

const double qct = Workspace::get_CT();

ConstRavel_P iA(A, true);
   loop(a, aH)
      {
        const Cell * cB = &B->get_cfirst();

        // find largest Celltype in A, starting with CT_INT and maybe
        // "increasing" it to CT_FLOAT or CT_COMPLEX as needed.
        //
        CellType ct_a = CT_INT;
        loop(h, aL)
            {
              const CellType ct = A->get_cravel(a + h*aH).get_cell_type();
              if (ct == CT_INT)            ;
              else if (ct == CT_FLOAT)     { if (ct_a == CT_INT)  ct_a = ct; }
              else if (ct == CT_COMPLEX)   ct_a = CT_COMPLEX;
              else                         DOMAIN_ERROR;
            }

        for (ConstRavel_P iB(B, true); +iB; ++iB)
            {
              CellType ct = ct_a;
              const CellType ct_b = cB->get_cell_type();
              if (ct_b == CT_INT)            ;
              else if (ct_b == CT_FLOAT)     { if (ct == CT_INT)  ct = ct_b; }
              else if (ct_b == CT_COMPLEX)   ct = CT_COMPLEX;
              else                           DOMAIN_ERROR;

              if (ct == CT_INT)          // A and B are both integer
                 encode_Int(*Z, aL, aH, iA, iB);
              else if (ct == CT_FLOAT)   // A and B are not APL_Complex
                 encode_Flt(*Z, aL, aH, iA, iB, qct);
              else                       // A or B contain APL_Complex
                 encode_Cpx(*Z, aL, aH, iA, iB, qct);
            }
         ++iA;
       }

   Z->set_default(*B.get(), LOC);

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
void
Bif_F12_ENCODE::encode_Int(Value & Z, ShapeItem aL, ShapeItem aH,
                           const ConstRavel_P & iA, const ConstRavel_P & iB)
{
const ShapeItem dZ = aH*iB.get_length();
Cell * cZ = &Z.get_wravel(iB()
          + iA()*iB.get_length())   // = the current Z (near bottom)
          + aL*dZ;                  // + the size of Z (above top)

const Cell * cA = &*iA + iA.get_length();      // the end of A (+aH)

   /* unfortunately the less significant weights of the number system base A
      are located at higher indices of A. We therefore work downwards from
      the higher indices of A resp. Z...

            ←     ← ... ← .. ← cA   // pre-decremented by aH
                           ↓
        ┌────┬────┬─────┬────┐
        │ a0 │ a1 │ ... │ aN │ // number base (MSB...LSB)
        └────┴────┴─────┴────┘
           ↓    ↓    ↓     ↓
          bi ← bi ← ... ← bi   // divided by ai
           ↓    ↓          ↓
        ┌────┬────┬─────┬────┐
        │ z0 │ z1 │ ... │ zN │ // remainders
        └────┴────┴─────┴────┘
           ↑    ↑    ↑     ↑
            ←     ← ... ← .. ← cZ   // pre-decremented by dZ
    */

APL_Integer bi = iB->get_int_value();   // the value being decoded
   loop(a, aL)
       {
         cA -= aH;
         cZ -= dZ;

        const IntCell cC(bi);
        cC.bif_residue(cZ, cA);

        if (cA->get_int_value() == 0)
           {
             bi = 0;
           }
         else
           {
             bi -= cZ->get_int_value();
             bi /= cA->get_int_value();
           }
       }
}
//----------------------------------------------------------------------------
void
Bif_F12_ENCODE::encode_Flt(Value & Z, ShapeItem aL, ShapeItem aH,
                           const ConstRavel_P & iA,
                           const ConstRavel_P & iB, double qct)
{
const ShapeItem dZ = aH*iB.get_length();
Cell * cZ = &Z.get_wravel(iB()
          + iA()*iB.get_length())   // = the current Z
          + aL*dZ;                  // + the size of Z

const Cell * cA = &*iA + iA.get_length();      // the end of A (+1)

   // work downwards from the higher indices (see encode_Int())...
   //
APL_Float bf = iB->get_real_value();   // the value being decoded
   loop(a, aL)
       {
         cA -= aH;
         cZ -= dZ;

        const FloatCell cC(bf);
        cC.bif_residue(cZ, cA);

        if (cA->is_near_zero())
           {
             bf = 0.0;
           }
         else
           {
             bf -= cZ->get_real_value();
             bf /= cA->get_real_value();
           }
       }
}
//----------------------------------------------------------------------------
void
Bif_F12_ENCODE::encode_Cpx(Value & Z, ShapeItem aL, ShapeItem aH,
                           const ConstRavel_P & iA,
                           const ConstRavel_P & iB, double qct)
{
const ShapeItem dZ = aH*iB.get_length();
Cell * cZ = &Z.get_wravel(iB()
          + iA()*iB.get_length())   // = the current Z
          + aL*dZ;                  // + the size of Z

   // work downwards from the higher indices (see encode_Int())...
   //
const Cell * cA = &*iA + iA.get_length();      // the end of A (+1)

APL_Complex bc = iB->get_complex_value();   // the value being decoded
   loop(a, aL)
       {
         cA -= aH;
         cZ -= dZ;

        const ComplexCell cC(bc);
        cC.bif_residue(cZ, cA);

        if (cA->is_near_zero())
           {
             bc = APL_Complex(0, 0);
           }
         else
           {
             bc -= cZ->get_complex_value();
             bc /= cA->get_complex_value();
           }
       }
}
//----------------------------------------------------------------------------
Value_P
Bif_F12_ELEMENT::do_eval_B(const Value * B)
{
   // enlist
   //
   if (B->element_count() == 0)   // empty argument
      {
        Value_P Z(ShapeItem(0), LOC);   // empty vector with proto ' ' or '0'
        const Cell * C = &B->get_cfirst();
        bool left = false;
        for (;;)
            {
              if (C->is_pointer_cell())
                 {
                   C = &C->get_pointer_value()->get_cfirst();
                   continue;
                 }

              if (left && C->is_lval_cell())
                 {
                   C = C->get_lval_value();
                   if (C == 0)
                      {
                        CERR << "0-pointer at " LOC << endl;
                        FIXME;
                      }
                   else if (C->is_pointer_cell())
                      {
                        C = &C->get_pointer_value()->get_cfirst();
                      }
                   else
                      {
                        const LvalCell & C_lval =
                              reinterpret_cast<LvalCell &>(C);
                        Value * owner = C_lval.get_cell_owner();
                        new (&Z->get_wproto())
                            LvalCell(C_lval.get_lval_value(), owner);
                        break;
                      }
                 }

              if (C->is_numeric())
                 {
                   Z->set_proto_Int();
                   break;
                 }

              if (C->is_character_cell())
                 {
                   Z->set_proto_Spc();
                   break;
                 }

              if (C->is_lval_cell())
                 {
                   const LvalCell & C_lval = reinterpret_cast<LvalCell &>(C);
                   left = C_lval.get_cell_owner() != 0;
                   C = C->get_lval_value();
                   continue;
                 }

               // not reached
               //
               FIXME;
            }

        Z->check_value(LOC);
        return  Z;
      }

const ShapeItem len_Z = B->get_enlist_count();
Value_P Z(len_Z, LOC);

   if (B->get_lval_cellowner())   B->enlist_left(*Z);
   else                           B->enlist_right(*Z);

   Z->set_default(*B, LOC);
   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------
Token
Bif_F12_ELEMENT::eval_AB(Value_P A, Value_P B) const
{
   // return Z←A ϵ B. Z[i] is 1 iff A[i] = B[j] for some j
   //
const double qct = Workspace::get_CT();
Value_P Z(A->get_shape(), LOC);

   if (A->element_count() == 0)
      {
        Z->set_default(*B, LOC);
        Z->check_value(LOC);
        return Token(TOK_APL_VALUE1, Z);
      }

   for (ConstRavel_P a(A, true); +a; ++a)
       {
         APL_Integer found_a_in_B = 0;
         for (ConstRavel_P b(B, true); +b; ++b)
             {
               if (a->equal(*b, qct))
                  {
                    found_a_in_B = 1;
                    break;
                  }
             }
         Z->next_ravel_Int(found_a_in_B);
       }

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Bif_F2_INDEX::eval_AB(Value_P A, Value_P B) const
{
   if (A->get_rank() > 1)   RANK_ERROR;

const ShapeItem ec_A = A->element_count();
   if (ec_A != B->get_rank())   RANK_ERROR;

   // index_expr is in reverse order!
   //
IndexExpr index_expr(ASS_none, LOC);
   loop(a, ec_A)
      {
         const Cell & cell = A->get_cravel(ec_A - a - 1);
         if (cell.is_pointer_cell())
            {
              Value_P val = CLONE_P(cell.get_pointer_value(), LOC);
              if (val->compute_depth() > 1)   DOMAIN_ERROR;
              index_expr.add_index(val);
            }
        else
            {
              const APL_Integer I = cell.get_near_int();
              if (I < 0)   DOMAIN_ERROR;
              index_expr.add_index(IntScalar(I, LOC));
            }
      }

   // the index() functions do set_default() and check_value(),
   // so we return immediately without doint it again.
   //
   index_expr.quad_io = Workspace::get_IO();

   if (index_expr.is_axis())   // [ ] or [ axis ]
      {
        Value_P single_index = index_expr.extract_axis();
        Value_P Z = B->index(single_index.get());
        return Token(TOK_APL_VALUE1, Z);
      }
   else
      {
        Value_P Z = B->index(index_expr);
        return Token(TOK_APL_VALUE1, Z);
      }
}
//----------------------------------------------------------------------------
Token
Bif_F2_INDEX::eval_AXB(Value_P A, Value_P X, Value_P B) const
{
   if (A->get_rank() > 1)   RANK_ERROR;

const AxesBitmap axes_X = X->to_bitmap("⌷[X] B", B->get_rank());

const ShapeItem ec_A = A->element_count();
   if (ec_A != X->element_count())   RANK_ERROR;
   if (ec_A > B->get_rank())              RANK_ERROR;

   // construct an IndexExpr in index (= parse-) order (i.e. the index_expr[0]
   // corresponds to the lasr axis ¯1↑⍴B of B). We therefore move backwards
   // from the end of A resp. X.
   //
IndexExpr index_expr(ASS_none, LOC);   // start with an empty IndexExpr
   index_expr.quad_io = Workspace::get_IO();

ShapeItem a = ec_A;   // index_expr[0] ←→  B[;;;b]
   for (sAxis b = B->get_rank() - 1; b >= 0; --b)
       {
         if (!(axes_X & 1 << b))   // Axis  b was not in X: elided idx
            {
              index_expr.add_index(Value_P());   // add elided index
              continue;
            }

         const Cell & cell_A = A->get_cravel(--a);
          if (cell_A.is_pointer_cell())
             {
               Value_P val = CLONE_P(cell_A.get_pointer_value(), LOC);
               if (val->compute_depth() > 1)   DOMAIN_ERROR;
              index_expr.add_index(val);
             }
         else   // single index
             {
               const APL_Integer I = cell_A.get_near_int();
               if (I < 0)   DOMAIN_ERROR;
               Value_P val = IntScalar(I, LOC);
              index_expr.add_index(val);
             }
       }

   if (index_expr.is_axis())   // Z←B[x] or Z←B[]
      {
        Value_P single_index = index_expr.extract_axis();
        Value_P Z = B->index(single_index.get());

        Z->check_value(LOC);
        return Token(TOK_APL_VALUE1, Z);
      }
   else                        // Z←B[x;...]
      {
        Value_P Z = B->index(index_expr);
        Z->check_value(LOC);
        return Token(TOK_APL_VALUE1, Z);
      }
}
//============================================================================
Token
Bif_F12_EQUIV::eval_B(Value_P B) const
{
const APL_Integer depth = B->compute_depth();

Value_P Z = IntScalar(depth, LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Bif_F12_EQUIV::eval_AB(Value_P A, Value_P B) const
{
   // match
   //

const double qct = Workspace::get_CT();

   if (!A->same_shape(*B))   //shape mismatch
      return Token(TOK_APL_VALUE1, IntScalar(0, LOC));

   for (ConstRavel_P a(A, true), b(B, true); +a; ++a, ++b)
       {
         if (!a->equal(*b, qct))
            {
              return Token(TOK_APL_VALUE1, IntScalar(0, LOC));   // no match
            }
       }

   return Token(TOK_APL_VALUE1, IntScalar(1, LOC));   // match
}
//============================================================================
Token
Bif_F12_NEQUIV::eval_B(Value_P B) const
{
   // Tally
   //
const ShapeItem len = B->is_scalar() ? 1 : B->get_shape().get_shape_item(0);

   return Token(TOK_APL_VALUE1, IntScalar(len, LOC));   // match
}
//----------------------------------------------------------------------------
Token
Bif_F12_NEQUIV::eval_AB(Value_P A, Value_P B) const
{
   // A ≢ B aka.  ∼A ≡ B
   //
const double qct = Workspace::get_CT();

   if (!A->same_shape(*B))   // shape mismatch
      return Token(TOK_APL_VALUE1, IntScalar(1, LOC));

   for (ConstRavel_P a(A, true), b(B, true); +a; ++a, ++b)
       {
         if (!a->equal(*b, qct))
            {
              return Token(TOK_APL_VALUE1, IntScalar(1, LOC));   // no match
            }
       }

   return Token(TOK_APL_VALUE1, IntScalar(0, LOC));   // match
}
//============================================================================
Token
Bif_F1_EXECUTE::eval_B(Value_P B) const
{
   if (B->get_rank() > 1)   RANK_ERROR;

UCS_string statement(*B.get());

   if (statement.size() == 0)   return Token(TOK_NO_VALUE);

   return execute_statement(statement);
}
//----------------------------------------------------------------------------
Token
Bif_F1_EXECUTE::eval_fill_B(Value_P B) const
{
   return Token(TOK_VOID);
}
//----------------------------------------------------------------------------
Token
Bif_F1_EXECUTE::execute_statement(UCS_string & statement)
{
   statement.remove_leading_and_trailing_whitespaces();

   // check for commands
   //
   if (statement.size() &&
       (statement[0] == UNI_R_PARENT ||
        statement[0] == UNI_R_BRACK))   return execute_command(statement);

ExecuteList * fun = ExecuteList::fix(statement.no_pad(), LOC);
   if (fun == 0)   SYNTAX_ERROR;

   Log(LOG_UserFunction__execute)   fun->print(CERR);

   // important special case: ⍎ of an APL literal value
   //
   if (fun->get_body().size() == 2 &&
       fun->get_body()[0].get_Class() == TC_VALUE)
      {
        Value_P Z = fun->get_body()[0].get_apl_val();
        delete fun;
        return Token(TOK_APL_VALUE1, Z);
      }

   Workspace::push_SI(fun, LOC);

   Log(LOG_StateIndicator__push_pop)
      {
        Workspace::SI_top()->info(CERR, LOC);
      }

   return Token(TOK_SI_PUSHED);
}
//----------------------------------------------------------------------------
Token
Bif_F1_EXECUTE::execute_command(UCS_string & command)
{
   if (copy_pending &&
       (
//      command.starts_iwith(")COPY")    ||
        command.starts_iwith(")ERASE")   ||
        command.starts_iwith(")FNS")     ||
        command.starts_iwith(")NMS")     ||
        command.starts_iwith(")QLOAD")   ||
        command.starts_iwith(")SYMBOLS") ||
        command.starts_iwith(")VARS")))
      {
        throw_apl_error(E_COPY_PENDING, LOC);
      }

   if (command.starts_iwith(")LOAD")  ||
       command.starts_iwith(")QLOAD") ||
       command.starts_iwith(")CLEAR") ||
       command.starts_iwith(")RESET") ||
       command.starts_iwith(")SIC"))
      {
        // the command modifies the SI stack. We throw E_COMMAND_PUSHED
        // but without displaying it. That should bring us back to
        // Command::do_APL_expression() with token.get_tag() == TOK_ERROR
        //
        Workspace::push_Command(command);
        Error error(E_COMMAND_PUSHED, LOC);
        throw error;
      }

UTF8_ostream out;   // the APL output (like stdout) of the command

   // check for user-defined commands (they are defined APL functions)
   //
const bool user_cmd = Command::do_APL_command(out, command);   // writes to out
   if (user_cmd)   return execute_statement(command);

   // system command. Append linefeed if needed.
   // To accommodate line_starts below.
   //
UTF8_string result_utf8 = out.get_data();
   if (result_utf8.size() == 0 ||
       result_utf8.back() != UNI_LF)
      result_utf8 += '\n';

   // result_utf8 may have multiple lines. Remember where the lines start.
   //
std::vector<ShapeItem> line_starts;
   line_starts.push_back(0);   // the first line
   loop(r, result_utf8.size())
      {
        if (result_utf8[r] == UNI_LF)   line_starts.push_back(r + 1);
      }

Value_P Z(ShapeItem(line_starts.size() - 1), LOC);
   loop(l, line_starts.size() - 1)
      {
        ShapeItem len;
        if (l < ShapeItem(line_starts.size() - 1))
           len = line_starts[l + 1] - line_starts[l] - 1;
        else
           len = result_utf8.size() - line_starts[l];

        UTF8_string line_utf8(&result_utf8[line_starts[l]], len);
        UCS_string line_ucs(line_utf8);
        Value_P ZZ(line_ucs, LOC);
        Z->next_ravel_Pointer(ZZ.get());
      }

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
ShapeItem
Bif_F12_UNION::append_zone(const Cell ** cells_Z, const Cell ** cells_B,
               Zone_list & B_from_to, double qct)
{
const Cell ** Z0 = cells_Z;

   while (B_from_to.size())
      {
        const Zone zone = B_from_to.back();
        const ShapeItem zone_count = zone.count();
        Assert(zone_count > 0);
        B_from_to.pop_back();

        const ShapeItem B_from = zone.from;
        const ShapeItem B_to   = zone.to;

        // Find the smallest pointer (i.e. smallest Cell address, not the
        // smallest Cell value) in the zone. This pointer will go into Z and
        //  it will kill all its neighbours that are equal within qct.
        //

#if 0
        // this fails when used with char cells but is quite handy for
        // testing the algorithm!
        Q1(LOC)
        fprintf(stderr, "%ld-element zone:\n", B_to - B_from);
        for (ShapeItem j = B_from; j < B_to; ++j)
            fprintf(stderr, "[%ld] value: %.12f\n", j,
                    cells_B[j]->get_real_value());
#endif


        // the by far most likely cases are zones with 1 or,
        // already less likely, 2 cells. These cases can be handled
        // without searching the smallest element and are, for
        // performance reasons, handled beforehand.
        //
        if (zone_count == 1)
           {
             *cells_Z++ = cells_B[B_from];
             continue;   // zone done
           }

        if (zone_count == 2)
           {
             if (cells_B[B_from] < cells_B[B_from + 1])
                *cells_Z++ = cells_B[B_from];
             else
                *cells_Z++ = cells_B[B_from + 1];
             continue;   // zone done
           }

        ShapeItem smallest = B_from;
        for (ShapeItem bb = B_from + 1; bb < B_to; ++bb)
            {
              if (cells_B[smallest] > cells_B[bb])   smallest = bb;
            }

        //  See if the zone is transitive or not.
        //
        if (cells_B[smallest]->equal(*cells_B[B_to - 1], qct))
           {
             // the zone is transitive (i.e. all cells are equal within ⎕CT).
             // The unique of the zone is then the smallest pointer (i.e.first,
             // not the smallest Cell) in the zone.
             //
             *cells_Z++ = cells_B[smallest];
             continue;   // zone done
           }

        // the zone is not transitive, i.e. cells_B[B_from] is NOT equal to
        // cells_B[B_to-1]. Divide the zone into 3 (possibly empty) zones:
        //
        // a.   cells < smallest within ⎕CT,
        // b.   cells = smallest within ⎕CT, and
        // c.  cells > smallest within ⎕CT

        // Start with the middle zone b. This zone initially contains only
        // smallest and its then blown up with elements that are equal (within)
        // ⎕CT) to smallest.
        //
        const Cell * first_B = cells_B[smallest];
        *cells_Z++ = first_B;
        ShapeItem from1 = smallest;      // initial start of zone b.
        ShapeItem to1   = smallest;      // initial end of zone b.

        // decrement from1 as long as its cell is within ⎕CT of smallest.
        // That makes all
        while (from1 > B_from && first_B->equal(*cells_B[from1], qct))  --from1;
        if (from1 > B_from)   // non-empty zone before smallest
           {
             const Zone smaller(B_from, from1);
             B_from_to.push_back(smaller);
             continue;   // zone done
           }

        // increment to1 as long as its cell is within ⎕CT of smallest.
        //
        while (to1 < B_to && first_B->equal(*cells_B[to1], qct))   ++to1;
        if (to1 < B_to)   // non-empty zone after smallest
           {
             Zone larger(to1, B_to);
             B_from_to.push_back(larger);
             continue;   // zone done
           }
      }

   return cells_Z - Z0;
}
//----------------------------------------------------------------------------
Token
Bif_F12_UNION::eval_B(Value_P B) const
{
   if (B->get_rank() > 1)   RANK_ERROR;

const ShapeItem len_B = B->element_count();
   if (len_B <= 1)   return Token(TOK_APL_VALUE1, CLONE_P(B, LOC));

const double qct = Workspace::get_CT();

   // 1. construct a vector of Cell pointers and sort it so that the
   //    cells being pointed to are sorted ascendingly.
   //
   // For efficiency we allocate both the Cell pointers for argument B and
   // for the result Z. Sorting is first done with ⎕CT←0; ⎕CT will be
   // considered later on.
   //
const Cell ** cells_B = new const Cell *[2*len_B];
   if (cells_B == 0)   WS_FULL;

const Cell ** cells_Z = cells_B + len_B;
ShapeItem len_Z = 0;

   for (ConstRavel_P b(B, true); +b; ++b)   cells_B[b()] = &*b;
   Heapsort<const Cell*>::sort(cells_B, len_B, 0, Cell::A_greater_B);

   // 2. divide the cells into zones. A zone is a sequence of cells
   //    B[from] (including)  ... B[to] (excluding) so that the difference
   //    between B[i] and B[i+1] is < ⎕CT.
   //
   //    Then append the unique element(s) of each zone to cells_Z.
   //
ShapeItem from = 0;
Zone_list from_to;
   for (ShapeItem b = 1; b < len_B; ++b)
       {
         if (cells_B[b]->equal(*cells_B[b-1], qct))
            {
              // cells_B[b] belongs to the zone
              //
              continue;
            }

         const Zone ft0(from, b);
         from_to.push_back(ft0);
         len_Z += append_zone(cells_Z + len_Z, cells_B, from_to, qct);
         Assert(from_to.size() == 0);
         from = b;
         continue;   // next b (in the next zone)
       }

   if (from < len_B)   // the rest
      {
        const Zone ft0(from, len_B);
        from_to.push_back(ft0);
        len_Z += append_zone(cells_Z + len_Z, cells_B, from_to, qct);
        Assert(from_to.size() == 0);
      }

   // 3. cells_Z now contains only the unique cells in cells_B, but sorted by
   //    cell contnt. Sort cells_Z by address (= position in B)  so that the
   //    original order in B is reconstructed
   //
   Heapsort<const Cell *>::sort(cells_Z, len_Z, 0, Cell::compare_ptr);

   // 4. construct the result.
   //
Value_P Z(len_Z, LOC);
   loop(z, len_Z)   Z->next_ravel_Cell(*cells_Z[z]);
   delete[] cells_B;   // also deletes cells_Z
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Bif_F2_INTER::eval_AB(Value_P A, Value_P B) const
{
   if (A->get_rank() > 1)   RANK_ERROR;
   if (B->get_rank() > 1)   RANK_ERROR;

const ShapeItem len_A = A->element_count();
const ShapeItem len_B = B->element_count();

const double qct = Workspace::get_CT();

   if (len_A*len_B > 60*60)
      {
        // large A or B: sort A and B to speed up searches
        //
        const Cell ** cells_A = new const Cell *[2*len_A + len_B];
        const Cell ** cells_Z = cells_A + len_A;
        const Cell ** cells_B = cells_A + 2*len_A;

        loop(a, len_A)   cells_A[a] = &A->get_cravel(a);
        loop(b, len_B)   cells_B[b] = &B->get_cravel(b);

        Heapsort<const Cell *>::sort(cells_A, len_A, 0, Cell::compare_stable);
        Heapsort<const Cell *>::sort(cells_B, len_B, 0, Cell::compare_stable);

        ShapeItem len_Z = 0;
        ShapeItem idx_B = 0;
        loop(idx_A, len_A)
            {
              const Cell * ref = cells_A[idx_A];
              while (idx_B < len_B)
                  {
                    if (ref->equal(*cells_B[idx_B], qct))
                       {
                         cells_Z[len_Z++] = ref;   // A is in B
                         break;   // for idx_B → next idx_A
                       }

                    // B is much (by ⎕CT) smaller or greater than A
                    //
                    if (ref->greater(*cells_B[idx_B]))    ++idx_B;
                    else                                  break;
                 }
            }

        // sort cells_Z by position so that the original order in A is
        //  reconstructed
        //
        Heapsort<const Cell *>::sort(cells_Z, len_Z, 0, Cell::compare_ptr);
        Value_P Z(len_Z, LOC);
        loop(z, len_Z)   Z->next_ravel_Cell(*cells_Z[z]);

        Z->set_default(*B, LOC);
        Z->check_value(LOC);
        delete[] cells_A;
        return Token(TOK_APL_VALUE1, Z);
      }
    else
      {
        // small A and B: use quadratic time algorithm.
        //
        const Cell ** cells_Z = new const Cell *[len_A];
        ShapeItem len_Z = 0;

        for (ConstRavel_P a(A, true); +a; ++a)
        for (ConstRavel_P b(B, true); +b; ++b)
            {
              if (a->equal(*b, qct))
                 {
                   cells_Z[len_Z++] = &*a;
                   break;   // loop(b)
                 }
            }

        Value_P Z(len_Z, LOC);
        loop(z, len_Z)   Z->next_ravel_Cell(*cells_Z[z]);

        Z->set_default(*B, LOC);
        Z->check_value(LOC);
        delete[] cells_Z;
        return Token(TOK_APL_VALUE1, Z);
      }
}
//----------------------------------------------------------------------------
Token
Bif_F12_UNION::eval_AB(Value_P A, Value_P B) const
{
   if (A->get_rank() > 1)   RANK_ERROR;
   if (B->get_rank() > 1)   RANK_ERROR;

   // A ∪ B ←→ A,B∼A
   //
Token BwoA = Bif_F12_WITHOUT::fun.eval_AB(B, A);

const ShapeItem len_A = A->element_count();
const ShapeItem len_B = BwoA.get_apl_val()->element_count();
Value_P Z(len_A + len_B, LOC);

   loop(a, len_A)   Z->next_ravel_Cell(A->get_cravel(a));
   loop(b, len_B)   Z->next_ravel_Cell(B->get_cravel(b));
   Z->set_default(*B, LOC);
   Z->check_value(LOC);
   return Bif_F12_COMMA::fun.eval_AB(A, BwoA.get_apl_val());
}
//============================================================================
Token
Bif_F2_RIGHT::eval_AXB(Value_P A, Value_P X, Value_P B) const
{
   // Z ← A ⊢[X] B: select corresponding items of A or B according to X.
   //  A, B, and X must have matching shapes
   //
const int inc_A = A->is_scalar_extensible() ? 0 : 1;
const int inc_B = B->is_scalar_extensible() ? 0 : 1;
const int inc_X = X->is_scalar_extensible() ? 0 : 1;

   if (inc_X == 0)   // single item X: pick entire A or B according to X
      {
        const APL_Integer x0 = X->get_cfirst().get_int_value();
        if (x0 == 0)   return Token(TOK_APL_VALUE1, CLONE_P(A, LOC));
        if (x0 == 1)   return Token(TOK_APL_VALUE1, CLONE_P(B, LOC));
        DOMAIN_ERROR;
      }

   // X is non-scalar, so it must match any non-scalar A and B
   //
   if (inc_A && ! X->same_shape(*A))
      {
        if (A->get_rank() != X->get_rank())   RANK_ERROR;
        else                                  LENGTH_ERROR;
      }

   if (inc_B && ! X->same_shape(*B))
      {
        if (B->get_rank() != X->get_rank())   RANK_ERROR;
        else                                  LENGTH_ERROR;
      }

const Shape * shape_Z = &A->get_shape();   // last resort if X and B are scalar
   if (inc_X)        shape_Z = &X->get_shape();
   else if (inc_B)   shape_Z = &B->get_shape();

Value_P Z(*shape_Z, LOC);
   loop(z, shape_Z->get_volume())
       {
        const APL_Integer xz = X->get_cravel(z*inc_X).get_int_value();   // X[z]
        if (xz == 0)        // take A[z]
           Z->next_ravel_Cell(A->get_cravel(z*inc_A));
        else if (xz == 1)   // take B[z]
           Z->next_ravel_Cell(B->get_cravel(z*inc_B));
        else
           {
             MORE_ERROR() << "non-Boolean X in A⊢[X]B";
             DOMAIN_ERROR;
           }
       }

   Z->set_default(*B, LOC);
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//============================================================================

