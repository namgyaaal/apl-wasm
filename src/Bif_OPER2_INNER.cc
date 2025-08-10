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
#include "Bif_F12_TAKE_DROP.hh"
#include "Bif_OPER2_INNER.hh"
#include "Bif_OPER1_REDUCE.hh"
#include "Macro.hh"
#include "PointerCell.hh"
#include "Workspace.hh"

Bif_OPER2_INNER   Bif_OPER2_INNER::fun;

Bif_OPER2_INNER::PJob_product Bif_OPER2_INNER::job;

//----------------------------------------------------------------------------
Token
Bif_OPER2_INNER::fill(const Shape shape_Z, Value_P A, cFunction_P fun,
                      Value_P B, const char * loc)
{
   // this function is called from A f.g B when A fun B is called with an
   // empty A or B. In this case shape_Z is empty since A->get_shape() or
   // B->get_shape() (or both) contain axes of length 0.

Value_P Fill_A;   // argument A of the fill function
Value_P Fill_B;   // argument B of the fill function

   if (A->is_empty())   Fill_A = A->prototype(LOC);
   else                 Fill_A = Bif_F12_TAKE::first(*A);

   if (B->is_empty())   Fill_B = B->prototype(LOC);
   else                 Fill_B = Bif_F12_TAKE::first(*B);

Token tok = fun->eval_fill_AB(Fill_A, Fill_B);

   if (tok.get_Class() != TC_VALUE)   return tok;

Value * Z = tok.get_apl_val().get();

Value_P Z1(shape_Z, LOC);   // shape_Z is empty
   Z1->get_wproto().init_from_value(Z, *Z1, loc);

   Z1->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z1);
}
//----------------------------------------------------------------------------
Token
Bif_OPER2_INNER::eval_ALRB(Value_P A, Token & _LO, Token & _RO, Value_P B) const
{
   if (!_LO.is_function() || !_RO.is_function())   SYNTAX_ERROR;

cFunction_P LO = _LO.get_function();
cFunction_P RO = _RO.get_function();
   Assert1(LO);
   Assert1(RO);

   // LO and RO must both be dyadic and must return a result.
   //
   if (LO->get_fun_valence() + RO->get_fun_valence() != 4)   SYNTAX_ERROR;
   if (!LO->has_result() || !RO->has_result())               DOMAIN_ERROR;

   if (!A->is_scalar_extensible() &&
       !B->is_scalar_extensible() &&
       A->get_rank() > 1          &&
       B->get_rank() > 1          && 
       A->get_shape().get_last_shape_item() !=
       B->get_shape().get_shape_item(0)
      )   LENGTH_ERROR;

const Shape shape_A1 =A->get_shape().without_last_axis();
const ShapeItem len_A = A->get_last_shape_item();

const Shape shape_B1 = B->get_shape().without_first_axis();
const ShapeItem len_B = B->get_first_shape_item();

   // we do not check len_A == len_B here, since a non-scalar LO may
   // accept different lengths of its left and right arguments

const ShapeItem items_A1 = shape_A1.get_volume();
const ShapeItem items_B1 = shape_B1.get_volume();

   if (items_A1 == 0 || items_B1 == 0)   // empty result
      {
        // the outer product portion of LO.RO is empty.
        // Apply the fill function of RO
        //
        const Shape shape_Z = shape_A1 + shape_B1;
        return fill(shape_Z, A, RO, B, LOC);
      }

   if (LO->may_push_SI() || RO->may_push_SI())   // user defined LO or RO
      {
        // ISO: if A1 and B1 are both vectors, return f/A1 g B1.
        //
        if (A->get_rank() <= 1 && B->get_rank() <= 1)
           return Macro::get_macro(Macro::MAC_Z__vA_LO_INNER_RO_vB)
                       ->eval_ALRB(A, _LO, _RO, B);
        else
           return Macro::get_macro(Macro::MAC_Z__A_LO_INNER_RO_B)
                       ->eval_ALRB(A, _LO, _RO, B);
      }

Value_P Z(shape_A1 + shape_B1, LOC);

   // an important (and the most likely) special case is LO and RO being scalar
   // functions. This case can be implemented in a far simpler fashion than
   // the general case.
   //
   job.LO = LO->get_scalar_f2();
   job.RO = RO->get_scalar_f2();
   if (job.LO && job.RO && A->is_simple() && B->is_simple() && len_A)
      {
        job.incA   = (A->element_count() == 1) ? 0 : 1;
        job.incB   = (B->element_count() == 1) ? 0 : 1;

   // len_A must be len_B, unless at least one length is 1
   //
        if (len_A != len_B && job.incA && job.incB)   LENGTH_ERROR;

        job.cZ     = &Z->get_wfirst();
        job.cA     = &A->get_cfirst();
        job.ZAh    = items_A1;
        job.LO_len = A->is_scalar() ? len_B : len_A;
        job.cB     = &B->get_cfirst();
        job.ZBl    = items_B1;
        job.ec     = E_NO_ERROR;

        scalar_inner_product();
        if (job.ec != E_NO_ERROR)   throw_apl_error(job.ec, LOC);

        Z->set_default(*B.get(), LOC);
 
        Z->check_value(LOC);
        return Token(TOK_APL_VALUE1, Z);
      }

const bool A_enclosed = A->get_rank() > 1;
const bool B_enclosed = B->get_rank() > 1;

   // enclose last axis of A if necessary
   //
   if (A_enclosed)
      {
        const Shape last_axis(A->get_rank() - 1);
        A = Bif_F12_PARTITION::enclose_with_axes(last_axis, A);
      }

   // enclose first axis of B if necessary
   //
   if (B_enclosed)
      {
        const Shape first_axis(0);
        B = Bif_F12_PARTITION::enclose_with_axes(first_axis, B);
      }

   loop (a, items_A1)
   loop (b, items_B1)
      {
        Value_P RO_A(A, LOC);
        if (A_enclosed)   RO_A = A->get_cravel(a).get_pointer_value();

        Value_P RO_B(B, LOC);
        if (B_enclosed)   RO_B = B->get_cravel(b).get_pointer_value();

        const Token T1 = RO->eval_AB(RO_A, RO_B);

        if (T1.get_tag() == TOK_ERROR)   return T1;

        Value_P A_RO_B = T1.get_apl_val();

        if (A_RO_B->is_simple_scalar())   // A_RO_B is A RO B
           {
             // A RO B has returned a scalar, so LO/A_RO_B is A_RO_B
             //
             Z->next_ravel_Cell(A_RO_B->get_cfirst());
           }
        else
           {
             // A RO B has returned vector A_RO_B, compute LO/A_RO_B
             //
             const Token T2 = Bif_OPER1_REDUCE::reduce(_LO, A_RO_B,
                                                       A_RO_B->get_rank() - 1);
             if (T2.get_tag() == TOK_ERROR)   return T2;

             Value_P V2 = T2.get_apl_val();
             if (V2->is_simple_scalar())   Z->next_ravel_Cell(V2->get_cfirst());
             else                          Z->next_ravel_Pointer(V2.get());
           }
      }

   Z->set_default(*B.get(), LOC);
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
void
Bif_OPER2_INNER::scalar_inner_product() const
{
#ifdef cfg_PERFORMANCE_COUNTERS_WANTED
const uint64_t start_1 = cycle_counter();
#endif

  // the empty cases have been ruled out already in inner_product()

   job.ec = E_NO_ERROR;

#if PARALLEL_ENABLED
   if (  Parallel::run_parallel
      && Thread_context::get_active_core_count() > 1
      && job.ZAh * job.ZBl > get_dyadic_threshold())
      {
        job.cores = Thread_context::get_active_core_count();
        Thread_context::do_work = PF_scalar_inner_product;
        Thread_context::M_fork("scalar_inner_product");   // start pool
        PF_scalar_inner_product(Thread_context::get_master());
        Thread_context::M_join();
      }
   else
#endif // PARALLEL_ENABLED
      {
        job.cores = CCNT_1;
        PF_scalar_inner_product(Thread_context::get_master());
      }

#ifdef cfg_PERFORMANCE_COUNTERS_WANTED
const uint64_t end_1 = cycle_counter();
   Performance::fs_OPER2_INNER_AB.add_sample(end_1-start_1, job.ZAh * job.ZBl);
#endif
}
//----------------------------------------------------------------------------
void
Bif_OPER2_INNER::PF_scalar_inner_product(Thread_context & tctx)
{
const ShapeItem Z_len = job.ZAh * job.ZBl;

const ShapeItem slice_len = (Z_len + job.cores - 1)/job.cores;
ShapeItem z = tctx.get_N() * slice_len;
ShapeItem end_z = z + slice_len;
   if (end_z > Z_len)   end_z = Z_len;

   for (; z < end_z; ++z)
       {
        const ShapeItem zah = z/job.ZBl;         // z row = A row
        const ShapeItem zbl = z - zah*job.ZBl;   // z column = B column
        const Cell * row_A = job.cA + job.incA*((zah + 1) * job.LO_len);
        const Cell * col_B = job.cB + job.incB*(zbl + job.LO_len*job.ZBl);

        // compute Z[z] ← LO / (row_A RO colB)
        //   e.g.  Z[z] ← +/ (row_A × colB)
        //
        // we use Z[z] as accumulator for LO /
        //
        // we use the terms sum and product as if Z←A LO.RO B were A +.* B
        //
        Cell * sum = job.cZ + z;
        loop(l, job.LO_len)
           {
             row_A -= job.incA;
             col_B -= job.incB*job.ZBl;

             if (l == 0)   // store first product in Z[z]
                {
                  job.ec = (col_B->*job.RO)(sum, row_A);
                  if (job.ec != E_NO_ERROR)   return;
                }
             else          // add subsequent product to Z[z]
                {
                  Cell product;   // the result of RO, e.g. of × in +/×
                  job.ec = (col_B->*job.RO)(&product, row_A);
                  if (job.ec != E_NO_ERROR)   return;

                  job.ec = (sum->*job.LO)(sum, &product);
                  if (job.ec != E_NO_ERROR)   return;
                }
           }
       }
}
//----------------------------------------------------------------------------
