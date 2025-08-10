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

#include "Bif_OPER1_REDUCE.hh"
#include "Macro.hh"
#include "PointerCell.hh"
#include "Workspace.hh"

Bif_OPER1_REDUCE    Bif_OPER1_REDUCE ::fun;
Bif_OPER1_REDUCE1   Bif_OPER1_REDUCE1::fun;

//----------------------------------------------------------------------------
Token
Bif_REDUCE::replicate(Value_P A, Value_P B, uAxis axis)
{
   // turn scalar B into ,B
   //
Shape shape_B = B->get_shape();
   if (shape_B.get_rank() == 0)
      {
         shape_B.add_shape_item(1);
         axis = 0;
      }

   if (A->get_rank() > 1)             RANK_ERROR;
   if (axis >= shape_B.get_rank())    AXIS_ERROR;

const ShapeItem len_B = shape_B.get_shape_item(axis);
ShapeItem len_A = A->element_count();

   // compute len_Z ← +/A
   //
ShapeItem len_Z = 0;
std::vector<ShapeItem> rep_counts;
   rep_counts.reserve(len_B);
   if (len_A == 1)   // single a -> a a ... a (len_B times)
      {
        len_A = len_B;
        APL_Integer rep_A = A->get_cfirst().get_near_int();
        loop(a, len_A)   rep_counts.push_back(rep_A);
        if (rep_A < 0)   len_Z = -rep_A*len_B;   // replicate ↑B
        else             len_Z =  rep_A*len_B;   // replicat B[a]
      }
   else              // normal A
      {
        ShapeItem nonneg_A = 0;   // number of items >= 0 in A
        loop(a, len_A)
           {
             const APL_Integer rep_A = A->get_cravel(a).get_near_int();
             rep_counts.push_back(rep_A);
             len_Z += rep_A;   ++nonneg_A;        // most likely:  rep_A >= 0
             if (rep_A < 0)    { len_Z -= 2*rep_A;  --nonneg_A; }   // rare
           }

        // the B axis shall have an item for every non-negative A
        if (len_B != 1 && nonneg_A != len_B)   LENGTH_ERROR;
      }

Shape shape_Z(shape_B);
   shape_Z.set_shape_item(axis, len_Z);

Value_P Z(shape_Z, LOC);

const Shape3 shape_B3(shape_B, axis);

   loop(h, shape_B3.h())
      {
        ShapeItem bm = 0;
        loop(m, rep_counts.size())
           {
             const ShapeItem rep = rep_counts[m];

             if (rep >= 0)   // copy l*rep items
                {
                  loop(r, rep)
                  loop(l, shape_B3.l())
                     {
                       const ShapeItem src = shape_B3.hml(h, bm, l);
                       Z->next_ravel_Cell(B->get_cravel(src));
                     }
                  if (shape_B3.m() > 1)   ++bm;
                }
             else  // init l*-rep items with the fill item
                {
                  loop(r, -rep)
                  loop(l, shape_B3.l())
                     {
                       const ShapeItem src = shape_B3.hml(h, 0, l);
                       Z->next_ravel_Proto(B->get_cravel(src));
                     }

                  // cB is not incremented when fill item is used.
                }
           }
      }

   Z->set_default(*B.get(), LOC);
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Bif_REDUCE::reduce(Token & tok_LO, Value_P B, uAxis axis)
{
   // if B is a scalar, then Z is B.
   //
   if (B->get_rank() == 0)      return Token(TOK_APL_VALUE1, CLONE_P(B, LOC));

   if (!tok_LO.is_function())
      {
        MORE_ERROR()
        << "The left argument of operator / resp. ⌿ is not a function";
        DOMAIN_ERROR;
      }

cFunction_P LO = tok_LO.get_function();
   Assert1(LO);
   if (!LO->has_result())
      {
        MORE_ERROR() << "The left argument of operator / resp. ⌿"
                        " is a function that returns no result";
        DOMAIN_ERROR;
      }

   if (LO->get_fun_valence() != 2)
      {
        MORE_ERROR() << "The left argument of operator / resp. ⌿"
                     << " is a function that is not dyadic";
        SYNTAX_ERROR;
      }

   if (axis >= B->get_rank())   AXIS_ERROR;

const ShapeItem m_len = B->get_shape_item(axis);
   if (m_len == 0)   // apply the identity function
      {
        /*
           Theory:   A)   f/B₁ B₂ ... Bₙ   ←→    (f/B₁, B₂, ... Bₙ₋₁) f Bₙ
           -------   B)   f/B₁   ←→    B₁

                             A)          B)
            therefore:  B₁   ←→   f/B₁   ←→   B₀ f B₁
                        if there is such a B₀ (i.e. with B₀ f B₁   ←→    B₁).
         */
        return LO->eval_identity_fun(B, axis);
      }

const Shape shape_Z = B->get_shape().without_axis(axis);
   if (m_len == 1)   return Bif_F12_RHO::do_reshape(shape_Z, *B);

   // non-trivial reduce (len > 1)
   //
const Shape3 B3(B->get_shape(), axis);
   if (LO->may_push_SI())   // user defined LO
      {
        Value_P X4(4, LOC);
        X4->next_ravel_Int(axis + Workspace::get_IO());
        X4->next_ravel_Int(B3.h());
        X4->next_ravel_Int(B3.m());
        X4->next_ravel_Int(B3.l());
        X4->check_value(LOC);
        return Macro::get_macro(Macro::MAC_Z__LO_REDUCE_X4_B)
                                ->eval_LXB(tok_LO, X4, B);
      }

const Shape3 Z3(B3.h(), 1, B3.l());
   return do_reduce(shape_Z, Z3, B3.m(), LO, B, B->get_shape_item(axis));
}
//----------------------------------------------------------------------------
Token
Bif_REDUCE::reduce_n_wise(Value_P A, Token & tok_LO,
                          Value_P B, uAxis axis) const
{
   if (!tok_LO.is_function())
      {
        MORE_ERROR() << "Function argument f of monadic operator A f/ B"
                        " resp. A f⌿ B is not a function";
        DOMAIN_ERROR;
      }

cFunction_P LO = tok_LO.get_function();
   if (!LO->has_result())
      {
        MORE_ERROR() << "In A " << LO->get_name() << get_name() << " B: "
                     <<  LO->get_name() << " returns no result";
        DOMAIN_ERROR;
      }

   if (LO->get_fun_valence() != 2)
      {
        MORE_ERROR() << "In A " << LO->get_name() << get_name() << " B: "
                     <<  LO->get_name() << " is not a dyadic function";
        VALENCE_ERROR;
      }

   if (A->element_count() != 1)   LENGTH_ERROR;
const APL_Integer A0 = A->get_cfirst().get_int_value();
const int n_wise = A0 < 0 ? -A0 : A0;   // the number of items (= M1 in ISO)

   if (B->is_scalar())
      {
        if (n_wise > 2)    DOMAIN_ERROR;
        if (n_wise == 0)
           {
              Token ident = LO->eval_identity_fun(B, axis);
              Value_P Z(2, LOC);
              Z->next_ravel_Cell(ident.get_apl_val()->get_cfirst());
              Z->next_ravel_Cell(ident.get_apl_val()->get_cfirst());
              return Token(TOK_APL_VALUE1, Z);
           }

        // n_wise is 1 or 2: return (2 - n_wise) ⍴ B
        //
        Shape sh;
        if (n_wise == 2)   sh.add_shape_item(0);
        return Bif_F12_RHO::do_reshape(sh, *B);
      }
   else
      {
        if (n_wise > (1 + B->get_shape_item(axis)))   DOMAIN_ERROR;
      }

   Assert1(LO);

   if (B->get_rank() == 0)      return Token(TOK_APL_VALUE1, CLONE_P(B, LOC));

   if (axis >= B->get_rank())   AXIS_ERROR;

   if (n_wise == 0)   // apply the identity function
      {
        Shape shape_B1 = B->get_shape().insert_axis(axis, 0);
        shape_B1.increment_shape_item(axis + 1);
        Value_P val(shape_B1, LOC);
        val->set_ravel_Cell(0, B->get_cproto()); // prototype

        Token result = LO->eval_identity_fun(val, axis);
        return result;
      }

   if (n_wise == (1 + B->get_shape_item(axis)))   // empty result
      {
        // the examples in ISO/IEC 13751 (2000) p. 116, as well as lrm,
        // suggest an empty result, while the algorithm on pp. 115/116 seems
        // not to work for n_wise == (1 + B->get_shape_item(axis)).
        // We follow the examples and lrm.
        //
        Shape shape_Z = B->get_shape();
        shape_Z.set_shape_item(axis, 0);
        Value_P Z(shape_Z, LOC);
        Z->set_ravel_Cell(0, B->get_cproto()); // prototype
        Z->check_value(LOC);
        return Token(TOK_APL_VALUE1, Z);
      }

Shape shape_Z(B->get_shape());
   shape_Z.set_shape_item(axis, shape_Z.get_shape_item(axis) - n_wise + 1);
   if (shape_Z.is_empty())   return LO->eval_identity_fun(B, axis);

   if (n_wise == 1)   return Bif_F12_RHO::do_reshape(shape_Z, *B);

const Shape3 Z3(shape_Z, axis);
const Shape3 B3(B->get_shape(), axis);
   if (LO->may_push_SI())   // user defined LO
      {
        Value_P A1 = IntScalar(A0 < 0 ?  A0 + 1 : 1 - A0, LOC);
        Value_P vsh_Z(LOC, &shape_Z);
        Value_P vsh_Z3(LOC, &Z3);
        Value_P vsh_B3(LOC, &B3);
        Value_P X4(4, LOC);
        X4->next_ravel_Int(axis + Workspace::get_IO());   // X
        X4->next_ravel_Pointer(vsh_Z.get());              // ⍴Z
        X4->next_ravel_Pointer(vsh_Z3.get());             // ⍴Z3
        X4->next_ravel_Pointer(vsh_B3.get());             // ⍴B3
        X4->check_value(LOC);
        if (A0 < 0)   return Macro::get_macro(Macro::MAC_Z__nA_LO_REDUCE_X4_B)
                                              ->eval_ALXB(A1, tok_LO, X4, B);
        else         return Macro::get_macro(Macro::MAC_Z__pA_LO_REDUCE_X4_B)
                                              ->eval_ALXB(A1, tok_LO, X4, B);
      }

   return do_reduce(shape_Z, Z3, A0, LO, B, B->get_shape_item(axis));
}
//----------------------------------------------------------------------------
Token
Bif_REDUCE::do_reduce(const Shape & shape_Z, const Shape3 & Z3, ShapeItem nwise,
                      cFunction_P LO, Value_P B, ShapeItem bm)
{
Value_P Z(shape_Z, LOC);

   // constants that are valid for the entire reduction
   //
const ShapeItem len_Z   = Z->element_count();
const ShapeItem len_L   = Z3.get_last_shape_item();
const ShapeItem len_BML = bm * len_L;
const ShapeItem len_ZM  = Z3.m();
const ShapeItem len_ZML = len_L * len_ZM;
prim_f2 scalar_LO       = LO->get_scalar_f2();

   loop(z, len_Z)
      {
        // break down z into H, M, and L components
        //
        const ShapeItem z_L =  z % len_L;
        const ShapeItem z_M = (z / len_L) % len_ZM;
        const ShapeItem z_H =  z / len_ZML;

        // compute LO_count (= the number of LO-reductions), and b (= the
        // starting point of the LO-reductions). They depend on z and nwise
        //
        const ShapeItem LO_count = (nwise == -1) ? z_M
                                 : (nwise < -1)  ? - nwise - 1
                                 :                 nwise - 1;

        ShapeItem b = z_L + z_H * len_BML       // start of row in B
                          + LO_count * len_L;   // end of beam in B

        // for reverse (i,e. nwise < -1) reduction direction go to the
        // start of the beam instead of the end of the beam.
        //
        if (nwise < -1)    b += (nwise + 1) * len_L;
        if (nwise != -1)   b += z_M * len_L; // start of beam

        // Z[z] = B[b]. We use Z[z] as accumulator for the reduction
        //
        Cell & accu = Z->get_wravel(z);
        accu.init(B->get_cravel(b), *Z, LOC);

        loop(lo, LO_count)
           {
             if (nwise < -1)   b += len_L;   // move forward in B
             else              b -= len_L;   // move backward in B

             // one reduction step (one call of LO)
             //
             const Cell & cB = B->get_cravel(b);
             if (scalar_LO && accu.is_simple_cell() && cB.is_simple_cell())
                {
                  const ErrorCode ec = (accu.*scalar_LO)(&accu, &cB);
                  if (ec)   throw_apl_error(ec, LOC);
                }
             else
                {
                  Value_P LO_A = cB.to_value(LOC);
                  Value_P LO_B = accu.to_value(LOC);
                  Token result = LO->eval_AB(LO_A, LO_B);
                  accu.release(LOC);

                  if (result.get_tag() == TOK_ERROR)
                    throw_apl_error(result.get_ErrorCode(), LOC);

                  Assert(result.get_Class() == TC_VALUE);
                  Value_P ZZ = result.get_apl_val();
                  accu.init_from_value(ZZ.get(), *Z, LOC);
                }
           }
      }

   Z->set_default(*B.get(), LOC);
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Bif_OPER1_REDUCE::eval_AXB(Value_P A, Value_P X, Value_P B) const
{
const sAxis axis = Value::get_single_axis(X.get(), B->get_rank());
   return replicate(A, B, axis);
}
//----------------------------------------------------------------------------
Token
Bif_OPER1_REDUCE::eval_LXB(Token & _LO, Value_P X, Value_P B) const
{
const sAxis axis = Value::get_single_axis(X.get(), B->get_rank());
   return reduce(_LO, B, axis);
}
//----------------------------------------------------------------------------
Token
Bif_OPER1_REDUCE::eval_ALXB(Value_P A, Token & _LO, Value_P X, Value_P B) const
{
const sAxis axis = Value::get_single_axis(X.get(), B->get_rank());
   return reduce_n_wise(A, _LO, B, axis);
}
//----------------------------------------------------------------------------
Token
Bif_OPER1_REDUCE1::eval_AXB(Value_P A, Value_P X, Value_P B) const
{
const sAxis axis = Value::get_single_axis(X.get(), B->get_rank());

   return replicate(A, B, axis);
}
//----------------------------------------------------------------------------
Token
Bif_OPER1_REDUCE1::eval_LXB(Token & LO, Value_P X, Value_P B) const
{
const sAxis axis = Value::get_single_axis(X.get(), B->get_rank());
   return reduce(LO, B, axis);
}
//----------------------------------------------------------------------------
Token
Bif_OPER1_REDUCE1::eval_ALXB(Value_P A, Token & LO, Value_P X, Value_P B) const
{
const sAxis axis = Value::get_single_axis(X.get(), B->get_rank());
   return reduce_n_wise(A, LO, B, axis);
}
//----------------------------------------------------------------------------
