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

#include "Bif_F12_TAKE_DROP.hh"
#include "Bif_OPER1_EACH.hh"
#include "Macro.hh"
#include "PointerCell.hh"
#include "UserFunction.hh"
#include "Workspace.hh"

Bif_OPER1_EACH Bif_OPER1_EACH::fun;

//----------------------------------------------------------------------------
Token
Bif_OPER1_EACH::eval_ALB(Value_P A, Token & _LO, Value_P B) const
{
   // dyadic EACH: call _LO for corresponding items of A and B

   if (!A->same_shape(*B))
      {
        // if the shapes differ then either A or B must be a scalar or
        // 1-element vector.
        //
        if (A->get_rank() != B->get_rank())
           {
             if      (A->is_scalar_or_len1_vector())    ;   // OK
             else if (B->is_scalar_or_len1_vector())    ;   // OK
             else if (A->get_rank() != B->get_rank())   RANK_ERROR;
             else                                       LENGTH_ERROR;
           }
      }

cFunction_P LO = _LO.get_function();
   Assert1(LO);

   // for the ambiguous operators /. ⌿, \, and ⍀ is_operator() returns true,
   // which is incorrect in this context. We use get_signature() instead.
   //
   if ((LO->get_signature() & SIG_DYA) != SIG_DYA)   VALENCE_ERROR;

   if (A->is_empty() || B->is_empty())
      {
        if (!LO->has_result())   return Token(TOK_VOID);

        Value_P proto_A = Bif_F12_TAKE::first(*A);
        Value_P proto_B = Bif_F12_TAKE::first(*B);
        Shape shape_Z;   // will be ⍴A or ⍴B and therefore empty

        if (A->is_empty())          shape_Z = A->get_shape();
        else if (!A->is_scalar())   DOMAIN_ERROR;

        if (B->is_empty())          shape_Z = B->get_shape();
        else if (!B->is_scalar())   DOMAIN_ERROR;

        Token tZ1 = LO->eval_fill_AB(proto_A, proto_B);
        if (tZ1.get_Class() != TC_VALUE)   return tZ1;

        Value_P Z1 = tZ1.get_apl_val();
        Value_P Z(shape_Z, LOC);

        // Z1 is the prototype of the empty Z
        //
        if (Z1->is_simple_scalar())     // then ⊂Z1 is Z1
           Z->set_ravel_Cell(0, Z1->get_cscalar());
        else   // need to encose Z1
           Z->set_ravel_Pointer(0, Z1.get());

        Z->check_value(LOC);
        return Token(TOK_APL_VALUE1, Z);
      }

   if (LO->may_push_SI())   // user defined LO
      {
         const bool extend_A = A->is_scalar_or_len1_vector() && !B->is_scalar();
         const bool extend_B = B->is_scalar_or_len1_vector();

         Macro * macro = 0;
         if (LO->has_result())
            {
              if (extend_A)
                 {
                   macro = Macro::get_macro(extend_B
                                          ? Macro::MAC_Z__sA_LO_EACH_sB
                                          : Macro::MAC_Z__sA_LO_EACH_vB);
                }
             else
                {
                  macro = Macro::get_macro(extend_B
                                         ? Macro::MAC_Z__vA_LO_EACH_sB
                                         : Macro::MAC_Z__vA_LO_EACH_vB);
                }
            }
         else   // LO has no result, so we can ignore the shape of the result
            {
              if (extend_A && extend_B)
                 {
                   macro = Macro::get_macro(Macro::MAC_sA_LO_EACH_sB);
                 }

              if (extend_B)
                 {
                   macro = Macro::get_macro(Macro::MAC_vA_LO_EACH_sB);
                 }
              else if (extend_A)
                 {
                   macro = Macro::get_macro(Macro::MAC_sA_LO_EACH_vB);
                 }
               else
                 {
                   macro = Macro::get_macro(Macro::MAC_vA_LO_EACH_vB);
                 }
            }

        if (extend_A && !A->is_scalar())        // 1-element non-scalar A
           {
             if (extend_B && !B->is_scalar())   // 1-element non-scalar B
                {
                  Value_P A1(LOC);   // A1 ← , A
                  A1->get_wscalar().init(A->get_cscalar(), *A1, LOC);
                  A1->check_value(LOC);

                  Value_P B1(LOC);   // B1 ← , B
                  B1->get_wscalar().init(B->get_cscalar(), *B1, LOC);
                  B1->check_value(LOC);

                  return macro->eval_ALB(A1, _LO, B1);
                }
             else
                {
                  Value_P A1(LOC);
                  A1->get_wscalar().init(A->get_cfirst(), *A1, LOC);
                  A1->check_value(LOC);

                  return macro->eval_ALB(A1, _LO, B);
                }
           }
        else if (extend_B && !B->is_scalar())   // 1-element non-scalar B
           {
             Value_P B1(LOC);
             B1->get_wscalar().init(B->get_cfirst(), *B1, LOC);
             B1->check_value(LOC);

             return macro->eval_ALB(A, _LO, B1);
           }
        else
           {
             return macro->eval_ALB(A, _LO, B);
           }
      }


   // use the same scheme as ScalarFunction::do_scalar_AB() to determine
   // the shape of the result. In order to detect conformity errors we compute
   // shape_Z even if no result is returned.
   //
const int inc_A = A->get_increment();
const int inc_B = B->get_increment();

const Shape * shape_Z = 0;
   if      (A->is_scalar())      shape_Z = &B->get_shape();
   else if (B->is_scalar())      shape_Z = &A->get_shape();
   else if (inc_A == 0)          shape_Z = &B->get_shape();
   else if (inc_B == 0)          shape_Z = &A->get_shape();
   else if (A->same_shape(*B))   shape_Z = &B->get_shape();
   else   // error
      {
        if (!A->same_rank(*B))   RANK_ERROR;
        else                     LENGTH_ERROR;
      }

const ShapeItem len_Z = shape_Z->get_volume();
Value_P Z;
   if (LO->has_result())   Z = Value_P(*shape_Z, LOC);

   loop(z, len_Z)
      {
        const Cell * cA = &A->get_cravel(inc_A * z);
        const Cell * cB = &B->get_cravel(inc_B * z);
        const bool left_val = cB->is_lval_cell();
        Value_P LO_A = cA->to_value(LOC);     // left argument of LO
        Value_P LO_B = cB->to_value(LOC);     // right argument of LO;
        if (left_val)
           {
             Cell * dest = cB->get_lval_value();
             if (dest->is_pointer_cell())
                {
                  Value_P sub = dest->get_pointer_value();
                  LO_B = sub->get_cellrefs(LOC);
                }
           }

        Token result = LO->eval_AB(LO_A, LO_B);

        // if LO was a primitive function, then result may be a value.
        // if LO was a user defined function then result may be TOK_SI_PUSHED.
        // in both cases result could be TOK_ERROR.
        //
        if (result.get_Class() == TC_VALUE)
           {
             Value_P vZ = result.get_apl_val();

             if (vZ->is_simple_scalar() || (left_val && vZ->is_scalar()))
                Z->next_ravel_Cell(vZ->get_cfirst());
             else
                Z->next_ravel_Pointer(vZ.get());

             continue;   // next z
           }

        if (result.get_tag() == TOK_ERROR)   return result;

        Q1(result);   FIXME;
      }

   if (!Z)   return Token(TOK_VOID);   // LO without result

   Z->set_default(*B.get(), LOC);
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Bif_OPER1_EACH::do_eval_LB(Token & _LO, Value_P B)
{
   // monadic EACH: call _LO for every item of B

cFunction_P LO = _LO.get_function();
   Assert1(LO);

   if (LO->is_operator() &&
       !is_SLASH_or_BACKSLASH(_LO.get_tag()))   SYNTAX_ERROR;
   if (!(LO->get_signature() & SIG_B))          VALENCE_ERROR;

   if (B->is_empty())
      {
        if (!LO->has_result())   return Token(TOK_VOID);    // no-op

        Value_P proto_B = Bif_F12_TAKE::first(*B);
        Token tZ1 = LO->eval_fill_B(proto_B);
        if (tZ1.get_Class() != TC_VALUE)   return tZ1;

        Value_P Z1 = tZ1.get_apl_val();
        Value_P Z(B->get_shape(), LOC);

        // Z1 is the prototype of the empty Z
        //
        if (Z1->is_simple_scalar())     // then ⊂Z1 is Z1
           Z->set_ravel_Cell(0, Z1->get_cscalar());
        else   // need to encose Z1
           Z->set_ravel_Pointer(0, Z1.get());

        Z->check_value(LOC);
        return Token(TOK_APL_VALUE1, Z);
      }

   if (LO->may_push_SI())   // user defined LO
      {
        if (!LO->has_result())
           return Macro::get_macro(Macro::MAC_LO_EACH_B)->eval_LB(_LO, B);

        if (LO == &Bif_F1_EXECUTE::fun)
           return Macro::get_macro(Macro::MAC_Z__EXEC_EACH_B)->eval_B(B);

        return Macro::get_macro(Macro::MAC_Z__LO_EACH_B)->eval_LB(_LO, B);
      }

const ShapeItem len_Z = B->element_count();
Value_P Z;
   if (LO->has_result())   Z = Value_P(B->get_shape(), LOC);

   loop (z, len_Z)
      {
        if (LO->get_fun_valence() == 0)
           {
             // we allow niladic functions N so that one can loop over them with
             // N ¨ 1 2 3 4
             //
             Token result = LO->eval_();

             if (result.get_Class() == TC_VALUE)
                {
                  Value_P vZ = result.get_apl_val();
                  if (vZ->is_simple_scalar())
                     Z->next_ravel_Cell(vZ->get_cfirst());
                  else
                     Z->next_ravel_Pointer(vZ.get());

                  continue;   // next z
           }

             if (result.get_tag() == TOK_VOID)   continue;   // next z

             if (result.get_tag() == TOK_ERROR)   return result;

             Q1(result);   FIXME;
           }
        else
           {
             const Cell * cB = &B->get_cravel(z);
             const bool left_val = cB->is_lval_cell();
             Value_P LO_B = cB->to_value(LOC);      // right argument of LO

             if (left_val)
                {
                  Cell * dest = cB->get_lval_value();
                  if (dest->is_pointer_cell())
                     {
                       Value_P sub = dest->get_pointer_value();
                       LO_B = sub->get_cellrefs(LOC);
                     }
                }

             Token result = LO->eval_B(LO_B);
             if (result.get_Class() == TC_VALUE)
                {
                  Value * vZ = result.get_apl_val().get();

                  if (vZ->is_simple_scalar() || (left_val && vZ->is_scalar()))
                     Z->next_ravel_Cell(vZ->get_cfirst());
                  else
                     Z->next_ravel_Pointer(vZ);

                  continue;   // next z
                }

             if (result.get_tag() == TOK_VOID)   continue;   // next z

             if (result.get_tag() == TOK_ERROR)   return result;

             Q1(result);   Q1(*LO) FIXME;
           }
      }

   if (!Z)   return Token(TOK_VOID);   // LO without result

   Z->set_default(*B.get(), LOC);
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------

