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
#include <strings.h>

#include "CDR.hh"
#include "Command.hh"
#include "Function.hh"
#include "IndexExpr.hh"
#include "IndexIterator.hh"
#include "IntCell.hh"
#include "Output.hh"
#include "PrintOperator.hh"
#include "ProcessorID.hh"
#include "QuadFunction.hh"
#include "Quad_TF.hh"
#include "StateIndicator.hh"
#include "Svar_signals.hh"
#include "Symbol.hh"
#include "SystemVariable.hh"
#include "UserFunction.hh"
#include "Value.hh"
#include "ValueHistory.hh"
#include "Workspace.hh"

//----------------------------------------------------------------------------
Symbol::Symbol(Id id)
   : NamedObject(id),
     next(0),
     name(ID::get_name_UCS(id)),
     monitor_callback(0)
{
   push();
}
//----------------------------------------------------------------------------
Symbol::Symbol(const UCS_string & ucs, Id id)
   : NamedObject(id),
     next(0),
     name(ucs),
     monitor_callback(0)
{
   push();
}
//----------------------------------------------------------------------------
ostream &
Symbol::print(ostream & out) const
{
   return out << name;
}
//----------------------------------------------------------------------------
ostream &
Symbol::print_verbose(ostream & out) const
{
   out << "Symbol ";
   print(out) << " " << voidP(this) << endl;

   loop(v, value_stack.size())
       {
         out << "[" << v << "] ";
         const ValueStackItem & item = value_stack[v];
         switch(item.get_NC())
            {
              case NC_INVALID:
                   out << "---INVALID---" << endl;
                   break;

              case NC_UNUSED_USER_NAME:
                   out << "Unused user defined name" << endl;
                   break;

              case NC_LABEL:
                   out << "Label line " << item.get_label() << endl;
                   break;

              case NC_VARIABLE:
                   {
                      const Value * val = item.get_val_cptr();
                      out << "Variable at " << voidP(val) << endl;
                      val->print_properties(out, 8, false);
                      out << endl;
                   }
                   break;

              case NC_FUNCTION:
              case NC_OPERATOR:
                   {
                     cFunction_P fun = item.get_function();
                     Assert(fun);

                     fun->print_properties(out, 4);
                     out << "    ⎕NC:            " << item.get_NC() << endl
                         << "    addr:           " << voidP(fun)    << endl;

                     out << endl;
                   }
                   break;

              default: break;
            }
       }

   return out;
}
//----------------------------------------------------------------------------
void
Symbol::assign(Value_P new_value, bool clone, const char * loc)
{
   Assert(+new_value);
   Assert(value_stack.size());

   if (!new_value->is_complete())
      {
        CERR << "Incomplete value at " LOC << endl;
        new_value->print_properties(CERR, 0, false);
        VH_entry::print_history(CERR, *new_value, LOC);
        Assert(0);
      }

ValueStackItem & vs = value_stack.back();

   switch(vs.get_NC())
      {
        case NC_UNUSED_USER_NAME:
             if (clone)  new_value = CLONE_P(new_value, loc);

             vs.set_apl_value(new_value);
             if (monitor_callback)   monitor_callback(*this, SEV_ASSIGNED);
             return;

        case NC_LABEL:
             MORE_ERROR() << "attempt to assign a value to label "
                          << get_name() << " (line " << vs.get_label() << ")";

        case NC_VARIABLE:
             if (vs.get_val_cptr() == new_value.get())   return;   // X←X

             if (clone)  new_value = CLONE_P(new_value, loc);

             vs.set_apl_value(new_value);
             if (monitor_callback)   monitor_callback(*this, SEV_ASSIGNED);
             return;

        case NC_SYSTEM_VAR:
             assign_shared_variable(new_value, loc);
             if (monitor_callback)   monitor_callback(*this, SEV_ASSIGNED);
             return;

        default: SYNTAX_ERROR;
      }
}
//----------------------------------------------------------------------------
void
Symbol::assign_indexed(const Value * X, Value_P B)   // A[X] ← B
{
   // this function is only called for A[X}←B when X is one-dimensional,
   // i.e. an index with no semicolons. If X contains semicolons, then
   // Symbol::assign_indexed(IndexExpr IX, ...) is called instead.
   // 
const APL_Integer qio = Workspace::get_IO();

   value_stack.back().isolate(LOC);
Value_P Z = get_apl_value();  // the current APL value of this Symbol

   if (Z->is_member())
      {
        // Z is indexed with a member name, e.g. Z['member'] ← 42
        const UCS_string name(*X);
        Cell * data = Z->get_member_data(name);
        if (data)   // existing member
           {
             if (data->is_pointer_cell() &&
                 data->get_pointer_value()->is_member())
                {
                  MORE_ERROR()
                     << "member access: cannot override non-leaf member "
                     << name << " of variable " << get_name()
                     << ".\n      )ERASE or ⎕EX that member first.";
                  DOMAIN_ERROR;
                }
             data->release(LOC);   // release old content
           }
        else        // new member
           {
             data = Z->get_new_member(name);
           }
        data->init_from_value(B.get(), *Z, LOC);
        return;
      }

const ShapeItem max_idx = Z->element_count();
   if (X              &&     // X exists,      and
       X->is_scalar() &&     // X is a scalar, and
       B->is_scalar() &&     // B is a scalar, and
       Z->get_rank() == 1)   // Z is a vector
      {
        const APL_Integer idx = X->get_cfirst().get_near_int() - qio;
        if (idx >= 0 && idx < max_idx)   // idx is a valid index of Z
           {
             Cell & cell = Z->get_wravel(idx);
             cell.release(LOC);   // release the old value if Z[X]
             cell.init(B->get_cfirst(), *Z, LOC);   // Z[X] ← ↑B
             return;
           }
      }

   if (Z->get_rank() != 1)   RANK_ERROR;

   if (!X)   // X[] ← B
      {
        // scalar B is scalar extended according to ⍴Z
        const Cell & src = B->get_cfirst();
        loop(a, max_idx)
            {
              Cell & dest = Z->get_wravel(a);
              dest.release(LOC);   // free sub-values etc (if any)
              dest.init(src, *Z, LOC);
            }
        if (monitor_callback)   monitor_callback(*this, SEV_ASSIGNED);
        return;
      }

const ShapeItem ec_B = B->element_count();
const ShapeItem ec_X = X->element_count();
const int incr_B = (ec_B == 1) ? 0 : 1;   // maybe scalar extend B
const Cell * cX = &X->get_cfirst();
const Cell * cB = &B->get_cfirst();

   if (ec_B != 1 && ec_B != ec_X)   LENGTH_ERROR;

   loop(x, ec_X)
      {
        const ShapeItem idx = cX++->get_near_int() - qio;
        if (idx < 0)          INDEX_ERROR;
        if (idx >= max_idx)   INDEX_ERROR;
        Cell & dest = Z->get_wravel(idx);
        dest.release(LOC);   // free sub-values etc (if any)
        dest.init(*cB, *Z, LOC);

         cB += incr_B;
      }

   if (monitor_callback)   monitor_callback(*this, SEV_ASSIGNED);
}
//----------------------------------------------------------------------------
void
Symbol::assign_indexed(const IndexExpr & IX, Value_P B)   // A[IX;...] ← B
{
   if (IX.is_axis() && +IX.values[0])   // one-dimensional index, not elided
      {
         assign_indexed(IX.values[0].get(), B);
        return;
      }

   // see comments in Value::index() above.

   value_stack.back().isolate(LOC);
Value_P Z = get_apl_value();

   if (Z->get_rank() != IX.get_rank())   RANK_ERROR;   // ISO p. 159

   // B must either be a scalar (and is then scalar extended to the size
   // of the updated area, or else have the shape of the concatenated index
   // items. For example:
   //
   //  X:   X1    ; X2    ; X3
   //  ⍴B:  b1 b2   b3 b4   b5 b6
   //
   if (!B->is_scalar())
      {
        // remove dimensions with len 1 from the shapes of X and B...
        // if we see an empty Xn then we return.
        //
        Shape B1;   // shape B with axes of length 1 removed
        loop(b, B->get_rank())
           {
             const ShapeItem sh_b = B->get_shape_item(b);
             if (sh_b != 1)   B1.add_shape_item(sh_b);
           }

        Shape IX1;
        for (ShapeItem ix = IX.get_rank() - 1; ix >= 0; --ix)
            {
              const Value * ix_val = IX.values[ix].get();
              if (ix_val)   // non-elided index
                 {
                   if (ix_val->element_count() == 0)   return;   // empty index
                   loop(xx, ix_val->get_rank())
                      {
                        const ShapeItem sxx = ix_val->get_shape_item(xx);
                        if (sxx != 1)   IX1.add_shape_item(sxx);
                      }
                 }
               else         // elided index: add corresponding B dimenssion
                 {
                   if (ix >= Z->get_rank())   RANK_ERROR;
                   const ShapeItem sbx =
                         Z->get_shape_item(Z->get_rank() - ix - 1);
                   if (sbx == 0)   return;   // empty index
                   if (sbx != 1)   IX1.add_shape_item(sbx);
                 }
            }

        if (B1 != IX1)
           {
             if (B1.get_rank() != IX1.get_rank())   RANK_ERROR;
             else                                   LENGTH_ERROR;
           }
      }

MultiIndexIterator mult(Z->get_shape(), IX);

const ShapeItem ec_B = B->element_count();
const Cell * cB = &B->get_cfirst();
const int incr_B = (ec_B == 1) ? 0 : 1;

   while (mult.has_more())
      {
        const ShapeItem offset_Z = mult++;
        if (offset_Z < 0)                     INDEX_ERROR;
        if (offset_Z >= Z->element_count())   INDEX_ERROR;
        Cell & dest = Z->get_wravel(offset_Z);
        dest.release(LOC);   // free sub-values etc (if any)
        dest.init(*cB, *Z, LOC);
        cB += incr_B;
     }

   if (monitor_callback)   monitor_callback(*this, SEV_ASSIGNED);
}
//----------------------------------------------------------------------------
bool
Symbol::assign_named_lambda(cFunction_P lambda, const char * loc)
{
ValueStackItem & vs = value_stack.back();
const UserFunction * ufun = lambda->get_func_ufun();
   Assert(ufun);
const Executable * uexec = ufun;
   Assert(uexec);

   switch(vs.get_NC())
      {
        case NC_FUNCTION:
        case NC_OPERATOR:
             {
               cFunction_P old_fun = vs.get_function();
               Assert(old_fun);
               if (!old_fun->is_lambda())   SYNTAX_ERROR;
               const UserFunction * old_ufun = old_fun->get_func_ufun();
               Assert(old_ufun);
               for (StateIndicator * si = Workspace::SI_top();
                    si; si = si->get_parent())
                   {
                     if (si->uses_function(old_ufun))
                        {
                          MORE_ERROR() << "function " << get_name()
                                       << " is suspended or used";
                          return true;
                        }
                   }

               const_cast<UserFunction *>(vs.get_function()->get_func_ufun())
                         ->decrement_refcount(LOC);
             }

             /* fall through */

        case NC_UNUSED_USER_NAME:
             vs.set_function(lambda);
             const_cast<UserFunction *>(ufun)->increment_refcount(LOC);
             if (monitor_callback)   monitor_callback(*this, SEV_ASSIGNED);
             return false;

        default: SYNTAX_ERROR;
      }

   return false;
}
//----------------------------------------------------------------------------
void
Symbol::pop()
{
   if (value_stack.size() == 0)
      {
        CERR << "Symbol is: '" << get_name() << "' at " << LOC << endl;
        FIXME;
      }

const ValueStackItem & vs = value_stack.back();

   if (get_NC() == NC_VARIABLE)
      {
        Log(LOG_SYMBOL_push_pop)
           {
             const Value * ret = get_val_cptr();
             CERR << "-pop-value " << name
                  << " flags " << ret->get_flags() << " ";
             if (value_stack.size() == 0)   CERR << " (last)";
             CERR << " addr " << voidP(ret) << endl;
           }

        value_stack.pop_back();
        if (monitor_callback)   monitor_callback(*this, SEV_POPED);
      }
   else
      {
        Log(LOG_SYMBOL_push_pop)
           {
             CERR << "-pop " << name
                  << " name_class " << vs.get_NC() << " ";
             if (value_stack.size() == 0)   CERR << " (last)";
             CERR << endl;
           }
        value_stack.pop_back();
        if (monitor_callback)   monitor_callback(*this, SEV_POPED);
      }
}
//----------------------------------------------------------------------------
void
Symbol::push()
{
   Log(LOG_SYMBOL_push_pop)
      {
        CERR << "+push " << name;
        if (value_stack.size() == 0)   CERR << " (initial)";
        CERR << endl;
      }

   value_stack.push_back(ValueStackItem());
   if (monitor_callback)   monitor_callback(*this, SEV_PUSHED);
}
//----------------------------------------------------------------------------
void
Symbol::push_label(Function_Line label)
{
   Log(LOG_SYMBOL_push_pop)
      {
        CERR << "+push_label " << name;
        if (value_stack.size() == 0)   CERR << " (initial)";
        CERR << endl;
      }

   value_stack.push_back(ValueStackItem(label));
   if (monitor_callback)   monitor_callback(*this, SEV_PUSHED);
}
//----------------------------------------------------------------------------
void
Symbol::push_function(cFunction_P function)
{
   Log(LOG_SYMBOL_push_pop)
      {
        CERR << "+push_function " << name << " " << voidP(function);
        if (value_stack.size() == 0)   CERR << " (initial)";
        CERR << endl;
      }

ValueStackItem vs;
   vs.set_function(function);
   value_stack.push_back(vs);
   if (monitor_callback)   monitor_callback(*this, SEV_PUSHED);
}
//----------------------------------------------------------------------------
void
Symbol::push_value(Value_P value)
{
ValueStackItem vs;
   value_stack.push_back(vs);
   if (monitor_callback)   monitor_callback(*this, SEV_PUSHED);
   assign(value, true, LOC);

   Log(LOG_SYMBOL_push_pop)
      {
        CERR << "+push-value " << name << " flags ";
        print_flags(CERR, get_var_value()->get_flags()) << " ";
        if (value_stack.size() == 0)   CERR << " (initial)";
        CERR << " addr " << voidP(get_var_value().get()) << endl;
      }
}
//----------------------------------------------------------------------------
int
Symbol::get_exec_ufun_depth(const UserFunction * ufun)
{
const Function * fun = ufun;
const int sym_stack_size = value_stack_size();

   loop(s, sym_stack_size)
      {
        const ValueStackItem & vsi = value_stack[s];
        if (vsi.get_NC() != NC_FUNCTION &&
            vsi.get_NC() != NC_OPERATOR)   continue;
        if (fun != vsi.get_function())       continue;

       // found at level s
       //
       return s;
      }

   // not found: return -1
   return -1;
}
//----------------------------------------------------------------------------
Value_P
Symbol::get_var_value()
{
   if (value_stack.size() && get_NC() == NC_VARIABLE)
      {
        return Value_P(value_stack.back().get_val_wptr(), LOC);
      }

   return Value_P();
}
//----------------------------------------------------------------------------
const char *
Symbol::cant_be_defined() const
{
// if (value_stack.size() > 1)         return "symbol was localized";
   if (Workspace::is_called(name))
      return "function is called (used on the )SI stack). Try )SIC first.";

   if (value_stack.back().get_NC() &
        (NC_UNUSED_USER_NAME | NC_FUNCTION | NC_OPERATOR))   return 0;   // OK

   return "bad name class";
}
//----------------------------------------------------------------------------
Value_P
Symbol::get_apl_value() const
{
   Assert(value_stack.size() > 0);
   if (value_stack.back().get_NC() != NC_VARIABLE)
      Error::throw_symbol_error(get_name(), LOC);

   return Value_P(const_cast<Value *>(value_stack.back().get_val_cptr()), LOC);
}
//----------------------------------------------------------------------------
const Cell *
Symbol::get_first_cell() const
{
   Assert(value_stack.size() > 0);
   if (value_stack.back().get_NC() != NC_VARIABLE)   return 0;
   return &value_stack.back().get_val_cptr()->get_cfirst();
}
//----------------------------------------------------------------------------
bool
Symbol::can_be_assigned() const
{
   return value_stack.back().get_NC() & NC_left;
}
//----------------------------------------------------------------------------
SV_key
Symbol::get_SV_key() const
{
   Assert(value_stack.size() > 0);

   if (value_stack.back().get_NC() != NC_SYSTEM_VAR)   return SV_key(0);

   return value_stack.back().get_key();
}
//----------------------------------------------------------------------------
bool
Symbol::M_is_F() const
{
   // this is a symbol left of an ambiguous operator (i.e. / ⌿ \ or ⍀).
   // return true if the operator shall be downgraded to a function and
   // false if it should remain an operator.

const NameClass nc = get_NC();
   if (nc & NC_values)      return true;
   if (nc & NC_functions)   return false;
   if (nc == NC_OPERATOR)
      {
        Assert(get_function());
        return get_function()->get_oper_valence() == 2;
      }
   if (nc & NC_unknown)   // user error
      {
        MORE_ERROR() << "unassigned symbol " << get_name()
                     << "when resolving the function argument "
                         "of a monadic operator";
        SYNTAX_ERROR;
      }

   CERR << "TODO: Nameclass " << nc << endl;
   FIXME;
   return false;
}
//----------------------------------------------------------------------------
void
Symbol::set_SV_key(SV_key key)
{
   value_stack.back().set_key(key);
}
//----------------------------------------------------------------------------
cFunction_P
Symbol::get_function() const
{
   Assert(value_stack.size() > 0);
   if (value_stack.back().get_NC() & NC_FUN_OPER)
      return value_stack.back().get_function();
   return 0;
}
//----------------------------------------------------------------------------
cFunction_P
Symbol::get_function(unsigned int si) const
{
   Assert(value_stack.size() > 0);
   if (value_stack[si].get_NC() & NC_FUN_OPER)
      return value_stack[si].get_function();
   return 0;
}
//----------------------------------------------------------------------------
cFunction_P
Symbol::get_function()
{
const ValueStackItem & vs = value_stack.back();

   if (vs.get_NC() & NC_FUN_OPER)   return vs.get_function();

   return 0;
}
//----------------------------------------------------------------------------
void
Symbol::get_attributes(int mode, Value & Z) const
{
const ValueStackItem & vs = value_stack.back();
int has_result = 0;   // no result

   switch(vs.get_NC())
      {
        case NC_LABEL:
        case NC_VARIABLE:
             has_result = 1;
             break;

        case NC_FUNCTION:
        case NC_OPERATOR:
             vs.get_function()->get_attributes(mode, Z);
             return;

        default: break;
      }

   switch(mode)
      {
        case 1: // valences
                Z.next_ravel_Int(has_result);
                Z.next_ravel_0();
                Z.next_ravel_0();
                break;

        case 2: // creation time
        case 3: // execution properties
                loop(j, Z.element_count())   Z.next_ravel_0();
                break;

        case 4: {
                  const Value * val = get_val_cptr();
                  const CDR_type cdr_type = val->get_CDR_type();
                  const int brutto = val->total_CDR_size_brutto(cdr_type);
                  const int data = val->CDR_data_size(cdr_type);

                  Z.next_ravel_Int(brutto);
                  Z.next_ravel_Int(data);
                }
                break;

        default:  Assert(0 && "bad mode");
      }
}
//----------------------------------------------------------------------------
void
Symbol::resolve_left(Token & tok, Function_PC & PC) const
{
   Log(LOG_SYMBOL_resolve)
      CERR << "resolve_left() symbol " << get_name() << endl; 

   Assert1(value_stack.size());

const ValueStackItem & vs = value_stack.back();
   switch(vs.get_NC())
      {
        case NC_UNUSED_USER_NAME:
             return;   // leave symbol as is

        case NC_LABEL:
             --PC;
             SYNTAX_ERROR;   // assignment to (read-only) label

        case NC_VARIABLE:
             return;   // leave symbol as is

        case NC_FUNCTION:
        case NC_OPERATOR:
             if (vs.get_function()->is_lambda())
                {
                  // lambda re-assign, e.g. SYM←{ ... }
                  //
                  return;
                }
             else
                {
                  Token tok_fun = vs.get_function()->get_token();
                  tok.move(tok_fun, LOC);
                  return;
                }

        case NC_SYSTEM_VAR:
             return;   // leave symbol as is

        default:
             CERR << "Symbol is '" << get_name() << "' at " << LOC << endl;
             --PC;
             SYNTAX_ERROR;
      }
}
//----------------------------------------------------------------------------
void
Symbol::resolve_right(Token & tok, Function_PC & PC) const
{
   Log(LOG_SYMBOL_resolve)
      CERR << "resolve_right() symbol " << get_name() << endl; 

   Assert1(value_stack.size());

const ValueStackItem & vs = value_stack.back();
   switch(vs.get_NC())
      {
        case NC_UNUSED_USER_NAME:
             --PC;
             Error::throw_symbol_error(get_name(), LOC);

        case NC_LABEL:
             {
               Value_P value = IntScalar(vs.get_label(), LOC);
               Token t(TOK_APL_VALUE1, value);
               tok.move(t, LOC);
             }
             return;

        case NC_VARIABLE:
             // if we resolve a variable. the value is considered grouped.
             {
               Token tok_val(TOK_APL_VALUE1, CLONE_P(get_apl_value(), LOC));
               tok.move(tok_val, LOC);
             }
             return;

        case NC_FUNCTION:
        case NC_OPERATOR:
             {
               Token tok_function(vs.get_function()->get_token());
               tok.move(tok_function, LOC);
             }
             return;

        case NC_SYSTEM_VAR:
             resolve_shared_variable(tok);
             return;

        default:
             CERR << "Symbol is '" << get_name() << "' at " << LOC << endl;
             --PC;
             SYNTAX_ERROR;
      }
}
//----------------------------------------------------------------------------
Token
Symbol::resolve_lv(const char * loc)
{
   Log(LOG_SYMBOL_resolve)
      CERR << "resolve_lv() symbol " << get_name() << endl; 

   Assert(value_stack.size());

   if (value_stack.back().get_NC() == NC_VARIABLE)
      {
        value_stack.back().isolate(loc);
        Value_P Z = get_var_value();
        return Token(TOK_APL_VALUE1, Z->get_cellrefs(loc));
      }

   MORE_ERROR() << "Symbol '" << get_name()
                << "' has changed type from variable to name class "
                << value_stack.back().get_NC()
                << "\nwhile executing an assignment\n";
   throw_apl_error(E_LEFT_SYNTAX_ERROR, loc);
}
//----------------------------------------------------------------------------
TokenClass
Symbol::resolve_class(bool left) const
{
   Assert1(value_stack.size());

   switch(value_stack.back().get_NC())
      {
        case NC_LABEL:
        case NC_VARIABLE:
        case NC_SYSTEM_VAR:
             return left ? TC_SYMBOL : TC_VALUE;

        case NC_FUNCTION:
             {
               const int valence = value_stack.back().get_function()
                                                    ->get_fun_valence();
               if (valence == 2)   return TC_FUN2;
               if (valence == 1)   return TC_FUN1;
               return TC_FUN0;
             }

        case NC_OPERATOR:
             {
               const int valence = value_stack.back().get_function()
                                 ->get_oper_valence();
               return (valence == 2) ? TC_OPER2 : TC_OPER1;
             }

        default: return TC_SYMBOL;
      }
}
//----------------------------------------------------------------------------
int
Symbol::expunge()
{
   if (value_stack.size() == 0)   return 1;   // empty stack

ValueStackItem & vs = value_stack.back();

   if (vs.get_NC() == NC_VARIABLE)
      {
        vs.reset_apl_value();   // delete the value owned by this var
      }
   else if (vs.get_NC() &  NC_FUN_OPER)
      {
        if (vs.get_function()->is_native())
           {
             // do not delete native functions
           }
        else if (vs.get_function()->is_lambda())
           {
             const UserFunction * ufun = vs.get_function()->get_func_ufun();
             Assert(ufun);
               const_cast<UserFunction *>(vs.get_function()->get_func_ufun())
                          ->decrement_refcount(LOC);
           }
        else
           {
             const UserFunction * ufun = vs.get_function()->get_func_ufun();
             const Executable * exec = ufun;
             StateIndicator * oexec = Workspace::oldest_exec(exec);
             if (oexec)
                {
                  // ufun is still used on the SI stack. We do not delete ufun,
                  // but merely remember it for deletion later on.
                  //
                  // CERR << "⎕EX function " << ufun->get_name()
                  //      << " is on SI !" << endl;
                  Workspace::add_expunged_function(ufun);
                }
             else
                {
                  delete ufun;
                }
           }
        vs.set_NC(NC_UNUSED_USER_NAME);
      }

   vs.clear();
   call_monitor_callback(SEV_ERASED);
   return 1;
}
//----------------------------------------------------------------------------
void
Symbol::set_NC(NameClass nc)
{
ValueStackItem & vs = value_stack.back();

   // the name class can only be set for unused names.
   //
   if (vs.get_NC() == NC_UNUSED_USER_NAME)
      {
        vs.set_NC(nc);
        return;
      }

   DEFN_ERROR;
}
//----------------------------------------------------------------------------
void
Symbol::share_var(SV_key key)
{
ValueStackItem & vs = value_stack.back();

   if (vs.get_NC() == NC_UNUSED_USER_NAME)   // new shared variable
      {
        set_SV_key(key);
        return;
      }

   if (vs.get_NC() == NC_VARIABLE)           // existing variable
      {
        // remember old value
        //
        Value_P old_value = get_apl_value();

        // change name class and store AP number
        //
        set_SV_key(key);

        // assign old value to shared variable
        assign(old_value, true, LOC);

        return;
      }

   DEFN_ERROR;
}
//----------------------------------------------------------------------------
SV_Coupling
Symbol::unshare_var()
{
   if (value_stack.size() == 0)   return NO_COUPLING;

ValueStackItem & vs = value_stack.back();
   if (vs.get_NC() != NC_SYSTEM_VAR)   return NO_COUPLING;

const SV_key key = get_SV_key();
const SV_Coupling old_coupling = Svar_DB::get_coupling(key);

   Svar_DB::retract_var(key);

   set_SV_key(0);
   vs.set_NC(NC_UNUSED_USER_NAME);

   return old_coupling;
}
//----------------------------------------------------------------------------
void
Symbol::set_NC(NameClass nc, cFunction_P fun)
{
ValueStackItem & vs = value_stack.back();

const bool can_set = (vs.get_NC() == NC_FUNCTION) ||
                     (vs.get_NC() == NC_OPERATOR) ||
                     (vs.get_NC() == NC_UNUSED_USER_NAME);

   Assert(nc == NC_FUNCTION || nc == NC_OPERATOR || nc == NC_UNUSED_USER_NAME);

   if (!can_set)   DEFN_ERROR;

   if (fun)   vs.set_function(fun);
   else       vs.clear_function();
}
//----------------------------------------------------------------------------
ostream &
Symbol::list(ostream & out) const
{
   out << "   ";
   loop(s, name.size())   out << name[s];

   for (int s = name.size(); s < 32; ++s)   out << " ";

   if (is_erased())   out << "   ERASED";
   Assert(value_stack.size());
   switch(value_stack.back().get_NC())
      {
        case NC_INVALID:          out << "   INVALID NC";   break;
        case NC_UNUSED_USER_NAME: out << "   Unused";       break;
        case NC_LABEL:            out << "   Label";        break;
        case NC_VARIABLE:         out << "   Variable";     break;
        case NC_FUNCTION:         out << "   Function";     break;
        case NC_OPERATOR:         out << "   Operator";     break;
        default:                  out << "   !!! Error !!!";
      }

   return out << endl;
}
//----------------------------------------------------------------------------
void
Symbol::write_OUT(FILE * out, uint64_t & seq) const
{
char buffer[128];   // a little bigger than needed (we need 80 + 2)
UCS_string data;

   switch(value_stack[0].get_NC())
      {
        case NC_VARIABLE:
             {
               data.append(UNI_A);
               Token tok = Quad_TF::tf2_var(get_name(),
                                            *value_stack[0].get_val_cptr());
               const UCS_string ucs(*tok.get_apl_val());
               data.append(ucs);
             }
             break;

        case NC_FUNCTION:
        case NC_OPERATOR:
             {
               // write a timestamp record
               //
               cFunction_P fun = value_stack[0].get_function();
               const YMDhmsu ymdhmsu(fun->get_creation_time());
               SPRINTF(buffer, "*(%d %d %d %d %d %d %d)",
                       ymdhmsu.year, ymdhmsu.month, ymdhmsu.day,
                       ymdhmsu.hour, ymdhmsu.minute, ymdhmsu.second,
                       ymdhmsu.micro/1000);

               for (char * cp = buffer + strlen(buffer);
                    cp < (buffer + 72); )   *cp++ = ' ';
                snprintf(buffer + 72,
                         sizeof(buffer) - 72,
                         "%8.8lld\r\n",
                         long_long(seq++));
               NULL_TERMINATE(buffer)
               fwrite(buffer, 1, 82, out);

               // write function record(s)
               //
               data.append(UNI_F);
               data.append(get_name());
               data.append(UNI_SPACE);
               Quad_TF::tf2_fun_ucs(data, get_name(), *fun);
             }
             break;

        default: return;
      }

   for (ShapeItem u = 0; u < data.size() ;)
      {
        const ShapeItem rest = data.size() - u;
        if (rest <= 71)   buffer[0] = 'X';   // last record
        else              buffer[0] = ' ';   // more records
        loop(uu, 71)
           {
             unsigned char cc = ' ';
             if (u < data.size()) cc = Avec::unicode_to_cp(data[u++]);
             buffer[1 + uu] = cc;
           }

        snprintf(buffer + 72,
                 sizeof(buffer) - 72,
                 "%8.8lld\r\n",
                 static_cast<long long>(seq++));
        NULL_TERMINATE(buffer)
        fwrite(buffer, 1, 82, out);
      }
}
//----------------------------------------------------------------------------
void
Symbol::unmark_all_values() const
{
   loop(v, value_stack.size())
       {
         const ValueStackItem & item = value_stack[v];
         switch(item.get_NC())
            {
              case NC_VARIABLE:
                   item.get_val_cptr()->unmark();
                   break;

              case NC_FUNCTION:
              case NC_OPERATOR:
                   if (item.get_function()->is_native())   break;
                   {
                     // ufun can be 0 for example if F is a function argument
                     // of a defined operator and F is a primitive function
                     //
                     if (const UserFunction * ufun =
                               item.get_function()->get_func_ufun())
                        {
                          ufun->unmark_all_values();
                        }
                   }
                   break;

              default: break;
            }
       }
}
//----------------------------------------------------------------------------
int
Symbol::show_owners(ostream & out, const Value & value) const
{
int count = 0;

   loop(v, value_stack.size())
       {
         const ValueStackItem & item = value_stack[v];
         switch(item.get_NC())
            {
              case NC_VARIABLE:
                   if (Value::is_or_contains(item.get_val_cptr(), &value))
                      {
                         out << "    Variable[vs=" << v << "] "
                             << get_name() << endl;
                         ++count;
                      }
                   break;

              case NC_FUNCTION:
              case NC_OPERATOR:
                   {
                     cFunction_P fun = item.get_function();
                     const Executable * ufun = fun->get_func_ufun();
                     Assert(ufun || fun->is_native());
                     if (ufun)
                        {
                          char cc[100];
                          SPRINTF(cc, "    VS[%lld] ", long_long(v));
                          count += ufun->show_owners(cc, out, value);
                        }
                   }
                   break;

              default: break;
            }
       }

   return count;
}
//----------------------------------------------------------------------------
void
Symbol::vector_assignment(std::vector<Symbol *> & symbols, Value_P values)
{
   if (values->get_rank() > 1)   RANK_ERROR;
   if (!values->is_scalar() &&
       size_t(values->element_count()) != symbols.size())   LENGTH_ERROR;

const int incr = values->is_scalar() ? 0 : 1;
const Cell * cV = &values->get_cfirst();
   loop(s, symbols.size())
      {
        Symbol * sym = symbols[symbols.size() - s - 1];
        if (cV->is_pointer_cell())
           {
             sym->assign(cV->get_pointer_value(), true, LOC);
           }
        else
           {
             Value_P val(LOC);
             val->next_ravel_Cell(*cV);
             val->check_value(LOC);
             sym->assign(val, true, LOC);
           }

        cV += incr;   // scalar extend values
      }
}
//----------------------------------------------------------------------------
void
Symbol::dump(ostream & out) const
{
const ValueStackItem & vs = value_stack[0];
   if (vs.get_NC() == NC_VARIABLE)
      {
        UCS_string_vector CR10;
        const Value * value = vs.get_val_cptr();
        Quad_CR::do_CR10_variable(CR10, get_name(), value);

        if (value->is_member())
           out << "⍝ structured variable " << get_name() << endl;

        loop(l, CR10.size())
           {
             if (l || value->is_member())   out << "  ";
             out << CR10[l] << endl;
           }

        if (value->is_member())
           out << "⍝ end of structured variable " << get_name() << endl;

        out << endl;
      }
   else if (vs.get_NC() & NC_FUN_OPER)
      {
        cFunction_P fun = vs.get_function();
        if (fun == 0)
           {
             out << "⍝ function " << get_name() << " has function pointer 0!"
                 << endl << endl;
             return;
           }

        const UserFunction * ufun = fun->get_func_ufun();
        if (ufun == 0)
           {
             out << "⍝ function " << get_name() << " has ufun1 pointer 0!"
                 << endl << endl;
             return;
           }

        const UCS_string text = ufun->canonical(false);
        if (ufun->is_lambda())
           {
             out << get_name();
             out << "←{";
             int t = 0;
             while (t < text.size())   // skip λ header
                {
                  const Unicode uni = text[t++];
                  if (uni == UNI_LF)   break;
                }

             // skip λ← and spaces
             while (t < text.size() && text[t] <= ' ')   ++t;
             if    (t < text.size() && text[t] == UNI_LAMBDA)   ++t;
             while (t < text.size() && text[t] <= ' ')   ++t;
             if    (t < text.size() && text[t] == UNI_LEFT_ARROW)   ++t;
             while (t < text.size() && text[t] <= ' ')   ++t;

             // copy body
             //
             while (t < text.size())
                {
                   const Unicode uni = text[t++];
                   if (uni == UNI_LF)   break;
                   out << uni;
                }

             // append local variables
             //
             loop(l, ufun->local_var_count())
                 {
                   out << ";" << ufun->get_local_var(l)->get_name();
                 }
             out << UNI_R_CURLY << endl;
           }
        else
           {
             UCS_string_vector lines;
             text.to_vector(lines);
             out << "∇";
             loop(l, lines.size())
                {
                  UCS_string & line = lines[l];
                  line.remove_leading_and_trailing_whitespaces();
                  if (l)   out << " ";
                  out << line << endl;
                }

             if (ufun->get_exec_properties()[0])   out << "⍫";
             else                                  out << "∇";
             out << endl << endl;
           }
      }
}
//----------------------------------------------------------------------------
int
Symbol::get_SI_level(cFunction_P fun) const
{
   loop(v, value_stack.size())
       {
         const ShapeItem from_tos = value_stack.size() - v - 1;
         const ValueStackItem & item = value_stack[from_tos];
         if (item.get_function() == fun)
            return Workspace::SI_top()->nth_push(this, from_tos);
       }

   FIXME;
}
//----------------------------------------------------------------------------
int
Symbol::get_SI_level(const Value & val) const
{
   loop(v, value_stack.size())
       {
         const ShapeItem from_tos = value_stack.size() - v - 1;
         const ValueStackItem & vs = value_stack[from_tos];
         if (vs.get_val_cptr() == &val)
            return Workspace::SI_top()->nth_push(this, from_tos);
       }

   FIXME;
}
//----------------------------------------------------------------------------
void
Symbol::clear_vs()
{
   while (value_stack.size() > 1)
      {
        pop();
      }

ValueStackItem & tos = value_stack[0];

   switch(tos.get_NC())
      {
        case NC_LABEL:
             // should not happen since stack height == 1"
             FIXME;
             break;

        case NC_VARIABLE:
             tos.reset_apl_value();   // delete the value owned by this var
             tos.set_NC(NC_UNUSED_USER_NAME);
             break;

        case NC_UNUSED_USER_NAME:
             break;

        case NC_FUNCTION:
        case NC_OPERATOR:
             const_cast<Function *>(tos.get_function())->destroy();
             tos.set_NC(NC_UNUSED_USER_NAME);
             break;

        default: break;
      }
}
//----------------------------------------------------------------------------
ostream &
operator <<(ostream & out, const Symbol & sym)
{
   return sym.print(out);
}
//----------------------------------------------------------------------------
void
Symbol::assign_shared_variable(Value_P new_value, const char * loc)
{
   // put new_value into a CDR string
   //
CDR_string cdr;
   CDR::to_CDR(cdr, new_value.get());
   if (cdr.size() > MAX_SVAR_SIZE)   LIMIT_ERROR_SVAR;

std::string data(charP(cdr.get_items()), cdr.size());

   // wait for shared variable to be ready
   //
const bool ws_to_ws = Svar_DB::is_ws_to_ws(get_SV_key());

   for (int w = 0; ; ++w)
       {
         if (Svar_DB::may_set(get_SV_key(), w))   // ready for writing
            {
              if (w)
                 {
                   Log(LOG_shared_variables)
                      CERR << " - OK." << endl;
                 }
              break;
            }

         if (w == 0)
            {
              Log(LOG_shared_variables)
                 {
                   CERR << "Shared variable ";
                   for (const uint32_t * varname =
                                         Svar_DB::get_svar_name(get_SV_key());
                        varname && *varname; ++varname)
                       CERR << Unicode(*varname++);
                   CERR << " is blocked on set. Waiting ...";
                 }
            }
         else if (w%25 == 0)
            {
              Log(LOG_shared_variables)   CERR << ".";
            }

         usleep(10000);   // wait 10 ms
       }

const TCP_socket tcp = Svar_DB::get_DB_tcp();

   if (ws_to_ws)
      {
        // variable shared between workspaces. It is stored on APserver
        //

        // update shared var state (BEFORE sending request to peer)
        //
        ASSIGN_WSWS_VAR_c(tcp, get_SV_key(), data);
        return;
      }

   // update shared var state (BEFORE sending request to peer)
   //
   Svar_DB::set_state(get_SV_key(), false, loc);

   ASSIGN_VALUE_c request(tcp, get_SV_key(), data);

char * del = 0;
char buffer[2*MAX_SIGNAL_CLASS_SIZE];
const char * err_loc = 0;
const Signal_base * response =
      Signal_base::recv_TCP(tcp, buffer, sizeof(buffer), del, 0, &err_loc);

   if (response == 0)
      {
        cerr << "TIMEOUT on signal ASSIGN_VALUE" << endl;
        if (del)   delete del;
        VALUE_ERROR;
      }

const ErrorCode ec = ErrorCode(response->get__SVAR_ASSIGNED__error());
   if (ec)
      {
        Log(LOG_shared_variables)
           {
             Error e(ec, response->get__SVAR_ASSIGNED__error_loc().c_str());
             cerr << Error::error_name(ec) << " assigning "
                  << get_name() << ", detected at "
                  << response->get__SVAR_ASSIGNED__error_loc()
                  << endl;
           }

        delete response;
        if (del)   delete del;
        throw_apl_error(ec, loc);
      }

   delete response;
   if (del)   delete del;
}
//----------------------------------------------------------------------------
void
Symbol::resolve_shared_variable(Token & tok) const
{
   // wait for shared variable to be ready
   //
const bool ws_to_ws = Svar_DB::is_ws_to_ws(get_SV_key());

   for (int w = 0; ; ++w)
       {
         if (Svar_DB::may_use(get_SV_key(), w))   // ready for reading
            {
              if (w)
                 {
                   Log(LOG_shared_variables)   cerr << " - OK." << endl;
                 }
              break;
            }

         if (w == 0)
            {
              Log(LOG_shared_variables)
                 {
                   CERR << "apl" << ProcessorID::get_id().proc
                                    << ": Shared variable ";
                   for (const uint32_t * varname =
                                         Svar_DB::get_svar_name(get_SV_key());
                        varname && *varname; ++varname)
                       CERR << Unicode(*varname++);
                   CERR << " is blocked on use. Waiting ...";
                 }
            }
         else if (w%25 == 0)
            {
              Log(LOG_shared_variables)   cerr << ".";
            }

         usleep(10000);   // wait 10 ms
       }

const TCP_socket tcp = Svar_DB::get_DB_tcp();

   if (ws_to_ws)
      {
        // variable shared between workspaces. It is stored on APserver
        //
        READ_WSWS_VAR_c(tcp, get_SV_key());

        char * del = 0;
        char buffer[MAX_SIGNAL_CLASS_SIZE + 40000];
        const char * err_loc = 0;
        const Signal_base * response =
              Signal_base::recv_TCP(tcp, buffer, sizeof(buffer),
                                    del, 0, &err_loc);

        if (response == 0)
           {
             if (del)   delete del;
             CERR << "no response to signal READ_WSWS_VAR" << endl;
             VALUE_ERROR;
           }

        const string & data = response->get__WSWS_VALUE_IS__cdr_value();
        if (data.size() == 0)
           {
             delete response;
             if (del)   delete del;
             CERR << "no data in signal WSWS_VALUE_IS" << endl;
             VALUE_ERROR;
           }

        CDR_string cdr;
        loop(d, data.size())   cdr.push_back(data[d]);
        delete response;
        if (del)   delete del;

        Value_P value = CDR::from_CDR(cdr, LOC);
        if (!value)     VALUE_ERROR;

        value->check_value(LOC);
        new (&tok) Token(TOK_APL_VALUE1, value);
        return;
      }

GET_VALUE_c request(tcp, get_SV_key());

char * del = 0;
char buffer[MAX_SIGNAL_CLASS_SIZE + 40000];
const char * err_loc = 0;
const Signal_base * response =
      Signal_base::recv_TCP(tcp, buffer, sizeof(buffer), del, 0, &err_loc);

   if (response == 0)
      {
        CERR << "TIMEOUT on signal GET_VALUE" << endl;
        VALUE_ERROR;
      }

const ErrorCode err(ErrorCode(response->get__VALUE_IS__error()));
   if (err)
      {
        Log(LOG_shared_variables)
           {
             cerr << Error::error_name(err) << " referencing "
                  << get_name() << ", detected at "
                  << response->get__VALUE_IS__error_loc() << endl;
           }

        string eloc = response->get__VALUE_IS__error_loc();
        delete response;
        if (del)   delete del;
        throw_apl_error(err, eloc.c_str());
      }

const string & data = response->get__VALUE_IS__cdr_value();
CDR_string cdr;
   loop(d, data.size())   cdr.push_back(data[d]);
   delete response;
   if (del)   delete del;

Value_P value = CDR::from_CDR(cdr, LOC);
   if (!value)     VALUE_ERROR;

   // update shared var state (AFTER sending request to peer)
   //
   Svar_DB::set_state(get_SV_key(), true, LOC);

   value->check_value(LOC);
   new (&tok) Token(TOK_APL_VALUE1, value);
}
//----------------------------------------------------------------------------
