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

#include "Common.hh"
#include "DerivedFunction.hh"
#include "Id.hh"
#include "Output.hh"
#include "PrintOperator.hh"
#include "StateIndicator.hh"
#include "Workspace.hh"

//============================================================================
DerivedFunction::DerivedFunction(Token & larg, cFunction_P dyop, Token & rfun,
                                 const char * loc)
   : Function(ID_USER_SYMBOL, TOK_FUN2),
     left_arg(larg),
     oper(dyop),
     right_fun(rfun),
     axis(Value_P())
{
   Assert1(oper);

   Log(LOG_FunOperX)
      {
        print(CERR<< "DerivedFunction(dyadic with 2 functions)");
        CERR << " at " << loc << endl;
     }
}
//----------------------------------------------------------------------------
DerivedFunction::DerivedFunction(Token & lfun, cFunction_P dyop, Value_P X,
                                 Token & rfun, const char * loc)
   : Function(ID_USER_SYMBOL, TOK_FUN2),
     left_arg(lfun, loc),
     oper(dyop),
     right_fun(rfun),
     axis(X, loc)
{
}
//----------------------------------------------------------------------------
DerivedFunction::DerivedFunction(Token & LO, cFunction_P monop,
                                 const char * loc)
   : Function(ID_USER_SYMBOL, TOK_FUN2),
     left_arg(LO, loc),
     oper(monop),
     right_fun(TOK_VOID),
     axis(Value_P())
{
   Assert1(oper);

   Log(LOG_FunOperX)
      {
        print(CERR<< "DerivedFunction(monadic)");
        CERR << " at " << loc << endl;
     }
}
//----------------------------------------------------------------------------
DerivedFunction::DerivedFunction(Token & lfun, cFunction_P monop,
                                 Value_P X, const char * loc)
   : Function(ID_USER_SYMBOL, TOK_FUN2),
     left_arg(lfun, loc),
     oper(monop),
     right_fun(TOK_VOID),
     axis(X, loc)
{
   Assert1(oper);

   Log(LOG_FunOperX)
      {
        print(CERR << "DerivedFunction(monadic with axis)");
        CERR << " at " << loc << endl;
     }
}
//----------------------------------------------------------------------------
DerivedFunction::DerivedFunction(cFunction_P fun, Value_P X, const char * loc)
   : Function(ID_USER_SYMBOL, TOK_FUN2),
     left_arg(TOK_VOID),
     oper(fun),
     right_fun(TOK_VOID),
     axis(X)
{
   Assert1(fun);

   Log(LOG_FunOperX)
      {
        print(CERR<< "DerivedFunction(function with axis)");
        CERR << " at " << loc << endl;
     }
}
//----------------------------------------------------------------------------
DerivedFunction::~DerivedFunction()
{
   Log(LOG_FunOperX)
      {
        CERR << "~DerivedFunction()" << endl;
      }
}
//----------------------------------------------------------------------------
void
DerivedFunction::destroy_derived(const char * loc)
{
   Log(LOG_FunOperX)
      {
        CERR << "DerivedFunction::destroy_derived("
             << get_name() << ")" << endl;
      }

   left_arg.clear(loc);
   right_fun.clear(loc);
   axis.clear(loc);
}
//----------------------------------------------------------------------------
Token
DerivedFunction::eval_B(Value_P B) const
{
   Log(LOG_FunOperX)
      {
        print(CERR << "entering DerivedFunction");
        CERR << "::eval_B() , this = "
             << voidP(this) << endl;
      }

   if (left_arg.get_tag() == TOK_VOID)   // function bound to axis
      {
        return oper->eval_XB(axis, B);
      }

   if (right_fun.get_tag() != TOK_VOID)   // dyadic operator
      {
        Token & left  = const_cast<Token &>(left_arg);
        Token & right = const_cast<Token &>(right_fun);
        return oper->eval_LRB(left, right, B);
      }
   else                                   // monadic operator
      {
        if (left_arg.is_function())   // normal operator
           {
             Token & left = const_cast<Token &>(left_arg);
             if (!axis)   return oper->eval_LB(left, B);
             else         return oper->eval_LXB(left, axis, B);
           }
        else                         // operator with left value operand
           {
             Value_P A = left_arg.get_apl_val();
             if (!axis)   return oper->eval_AB(A, B);
             else         return oper->eval_AXB(A, axis, B);
           }
      }
}
//----------------------------------------------------------------------------
Token
DerivedFunction::eval_XB(Value_P X, Value_P B) const
{
   Log(LOG_FunOperX)
      {
        print(CERR << "entering DerivedFunction");
        CERR << "::eval_XB() , this = "
             << voidP(this) << endl;
      }

   if (right_fun.get_tag() != TOK_VOID)   // dyadic operator
      {
        Token & left  = const_cast<Token &>(left_arg);
        Token & right = const_cast<Token &>(right_fun);
        return oper->eval_LRXB(left, right, X, B);
      }
   else                                   // monadic operator
      {
        if (left_arg.is_function())   // normal operator
           {
             Token & left = const_cast<Token &>(left_arg);
             if (!axis)   return oper->eval_LB(left, B);
             else         return oper->eval_LXB(left, axis, B);
           }
        else                         // operator with left value operand
           {
             Value_P A = left_arg.get_apl_val();
             if (!axis)   return oper->eval_AB(A, B);
             else         return oper->eval_AXB(A, axis, B);
           }
      }
}
//----------------------------------------------------------------------------
Token
DerivedFunction::eval_AB(Value_P A, Value_P B) const
{
   Log(LOG_FunOperX)
      {
        print(CERR << "entering DerivedFunction");
        CERR << "::eval_AB()" << endl;
      }

   if (left_arg.get_tag() == TOK_VOID)   // function bound to axis
      {
        return oper->eval_AXB(A, axis, B);
      }

   if (right_fun.get_tag() != TOK_VOID)   // dyadic operator
      {
        Token & left  = const_cast<Token &>(left_arg);
        Token & right = const_cast<Token &>(right_fun);
        if (!axis)   return oper->eval_ALRB(A, left, right, B);
        else         return oper->eval_ALRXB(A, left, right, axis, B);
      }
   else                                   // monadic operator
      {
        if (left_arg.is_function())
           {
             Token & left  = const_cast<Token &>(left_arg);
             if (!axis)   return oper->eval_ALB(A, left, B);
             else         return oper->eval_ALXB(A, left, axis, B);
           }
        else
           {
             Value_P A = left_arg.get_apl_val();
             if (!axis)   return oper->eval_AB(A, B);
             else         return oper->eval_AXB(A, axis, B);
           }
      }
}
//----------------------------------------------------------------------------
Token
DerivedFunction::eval_AXB(Value_P A, Value_P X, Value_P B) const
{
   Log(LOG_FunOperX)
      {
        print(CERR << "entering DerivedFunction");
        CERR << "::eval_AXB()" << endl;
      }

   if (right_fun.get_tag() != TOK_VOID)   // dyadic operator
      {
        Token & left  = const_cast<Token &>(left_arg);
        Token & right = const_cast<Token &>(right_fun);
        return oper->eval_ALRXB(A, left, right, X, B);
      }
   else                                   // monadic operator
      {
        Token & left  = const_cast<Token &>(left_arg);
        return oper->eval_ALXB(A, left, X, B);
      }
}
//----------------------------------------------------------------------------
UCS_string
DerivedFunction::get_name() const
{
UCS_string name;
   name << "(";

   if (left_arg.is_function())   name << left_arg.get_function()->get_name();
   else                          name << "VAL";
   name << " ";

   name << oper->get_name();
   if (+axis)   name << "[]";

   if (right_fun.get_tag() != TOK_VOID)   // dyadic operator
      {
        name << " ";
        if (right_fun.is_function())
           name << right_fun.get_function()->get_name();
        else
           name << "VAL";
      }

   name << ")";
   return name;
}
//----------------------------------------------------------------------------
bool
DerivedFunction::has_result() const
{
   if (!oper->has_result())   return false;   // unlikely
   if (left_arg.is_function())   return left_arg.get_function()->has_result();
   return true;   // operator bound to left value
}
//----------------------------------------------------------------------------
void
DerivedFunction::unmark_all_values() const
{
   if (+axis)   axis->unmark();
   if (left_arg.is_apl_val())   left_arg.get_apl_val()->unmark();
}
//----------------------------------------------------------------------------
void
DerivedFunction::print_properties(ostream & out, int indent) const
{
UCS_string ind(indent, UNI_SPACE);
   out << ind << "Function derived from operator" << endl
       << ind << "Left Function: ";
   if (left_arg.is_function())   left_arg.get_function()->print(out);
   else                          out << "VAL";
   out << endl << ind << "Operator:  ";
   oper->print(out);
   if (+axis)   out << "Axis: " << *axis << endl;

   if (right_fun.get_tag() != TOK_VOID)   // dyadic operator
      {
         out << ind << "Right Function:  ";
        if (right_fun.is_function())   right_fun.get_function()->print(out);
        else                           out << "VAL";
      }

   out << endl;
}
//============================================================================
DerivedFunctionCache::DerivedFunctionCache()
   : idx(0)
{
   Log(LOG_FunOperX)
      {
         CERR << "DerivedFunctionCache created, cache at "
              << voidP(cache) << "..."
              << voidP(cache + MAX_FUN_OPER)
              << endl;
      }
}
//----------------------------------------------------------------------------
DerivedFunctionCache::~DerivedFunctionCache()
{
   reset();
   Log(LOG_FunOperX)
      {
         CERR << "DerivedFunctionCache deleted, cache at "
              << voidP(cache) << "..."
              << voidP(cache + MAX_FUN_OPER)
              << endl;
      }
}
//----------------------------------------------------------------------------
void
DerivedFunctionCache::reset()
{
   while (idx)
       {
         --idx;   // back to last item
         get(LOC)->destroy_derived(LOC);
         --idx;   // undo idx++ by get(LOC)
       }

   Log(LOG_FunOperX)
      {
         CERR << "DerivedFunctionCache reset, cache at "
              << voidP(cache) << "..."
              << voidP(cache + MAX_FUN_OPER)
              << endl;
      }
}
//----------------------------------------------------------------------------
DerivedFunction *
DerivedFunctionCache::get(const char * loc)
{
   if (idx >= MAX_FUN_OPER)   LIMIT_ERROR_FUNOPER;

   Log(LOG_FunOperX)
      {
         CERR << "DerivedFunctionCache get( " << idx << " ), cache at "
              << voidP(cache) << "..."
              << voidP(cache + MAX_FUN_OPER)
              << " at " << loc << endl;
      }

   return reinterpret_cast<DerivedFunction *>
                          (cache + idx++*sizeof(DerivedFunction));
}
//============================================================================

