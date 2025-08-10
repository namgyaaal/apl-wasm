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

#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <errno.h>

#include "Bif_F12_TAKE_DROP.hh"
#include "Error.hh"
#include "Output.hh"
#include "Parser.hh"
#include "StateIndicator.hh"
#include "Symbol.hh"
#include "UserFunction.hh"
#include "UserPreferences.hh"
#include "Value.hh"
#include "Workspace.hh"

//----------------------------------------------------------------------------
UserFunction::UserFunction(const UCS_string txt, const char * loc,
                           const UTF8_string & _creator, bool tolerant,
                           bool macro)
  : Function(ID_USER_SYMBOL, TOK_FUN2),
    Executable(txt, true, PM_FUNCTION, loc),
    header(txt, macro),
    creator(_creator),
    error_line(0),   // assume header is wrong
    error_info("Unspecified")
{
   if (header.get_error())
      {
        error_info = header.get_error_info();
        return;
      }

   set_creation_time(now());

   exec_properties[0] = 0;
   exec_properties[1] = 0;
   exec_properties[2] = 0;
   exec_properties[3] = 0;

   line_starts.push_back(Function_PC_0);   // will be set later.

   if (header.get_error() != E_NO_ERROR)   // bad header
      {
        error_info = header.get_error_info();
        return;
      }

   // set Function::tag
   //
   if      (header.RO())   tag = TOK_OPER2;
   else if (header.LO())   tag = TOK_OPER1;
   else if (header.A())    tag = TOK_FUN2;
   else if (header.B())    tag = TOK_FUN1;
   else                    tag = TOK_FUN0;

   parse_body(loc, tolerant, macro);
   if (error_line > 0)
      {
        error_info = "Error in function body";
        return;
      }

   if (UserPreferences::uprefs.discard_indentation)   // really ?
      {
        loop(l, get_text_size())
            {
              UCS_string line = get_text(l);
              line.remove_leading_and_trailing_whitespaces();
              set_text(l, line);
            }
      }

   error_line = -1;   // no error
   error_info = 0;
}
//----------------------------------------------------------------------------
UserFunction::UserFunction(Fun_signature sig, int lambda_num,
                           const UCS_string & text, Token_string & lambda_body,
                           const basic_string<Symbol *> & lvars)
  : Function(ID_USER_SYMBOL, TOK_FUN0),
    Executable(sig, lambda_num, text, LOC),
    header(sig, lambda_num),
    creator(UNI_LAMBDA),
    error_line(0),
    error_info("Unspecified")
{
   set_creation_time(now());

   exec_properties[0] = 0;
   exec_properties[1] = 0;
   exec_properties[2] = 0;
   exec_properties[3] = 0;

   if (header.get_error() != E_NO_ERROR)   // bad header
      {
        error_info = header.get_error_info();
        return;
      }

   if      (header.RO())   tag = TOK_OPER2;
   else if (header.LO())   tag = TOK_OPER1;
   else if (header.A())    tag = TOK_FUN2;
   else if (header.B())    tag = TOK_FUN1;
   else                    tag = TOK_FUN0;

   loop(lv, lvars.size())   header.add_local_var(lvars[lv]);

   // order of local vars is reversed. Fix that.
   //
   header.reverse_local_vars();

   parse_body_line(Function_Line_0, lambda_body, false, false, LOC);
   setup_lambdas();
   line_starts.push_back(Function_PC(lambda_body.size() - 1));
   line_starts.push_back(Function_PC_0);
   error_line = -1;   // no error
   error_info = 0;
}
//----------------------------------------------------------------------------
UserFunction::~UserFunction()
{
   Log(LOG_UserFunction__enter_leave)
      CERR << "Function " << get_name() << " deleted." << endl;
}
//----------------------------------------------------------------------------
Token
UserFunction::eval_() const
{
   Log(LOG_UserFunction__enter_leave)
      CERR << "Function " << get_name() << " calls eval_()" << endl;

   if (header.B())   SYNTAX_ERROR;   // not defined niladic

   Workspace::push_SI(this, LOC);
   if (header.Z())   header.Z()->push();

   header.eval_common();

   return Token(TOK_SI_PUSHED);
}
//----------------------------------------------------------------------------
Token
UserFunction::eval_B(Value_P B) const
{
   Log(LOG_UserFunction__enter_leave)
      {
        CERR << "Function " << get_name() << " calls eval_B("
             << Token(TOK_APL_VALUE1, B) << ")" << endl;
      }

   if (header.LO())    SYNTAX_ERROR;   // defined as operator

   Workspace::push_SI(this, LOC);

   if (header.Z())   header.Z()->push();
   if (header.A())   header.A()->push();
   if (header.X())   header.X()->push();
   if (header.B())   header.B()->push_value(B);

   header.eval_common();

   return Token(TOK_SI_PUSHED);
}
//----------------------------------------------------------------------------
Token
UserFunction::eval_XB(Value_P X, Value_P B) const
{
   Log(LOG_UserFunction__enter_leave)
      {
        CERR << "Function " << get_name() << " calls eval_B("
             << Token(TOK_APL_VALUE1, B) << ")" << endl;
      }

   if (!header.X())    AXIS_ERROR;
   if (header.LO())    SYNTAX_ERROR;   // defined as operator

   Workspace::push_SI(this, LOC);

   if (header.Z())   header.Z()->push();
   if (header.A())   header.A()->push();
   if (header.X())   header.X()->push_value(X);
   if (header.B())   header.B()->push_value(B);

   header.eval_common();

   return Token(TOK_SI_PUSHED);
}
//----------------------------------------------------------------------------
Token
UserFunction::eval_AB(Value_P A, Value_P B) const
{
   Log(LOG_UserFunction__enter_leave)
      {
        CERR << "Function " << get_name() << " calls eval_AB("
             << Token(TOK_APL_VALUE1, A) << ", "
             << Token(TOK_APL_VALUE1, B) << ")" << endl;
      }

   if (header.LO())    SYNTAX_ERROR;    // defined as operator
   if (!header.A())    VALENCE_ERROR;   // monadic

   Workspace::push_SI(this, LOC);

   if (header.Z())   header.Z()->push();
   if (header.A())   header.A()->push_value(A);
   if (header.X())   header.X()->push();
   if (header.B())   header.B()->push_value(B);

   header.eval_common();

   return Token(TOK_SI_PUSHED);
}
//----------------------------------------------------------------------------
Token
UserFunction::eval_AXB(Value_P A, Value_P X, Value_P B) const
{
   Log(LOG_UserFunction__enter_leave)
      {
        CERR << "Function " << get_name() << " calls eval_AB("
             << Token(TOK_APL_VALUE1, A) << ", "
             << Token(TOK_APL_VALUE1, B) << ")" << endl;
      }

   if (header.LO())    SYNTAX_ERROR;    // defined as operator
   if (!header.A())    VALENCE_ERROR;   // monadic
   if (!header.X())    AXIS_ERROR;

   Workspace::push_SI(this, LOC);

   if (header.Z())   header.Z()->push();
   if (header.A())   header.A()->push_value(A);
   if (header.X())   header.X()->push_value(X);
   if (header.B())   header.B()->push_value(B);

   header.eval_common();

   return Token(TOK_SI_PUSHED);
}
//----------------------------------------------------------------------------
Token
UserFunction::eval_LB(Token & LO, Value_P B) const
{
   Log(LOG_UserFunction__enter_leave)
      {
        CERR << "Function " << get_name() << " calls " << __FUNCTION__ << "(";
        print_val_or_fun(CERR, LO) << ", "
             << Token(TOK_APL_VALUE1, B) << ")" << endl;
      }

   if (header.RO())    SYNTAX_ERROR;   // dyadic operator called monadically

   Workspace::push_SI(this, LOC);

   if (header.Z())         header.Z() ->push();
   if (header.A())         header.A() ->push();
   if (LO.is_function())   header.LO()->push_function(LO.get_function());
   else                    header.LO()->push_value(LO.get_apl_val());
   if (header.X())         header.X() ->push();
   header                        .B() ->push_value(B);

   header.eval_common();

   return Token(TOK_SI_PUSHED);
}
//----------------------------------------------------------------------------
Token
UserFunction::eval_LXB(Token & LO, Value_P X, Value_P B) const
{
   Log(LOG_UserFunction__enter_leave)
      {
        CERR << "Function " << get_name() << " calls " << __FUNCTION__ << "(";
        print_val_or_fun(CERR, LO) << ", "
             << Token(TOK_APL_VALUE1, X) << ", "
             << Token(TOK_APL_VALUE1, B) << ")" << endl;
      }

   if (header.RO())    SYNTAX_ERROR;   // dyadic operator called monadically
   if (!header.X())    AXIS_ERROR;

   Workspace::push_SI(this, LOC);

   if (header.Z())         header.Z()->push();
   if (header.A())         header.A()->push();
   if (LO.is_function())   header.LO()->push_function(LO.get_function());
   else                    header.LO()->push_value(LO.get_apl_val());
   if (header.X())         header.X()->push_value(X);
   header                        .B()->push_value(B);

   header.eval_common();

   return Token(TOK_SI_PUSHED);
}
//----------------------------------------------------------------------------
Token
UserFunction::eval_ALB(Value_P A, Token & LO, Value_P B) const
{
   Log(LOG_UserFunction__enter_leave)
      {
        CERR << "Function " << get_name() << " calls " << __FUNCTION__ << "("
             << Token(TOK_APL_VALUE1, A) << ", " << endl;
        print_val_or_fun(CERR, LO) << ", "
             << Token(TOK_APL_VALUE1, B) << ")" << endl;
      }

   if (header.RO())    SYNTAX_ERROR;    // defined as dyadic operator
   if (!header.A())    VALENCE_ERROR;   // monadic

   Workspace::push_SI(this, LOC);

   if (header.X())         header.X()->push();

   if (header.Z())         header.Z()->push();
   header                        .A()->push_value(A);
   if (LO.is_function())   header.LO()->push_function(LO.get_function());
   else                    header.LO()->push_value(LO.get_apl_val());
   header                        .B()->push_value(B);

   header.eval_common();

   return Token(TOK_SI_PUSHED);
}
//----------------------------------------------------------------------------
Token
UserFunction::eval_ALXB(Value_P A, Token & LO, Value_P X, Value_P B) const
{
   Log(LOG_UserFunction__enter_leave)
      {
        CERR << "Function " << get_name() << " calls " << __FUNCTION__ << "("
             << Token(TOK_APL_VALUE1, A) << ", " << endl;
        print_val_or_fun(CERR, LO) << ", "
             << Token(TOK_APL_VALUE1, X) << ", "
             << Token(TOK_APL_VALUE1, B) << ")" << endl;
      }

   if (header.RO())    SYNTAX_ERROR;    // defined as dyadic operator
   if (!header.A())    VALENCE_ERROR;   // monadic
   if (!header.X())    AXIS_ERROR;

   Workspace::push_SI(this, LOC);

   if (header.Z())         header.Z()->push();
   header                        .A()->push_value(A);
   if (LO.is_function())   header.LO()->push_function(LO.get_function());
   else                    header.LO()->push_value(LO.get_apl_val());
   if (header.X())         header.X()->push_value(X);
   header                        .B()->push_value(B);

   header.eval_common();

   return Token(TOK_SI_PUSHED);
}
//----------------------------------------------------------------------------
Token
UserFunction::eval_LRB(Token & LO, Token & RO, Value_P B) const
{
   Log(LOG_UserFunction__enter_leave)
      {
        CERR << "Function " << get_name() << " calls " << __FUNCTION__ << "(";
        print_val_or_fun(CERR, LO) << ", ";
        print_val_or_fun(CERR, RO) << ", "
             << Token(TOK_APL_VALUE1, B) << ")" << endl;
      }

   if (!header.RO())    SYNTAX_ERROR;   // not defined as dyadic operator

   Workspace::push_SI(this, LOC);

   if (header.Z())         header.Z()->push();
   if (header.A())         header.A()->push();

   if (LO.is_function())   header.LO()->push_function(LO.get_function());
   else                    header.LO()->push_value(LO.get_apl_val());
   if (RO.is_function())   header.RO()->push_function(RO.get_function());
   else                    header.RO()->push_value(RO.get_apl_val());
   if (header.X())         header.X() ->push();
   header                        .B() ->push_value(B);

   header.eval_common();

   return Token(TOK_SI_PUSHED);
}
//----------------------------------------------------------------------------
Token
UserFunction::eval_LRXB(Token & LO, Token & RO, Value_P X, Value_P B) const
{
   Log(LOG_UserFunction__enter_leave)
      {
        CERR << "Function " << get_name() << " calls " << __FUNCTION__ << "(";
        print_val_or_fun(CERR, LO) << ", ";
        print_val_or_fun(CERR, RO) << ", "
             << Token(TOK_APL_VALUE1, X) << ", "
             << Token(TOK_APL_VALUE1, B) << ")" << endl;
      }

   if (!header.RO())   SYNTAX_ERROR;   // not defined as dyadic operator
   if (!header.X())    AXIS_ERROR;

   Workspace::push_SI(this, LOC);

   if (header.Z())         header.Z()->push();
   if (header.A())         header.A()->push();
   if (LO.is_function())   header.LO()->push_function(LO.get_function());
   else                    header.LO()->push_value(LO.get_apl_val());
   if (RO.is_function())   header.RO()->push_function(RO.get_function());
   if (header.X())         header.X()->push_value(X);
   else                    header.RO()->push_value(RO.get_apl_val());
   header                        .B()->push_value(B);

   header.eval_common();

   return Token(TOK_SI_PUSHED);
}
//----------------------------------------------------------------------------
Token
UserFunction::eval_ALRB(Value_P A, Token & LO, Token & RO, Value_P B) const
{
   Log(LOG_UserFunction__enter_leave)
      {
        CERR << "Function " << get_name() << " calls " << __FUNCTION__ << "("
             << Token(TOK_APL_VALUE1, A) << ", " << endl;
        print_val_or_fun(CERR, LO) << ", ";
        print_val_or_fun(CERR, RO) << ", "
             << Token(TOK_APL_VALUE1, B) << ")" << endl;
      }

   if (!header.RO())    SYNTAX_ERROR;   // defined monadic op called dyadically
   if (!header.A())    VALENCE_ERROR;   // monadic

   Workspace::push_SI(this, LOC);

   if (header.Z())         header.Z()->push();

   header                        .A()->push_value(A);
   if (LO.is_function())   header.LO()->push_function(LO.get_function());
   else                    header.LO()->push_value(LO.get_apl_val());
   if (RO.is_function())   header.RO()->push_function(RO.get_function());
   else                    header.RO()->push_value(RO.get_apl_val());
   if (header.X())         header.X()->push();
   header                        .B()->push_value(B);

   header.eval_common();

   return Token(TOK_SI_PUSHED);
}
//----------------------------------------------------------------------------
Token
UserFunction::eval_ALRXB(Value_P A, Token & LO, Token & RO,
                         Value_P X, Value_P B) const
{
   Log(LOG_UserFunction__enter_leave)
      {
        CERR << "Function " << get_name() << " calls " << __FUNCTION__ << "("
             << Token(TOK_APL_VALUE1, A) << ", " << endl;
        print_val_or_fun(CERR, LO) << ", ";
        print_val_or_fun(CERR, RO) << ", "
             << Token(TOK_APL_VALUE1, X) << ", "
             << Token(TOK_APL_VALUE1, B) << ")" << endl;
      }

   if (!header.RO())   SYNTAX_ERROR;   // defined monadic op called dyadically
   if (!header.A())    VALENCE_ERROR;   // monadic
   if (!header.X())    AXIS_ERROR;

   Workspace::push_SI(this, LOC);

   if (header.Z())         header.Z()->push();
   header                        .A()->push_value(A);
   if (LO.is_function())   header.LO()->push_function(LO.get_function());
   else                    header.LO()->push_value(LO.get_apl_val());
   if (RO.is_function())   header.RO()->push_function(RO.get_function());
   else                    header.RO()->push_value(RO.get_apl_val());
   if (header.X())         header.X()->push_value(X);
   header                        .B()->push_value(B);

   header.eval_common();

   return Token(TOK_SI_PUSHED);
}
//----------------------------------------------------------------------------
Token
UserFunction::eval_fill_B(Value_P B) const
{
Value_P Z = CLONE_P(B, LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
UserFunction::eval_fill_AB(Value_P A, Value_P B) const
{
Value_P Z = CLONE_P(B, LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
void
UserFunction::set_locked_error_info(Error & error) const
{
UCS_string message_2(error.get_error_line_2());

#define SHORT 0
   if (header.A())
      {
#if SHORT
        message_2.append(header.A()->get_name());
        message_2.append(UNI_SPACE);
#else
        Value_P val_A = header.A()->get_apl_value();
        if (+val_A)
           {
             PrintContext pctx(PR_BOXED_GRAPHIC);
             PrintBuffer pb(*val_A, pctx, 0);
             message_2.append(UCS_string(pb, 1, DEFAULT_Quad_PW));
             message_2.append(UNI_SPACE);
           }
#endif
      }

   message_2.append(header.get_name());

   if (header.B())
      {
#if SHORT
        message_2.append(UNI_SPACE);
        message_2.append(header.B()->get_name());
#else
        Value_P val_B = header.B()->get_apl_value();
        if (+val_B)
           {
             message_2.append(UNI_SPACE);
             PrintContext pctx(PR_APL_FUN);
             PrintBuffer pb(*val_B, pctx, 0);
             message_2.append(UCS_string(pb, 1, DEFAULT_Quad_PW));
           }
#endif
      }

   {
     UTF8_string utf(message_2);
     error.set_error_line_2(utf.c_str());
   }

   error.set_right_caret(error.get_left_caret() + message_2.size() - 7);
}
//----------------------------------------------------------------------------
void
UserFunction::set_trace_stop(std::basic_string<Function_Line> & B, bool stop)
{
   // Sort B, so that stop_lines resp. trace_lines will be sorted.
   //
std::vector<bool> ts_lines;

   // clear all bits in ts_lines
   loop(ts, line_starts.size())   ts_lines.push_back(false);

   // set all bits in B, ignorin invalid ones.
   loop(b, B.size())
      {
        const Function_Line Bb = B[b];
        if (Bb < 1)                          continue;   // not valid
        if (Bb >= int(line_starts.size()))   continue;   // not valid
        ts_lines[Bb] = true;
      }

   if (stop)   // ⎕STOP or S∆
      {
        stop_lines.clear();
        loop(ts, ts_lines.size())
           {
             if (ts_lines[ts])   stop_lines.push_back(Function_Line(ts));
           }
      }
   else        // ⎕TRACE or T∆
      {
        trace_lines.clear();
        loop(ts, ts_lines.size())
           {
             if (ts_lines[ts])   trace_lines.push_back(Function_Line(ts));
           }
      }

   parse_body(LOC, false, false);
}
//----------------------------------------------------------------------------
ErrorCode
UserFunction::transform_old_multi_lines()
{
  /* old-style multi-line strings. convert sets of lines like

     [k+1] PREFIX "L1
     [k+2] L2 ...
     [k+N] Ln" SUFFIX

    into:

    [k+1] PREFIX "L1" "L2" ... "LN" SUFFIX
    [...] (empty)
    [k+N] (empty)
   */

enum Line_status
   {
     Function_header = 0,
     APL_text        = 1,
     Start_of_string = 2,
     Inside_string   = 3,
     End_of_string   = 4
   };

char status[get_text_size()];   status[0] = Function_header;
Line_status current = APL_text;

   // determine line status of each line...
   //
   for (int l = 1; l < get_text_size(); ++l)
       {
         const int count = get_text(l).double_quote_count(false);
         if (count & 1)   // start or end of string
            {
              if (current == APL_text)   // start of multi-line string
                 {
                    status[l] = Start_of_string;
                    current = Inside_string;
                 }
              else                       // end of multi-line string
                 {
                    status[l] = End_of_string;
                    current = APL_text;
                 }
            }
         else              // no status change
            {
              status[l] = current;
            }
       }

   if (current == Start_of_string || current == Inside_string)
      {
         // multi-line string started but not ended.
         //
         return E_DEFN_ERROR;
      }

   // modify lines...
   //
   for (int l = 1; l < get_text_size();)
       {
         if (status[l] == APL_text)   { ++l;   continue; }
         const int start = l;
         Assert1(status[l] == Start_of_string);
         UCS_string accu = get_text(l++);
         accu << "\" ";
         while (status[l] == Inside_string)
               {
                 accu << " \"" << get_text(l).do_escape(true) << "\"";
                 clear_text(l++);
               }
         Assert(status[l] == End_of_string);
         accu << "\"" << get_text(l);
         set_text(start, accu);
         clear_text(l++);
       }

   // remove trailing empty lines...
   //
   while (get_text_size() && get_text(get_text_size() - 1).size() == 0)
         text.pop_back();

// CERR << endl;
// loop(l, get_text_size())   CERR << "[" << l << "]  " << get_text(l) << endl;

   return E_NO_ERROR;
}
//----------------------------------------------------------------------------
ErrorCode
UserFunction::transform_new_multi_lines()
{
  /* new-style multi-line strings. convert sets of lines like

     [k+1] PREFIX """
     [k+2] L2 ...
     [k+N] """

    into:

    [k+1] PREFIX "L2" ... "LN"
    [...] (empty)
    [k+N] (empty)

     In contrast to transform_multi_line_strings(), line k+N may only
     consist of spaces and the terminating """.
   */

enum Line_status
   {
     Function_header = 0,
     APL_text        = 1,
     Start_of_string = 2,
     Inside_string   = 3,
     End_of_string   = 4
   };

char status[get_text_size()];   status[0] = Function_header;
Line_status current = APL_text;

   // determine line status of each line...
   //
   for (ShapeItem l = 1; l < get_text_size(); ++l)
       {
         const UCS_string & line = get_text(l);
         const ShapeItem len = line.size();
         if (-1 != line.multi_pos(current == Inside_string))
            {
              if (current == APL_text)   // start of multi-line string
                 {
                    status[l] = Start_of_string;
                    current = Inside_string;
                 }
              else                       // end of multi-line string
                 {
                   bool blanks_only = true;
                   for (ShapeItem c = 0; c < (len - 3); ++c)
                       {
                          if (line[c] != UNI_SPACE &&
                              line[c] != UNI_HT)   blanks_only = false;
                       }

                    if (blanks_only)
                       {
                         status[l] = End_of_string;
                         current = APL_text;
                       }
                 }
            }
         else                            // no status change
            {
              status[l] = current;
            }
       }

   if (current == Start_of_string || current == Inside_string)
      {
         // multi-line string started but not ended.
         //
         MORE_ERROR() << "No end of multi-line string found.";
         return E_DEFN_ERROR;
      }

   // modify lines...
   //
   for (int li = 1; li < get_text_size(); )
       {
         if (status[li] == APL_text)   { ++li;   continue; }

         // a multi-line string starts at line li...
         //
         const int start = li;
         Assert1(status[li] == Start_of_string);
         UCS_string prefix = get_text(li++);
         prefix.resize(int(prefix.size() - 3));   // remove the trailing """
         UCS_string accu;
         int count = 0;
         while (status[li] == Inside_string)
               {
                 ++count;
                 accu << " \"" << get_text(li).do_escape(true) << "\"";
                 clear_text(li++);
               }

         Assert(status[li] == End_of_string);

         if (count == 0)        // nothing
            {
              const UTF8_string utf("0⍴⊂\"\"");   // 0⍴⊂""
              prefix << UCS_string(utf);
              set_text(start, prefix);
            }
         else if (count == 1)   // a single "string" would not nest
            {
              const UTF8_string utf("(,⊂");   // enclose the accu...)
              UCS_string ucs(utf);
              prefix << UCS_string(utf) << accu << ")";
              set_text(start, prefix);
            }
         else
            {
              set_text(start, prefix + accu);
            }
         clear_text(li++);
       }

   // remove trailing empty lines...
   //
   while (get_text_size() && get_text(get_text_size() - 1).size() == 0)
         text.pop_back();

// CERR << endl;
// loop(l, get_text_size())   CERR << "[" << l << "]  " << get_text(l) << endl;

   return E_NO_ERROR;
}
//----------------------------------------------------------------------------
void
UserFunction::parse_body(const char * loc, bool tolerant, bool macro)
{
   line_starts.clear();
   line_starts.push_back(Function_PC_0);   // will be set later.

UCS_string_vector original_text;
   //
   // The function text is modified for parsing but restored afterwards
   // so that e.g. ∇FUN[⎕]∇ shows the text entered by the user.
   //
   // original_text is only set if text was modified.
   //
   clear_body();

   if (UserPreferences::uprefs.multi_line_strings_3)   // new-style multi-line
      {
        for (int li = 1; li < get_text_size(); ++li)
            {
              const UCS_string & line = get_text(li);
              const ShapeItem multi = line.multi_pos(false);
              if (multi == -1)   continue;   // line contains no """

              original_text = text;   // precaution for errors
              if (const ErrorCode ec = transform_new_multi_lines())
                 {
                   text = original_text;   // restore function text
                   error_line = li;
                   return;
                 }

              break;   // transform_new_multi_lines() does all
            }
      }

   if (UserPreferences::uprefs.multi_line_strings)     // old-style multi-line
      {
        for (int li = 1; li < get_text_size(); ++li)
            {
              const UCS_string & line = get_text(li);
              if (!(line.double_quote_count(false) & 1))   continue;

              original_text = text;   // precaution for errors.
              if (const ErrorCode ec = transform_old_multi_lines())
                 {
                   text = original_text;   // restore function text
                   error_line = li;
                   return;
                 }

              break;   // transform_old_multi_lines() does all
            }
      }

   for (int l = 1; l < get_text_size(); ++l)
      {
        bool stop_line = false;
        loop(s, stop_lines.size())
           {
             if (stop_lines[s] == l)
                {
                  stop_line = true;
                  break;
                }
           }

        bool trace_line = false;
        loop(t, trace_lines.size())
           {
             if (trace_lines[t] == l)
                {
                  trace_line = true;
                  break;
                }
           }

        error_line = l;   // assume error
        line_starts.push_back(Function_PC(body.size()));

        if (stop_line)
           {
             body.push_back(Token(TOK_STOP_LINE));
             const int64_t tr = 0;
             body.push_back(Token(TOK_END, tr));
           }

        const UCS_string & line = get_text(l);
        ErrorCode ec = E_SYNTAX_ERROR;
        try {
              ec = parse_body_line(Function_Line(l), line, trace_line,
                                   tolerant, loc, macro);

              if (tolerant && ec != E_NO_ERROR)
                 {
                   UCS_string new_line(UTF8_string("## "));
                   new_line.append(line);
                   text[l] = new_line;
                   CERR << "WARNING: SYNTAX ERROR in function "
                        << header.get_name() << endl;
                 }
            }
        catch(const Error & err)
            {
              return;
            }
      }

   error_line = -1;   // OK
   setup_lambdas();

   Log(LOG_UserFunction__fix)
      {
        CERR << "body.size() is " << body.size() << endl
             << "line_starts.size() is " << line_starts.size() <<endl; 
      }

   // let [0] be the end of the function.
   line_starts[0] = Function_PC(body.size());

   if (header.Z())   body.push_back(Token(TOK_RETURN_SYMBOL, header.Z()));
   else              body.push_back(Token(TOK_RETURN_VOID));

   // restore the original text (before any multi-line expansion)
   if (original_text.size())   text = original_text;

   // recompute the →→ ←→ ←← jump PCs
   compute_if_else_targets();
}
//----------------------------------------------------------------------------
UserFunction *
UserFunction::load(const char * workspace, const char * function)
{
UserFunction * fun = 0;

   try
      {
        load(workspace, function, fun);
      }
   catch (Error & err)
      {
        delete fun;

        err.print(CERR, LOC);
      }
   catch (...)
      {
        delete fun;
        CERR << "Caught unexpected exception at " << LOC << endl;
        return 0;
      }

   return fun;
}
//----------------------------------------------------------------------------
bool
UserFunction::resolve_labels()
{
   if (DONT_FT_LABEL_LITERAL)   return false;

const int labels_declared = header.get_label_count();
   if (labels_declared == 0)   return false;   // no labels defined

int labels_seen = 0;

   // pass 1: replace all labels in the body with integers. At this point
   //         the parser has removed all TOK_INTEGER, so we can temporarily
   //         use them as markers.
   //
   loop(pc, body.size())
       {
         const Token & tok = body[pc];
         if (tok.get_tag() != TOK_SYMBOL)   continue;
         const Symbol * symbol = tok.get_sym_ptr();
         loop(idx, labels_declared)
             {
               const labVal & label = header.get_label(idx);
               if (symbol == label.sym)
                  {
                    // do not (yet) create a Value since we may need to
                    // collect several labels below.
                    //
                    body[pc] = Token(TOK_INTEGER, int64_t(label.line));
                    OptmizationStatistics::count(OPTI_FT_LABEL_LITERAL);

                    ++labels_seen;
                  }
             }
       }

   if (labels_seen == 0)   return false;   // no labels in body

   // pass 2: optimize frequent branch cases.
   //
   //         The typical branch cases are:
   //
   // case 1:   → N                 branch to single label
   // case 2:   → SEL/N1 N2 N3...   switch with strand notation
   // case 3:   → SEL/N1,N2,N3...   switch with comma separated labels
   // case 4:   a mix of cases 2 and 3
   //
bool VOID_inserted = false;
int line = 1;
   loop(pc, body.size())
       {
         if (body[pc].get_tag() == TOK_ENDL)   { ++line; continue; }
         if (body[pc].get_tag() != TOK_INTEGER)          continue;

         // collect multiple labels (if any), starting at pc. Remember that
         // the labels run backwards (i.e. start with the rightmost label).

         // at this point, pc is the first label (of one or more).
         // Collect all of them and invalidate their token.

         basic_string<int> labels_in_statement;
         labels_in_statement.push_back(body[pc].get_int_val());
         body[pc] = Token();
         for (int pc_1 = pc + 1; pc_1 < body.size(); ++pc_1)
             {
               const int pc_2 = pc_1 + 1;

               const Token & tok_1 = body[pc_1];
               if (tok_1.get_Class() == TC_END)   break;   // end of statement

               const TokenTag tag_1 = tok_1.get_tag();
               const TokenTag tag_2 = pc_2 < body.size()
                                    ? body[pc_2].get_tag() : TOK_INVALID;


               if (tag_1 == TOK_INTEGER)   // former label (set above)
                  {
                    // case 2. : INT INT
                    labels_in_statement.push_back(body[pc_1].get_int_val());
                    body[pc_1] = Token();
                    VOID_inserted = true;
                    continue;   // next pc_1
                  }
               else if (tag_1 == TOK_F12_COMMA && tag_2 == TOK_INTEGER)
                  {
                    // case 3. : INT , INT
                    labels_in_statement.push_back(body[pc_2].get_int_val());
                    body[pc_1] = Token();
                    body[pc_2] = Token();
                    VOID_inserted = true;
                    continue;   // next pc_1
                  }
               else   // end of label vector
                  {
                    break;
                  }
             }

         const int label_count = labels_in_statement.size();
         Assert(label_count);   // since body[tok_1] is one

         if (label_count == 1)   // single label (case 1 above)
            {
              Value_P single = IntScalar(labels_in_statement[0], LOC);
              Token tok(TOK_APL_VALUE1, single);
              body[pc].move(tok, LOC);
              Log(LOG_optimization) CERR << "optimizing scalar label"
                                         << " on line [" << line << "]"
                                         << endl;
            }
         else                    // multiple labels (cases 2 and 3 above)
            {
              Value_P value(label_count, LOC);
              loop(l, label_count)
                  value->next_ravel_Int(labels_in_statement[l]);
              value->check_value(LOC);
              Token tok(TOK_APL_VALUE1, value);
              body[pc].move(tok, LOC);
              Log(LOG_optimization)
                 {
                   CERR << "optimizing label vector["
                        << label_count << "] =";
                   loop(l, label_count)
                       CERR << " ["
                            << labels_in_statement[label_count - l - 1]
                            << "]" << " on line [" << line << "]" << endl;
                 }
            }
       }

   if (VOID_inserted)   remove_TOK_VOID();

   return VOID_inserted;
}
//----------------------------------------------------------------------------
bool
UserFunction::optimize_unconditional_branches()
{
   if (DONT_FT_DIRECT_BRANCHES)   return false;

   /* check for: VALUE → ENDL      e.g. → 4
      or:        SYMBOL → ENDL     e.g. → LABEL

      but rule out expressions:   e.g. → 4 + 5

      ⎕FX "FOO" "X←2" "→2" "Y←5"
      ⎕FX "FOO" "X←2" "→0" "'NOT REACHED'"
      ⎕FX "FOO" "X←2" "LABEL: Z←3 ◊ →LABEL" "Y←5"

    */
bool VOID_inserted = false;
   for (Function_PC pc = Function_PC_0; pc < body.size() - 3; ++pc)
      {
        /*
           1.  look for (and noting that body is in inverse order)
  
                PC  +3  +2 +1 +0
                    ↓   ↓  ↓  ↓
                   END  →  N END
         */
        if (body[pc    ].get_Class() != TC_END)       continue;
        if (body[pc + 1].get_Class() != TC_VALUE)     continue;
        if (body[pc + 2].get_Class() != TC_R_ARROW)   continue;
        if (body[pc + 3].get_Class() != TC_END)       continue;

        // at this point we have →VALUE.
        // figure the function line (which may be impossibe)
        //
        const Value & v_line = *body[pc + 1].get_apl_val();
        if (!v_line.is_int_scalar())                  continue;

        const APL_Integer line_number = v_line.get_cscalar().get_int_value();
        if (line_number < Function_Line_1)            continue;   // e.g. →0
        if (line_number >= int(line_starts.size()))   continue;

        const int64_t target_PC = line_starts[line_number];

        // maybe do it. This optimization does not work well with
        // conditonals,so we don't if we see one.
        //
        body[pc + 1].clear(LOC);   // release N
        body[pc + 1] = Token(TOK_GOTO_PC, target_PC);   // B with →PC
        body[pc + 2].copy_N(body[pc + 3]);              // → with ENDL
        body[pc + 3] = Token();
        VOID_inserted = true;
        OptmizationStatistics::count(OPTI_FT_DIRECT_BRANCHES);
      }

   if (VOID_inserted)   remove_TOK_VOID();

   return VOID_inserted;
}
//----------------------------------------------------------------------------
void
UserFunction::load(const char * workspace, const char * function,
                   UserFunction * & fun)
{
char filename[FILENAME_MAX + 1];
   SPRINTF(filename, "workspaces/%s/%s.fun", workspace, function);

   if (strlen(filename) > FILENAME_MAX)
      {
        CERR << "file name '" << filename << "' is too long" << endl;
        throw_apl_error(E_SYS_LIMIT_FILENAME, LOC);
      }

int in = open(filename, O_RDONLY);
   if (in == -1)
      {
        CERR << "Can't open() workspace file '" 
             << filename << "': " << strerror(errno) << endl;
        throw_apl_error(E_WS_OPEN, LOC);
      }

struct stat st;
   if (fstat(in, &st) == -1)
      {
        CERR << "Can't fstat() workspace file '" 
             << filename << "': " << strerror(errno) << endl;
        close(in);
        throw_apl_error(E_WS_FSTAT, LOC);
      }

off_t len = st.st_size;
void * start = mmap(0, len, PROT_READ, MAP_SHARED, in, 0);

   if (start == reinterpret_cast<const void *>(-1))
      {
        CERR << "Can't mmap() workspace file '" 
             << filename << "': " << strerror(errno) << endl;
        close(in);
        throw_apl_error(E_WS_MMAP, LOC);
      }

UTF8_string utf(utf8P(start), len);

   // skip trailing \r and \n.
   //
   while (utf.size() &&
          (utf.back() == '\r' || utf.back() == '\n'))   utf.pop_back();

   munmap(start, st.st_size);
   close(in);

UCS_string ucs(utf);
int error_line = -1;
   fun = fix(ucs, error_line, false, LOC, filename, false);
}
//----------------------------------------------------------------------------
Function_PC
UserFunction::pc_for_line(Function_Line line) const
{
   if (line <= Function_Line_0 || line >= Function_Line(line_starts.size()))
      return Function_PC(body.size() - 1);

   return line_starts[line];
}
//----------------------------------------------------------------------------
UserFunction *
UserFunction::fix(const UCS_string & text, int & err_line,
                  bool keep_existing, const char * loc,
                  const UTF8_string & creator, bool tolerant)
{
   Log(LOG_UserFunction__fix)
      {
        CERR << "fix pmode=user function:" << endl << text << endl
             <<  "------------------- UserFunction::fix() --" << endl;
      }

UserFunction * ufun = new UserFunction(text, loc, creator, tolerant, false);
const char * info = ufun->get_error_info();
   err_line = ufun->get_error_line();

const bool bad_function = info || err_line != -1;
   if (bad_function)   // something went wrong
      {
        if (info)
           {
             Log(LOG_UserFunction__fix)   CERR << "Error: " << info << endl;
             MORE_ERROR() << info;
           }
         else info = "Error";

         if (err_line == 0)
           {
             MORE_ERROR() << info << " in function header";
             Log(LOG_UserFunction__fix) CERR << "Bad header line" <<  endl;
           }
         else if (err_line > 0)
           {
             UCS_string & more = MORE_ERROR();
             more << info << " in function line [" << err_line << "] of:\n";
             loop(l, ufun->get_text_size())
                more << "[" << l << "] " << ufun->get_text(l) << "\n";

             Log(LOG_UserFunction__fix)
                CERR << "Bad function line: " << err_line << endl;
           }

        delete ufun;
        return 0;
      }

Symbol * symbol = Workspace::lookup_symbol(ufun->header.get_name());
cFunction_P old_function = symbol->get_function();
   if (old_function && keep_existing)
      {
        Log(LOG_UserFunction__fix)
           {
             CERR << "not fixing '" << ufun->header.get_name()
                  << "' (function already exists, and keep_existing set)"
                  << endl;
           }
        err_line = 0;
        delete ufun;
        return 0;
      }

   // check that the function can be defined (e.g. is not on the )SI stack)
   // and is not native
   //
   if (old_function)
      {
        if (const char * reason = symbol->cant_be_defined())
           {
             Log(LOG_UserFunction__fix)
                {
                  CERR << "not fixing '" << ufun->header.get_name()
                       << "' (function already exists, and " << reason << endl;
                }
             err_line = 0;
             delete ufun;
             return 0;
           }

        if (old_function->is_native())
           {
             MORE_ERROR() << "Attempt to re-define native function '"
                          << old_function->get_name() << "'. ⎕EX it first.";
             DEFN_ERROR;
           }

        const UserFunction * old_ufun = old_function->get_func_ufun();
        Assert(old_ufun);
        delete old_ufun;
      }

   // bind function to symbol
   //
   if (ufun->header.LO())   ufun->header.FUN()->set_NC(NC_OPERATOR, ufun);
   else                     ufun->header.FUN()->set_NC(NC_FUNCTION, ufun);

   Log(LOG_UserFunction__fix)
      {
        CERR << " addr " << voidP(ufun) << endl;
        ufun->print(CERR);
        CERR <<  "------------------- UserFunction::fix() OK --" << endl;
      }

   ufun->resolve_labels();
   ufun->optimize_unconditional_branches();
   if (ufun->compute_if_else_targets())
      {
        // must NOT: delete ufun;

        if (tolerant)   return 0;   // caller checks result
        DEFN_ERROR;
      }

   return ufun;
}
//----------------------------------------------------------------------------
UserFunction *
UserFunction::fix_lambda(Symbol & var, const UCS_string & text)
{
   // NOTE: only called from Archive::read_Function to adjust the different
   // texts of normal defined functions (where local vars are in the header)
   // and lambdas (where the local vars are at the end of the lambda).

   /* Example: consider {⍺+⍵;LOCAL}

      the )SAVE'd function body is:

      λ←⍺ λ1 ⍵;LOCAL
      λ← ⍺+⍵

      this function restores it to:

      λ← ⍺+⍵;LOCAL

    */
int signature = SIG_FUN | SIG_Z;
int t = 0;

ShapeItem semi = -1;
   while (t < text.size())
       {
         switch(text[t++])
            {
              case UNI_CHI:            signature |= SIG_X;    continue;
              case UNI_OMEGA:          signature |= SIG_B;    continue;
              case UNI_ALPHA_UNDERBAR: signature |= SIG_LO;   continue;
              case UNI_OMEGA_UNDERBAR: signature |= SIG_RO;   continue;
              case UNI_ALPHA:          signature |= SIG_A;    continue;

              case UNI_SEMICOLON:      if (semi == -1)   semi = t - 1;
                                       continue;

              case UNI_LF:             break;   // header line done
              default:                 continue;
            }

         break;   // header done
       }

   // discard leading spaces
   //
   while (t < text.size() && text[t] == UNI_SPACE)   ++t;

UCS_string body_text;
   for (; t < text.size(); ++t)   body_text.append(text[t]);

   while (body_text.back() == UNI_LF)  body_text.pop_back();

Token_string body;
   {
     Token ret_lambda(TOK_RETURN_SYMBOL, &Workspace::get_v_LAMBDA());
     body.push_back(ret_lambda);
     const int64_t trace = 0;
     Token tok_endl(TOK_ENDL, trace);
     body.push_back(tok_endl);
   }

   if (semi != -1)
      {
        for (ShapeItem s = semi; text[s] != UNI_LF; ++s)
           {
             body_text.append(text[s]);
           }
      }

const Parser parser(PM_FUNCTION, LOC, false);
   if (const ErrorCode ec = parser.parse(body_text, body, true))
      {
        CERR << "Parsing '" << body_text << "' failed" << endl;
        return 0;
      }

basic_string<Symbol *> local_vars;
   while (body.size() >= 2)
      {
        const size_t semi = body.size() - 2;
        if (body[semi]    .get_tag() != TOK_SEMICOL)   break;
        if (body[semi + 1].get_Class() != TC_SYMBOL)   break;
        local_vars.push_back(body[semi + 1].get_sym_ptr());
        body.resize(semi);   // leave ENDL and RETURN_SYMBOL
      }

UserFunction * ufun = new UserFunction(Fun_signature(signature), 0,
                                       body_text, body, local_vars);
   return ufun;
}
//----------------------------------------------------------------------------
void
UserFunction::destroy()
{
   // delete will call ~Executable(), which releases the values owned by body.
   //
   if (is_lambda())   decrement_refcount(LOC);
   else               delete this;
}
//----------------------------------------------------------------------------
bool
UserFunction::pushes_sym(const Symbol * sym) const
{
   if (sym == header.Z())   return true;
   if (sym == header.A())   return true;
   if (sym == header.LO())   return true;
   if (sym == header.X())   return true;
   if (sym == header.RO())   return true;
   if (sym == header.B())   return true;

   loop(l, local_var_count())
       {
         if (sym == get_local_var(l))   return true;
       }

   return false;
}
//----------------------------------------------------------------------------
void 
UserFunction::help(ostream & out) const
{
   CERR << "    Header: " << get_text(0) << endl;

   if (is_lambda())
      {
         UCS_string body(get_text(1), 2, get_text(1).size() - 2);
         CERR << "Lambda: { " << body << " ";
         loop(v, local_var_count())
            {
              const Symbol & sym = *get_local_var(v);
              CERR << ";" << sym.get_name();
            }
         CERR << " }" << endl;
         return;
      }

bool got_lamps = false;
bool toronto = false;
const UCS_string two_lamps(UTF8_string("⍝⍝"));
   for (int l = 1; l < get_text_size(); ++l)
       {
         UCS_string line(get_text(l));
         line.remove_leading_and_trailing_whitespaces();
         if (line.size() < 2)          continue;   // too short

         if (line[0] != UNI_COMMENT)   // not a comment
            {
              toronto = false;
              continue;
            }

         const bool double_lamps = line[1] == UNI_COMMENT;   // ⍝⍝ line
         if (line[1] == UNI_FULLSTOP)                  // ⍝. line
            {
              toronto = true;
            }

         if (double_lamps || toronto)
            {
              got_lamps = true;
              CERR << "    " << line << endl;
            }
       }

   if (!got_lamps)   CERR << "    (no ⍝⍝ or ⍝. comment lines)" << endl;
}
//----------------------------------------------------------------------------
ostream &
UserFunction::print(ostream & out) const
{
   out << header.get_name();
   return out;

/*
   out << "Function header:" << endl;
   if (header.Z())     out << "Result:         " << *header.Z()   << endl;
   if (header.A())     out << "Left Argument:  " << *header.A()   << endl;
   if (header.LO())    out << "Left Op Arg:    " << *header.LO()  << endl;
                       out << "Function:       " << header.get_name() << endl;
   if (header.RO())    out << "Right Op Arg:   " << *header.RO()  << endl;
   if (header.B())     out << "Right Argument: " << *header.B()   << endl;
   return Executable::print(out);
*/
}
//----------------------------------------------------------------------------
void
UserFunction::print_properties(ostream & out, int indent) const
{
   header.print_properties(out, indent);
UCS_string ind(indent, UNI_SPACE);
   out << ind << "Body Lines:      " << line_starts.size() << endl
       << ind << "Creator:         " << get_creator()      << endl
       << ind << "Body: " << body << endl;
}
//----------------------------------------------------------------------------
UCS_string
UserFunction::get_name_and_line(Function_PC pc) const
{
UCS_string ret = header.get_name();
   if (ret.size() && ret[0] == UNI_LAMBDA)
      {
        UCS_string name = Workspace::find_lambda_name(this);
        if (name.size())   ret = name;
      }
   ret.append(UNI_L_BRACK);

   // pc may point to the next token already. If that is the case then
   // we go back one token.
   //
   if (pc > 0 && body[pc - 1].get_Class() == TC_END)   pc = Function_PC(pc - 1);

const Function_Line line = get_line(pc);
   ret.append_number(line);
   ret.append(UNI_R_BRACK);
   return ret;
}
//----------------------------------------------------------------------------
Function_Line
UserFunction::get_line(Function_PC pc) const
{
   Assert(pc >= -1);
   if (pc < 0)   pc = Function_PC_0;

   // search line_starts backwards until a line with non-greater pc is found.
   //
   for (int l = line_starts.size() - 1; l > 0; --l)
       {
         if (line_starts[l] <= pc)   return Function_Line(l);
       }

   return Function_Line_1;
}
//----------------------------------------------------------------------------
UCS_string
UserFunction::canonical(bool with_lines) const
{
UCS_string ucs;
   loop(t, text.size())
      {
        if (with_lines)   ucs.append(line_prefix(Function_Line(t)));
        ucs.append(text[t]);
        ucs.append(UNI_LF);
      }

   return ucs;
}
//----------------------------------------------------------------------------
void
UserFunction::print_body_by_line(const char * where) const
{
   CERR << where << endl;
   loop(line, line_starts.size())
      {
        CERR << "[" << line << "]:";
        ShapeItem next = body.size();   // assume last line
        if ((line + 1) < int(line_starts.size()))
           next = line_starts[line + 1];
        loop(offset, next - line_starts[line])
            CERR << " " << body[int(line_starts[line]) + offset].get_Class();
        CERR << endl;
      }
}
//----------------------------------------------------------------------------
void
UserFunction::print_line_PCs(const char * loc) const
{
   CERR << "At " << loc << ":" << endl;
   for (size_t j = 1; j < line_starts.size(); ++j)
       {
         CERR << "   " << get_name() << "[" << j << "]: PC= ";
         const Function_PC PC_from = line_starts[j];

         // Note: line_starts[0] is the end of the function
         const int next = j < (line_starts.size() - 1) ? j + 1 : 0;
         const Function_PC PC_to = line_starts[next];
         CERR << PC_from << "..." << (PC_to - 1) << endl;
       }
}
//----------------------------------------------------------------------------
VoidCount
UserFunction::remove_TOK_VOID()
{
   // if line_starts is empty (= not yet initialized )then line_starts need
   // not be updated. Only the TOK_VOID need to be removed, and we pretend
   // that no token were removed (so the caller needs not care),
   //
   if (line_starts.size() == 0)
      {
        Parser::remove_TOK_VOID(body);
        return NO_VOID_TOKEN_REMOVED;
      }

   // line_starts was initialized.
   // 
size_t src_line    = Function_Line_1;
Function_PC dst_PC = Function_PC_0;

   // be careful not to increment src_PC if a line is empty!

   loop(src_PC, body.size())
      {
        while (src_PC == line_starts[src_line + 1])
           {
             // src_PC is the first token of the next line
             ++src_line;
             line_starts[src_line] = Function_PC(dst_PC);
           }

        if (body[src_PC].get_tag() == TOK_VOID)   continue;   // ignore (skip)
        if (src_PC != dst_PC)   body[dst_PC].move(body[src_PC], LOC);
         ++dst_PC;
      }

const VoidCount ret = VoidCount(body.size() - dst_PC);
   body.resize(dst_PC);
   line_starts[0] = dst_PC;   // convention: line_starts[0] is the end of body

   return ret;
}
//----------------------------------------------------------------------------
Function_PC
UserFunction::line_start(Function_Line line) const
{
   if (line < 0 || size_t(line) >= line_starts.size())
      {
        Q1(line)
        Q1(line_starts.size())
        Assert(0);
      }

   return line_starts[line];
}
//----------------------------------------------------------------------------
ostream &
UserFunction::print_val_or_fun(ostream & out, Token & tok)
{
   if (tok.is_function())         out << *tok.get_function();
   else if (tok.is_apl_val())     out << tok;
   else if (tok.is_void())        out << "((VOID))";
   else                           FIXME;

   return out;
}
//----------------------------------------------------------------------------
UCS_string
UserFunction::line_prefix(Function_Line l) const
{
char cc[40];
   if      (text.size() > 100)   SPRINTF(cc, "[%3d] ", l)
   else if (text.size() > 10)    SPRINTF(cc, "[%2d] ", l)
   else                          SPRINTF(cc, "[%d] ",  l)
   return UCS_ASCII_string(cc);
}
//----------------------------------------------------------------------------
