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

#include "Backtrace.hh"
#include "Error.hh"
#include "Output.hh"
#include "Parser.hh"
#include "StateIndicator.hh"
#include "Tokenizer.hh"
#include "Symbol.hh"
#include "UserFunction_header.hh"
#include "Value.hh"
#include "Workspace.hh"

//=========================================================================
UserFunction_header::UserFunction_header(const UCS_string & text0, bool macro)
  : error(E_DEFN_ERROR),   // assume bad headr
    error_info("Bad header"),
    sym_Z(0),
    sym_A(0),
    sym_LO(0),
    sym_FUN(0),
    sym_RO(0),
    sym_X(0),
    sym_B(0)
{
UCS_string text(text0);
   text.remove_comment();

UCS_string signature_text;
UCS_string lvar_text;

   // split header text into signature and local variables strings
   {
     bool in_signature = true;
     loop(t, text.size())
         {
           const Unicode uni = text[t];
           if (uni == UNI_CR)   continue;   // ignore CR
           if (uni == UNI_LF)   break;      // stop at LF
           if (uni == UNI_SEMICOLON)   in_signature = false;
           if (in_signature)   signature_text.append(uni);
           else                lvar_text.append(uni);
         }
   }

   if (signature_text.size() == 0)
      {
        error_info = "Empty header line";
        return;
      }

   Log(LOG_UserFunction__set_line)
      {
        CERR << "[0] " << signature_text << lvar_text << endl;
        // show_backtrace(__FILE__, __LINE__);
      }

   if ((error_info = init_signature(signature_text, macro)))   return;
   if ((error_info = init_local_vars(lvar_text, macro)))       return;

   error = E_NO_ERROR;
}
//-------------------------------------------------------------------------
UserFunction_header::UserFunction_header(Fun_signature sig, int lambda_num)
  : error(E_DEFN_ERROR),
    error_info("Bad header"),
    sym_Z(0),
    sym_A(0),
    sym_LO(0),
    sym_FUN(0),
    sym_RO(0),
    sym_X(0),
    sym_B(0)
{
   function_name.append(UNI_LAMBDA);
   function_name.append_number(lambda_num);

   if (!signature_is_valid(sig))
      {
         error_info = "Invalid signature";
         return;
      }

                       sym_Z  = &Workspace::get_v_LAMBDA();
   if (sig & SIG_A)    sym_A  = &Workspace::get_v_ALPHA();
   if (sig & SIG_LO)   sym_LO = &Workspace::get_v_ALPHA_U();
   if (sig & SIG_RO)   sym_RO = &Workspace::get_v_OMEGA_U();
   if (sig & SIG_B)    sym_B  = &Workspace::get_v_OMEGA();
   if (sig & SIG_X)    sym_X  = &Workspace::get_v_CHI();

   error_info = 0;
   error = E_NO_ERROR;
}
//-------------------------------------------------------------------------
bool
UserFunction_header::signature_is_valid(Fun_signature sig)
{
   // if sig is valid then (sig | SIG_Z) is also valid. We can therefore
   // reduce the number of cases.by pretending that SIG_Z is set.
   //
   switch(sig | SIG_Z)
      {
        // niladic
        //
        case SIG_Z_F0:

        // monadic
        //
        case SIG_Z_F1_B:
        case SIG_Z_F1_X_B:
        case SIG_Z_LO_OP1_B:
        case SIG_Z_LO_OP1_X_B:
        case SIG_Z_LO_OP2_RO_B:

        // dyadic
        //
        case SIG_Z_A_F2_B:
        case SIG_Z_A_F2_X_B:
        case SIG_Z_A_LO_OP1_B:
        case SIG_Z_A_LO_OP1_X_B:
        case SIG_Z_A_LO_OP2_RO_B:   return true;    // valid signature
        default:                    return false;   // invalid signature
      }
}
//--------------------------------------------------------------------------
const char *
UserFunction_header::init_signature(const UCS_string & text, bool macro)
{
Token_string tos;
   {
     const Tokenizer tokenizer(PM_FUNCTION, LOC, macro);
     if (const ErrorCode err = tokenizer.tokenize(text, tos))
        {
          error = err;
          return "Tokenize error (function signature)";
        }
   }

size_t start = 0;
size_t len   = tos.size();

   if (len >= 2 && tos[1].get_Class() == TC_ASSIGN)   // expect Z ← ...
      {
        if (tos[0].get_Class() != TC_SYMBOL)   return "Bad Z in Z ←";

        sym_Z = tos[0].get_sym_ptr();   // Z ← or λ ←
        start = 2;
        len -= 2;
      }

   if (len <= 3)   // F0, F1 B, or A F2 B
      {
        if (len == 0)   // error: empty signature
           return sym_Z ? "Empty header (after Z ←)" : "Empty header";

        if (len == 1)   // F0
           {
             if (tos[start].get_Class() != TC_SYMBOL)   return "Bad F0";

             sym_FUN = tos[start].get_sym_ptr();
             function_name = sym_FUN->get_name();
             return 0;   // OK
           }

        if (len == 2)   // F1 B
           {
             if (tos[start].get_Class() != TC_SYMBOL)
                return "Bad F1 in F1 B";

             if (tos[start + 1].get_Class() != TC_SYMBOL)
                return "Bad B in F1 B";

             sym_FUN = tos[start].get_sym_ptr();
             sym_B = tos[start + 1].get_sym_ptr();
             function_name = sym_FUN->get_name();
             return 0;   // OK
           }

          // otherwise: A F2 B
             if (tos[start].get_Class() != TC_SYMBOL)
                return "Bad A in A F2 B";

             if (tos[start + 1].get_Class() != TC_SYMBOL)
                return "Bad F2 in A F2 B";

             if (tos[start + 2].get_Class() != TC_SYMBOL)
                return "Bad B in A F2 B";
             sym_A   = tos[start]    .get_sym_ptr();
             sym_FUN = tos[start + 1].get_sym_ptr();
             sym_B   = tos[start + 2].get_sym_ptr();
             function_name = sym_FUN->get_name();
             return 0;   // OK
      }

   // at this point the signature has 4 or more token after the
   // optional Z← or λ←. Strip of the final B.
   //
   if (tos[start + len - 1].get_Class() != TC_SYMBOL)
      return "Bad B in ... B";
   sym_B   = tos[start + len - 1].get_sym_ptr();
   len--;

   // maybe strip off the optional axis [X]
   //
   if (tos[start + len - 1].get_Class() == TC_R_BRACK)
      {
        if (tos[start + len - 2].get_Class() != TC_SYMBOL)
           return "Bad X in ... [X] B";

        if (tos[start + len - 3].get_tag() != TOK_L_BRACK)
             return "Bad [ in ... [X] B";

        sym_X   = tos[start + len - 2].get_sym_ptr();
        len -= 3;
      }

   // at this point we should have one of:
   //
   // 1.          F1
   // 2.   A      F2
   // 3.   A ( LO OP1 )
   // 4.     ( LO OP1 )
   // 5.   A ( LO OP2 RO )
   // 6.     ( LO OP2 RO )
   //
   if (len == 1)   // case 1.
      {
        if (tos[start].get_tag() != TOK_SYMBOL)
           return "Bad F1 in F1 [X] B";

        sym_FUN = tos[start].get_sym_ptr();
        function_name = sym_FUN->get_name();
        return 0;   // OK
      }

   // cases 2-6
   //
   if (tos[start].get_Class() == TC_SYMBOL)   // case 2, 3, or 5: strip A
      {
        sym_A = tos[start].get_sym_ptr();
        ++start;
        --len;
      }

   // cases 2, 4, or 6
   //
   if (len == 1)   // case 2.
      {
        if (tos[start].get_Class() != TC_SYMBOL)
           return "Bad F2 in A F2 [X] B";

        sym_FUN = tos[start].get_sym_ptr();
        function_name = sym_FUN->get_name();
        return 0;   // OK
      }

   // cases 3-6
   //
   if (tos[start].get_tag() != TOK_L_PARENT)
      return "Bad ( in (F2 OP ... )";

   if (tos[start + len - 1].get_tag() != TOK_R_PARENT)
      return "Bad ) in (F2 OPn ... )";

   if (tos[start + 1].get_Class() != TC_SYMBOL)
      return "Bad F2 in (F2 OPn ... )";

   sym_LO = tos[start + 1].get_sym_ptr();

   if (tos[start + 2].get_Class() != TC_SYMBOL)   // LO
      return "Bad OPn in (F2 OPn ... )";

   sym_FUN = tos[start + 2].get_sym_ptr();
   function_name = sym_FUN->get_name();

   if (len == 4)   return 0;   // OK: ( LO OP1 )

   if (len != 5)   // ( LO OP2 RO )
      return "Bad length in (F2 OPn ... )";

   if (tos[start + 3].get_Class() != TC_SYMBOL)
      return "Bad G2 in (F2 OP2 G2)";

   sym_RO = tos[start + 3].get_sym_ptr();
   return 0;
}
//--------------------------------------------------------------------------
const char *
UserFunction_header::init_local_vars(const UCS_string & text, bool macro)
{
Token_string tos;
   {

     const Tokenizer tokenizer(PM_FUNCTION, LOC, macro);
     if (const ErrorCode err = tokenizer.tokenize(text, tos))
        {
          error = err;
          return "Tokenize error (local variables)";
        }
   }

   if (tos.size() & 1)
      {
        // since each local variable is preceeded by a semicolon, the
        // number of remaining tokens must be even.
        //
        return "local variable list (odd length)";
      }

   loop(tos_idx, tos.size())
      {
        if (tos_idx & 1)   // expect variable
           {
             const TokenTag tag = tos[tos_idx].get_tag();
             if (tag != TOK_SYMBOL && tag != TOK_Quad_CT
                                   && tag != TOK_Quad_FC
                                   && tag != TOK_Quad_IO
                                   && tag != TOK_Quad_PP
                                   && tag != TOK_Quad_PR
                                   && tag != TOK_Quad_PW
                                   && tag != TOK_Quad_RL)
                {
                  CERR << "Offending token at " LOC " is: "
                       << tos[tos_idx] << endl;
                  return "Bad token";
                }

             local_vars.push_back(tos[tos_idx].get_sym_ptr());
           }
        else if (tos[tos_idx].get_tag() != TOK_SEMICOL)
           {
             return "Semicolon expected";
           }
      }

   // NOTE: duplicated variables are not an APL error,
   // but we want to localize them only once.
   //
   remove_duplicate_local_variables();

   return 0;
}
//----------------------------------------------------------------------------
void
UserFunction_header::pop_local_vars() const
{
   loop(l, label_values.size())   label_values[l].sym->pop();

   loop(l, local_vars.size())   local_vars[l]->pop();

   if (sym_B)    sym_B ->pop();
   if (sym_X)    sym_X ->pop();
   if (sym_RO)   sym_RO->pop();
   if (sym_LO)   sym_LO->pop();
   if (sym_A)    sym_A ->pop();
   if (sym_Z)    sym_Z ->pop();
}
//----------------------------------------------------------------------------
void
UserFunction_header::print_local_vars(ostream & out) const
{
   if (sym_Z)     out << " " << *sym_Z;
   if (sym_A)     out << " " << *sym_A;
   if (sym_LO)    out << " " << *sym_LO;
   if (sym_RO)    out << " " << *sym_RO;
   if (sym_B)     out << " " << *sym_B;

   loop(l, local_vars.size())   out << " " << *local_vars[l];
}
//----------------------------------------------------------------------------
void
UserFunction_header::reverse_local_vars()
{
const ShapeItem half = local_vars.size() / 2;   // = rounded down!
   loop(v, half)
      {
        Symbol * tmp = local_vars[v];
        local_vars[v] = local_vars[local_vars.size() - v - 1];
        local_vars[local_vars.size() - v - 1] = tmp;
      }
}
//----------------------------------------------------------------------------
void
UserFunction_header::remove_duplicate_local_variables()
{
   // remove local vars that are also labels, arguments or return values.
   // This is to avoid pushing them twice
   //
   remove_duplicate_local_var(sym_Z,   0);
   remove_duplicate_local_var(sym_A,   0);

   // sym_LO, sym_FUN, and sym_RO are not pushed, so they are never duplicates

   remove_duplicate_local_var(sym_X,   0);
   remove_duplicate_local_var(sym_B,   0);

   loop(l, label_values.size())
      remove_duplicate_local_var(label_values[l].sym, 0);

   loop(l, local_vars.size())
      remove_duplicate_local_var(local_vars[l], l + 1);
}
//----------------------------------------------------------------------------
void
UserFunction_header::remove_duplicate_local_var(const Symbol * sym, size_t pos)
{
   // remove sym from the vector of local variables. Only the local vars
   // at pos or higher are being removed
   //
   if (sym == 0)   return;   // unused symbol

   while (pos < local_vars.size())
       {
         if (sym == local_vars[pos])
            {
              local_vars[pos] = local_vars.back();
              local_vars.pop_back();
              continue;
            }
         ++pos;
       }
}
//----------------------------------------------------------------------------
UCS_string
UserFunction_header::lambda_header(Fun_signature sig, int lambda_num)
{
UCS_string u;

   if (sig & SIG_Z)      u.append_UTF8("λ←");
   if (sig & SIG_A)      u.append_UTF8("⍺ ");
   if (sig & SIG_LORO)   u.append_UTF8("(");
   if (sig & SIG_LO)     u.append_UTF8("⍶ ");
   u.append_UTF8("λ"); 
   u.append_number(lambda_num); 
   if (sig & SIG_RO)     u.append_UTF8(" ⍹ ");
   if (sig & SIG_LORO)   u.append_UTF8(")");
   if (sig & SIG_X)      u.append_UTF8("[χ]");
   if (sig & SIG_B)      u.append_UTF8(" ⍵");

   return u;
}
//----------------------------------------------------------------------------
bool
UserFunction_header::localizes(const Symbol * sym) const
{
   if (sym == sym_Z)   return true;
   if (sym == sym_A)   return true;
   if (sym == sym_LO)  return true;
   if (sym == sym_RO)  return true;
   if (sym == sym_X)   return true;
   if (sym == sym_B)   return true;

   loop(l, local_vars.size())     if (sym == local_vars[l])         return true;
   loop(l, label_values.size())   if (sym == label_values[l].sym)   return true;

   return false;
}
//----------------------------------------------------------------------------
void
UserFunction_header::print_properties(ostream & out, int indent) const
{
UCS_string ind(indent, UNI_SPACE);
   if (is_operator())   out << "Operator " << function_name << endl;
   else                 out << "Function " << function_name << endl;

   if (sym_Z)    out << ind << "Result:         " << *sym_Z  << endl;
   if (sym_A)    out << ind << "Left Argument:  " << *sym_A  << endl;
   if (sym_LO)   out << ind << "Left Op Arg:    " << *sym_LO << endl;
   if (sym_RO)   out << ind << "Right Op Arg:   " << *sym_RO << endl;
   if (sym_B)    out << ind << "Right Argument: " << *sym_B  << endl;

   if (local_vars.size())
      {
        out << ind << "Local Variables:";
        loop(l, local_vars.size())   out << " " << *local_vars[l];
        out << endl;
      }

   if (label_values.size())
      {
        out << ind << "Labels:        ";
        loop(l, label_values.size())
           {
             if (l)   out << ",";
             out << " " << *label_values[l].sym
                 << "=" << label_values[l].line;
           }
        out << endl;
      }
}
//----------------------------------------------------------------------------
void
UserFunction_header::eval_common() const
{
   Log(LOG_UserFunction__enter_leave)   CERR << "eval_common()" << endl;

   // push local variables...
   //
   loop(l, local_vars.size())   local_vars[l]->push();

   // push labels...
   //
   loop(l, label_values.size())
       label_values[l].sym->push_label(label_values[l].line);
}
//----------------------------------------------------------------------------
