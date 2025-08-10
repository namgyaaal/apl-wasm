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

#include "Avec.hh"
#include "Bif_F12_FORMAT.hh"
#include "CDR.hh"
#include "CharCell.hh"
#include "ComplexCell.hh"
#include "FloatCell.hh"
#include "IndexIterator.hh"
#include "IntCell.hh"
#include "LineInput.hh"
#include "Macro.hh"
#include "Output.hh"
#include "PointerCell.hh"
#include "PrintOperator.hh"
#include "QuadFunction.hh"
#include "Quad_FX.hh"
#include "Quad_FFT.hh"
#include "Quad_JSON.hh"
#include "Quad_MAP.hh"
#include "Quad_PLOT.hh"
#include "Quad_RE.hh"
#include "Quad_RVAL.hh"
#include "Quad_TF.hh"
#include "Quad_XML.hh"
#include "StateIndicator.hh"
#include "Tokenizer.hh"
#include "UserFunction.hh"
#include "Value.hh"
#include "Workspace.hh"

#include "Workspace.icc"

extern char **environ;

// ⎕-function instances
//
Quad_AF    Quad_AF   ::fun;
Quad_AT    Quad_AT   ::fun;
Quad_DL    Quad_DL   ::fun;
Quad_EA    Quad_EA   ::fun;
Quad_EB    Quad_EB   ::fun;
Quad_ENV   Quad_ENV  ::fun;
Quad_EX    Quad_EX   ::fun;
Quad_INP   Quad_INP  ::fun;
Quad_NA    Quad_NA   ::fun;
Quad_NC    Quad_NC   ::fun;
Quad_NL    Quad_NL   ::fun;
Quad_SI    Quad_SI   ::fun;
Quad_UCS   Quad_UCS  ::fun;
Quad_STOP  Quad_STOP ::fun;   // S∆
Quad_TRACE Quad_TRACE::fun;   // T∆

//============================================================================
Token
Quad_AF::eval_B(Value_P B) const
{
const ShapeItem ec = B->element_count();
Value_P Z(B->get_shape(), LOC);

   loop(v, ec)
       {
         const Cell & cell_B = B->get_cravel(v);

         if (cell_B.is_character_cell())   // Unicode to AV index
            {
              const Unicode uni = cell_B.get_char_value();
              int32_t pos = Avec::find_av_pos(uni);
              if (pos < 0)   Z->next_ravel_Int(Avec::MAX_AV);
              else           Z->next_ravel_Int(pos);
              continue;
            }

         if (cell_B.is_integer_cell())
            {
              const APL_Integer idx = cell_B.get_near_int();
              Z->next_ravel_Char(Quad_AV::indexed_at(idx));
              continue;
            }

         DOMAIN_ERROR;
       }

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//============================================================================
Token
Quad_AT::eval_AB(Value_P A, Value_P B) const
{
   // A is an integer scalar 1, 2, 3, or 4 (the mode)
   // B is a matrix of symbol names

   if (A->get_rank() > 0)   RANK_ERROR;

const APL_Integer mode = A->get_cfirst().get_near_int();
   if (mode < 1)   DOMAIN_ERROR;
   if (mode > 4)   DOMAIN_ERROR;

const ShapeItem cols = B->get_cols();
const ShapeItem rows = B->get_rows();
   if (rows == 0)   LENGTH_ERROR;

               // mode:  1  2  3  4
const int mode_vec[] = { 3, 7, 4, 2 };
const int mode_len = mode_vec[mode - 1];
Shape shape_Z(rows);
   shape_Z.add_shape_item(mode_len);

Value_P Z(shape_Z, LOC);

   loop(r, rows)
      {
        // get the symbol name by stripping trailing spaces
        //
        const ShapeItem b = r*cols;   // start of the symbol name
        UCS_string symbol_name;
        loop(c, cols)
           {
            const Unicode uni = B->get_cravel(b + c).get_char_value();
            if (uni == UNI_SPACE)   break;
            symbol_name.append(uni);
           }

        const NamedObject * obj = Workspace::lookup_existing_name(symbol_name);
        if (obj == 0)   Error::throw_symbol_error(symbol_name, LOC);

        if (const Function * function = obj->get_function())
           {
             // defined or system function.
             function->get_attributes(mode, *Z);
             continue;
           }

        if (const Symbol * symbol = obj->get_symbol())
           {
             // defined or sys var
             symbol->get_attributes(mode, *Z);
             continue;
           }

        // neither function nor variable (e.g. unused name)
        VALUE_ERROR;

#if 0 // not reached ??
        if (Avec::is_quad(symbol_name[0]))   // system function or variable
           {
             int l;
             const Token t(Workspace::get_quad(symbol_name, l));
             if (t.get_Class() == TC_SYMBOL)   // system variable
                {
                  // Assert() if symbol_name is not a function
                  t.get_sym_ptr();
                }
             else
                {
                  // throws SYNTAX_ERROR if t is not a function
                  t.get_function();
                }
           }
        else                                   // user defined
           {
             Symbol * symbol = Workspace::lookup_existing_symbol(symbol_name);
             if (symbol == 0)   VALUE_ERROR;

             symbol->get_attributes(mode, &Z->get_cravel(r*mode_len));
           }
#endif
      }

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//============================================================================
Token
Quad_DL::eval_B(Value_P B) const
{
const APL_time_us start = now();

   // B should be an integer or real scalar
   //
   if (B->get_rank() > 0)                 RANK_ERROR;
   if (!B->get_cfirst().is_real_cell())   DOMAIN_ERROR;

const APL_time_us end = start + 1000000 * B->get_cfirst().get_real_value();
   if (end < start)                           DOMAIN_ERROR;
   if (end > start + 31*24*60*60*1000000LL)   DOMAIN_ERROR;   // > 1 month

bool need_CR = false;
   while (now() < end)
       {
         usleep(20000);
         if (attention_is_raised())   need_CR = true;
         if (interrupt_is_raised())
            {
              need_CR = true;
              break;
            }
       }

   // interrupt or attention may have displayed ^C, start a new line if so.
   if (need_CR)   CERR << endl;

   // we do not clear_attention_raised(LOC) or clear_interrupt_raised(LOC);
   // so that the user can continue with →''

   // return the time elapsed.
   //
   return Token(TOK_APL_VALUE1, FloatScalar(0.000001*(now() - start), LOC));
}
//============================================================================
Token
Quad_EA::eval_AB(Value_P A, Value_P B) const
{
   if (!A->is_char_string())
      {
        if (A->get_rank() > 1)   RANK_ERROR;
        else                     DOMAIN_ERROR;
      }

   if (!B->is_char_string())
      {
        if (B->get_rank() > 1)   RANK_ERROR;
        else                     DOMAIN_ERROR;
      }

   return Macro::get_macro(Macro::MAC_Z__A_Quad_EA_B)->eval_AB(A, B);
}
//============================================================================
Token
Quad_EB::eval_AB(Value_P A, Value_P B) const
{
   if (!A->is_char_string())
      {
        if (A->get_rank() > 1)   RANK_ERROR;
        else                     DOMAIN_ERROR;
      }

   if (!B->is_char_string())
      {
        if (B->get_rank() > 1)   RANK_ERROR;
        else                     DOMAIN_ERROR;
      }

   return Macro::get_macro(Macro::MAC_Z__A_Quad_EB_B)->eval_AB(A, B);
}
//============================================================================
Token
Quad_EC::eval_B(Value_P B) const
{
const UCS_string statement_B(*B.get());

ExecuteList * fun = 0;
   try {
         fun = ExecuteList::fix(statement_B, LOC);
       }
     catch (...) {}

   if (fun == 0)
      {
        // syntax error in B
        //
        Value_P Z2(2, LOC);
            Z2->next_ravel_Int(Error::error_major(E_SYNTAX_ERROR));
            Z2->next_ravel_Int(Error::error_minor(E_SYNTAX_ERROR));
            Z2->check_value(LOC);

        UTF8_string Z3_utf(Error::error_name(E_SYNTAX_ERROR));
        UCS_string Z3_ucs(Z3_utf);
        Value_P Z3(Z3_ucs, LOC);
        Value_P Z(3, LOC);
        Z->next_ravel_0();              // return code = error
        Z->next_ravel_Pointer(Z2.get());   // ⎕ET value
        Z->next_ravel_Pointer(Z3.get());   // ⎕EM

        Z->check_value(LOC);
        return Token(TOK_APL_VALUE1, Z);
      }

   Assert(fun);

   Log(LOG_UserFunction__execute)   fun->print(CERR);

   Workspace::push_SI(fun, LOC);
   Workspace::SI_top()->set_safe_execution_count();

   return Token(TOK_SI_PUSHED);
}
//----------------------------------------------------------------------------
Token
Quad_EC::eval_fill_B(Value_P B) const
{
Value_P Z2(2, LOC);                             // Z2←0 0 0
   Z2->next_ravel_0();
   Z2->next_ravel_0();
   Z2->check_value(LOC);

Value_P Zsub(3, LOC);                           // Zsub ← 3 (⊂Z2) (⊂⍬)...
   Zsub->next_ravel_Int(3);                     // Zsub[1] ← 3
   Zsub->next_ravel_Pointer(Z2.get());          // Zsub[2] ← ⊂ 0 0 0
   Zsub->next_ravel_Pointer(Idx0(LOC).get());   // Zsub[3] ← ⊂⍬
   Zsub->check_value(LOC);

Value_P Z(ShapeItem(0), LOC);                   // Z ← 0 ⍴ ⊂Zsub
  new (&Z->get_wproto())   PointerCell(Zsub.get(), *Z);
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
void
Quad_EC::eoc(Token & result)
{
   // set Token result to an APL value Z = (Z1 Z2 Z3) where:
   //
   // Z1 is an integer scalar 0-5 (return code):
   //
   //    0: Error
   //    1: Value
   //    2: Committed Value
   //    3: Void
   //    4: Branch
   //    5: Escape
   //
   // Z2 is a two-element integer vector with the value that ⎕ET would have,
   // ⎕ET is not set while in ⎕EC.
   //
   // Z3 is a value that depends on Z1:
   //
   // 0:      ⎕EM
   // 1 or 2: the value
   // 3 or 5: 0⍴0
   // 4:      the branch line

Value_P Z(3, LOC);
   if (result.get_tag() == TOK_ERROR)
      {
        StateIndicator * si = Workspace::SI_top();
        si->clear_safe_execution();

        const Error & err = StateIndicator::get_error(si);
        const ErrorCode ec = ErrorCode(result.get_int_val());

        PrintBuffer pb;
        pb.append_ucs(UTF8_string(Error::error_name(ec)));
        pb.append_ucs(err.get_error_line_2());
        pb.append_ucs(err.get_error_line_3());

        Value_P Z2(2, LOC);
            Z2->next_ravel_Int(Error::error_major(ec));
            Z2->next_ravel_Int(Error::error_minor(ec));
        Z2->check_value(LOC);

        Value_P Z3(pb, LOC);   // 3 line message like ⎕EM

        Z->next_ravel_0();
        Z->next_ravel_Pointer(Z2.get());
        Z->next_ravel_Pointer(Z3.get());

        Z->check_value(LOC);
        Token tok_Z(TOK_APL_VALUE1, Z);
        result.move(tok_Z, LOC);
        return;
      }

   // all other cases have Z2 = 0 0
   //
Value_P Z2(2, LOC);
   Z2->next_ravel_0();
   Z2->next_ravel_0();
   Z2->check_value(LOC);

   switch(result.get_tag())
      {
        case TOK_APL_VALUE1:
        case TOK_APL_VALUE3:
        case TOK_APL_VALUE4:
             Z->next_ravel_1();
             Z->next_ravel_Pointer(Z2.get());
             Z->next_ravel_Value(result.get_apl_val().get());
             break;

        case TOK_APL_VALUE2:
             Z->next_ravel_Int(2);
             Z->next_ravel_Pointer(Z2.get());
             Z->next_ravel_Value(result.get_apl_val().get());
             break;

        case TOK_NO_VALUE:
        case TOK_VOID:
             Z->next_ravel_Int(3);
             Z->next_ravel_Pointer(Z2.get());
             Z->next_ravel_Pointer(Idx0_0(LOC).get());
             break;

        case TOK_BRANCH:
             Z->next_ravel_Int(4);
             Z->next_ravel_Pointer(Z2.get());
             Z->next_ravel_Int(result.get_int_val());
             break;

        case TOK_ESCAPE:
             Z->next_ravel_Int(5);
             Z->next_ravel_Pointer(Z2.get());
             Z->next_ravel_Pointer(Idx0_0(LOC).get());
             break;

        default: CERR << "unexpected result tag " << result.get_tag()
                      << " in Quad_EC::eoc()" << endl;
                 Assert(0);
      }

   Z->check_value(LOC);
Token tok_Z(TOK_APL_VALUE1, Z);
   result.move(tok_Z, LOC);
}
//============================================================================
Token
Quad_ENV::eval_B(Value_P B) const
{
   if (!B->is_char_string())   DOMAIN_ERROR;

const ShapeItem ec_B = B->element_count();

std::vector<const char *> evars;

   for (char **e = environ; *e; ++e)
       {
         const char * env = *e;

         // check if env starts with B.
         //
         bool match = true;
         loop(b, ec_B)
            {
              if (B->get_cravel(b).get_char_value() != Unicode(env[b]))
                 {
                   match = false;
                   break;
                 }
            }

         if (match)   evars.push_back(env);
       }

const Shape sh_Z(evars.size(), 2);
Value_P Z(sh_Z, LOC);

   loop(e, evars.size())
      {
        const char * env = evars[e];
        UCS_string ucs;
        while (*env)
           {
             if (*env != '=')   ucs.append(Unicode(*env++));
             else               break;
           }
        ++env;   // skip '='

        Value_P varname(ucs, LOC);

        ucs.clear();
        while (*env)   ucs.append(Unicode(*env++));

        Value_P varval(ucs, LOC);

        Z->next_ravel_Pointer(varname.get());
        Z->next_ravel_Pointer(varval.get());
      }

   Z->set_proto_Spc();
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//============================================================================
Token
Quad_ES::eval_AB(Value_P A, Value_P B) const
{
const UCS_string ucs(*A.get());
Error error(E_NO_ERROR, LOC);
const Token ret = event_simulate(&ucs, B, error);
   if (error.get_error_code() == E_NO_ERROR)   return ret;

   throw error;
}
//----------------------------------------------------------------------------
Token
Quad_ES::eval_B(Value_P B) const
{
Error error(E_NO_ERROR, LOC);
const Token ret = event_simulate(0, B, error);
   if (error.get_error_code() == E_NO_ERROR)              return ret;
   if (Workspace::SI_top()->get_safe_execution_count())   return ret;

   throw error;
}
//----------------------------------------------------------------------------
Token
Quad_ES::event_simulate(const UCS_string * A, Value_P B, Error & error)
{
   // B is empty: no action
   //
   if (B->element_count() == 0)   return Token();

const ErrorCode ec = get_error_code(B);
   if (ec == E_QUAD_ES_BRA)   return Token(TOK_QUAD_ES_BRA, CLONE_P(B, LOC));
   if (ec == E_QUAD_ES_COM)   return Token(TOK_QUAD_ES_COM, CLONE_P(B, LOC));
   if (ec == E_QUAD_ES_ERR)   return Token(TOK_QUAD_ES_ERR, CLONE_P(B, LOC));
   if (ec == E_QUAD_ES_ESC)   return Token(TOK_QUAD_ES_ESC, CLONE_P(B, LOC));

   new (&error)   Error(ec, error.get_throw_loc());

   if (error.get_error_code() == E_NO_ERROR)   // B = 0 0: reset ⎕ET and ⎕EM.
      {
        Workspace::clear_error(LOC);
        return Token();
      }

   if (error.get_error_code() == E_ASSERTION_FAILED)   // B = 0 ASSERTION_FAILED
      {
        Assert(0 && "simulated ASSERTION_FAILED in ⎕ES");
      }

   // at this point we shall throw the error. Add some error details.
   //
   // set up error message 1
   //
   if (A)                                 // A ⎕ES B
      {
        UCS_string msg1_ucs(*A);
        UTF8_string msg1_utf(msg1_ucs);
        error.set_error_line_1(msg1_utf.c_str());
      }
   else if (error.get_error_code() ==
            E_USER_DEFINED_ERROR)   // ⎕ES with character B
      {
        UCS_string msg1_ucs(*B.get());
        UTF8_string msg1_utf(msg1_ucs);
        error.set_error_line_1(msg1_utf.c_str());
      }
   else if (error.is_known())             //  ⎕ES B with known major/minor B
      {
        /* error_message_1 already OK */ ;
      }
   else                                   //  ⎕ES B with unknown major/minor B
      {
        char cc[58];
        SPRINTF(cc, "Unkown error (major %d, minor %d) in ⎕ES B",
                    error.get_error_code() >> 16,
                    error.get_error_code() & 0xFFFF);
        error.set_error_line_1(cc);
      }

   error.set_show_locked(true);

   Assert(Workspace::SI_top());
   if (StateIndicator * si = Workspace::SI_top()->get_parent())
      {
        const UserFunction * ufun = si->get_executable()->get_exec_ufun();
        if (ufun)
           {
             // lrm p 282: When ⎕ES is executed from within a defined function
             // and B is not empty, the event action is generated as though
             // the function were primitive.
             //
             UCS_string ufun_name(UTF8_string("      "));
             ufun_name.append(ufun->get_name());
             error.set_error_line_2(ufun_name, 6, -1);
             Workspace::pop_SI(LOC);
             StateIndicator::get_error(Workspace::SI_top()) = error;
             error.print_em(UERR, LOC);
             return Token();
           }
      }

   error.update_error_info(Workspace::SI_top());
   return Token();
}
//----------------------------------------------------------------------------
ErrorCode
Quad_ES::get_error_code(Value_P B)
{
   // B shall be one of:                  → Error code
   //
   // 1. empty, or                        → No error
   // 2. a 2 element integer vector, or   → B[0]/N[1]
   // 3. a simple character vector.       → user defined
   //
   // Otherwise, throw an error

   // We extend the standard error codes by a markers E_QUAD_ES_* (10-13) which
   // are only used by the Macros Z__A_Quad_EA_B and Z__A_Quad_EB_B in order to
   // return branches and the like as a result.

   if (B->get_rank() > 1)   RANK_ERROR;

   if (B->element_count() == 0)   return E_NO_ERROR;
   if (B->is_char_string())       return E_USER_DEFINED_ERROR;

const APL_Integer err = (B->get_cfirst().get_near_int() << 16)
                      | (B->get_cravel(1).get_near_int());

   if ((err >> 16) == (E_QUAD_ES_BRA >> 16))   // one of the ⎕EA or ⎕EB events
      {
        const ShapeItem len_B = B->element_count();
        if (err == E_QUAD_ES_COM && len_B == 3)   return E_QUAD_ES_COM;
        if (err == E_QUAD_ES_ERR && len_B == 5)   return E_QUAD_ES_ERR;
        if (err == E_QUAD_ES_BRA && len_B == 3)   return E_QUAD_ES_BRA;
        if (err == E_QUAD_ES_ESC && len_B == 2)   return E_QUAD_ES_ESC;
        DOMAIN_ERROR;
      }

   if (B->element_count() != 2)   LENGTH_ERROR;

   return ErrorCode(err);
}
//============================================================================
Token
Quad_EX::eval_B(Value_P B) const
{
   if (B->get_rank() > 2)   RANK_ERROR;

   // we don't throw a DOMAIN ERROR if B is bad, but provide info for
   // the user if she asks for it with )MORE.
   loop(b, B->element_count())
       {
         const Cell & cell = B->get_cravel(b);
         if (!cell.is_character_cell())
            {
              MORE_ERROR() << "⎕EX B: non-character in list B "
                              "(of symbol names)";
              break;
            }

        const Unicode uni = cell.get_char_value();
        if (!( Avec::is_symbol_char(uni) ||
               Avec::is_white(uni)       ||
               (uni == UNI_FULLSTOP)))
           {
              MORE_ERROR() << "⎕EX B: invalid character "
                           << uni << " in list B (of symbol names)";
              break;
           }
       }

const ShapeItem var_count = B->get_rows();
const UCS_string_vector vars(*B, false);

Shape sh_Z;
   if (var_count > 1)   sh_Z.add_shape_item(var_count);
Value_P Z(sh_Z, LOC);

   loop(z, var_count)
      {
        const UCS_string & var = vars[z];
        const int erased = expunge(var);

        Z->next_ravel_Int(erased);
      }

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
int
Quad_EX::expunge(const UCS_string & name)
{
   if (name.size() == 0)   return 0;

   if (!name.contains(UNI_FULLSTOP))   // unless member access
      {
        Symbol * symbol = Workspace::lookup_existing_symbol(name);
        if (symbol == 0)   return 0;
        return symbol->expunge();
      }

   // expunge a member...
   //
int ret = 0;   // assume ⎕EX failure

   // build vector of member names in reverse order
   //
vector<const UCS_string *>members;
   {
     int dot = name.size();
     for (int from = dot - 1; from >= 0; --from)
         {
           if (name[from] != UNI_FULLSTOP)   continue;

           members.push_back(new UCS_string(name, from + 1, dot - from - 1));
           dot = from;
//         CERR << "MEMBER '" << *members.back() << "'" << endl;
         }
     members.push_back(new const UCS_string(name, 0, dot));
   }

//      CERR << "VAR '"     << *members.back() << "'" << endl;

Symbol * symbol = Workspace::lookup_existing_symbol(*members.back());
   if (symbol == 0)     goto cleanup;

   {
     Value_P toplevel_val = symbol->get_var_value();
     if (!toplevel_val)   goto cleanup;
     Value * owner = 0;   // not used
     if (Cell * cell = toplevel_val->get_member(members, owner, false, false))
        {
          cell->release(LOC);   IntCell::z0(cell--);   // member value
          cell->release(LOC);   IntCell::z0(cell);     // member name
          ret = 1;   // ⎕EX success
        }
   }

cleanup:
   loop(m, members.size())   delete members[m];
   return ret;
}
//============================================================================
UCS_string Quad_INP::esc1;
UCS_string Quad_INP::esc2;
UCS_string Quad_INP::end_marker;
UCS_string_vector Quad_INP::raw_lines;
UCS_string_vector Quad_INP::prefixes;
UCS_string_vector Quad_INP::escapes;
UCS_string_vector Quad_INP::suffixes;
bool Quad_INP::Quad_INP_running = false;

Token
Quad_INP::eval_AB(Value_P A, Value_P B) const
{
   if (Quad_INP_running)
      {
        MORE_ERROR() << "⎕INP called recursively";
        SYNTAX_ERROR;
      }

   // make sure that B is a non-empty string
   //
   if (B->get_rank() > 1)         RANK_ERROR;
   if (B->element_count() == 0)   LENGTH_ERROR;

   // temporary strings if get_esc() should fail
   //
UCS_string e1;
UCS_string e2;
   get_esc(A, e1, e2);

   Quad_INP_running = true;

   end_marker = B->get_UCS_ravel();
   esc1 = e1;
   esc2 = e2;

   prefixes.clear();
   escapes.clear();
   suffixes.clear();

   read_strings();    // read lines from file or stdin
   split_strings();   // split lines into prefixes, escapes, and suffixes

const ShapeItem line_count = raw_lines.size();
Value_P BB(line_count, LOC);
   loop(l, line_count)
       {
         if (escapes[l].size() == 0)   // prefix only
            {
              Value_P c1(prefixes[l], LOC);
              Value_P row(1, LOC);
              row->next_ravel_Pointer(c1.get());
              row->check_value(LOC);
              BB->next_ravel_Pointer(row.get());
            }
         else if (suffixes[l].size() == 0)   // prefix and escape
            {
              Value_P c1(prefixes[l], LOC);
              Value_P c2(escapes [l], LOC);
              Value_P row(2, LOC);
              row->next_ravel_Pointer(c1.get());
              row->next_ravel_Pointer(c2.get());
              row->check_value(LOC);
              BB->next_ravel_Pointer(row.get());
            }
         else                                // prefix, escape, and suffix
            {
              Value_P c1(prefixes[l], LOC);
              Value_P c2(escapes [l], LOC);
              Value_P c3(suffixes[l], LOC);
              Value_P row(3, LOC);
              row->next_ravel_Pointer(c1.get());
              row->next_ravel_Pointer(c2.get());
              row->next_ravel_Pointer(c3.get());
              row->check_value(LOC);
              BB->next_ravel_Pointer(row.get());
            }
       }
   BB->check_value(LOC);

Token ret = Macro::get_macro(Macro::MAC_Z__Quad_INP_B)->eval_B(BB);
   Assert1(ret.get_tag() == TOK_SI_PUSHED);

// loop(l, line_count)   BB->release(l, LOC);

   Quad_INP_running = false;
   return Token(TOK_SI_PUSHED);
}
//----------------------------------------------------------------------------
Token
Quad_INP::eval_B(Value_P B) const
{
   if (Quad_INP_running)
      {
        MORE_ERROR() << "⎕INP called recursively";
        SYNTAX_ERROR;
      }

   // make sure that B is a non-empty string
   //
   if (B->get_rank() > 1)         RANK_ERROR;
   if (B->element_count() == 0)   LENGTH_ERROR;

   Quad_INP_running = true;

   end_marker = B->get_UCS_ravel();
   read_strings();    // read lines from file or stdin

Value_P Z(raw_lines.size(), LOC);
   loop(l, raw_lines.size())
       {
         Value_P ZZ(raw_lines[l], LOC);
         Z->next_ravel_Pointer(ZZ.get());
       }

   Quad_INP_running = false;
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Quad_INP::eval_XB(Value_P X, Value_P B) const
{
   if (X->element_count() != 1)
      {
        if (X->get_rank() > 1)   RANK_ERROR;
        else                     LENGTH_ERROR;
      }

APL_Integer x = X->get_cfirst().get_near_int();
   if (x == 0)   return eval_B(B);
   if (x > 1)    DOMAIN_ERROR;

   // B is the end of document marker for a 'HERE document', similar
   // to ENDCAT in cat << ENDCAT.
   //
   // make sure that B is a non-empty string and extract it.
   //
   if (B->get_rank() > 1)         RANK_ERROR;
   if (B->element_count() == 0)   LENGTH_ERROR;

UCS_string end_marker(B->get_UCS_ravel());

UCS_string_vector lines;
Parser parser(PM_EXECUTE, LOC, false);

   for (;;)
      {
         bool eof = false;
         UCS_string line;
         UCS_string prompt;
         InputMux::get_line(LIM_Quad_INP, prompt, line, eof,
                            LineHistory::quad_INP_history);
         if (eof)   break;
         const int end_pos = line.substr_pos(end_marker);
         if (end_pos != -1)  break;

         Token_string tos;
         ErrorCode ec = parser.parse(line, tos, true);
         if (ec != E_NO_ERROR)
            {
              throw_apl_error(ec, LOC);
            }

         if ((tos.size() & 1) == 0)   LENGTH_ERROR;

         // expect APL values at even positions and , at odd positions
         //
         loop(t, tos.size())
            {
              Token & tok = tos[t];
              if (t & 1)   // , or ⍪
                 {
                   if (tok.get_tag() != TOK_F12_COMMA &&
                       tok.get_tag() != TOK_F12_COMMA1)   DOMAIN_ERROR;
                 }
              else         // value
                 {
                   if (tok.get_Class() != TC_VALUE)   DOMAIN_ERROR;
                 }

            }
         lines.push_back(line);
      }

Value_P Z(lines.size(), LOC);
   loop(z, lines.size())
      {
         Token_string tos;
         parser.parse(lines[z], tos, true);
         const ShapeItem val_count = (tos.size() + 1)/2;
         Value_P ZZ(val_count, LOC);
         loop(v, val_count)
            {
              Value_P val = tos[2*v].get_apl_val();
              ZZ->next_ravel_Value(val.get());
            }

         ZZ->check_value(LOC);
         Z->next_ravel_Pointer(ZZ.get());
      }

   if (lines.size() == 0)   // empty result
      {
        Value_P ZZ(UCS_string(), LOC);
        Z->set_ravel_Pointer(0, ZZ.get());
      }

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
void
Quad_INP::get_esc(Value_P A, UCS_string & esc1, UCS_string & esc2)
{

   // A is either one string (esc1 == esc2) or two (nested) strings
   // for esc1 and esc2 respectively.
   //
   if (A->compute_depth() == 2)   // two (nested) strings
      {
        if (A->get_rank() != 1)        RANK_ERROR;
        if (A->element_count() != 2)   LENGTH_ERROR;

        loop(e, 2)
           {
             const Cell & cell = A->get_cravel(e);
             if (cell.is_pointer_cell())   // char vector
                {
                  if (e)   esc2 = cell.get_pointer_value()->get_UCS_ravel();
                  else     esc1 = cell.get_pointer_value()->get_UCS_ravel();
                }
             else                          // char scalar
                {
                  if (e)   esc2 = cell.get_pointer_value()->get_UCS_ravel();
                  else     esc1 = cell.get_pointer_value()->get_UCS_ravel();
                }
           }

        if (esc1.size() == 0)   LENGTH_ERROR;
      }
   else                       // one string 
      {
        esc1 = A->get_UCS_ravel();
        if (esc1.size() == 0)   LENGTH_ERROR;
        esc2 = esc1;
      }
}
//----------------------------------------------------------------------------
void
Quad_INP::read_strings()
{
   // read lines until an end-maker is detected
   //
   raw_lines.clear();
   for (;;)
      {
        bool eof = false;
        UCS_string prompt;
        UCS_string line;
        InputMux::get_line(LIM_Quad_INP, prompt, line, eof,
                                 LineHistory::quad_INP_history);

        const int end = line.substr_pos(end_marker);
        if (end != -1)   // end marker found
           {
             line.resize(end);
             if (line.size())   raw_lines.push_back(line);
             break;
           }

        if (eof && !line.size())   break;
        raw_lines.push_back(line);
        if (eof)   break;
      }
}
//----------------------------------------------------------------------------
void
Quad_INP::split_strings()
{
UCS_string empty;
   loop(r, raw_lines.size())
      {
        UCS_string & line = raw_lines[r];
        const ShapeItem epos = line.substr_pos(esc1);
        if (esc1.size() == 0 || epos == -1)   // no escape in this line
           {
             prefixes.push_back(line);
             escapes.push_back(empty);
             suffixes.push_back(empty);
             continue;
           }

        // at this point line did contain an exec string
        //
        UCS_string pref = line;
        pref.resize(epos);
        prefixes.push_back(pref);

        line = line.drop(epos + esc1.size());   // skip prefix and esc1
        if (esc2.size())   // end defined
           {
             const ShapeItem eend = line.substr_pos(esc2);
             if (eend == -1)   // no exec end in this line
                {
                  escapes.push_back(line);
                  suffixes.push_back(empty);
                  continue;
                }
             else              // found an exec end in this line
                {
                  UCS_string exec = line;
                  exec.resize(eend);
                  escapes.push_back(exec);
                  line = line.drop(eend + esc2.size());   // skip exec and esc2
                  suffixes.push_back(line);
                  continue;
                }
           }
        else               // no end defined
           {
             escapes.push_back(line);
             suffixes.push_back(empty);
           }
      }

   Assert(prefixes.size() >= raw_lines.size());
   Assert(prefixes.size() == escapes.size());
   Assert(prefixes.size() == suffixes.size());
}
//============================================================================
Token
Quad_NC::eval_B(Value_P B) const
{
   if (B->get_rank() > 2)   RANK_ERROR;

const ShapeItem var_count = B->get_rows();
const UCS_string_vector vars(*B, false);

Shape sh_Z;
   if (var_count > 1)   sh_Z.add_shape_item(var_count);
Value_P Z(sh_Z, LOC);

   loop(v, var_count)
       {
         const int nc = get_NC(vars[v]);
         if (nc == NC_INVALID)   Z->next_ravel_Int(-1);
         else                    Z->next_ravel_Int(nc & NC_case_mask);
       }

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
APL_Integer
Quad_NC::get_NC(const UCS_string ucs)
{
   if (ucs.size() == 0)   return NC_INVALID;   // invalid name

const Unicode uni = ucs[0];

   // system name ?
   {
     const Symbol * sys = 0;

     // ⍺, ⍶, ⍵, and ⍹ are local variables of lambdas and may change their
     // NC at runtime. We will therefore consult their symbol later on.
     //
     if      (uni == UNI_ALPHA)           sys = &Workspace::get_v_ALPHA();
     else if (uni == UNI_LAMBDA)          sys = &Workspace::get_v_LAMBDA();
     else if (uni == UNI_CHI)             sys = &Workspace::get_v_CHI();
     else if (uni == UNI_OMEGA)           sys = &Workspace::get_v_OMEGA();
     else if (uni == UNI_ALPHA_UNDERBAR)  sys = &Workspace::get_v_OMEGA_U();
     else if (uni == UNI_OMEGA_UNDERBAR)  sys = &Workspace::get_v_OMEGA_U();

     // the caller may ask for e.g. ⍺123 but we accept ⍺ and friends only if
     // their lengths is 1.
     //
     if (ucs.size() != 1)   sys = 0;    // more than one char

     if (Avec::is_quad(uni))   // distinguished name
        {
          int len = 0;
          const Token t = Workspace::get_quad(ucs, len);
          if (len < 2)                      return NC_SYSTEM_VAR;   // ⎕ or ⍞
          if (t.get_Class() == TC_VOID)     return NC_INVALID;
          if (t.get_Class() == TC_SYMBOL)   sys = t.get_sym_ptr();
          else                              return  NC_SYSTEM_FUN;
        }
    
     if (sys)   // system variable (⎕xx, ⍺, ⍶, ⍵, ⍹, λ, or χ
        {
          const NameClass nc = sys->get_NC();
          if (nc == NC_UNUSED_USER_NAME)   return NC_SYSTEM_FUN;
          return nc + 3;
        }
     }

   // user-defined name
   //
const Symbol * user_sym = Workspace::lookup_existing_symbol(ucs);

   if (!user_sym)
      {
        // user_sym not found. Distinguish between invalid and unused names
        //
        if (!Avec::is_first_symbol_char(ucs[0]))   return NC_INVALID;
        loop (u, ucs.size())
           {
             if (!Avec::is_symbol_char(ucs[u]))   return NC_INVALID;
           }
        return 0;   // unused
      }

   switch(const NameClass nc = user_sym->get_NC())
      {
        case NC_INVALID:      return NC_INVALID;
        case NC_SYSTEM_VAR:   return NC_VARIABLE;
        default:              return nc & NC_case_mask;
      }
}
//============================================================================
Token
Quad_NL::do_quad_NL(Value_P A, Value_P B)
{
   if (+A && !A->is_char_string())   DOMAIN_ERROR;
   if (B->get_rank() > 1)            RANK_ERROR;
   if (B->element_count() == 0)   // nothing requested
      {
        return Token(TOK_APL_VALUE1, Str0_0(LOC));
      }

UCS_string first_chars;
   if (+A)   first_chars = UCS_string(*A);

   // 1. create a bitmap of ⎕NC values requested in B
   //
int requested_NCs = 0;
   {
     loop(b, B->element_count())
        {
          const APL_Integer bb = B->get_cravel(b).get_near_int();
          if (bb < 1)   DOMAIN_ERROR;
          if (bb > 6)   DOMAIN_ERROR;
          requested_NCs |= 1 << bb;
        }
   }

   // 2, build a name table, starting with user defined names
   //
UCS_string_vector names;
   {
     std::vector<const Symbol *> symbols = Workspace::get_all_symbols();

     loop(s, symbols.size())
        {
          const Symbol * symbol = symbols[s];
          if (symbol->is_erased())   continue;

          NameClass nc = symbol->get_NC();
          if (nc == NC_SYSTEM_VAR)   nc = NC_VARIABLE;

          if (!(requested_NCs & 1 << nc))   continue;   // name class not in B

          if (first_chars.size())
             {
               const Unicode first_char = symbol->get_name()[0];
               if (!first_chars.contains(first_char))   continue;
             }

          names.push_back(symbol->get_name());
        }
   }

   // 3, append ⎕-vars and ⎕-functions to name table (unless prevented by A)
   //
const bool vars = requested_NCs & 1 << 5;
const bool funs = requested_NCs & 1 << 6;

   if (first_chars.size() == 0 ||                  // all
       first_chars.contains(UNI_Quad_Quad))   // ⎕-variables and -functions
      {
#define ro_sv_def(x, _str, _txt)                            \
   { const Symbol * symbol = &Workspace::get_v_ ## x();     \
     if (vars && symbol->get_NC() != 0)                     \
        names.push_back(symbol->get_name()); }

#define rw_sv_def(x, _str, _txt)                            \
   { const Symbol * symbol = &Workspace::get_v_ ## x();     \
     if (vars && symbol->get_NC() != 0)                     \
        names.push_back(symbol->get_name()); }

#define sf_def(_x, str, _txt)                               \
   { if (funs)   names.push_back(UCS_ASCII_string("⎕" str)); }
#include "SystemVariable.def"
      }


   // 4. compute length of longest name
   //
ShapeItem longest = 0;
   loop(n, names.size())
      {
        if (longest < names[n].size())
           longest = names[n].size();
      }

const Shape shZ(names.size(), longest);
Value_P Z(shZ, LOC);

   // 5. construct result. The number of symbols is small and ⎕NL is
   // (or should) not be a perfomance // critical function, so we can
   // use a simple (find smallest and print) // approach.
   //
   for (int count = names.size(); count; --count)
      {
        // find smallest.
        //
        uint32_t smallest = count - 1;
        loop(t, count - 1)
           {
             if (names[smallest].compare(names[t]) > 0)   smallest = t;
           }
        // copy name to result, padded with spaces.
        //
        loop(l, longest)
           {
             const UCS_string & ucs = names[smallest];
             Z->next_ravel_Char(l < ucs.size() ? ucs[l] : UNI_SPACE);
           }

        // remove smalles from table
        //
        names[smallest] = names[count - 1];
      }

   Z->set_proto_Spc();
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//============================================================================
Token
Quad_SI::eval_AB(Value_P A, Value_P B) const
{
   if (A->element_count() != 1)   // not scalar-like
      {
        if (A->get_rank() > 1)   RANK_ERROR;
        else                     LENGTH_ERROR;
      }
APL_Integer a = A->get_cfirst().get_near_int();
const ShapeItem len = Workspace::SI_entry_count();
   if (a >= len)   DOMAIN_ERROR;
   if (a < -len)   DOMAIN_ERROR;
   if (a < 0)   a += len;   // negative a counts backwards from end

const StateIndicator * si = 0;
   for (si = Workspace::SI_top(); si; si = si->get_parent())
       {
         if (si->get_level() == a)   break;   // found
       }

   Assert(si);

   if (B->element_count() != 1)   // not scalar-like
      {
        if (B->get_rank() > 1)   RANK_ERROR;
        else                     LENGTH_ERROR;
      }

const Function_PC PC = Function_PC(si->get_PC() - 1);
const Executable * exec = si->get_executable();
const ParseMode pm = exec->get_parse_mode();
const UCS_string & fun_name = exec->get_name();
const Function_Line fun_line = exec->get_line(PC);

Value_P Z;

const APL_Integer b = B->get_cfirst().get_near_int();
   switch(b)
      {
        case 1:  Z = Value_P(fun_name, LOC);
                 break;

        case 2:  Z = Value_P(LOC);
                Z->next_ravel_Int(fun_line);
                break;

        case 3:  {
                   UCS_string fun_and_line(fun_name);
                   fun_and_line.append(UNI_L_BRACK);
                   fun_and_line.append_number(fun_line);
                   fun_and_line.append(UNI_R_BRACK);
                   Z = Value_P(fun_and_line, LOC); 
                 }
                 break;

        case 4: if (StateIndicator::get_error(si).get_error_code())
                   {
                     const UCS_string text(UTF8_string(
                           StateIndicator::get_error(si).get_error_line_2()));
                     Z = Value_P(text, LOC);
                   }
                else
                    {
                      const UCS_string text = exec->statement_text(PC);
                      Z = Value_P(text, LOC);
                    }
                break;

        case 5: Z = Value_P(LOC);
                Z->next_ravel_Int(PC);
                break;

        case 6: Z = Value_P(LOC);
                Z->next_ravel_Int(pm);
                break;

        default: DOMAIN_ERROR;
      }

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Quad_SI::eval_B(Value_P B) const
{
   if (B->element_count() != 1)   // not scalar-like
      {
        if (B->get_rank() > 1)   RANK_ERROR;
        else                     LENGTH_ERROR;
      }

const APL_Integer b = B->get_cfirst().get_near_int();
const ShapeItem len = Workspace::SI_entry_count();

   if (b < 1)   DOMAIN_ERROR;
   if (b > 4)   DOMAIN_ERROR;

   // at this point we should not fail...
   //
Value_P Z(len, LOC);

   // move from oldest SI entry towards SI_top()...
   //
   for (const StateIndicator * parent = 0; parent != Workspace::SI_top();)
       {
         const StateIndicator * si = parent->find_child();
         parent = si;   // for the next iteration

         const Function_PC PC = Function_PC(si->get_PC() - 1);
         const Executable * exec = si->get_executable();
         const ParseMode pm = exec->get_parse_mode();
         const UCS_string & fun_name = exec->get_name();
         const Function_Line fun_line = exec->get_line(PC);

         switch (b)
           {
             case 1:  {
                        Value_P name(fun_name, LOC);
                        Z->next_ravel_Pointer(name.get());
                      }
                      break;

             case 2:  Z->next_ravel_Int(fun_line);
                      break;

             case 3:  {
                        UCS_string fun_and_line(fun_name);
                        fun_and_line.append(UNI_L_BRACK);
                        fun_and_line.append_number(fun_line);
                        fun_and_line.append(UNI_R_BRACK);
                        Value_P name_and_line(fun_and_line, LOC);
                        Z->next_ravel_Pointer(name_and_line.get());
                      }
                      break;

             case 4:  if (StateIndicator::get_error(si).get_error_code())
                         {
                           const Error & e = StateIndicator::get_error(si);
                           const UCS_string ucs = e.get_error_line_2();
                           Value_P ZZ(ucs, LOC);
                           Z->next_ravel_Pointer(ZZ.get());
                         }
                      else   // no error in context si
                         {
                           const UCS_string text = exec->statement_text(PC);
                           Value_P text_val(text, LOC);
                           Z->next_ravel_Pointer(text_val.get());
                         }
                      break;

             case 5:  Z->next_ravel_Int(PC);   break;
             case 6:  Z->next_ravel_Int(pm);   break;
           }
       }

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//============================================================================
Token
Quad_UCS::eval_B(Value_P B) const
{
Value_P Z(B->get_shape(), LOC);
const ShapeItem ec = B->element_count();

   if (ec == 0)   // prototype
      {
        if (B->get_cfirst().is_character_cell())   // char to integer Unicode
           Z->set_proto_Int();
        else
           Z->set_proto_Spc();
      }

   loop(v, ec)
       {
         const Cell & cell_B = B->get_cravel(v);

         if (cell_B.is_character_cell())   // char to Unicode
            {
              const Unicode uni = cell_B.get_char_value();
              Z->next_ravel_Int(uni);
              continue;
            }

         if (cell_B.is_integer_cell())
            {
              const APL_Integer bint = cell_B.get_near_int();
              if (bint < -0x80)        DOMAIN_ERROR;
              if (bint > 0x7FFFFFFF)   DOMAIN_ERROR;

              Z->next_ravel_Char(Unicode(bint));
              continue;
            }

         if (cell_B.is_float_cell())
            {
              const APL_Integer bint = cell_B.get_near_int();
              if (bint < -0x80)        DOMAIN_ERROR;
              if (bint > 0x7FFFDFFF)   DOMAIN_ERROR;

              Z->next_ravel_Char(Unicode(bint));
              continue;
            }

         if (cell_B.is_complex_cell())
            {
              if (!Cell::is_near_zero(cell_B.get_imag_value()))   DOMAIN_ERROR;
              const APL_Integer bint = cell_B.get_near_int();
              if (bint < -0x80)        DOMAIN_ERROR;
              if (bint > 0x7FFFFFFF)   DOMAIN_ERROR;

              Z->next_ravel_Char(Unicode(bint));
              continue;
            }

         MORE_ERROR() << "⎕UCS got unexpected Cell type "
                      << cell_B.get_classname()
                      << ". Expecting int or character";
         DOMAIN_ERROR;
       }

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//============================================================================
const UserFunction *
Stop_Trace::locate_fun(const Value & fun_name)
{
   if (!fun_name.is_char_string())   return 0;

UCS_string fun_name_ucs(fun_name);
   if (fun_name_ucs.size() == 0)   return 0;

Symbol * fun_symbol = Workspace::lookup_existing_symbol(fun_name_ucs);
   if (fun_symbol == 0)
      {
        CERR << "symbol " << fun_name_ucs << " not found" << endl;
        return 0;
      }

cFunction_P fun = fun_symbol->get_function();
   if (fun == 0)
      {
        CERR << "symbol " << fun_name_ucs << " is not a function" << endl;
        return 0;
      }

const UserFunction * ufun = fun->get_func_ufun();
   if (ufun == 0)
      {
        CERR << "symbol " << fun_name_ucs
             << " is not a defined function" << endl;
        return 0;
      }

   return ufun;
}
//----------------------------------------------------------------------------
Token
Stop_Trace::reference(const std::vector<Function_Line> & lines,
                      bool assigned)
{
Value_P Z(lines.size(), LOC);

   loop(z, lines.size())   Z->next_ravel_Int(lines[z]);

   if (assigned)   return Token(TOK_APL_VALUE2, Z);
   else            return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
void
Stop_Trace::assign(UserFunction * ufun, const Value & new_value, bool stop)
{
std::vector<Function_Line> lines;
   lines.reserve(new_value.element_count());

   loop(l, new_value.element_count())
      {
        APL_Integer line = new_value.get_cravel(l).get_near_int();
        if (line < 1)   continue;
        lines.push_back(Function_Line(line));
      }

   ufun->set_trace_stop(lines, stop);
}
//============================================================================
Token
Quad_STOP::eval_AB(Value_P A, Value_P B) const
{
   // Note: Quad_STOP::eval_AB can be called directly or via S∆. If
   //
   // 1. called via S∆   then A is the function and B are the lines.
   // 2. called directly then B is the function and A are the lines.
   //
   if (const UserFunction * ufun = locate_fun(*A))   // case 1.
      {
        assign(const_cast<UserFunction *>(ufun), *B, true);
        return reference(ufun->get_stop_lines(), true);
      }

   // case 2.
   //
   if (const UserFunction * ufun = locate_fun(*B))   // case 2.
      {
        assign(const_cast<UserFunction *>(ufun), *A, true);
        return reference(ufun->get_stop_lines(), true);
     }

   DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
Token
Quad_STOP::eval_B(Value_P B) const
{
   if (const UserFunction * ufun = locate_fun(*B))
      return reference(ufun->get_stop_lines(), false);

   DOMAIN_ERROR;
}
//============================================================================
Token
Quad_TRACE::eval_AB(Value_P A, Value_P B) const
{
   // Note: Quad_TRACE::eval_AB can be called directly or via S∆. If
   //
   // 1. called via S∆   then A is the function and B are the lines.
   // 2. called directly then B is the function and A are the lines.
   //
   if (const UserFunction * ufun = locate_fun(*A))   // case 1.
      {
        assign(const_cast<UserFunction *>(ufun), *B, false);
        return reference(ufun->get_trace_lines(), true);
      }

   if (const UserFunction * ufun = locate_fun(*B))   // case 2.
      {
        assign(const_cast<UserFunction *>(ufun), *A, false);
        return reference(ufun->get_trace_lines(), true);
      }

   DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
Token
Quad_TRACE::eval_B(Value_P B) const
{
   if (const UserFunction * ufun = locate_fun(*B))
      return reference(ufun->get_trace_lines(), false);

   DOMAIN_ERROR;
}
//============================================================================

