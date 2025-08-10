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


#include "Executable.hh"
#include "Parser.hh"
#include "Output.hh"
#include "PrintOperator.hh"
#include "UCS_string.hh"
#include "UserFunction.hh"
#include "Value.hh"
#include "Workspace.hh"

#include "Workspace.icc"

//----------------------------------------------------------------------------
Executable::Executable(const UCS_string & ucs,  bool multi_line,
                       ParseMode pm, const char * loc)
   : alloc_loc(loc),
     pmode(pm),
     refcount(0)
{
// { cerr << "Executable " << (void *)this << " created at " << loc << endl; }

   // initialize text, stripping carriage returns and line feeds.
   //
   if (multi_line)
      {
        UCS_string line;

        loop(t, ucs.size())
           {
             const Unicode uni = ucs[t];
             switch (uni)
                {
                  case UNI_CR:                         break;
                  case UNI_LF: text.push_back(line);
                                     line.clear();           break;
                  default:           line.append(uni);
               }
           }

        if (line.size())   text.push_back(line);
      }
   else
      {
        text.push_back(ucs);
      }
}
//----------------------------------------------------------------------------
Executable::Executable(Fun_signature sig, int lambda_num,
                       const UCS_string & lambda_text, const char * loc)
   : alloc_loc(loc),
     pmode(PM_FUNCTION),
     refcount(0)
{
UCS_string header = UserFunction_header::lambda_header(sig, lambda_num);

   // remove local vars from lambda_text and add them to the header
   //
ShapeItem last_semi = -1;
   rev_loop(t,lambda_text.size())
       {
         const Unicode cc = lambda_text[t];
         if (cc == UNI_SPACE)      continue;
         if (Avec::is_symbol_char(cc))   continue;
         if (Avec::is_quad(cc))          continue;
         if (cc == UNI_SEMICOLON)   last_semi = t;
         else                             break;
       }

   if (last_semi != -1)
      {
        for (ShapeItem t = last_semi; t < lambda_text.size(); ++t)
            header.append(lambda_text[t]);
        text.push_back(header);

        UCS_string new_text(lambda_text);
        new_text.resize(last_semi);
        new_text.remove_trailing_whitespaces();
        text.push_back(new_text);
      }
   else
      {
        text.push_back(header);
        text.push_back(lambda_text);
     }
}
//----------------------------------------------------------------------------
Executable::~Executable()
{
   Log(LOG_UserFunction__fix)
      {
        CERR << "deleting Executable " << voidP(this)
             << " (body size=" << body.size() << ")" << endl;
      }

   clear_body();
}
//----------------------------------------------------------------------------
void
Executable::clear_body()
{
   loop(b, body.size())
      {
        body[b].release_apl_val(LOC);

        if (body[b].is_function())
           {
             cFunction_P fun = body[b].get_function();
             const UserFunction * ufun = fun->get_func_ufun();
             if (ufun && ufun->is_lambda())
                const_cast<UserFunction *>(ufun)->decrement_refcount(LOC);
             new (&body[b]) Token();
           }
      }

   body.clear();
}
//----------------------------------------------------------------------------
ErrorCode
Executable::parse_body_line(Function_Line line, const UCS_string & ucs_line,
                            bool trace, bool tolerant, const char * loc,
                            bool macro)
{
   Log(LOG_UserFunction__set_line)
      CERR << "[" << line << "]" << ucs_line << endl;

Token_string in;
const Parser parser(get_parse_mode(), loc, macro);
   if (const ErrorCode ec = parser.parse(ucs_line, in, true))
      {
        if (tolerant)   return ec;    // the caller will check ec

        if (ec == E_NO_TOKEN)
           {
             Error error(ec, LOC);
             throw error;
           }

        if (ec != E_NO_ERROR)   Error::throw_parse_error(ec, LOC, LOC);
      }

   return parse_body_line(line, in, trace, tolerant, loc);
} 
//----------------------------------------------------------------------------
ErrorCode
Executable::parse_body_line(Function_Line line, const Token_string & input,
                            bool trace, bool tolerant, const char * loc)
{
ShapeItem idx = 0;
const ShapeItem end = input.size();

   // handle label (if any)
   //
   if (get_parse_mode() == PM_FUNCTION &&   // defined function, and
       end > 1                         &&   // at least 2 token, and
       input[1].get_tag() == TOK_COLON &&   // second token is :, and
       input[0].get_tag() == TOK_SYMBOL)    // first token is (label-) name
      {
        Token tok_sym = input[0];   // get the label symbol
        idx = 2;                    // skip label :

        UserFunction * ufun = get_exec_ufun();
        Assert(ufun);
        ufun->add_label(tok_sym.get_sym_ptr(), line);
      }

   // each →→ ←→ and →→ bind to the statement left of it,
   // which must therefore not be empty.
   //
   if (idx < end && input[idx].is_COND())
      {
        CERR << "NOTE: Invalid ";
        if      (input[idx].get_tag() == TOK_IF_THEN)   CERR << "→→";
        else if (input[idx].get_tag() == TOK_IF_ELSE)   CERR << "←→";
        else if (input[idx].get_tag() == TOK_IF_END)    CERR << "←←";

        CERR << " at start of line " << line << endl;
        if (tolerant)   return E_SYNTAX_ERROR;
        else            SYNTAX_ERROR;
      }

   /*
       Convert (APL order) :

       ┌───┐ ┌───┐ ┌───┐     ┌───┐ ┌─────┐ 
       │ A │ │ B │ │ C │ ... │ Z │ │ END │ 
       └───┘ └───┘ └───┘     └───┘ └─────┘ 

       To (reverse order) :

       ┌───┐     ┌───┐ ┌───┐ ┌───┐ ┌─────┐ 
       │ Z │ ... │ C │ │ B │ │ A │ │ END │ 
       └───┘     └───┘ └───┘ └───┘ └─────┘ 

       The END token may be missing.
    */

Token_string output;   // in reverse order
   while (idx < end)   // loop over one line
      {
        // 1. determine the number of token (excluding ◊) in the statement
        //    that starts at idx
        //
        ShapeItem stat_len = 0;   // statement length, not counting ◊
        int diamond_len    = 0;   // ◊ length (if any)
        TokenTag tag_end   = TOK_END;
        int64_t tr         = trace ? 1 : 0;
        loop(t, end - idx)   // loop over one statement
            {
              const Token & tok = input[idx + t];
              const TokenTag tag = tok.get_tag();
              if (tag == TOK_DIAMOND)   // ◊ is allowed (and discarded)
                 {
                   diamond_len = 1;
                   break;
                 }
              else if (tok.is_COND())   // TOK_IF_xxx
                 {
                   tr = -2;
                   diamond_len = 1;
                   tag_end = tok.get_tag();
                   break;
                 }
              else if (tok.get_Class() >= TC_MAX_PERM &&   // bad class,
                       tag != TOK_L_CURLY             &&   // but allow {
                       tag != TOK_R_CURLY)                 // and allow }
                 {
                   if (!tolerant)   // then complain
                      {
                        CERR << "Line " << line << endl
                             << "Offending token: (tag > TC_MAX_PERM) "
                             << tag << " " << tok << "\nStatement: ";
                        loop(t, input.size())   CERR << "`" << input[t] << "  ";
                        CERR << endl;
                      }

                   return E_SYNTAX_ERROR;
                 }

              // none of the above: count and continue;
              ++stat_len;
            }

        // 2. copy input (in APL order) to  output (in reverse order)
        //
        for (ShapeItem j = idx + stat_len - 1; j >= idx; --j)
            {
              output.push_back(input[j]);
            }
        Assert(stat_len + diamond_len);

        // 3. maybe add a TC_END token
        //
        if (stat_len == 0)   // empty statement
           {
             if (tag_end != TOK_END)
                {
                  if (output.size() && output.back().get_tag() == TOK_END)
                         output.pop_back();
                   output.push_back(Token(tag_end, tr));
                }
              ++idx;
           }
        else                 // non-empty statement
           {
             if (tr < 0)   // always add COND token
                {
                  if (output.size() && output.back().get_tag() == TOK_END)
                         output.pop_back();
                  output.push_back(Token(tag_end, tr));
                }
             else if (diamond_len || get_parse_mode() != PM_EXECUTE)
                {
                  output.push_back(Token(tag_end, tr));
                }

             idx += stat_len + diamond_len;   // skip statement and (if any) ◊
           }
      }   // end of loop over statements of one line

   // replace the trailing TOK_END (if any) with TOK_ENDL
   //
   if (output.size() && output.back().get_tag() == TOK_END)
      {
        output.back().ChangeTag(TOK_ENDL);
       }

   Log(LOG_UserFunction__set_line)
      {
        CERR << "[final line " << line << "] ";
        output.print(CERR, false);
      }

   loop(t, output.size())   body.push_back(output[t]);
   return E_NO_ERROR;
}
//----------------------------------------------------------------------------
bool
Executable::compute_if_else_targets()
{
   Log(LOG_IfElse)
      {
         CERR << "initial body at " << LOC ":" << endl;
         print_token(CERR, 3);
      }

const char * cause = "";
Function_Line cause_line = Function_Line_0;
Function_PC2 cause_PC(Function_PC_invalid, Function_PC_invalid);

vector<conditional> conditionals;

   for (Function_PC pc = Function_PC_0; pc < body.size(); ++pc)
       {
         switch(body[pc].get_tag())
            {
              case TOK_IF_THEN:   // →→ (always allowed).
                   Log(LOG_IfElse)
                      CERR << "SEE →→ (then)  at [" << pc << "]" << endl;
          
                   {
                     const conditional cond = { pc, Function_PC(-2) };
                     conditionals.push_back(cond);
                   }
                   LOG_IfElse && pc && body[pc-1].get_Class() == TC_END &&
                     CERR << "\n*** WARNING: TC_END before TOK_IF_THEN" << endl;
                   continue;
          
              case TOK_IF_ELSE:   // ←→
                   Log(LOG_IfElse)
                      CERR << "SEE ←→ (else)  at [PC=" << pc << "]" << endl;
          
                   if (conditionals.size() == 0)
                      {
                        cause = "←→ without →→ (aka. ELSE without IF)";
                        cause_line = get_line(pc);
                        cause_PC.low = pc;
                        goto error;
                      }
                   if (conditionals.back().if_ELSE >= 0)
                      {
                        cause = "duplicate ←→ (aka. duplicate ELSE)";
                        cause_line = get_line(pc);
                        cause_PC.low = conditionals.back().if_ELSE;
                        cause_PC.high = pc;
                        goto error;
                      }
                   conditionals.back().if_ELSE = pc;
                   LOG_IfElse && pc && body[pc-1].get_Class() == TC_END &&
                     CERR << "\n*** WARNING: TC_END before TOK_IF_ELSE" << endl;
                   continue;
          
              case TOK_IF_END:    // ←←
                   Log(LOG_IfElse)
                      CERR << "SEE ←← (endif) at [" << pc << "]" << endl;
          
                   if (conditionals.size() == 0)
                      {
                        cause = "←← without →→ (aka. ENDIF without IF)";
                        cause_line = get_line(pc);
                        cause_PC.low = pc;
                        goto error;
                      }
          
                   {
                     const conditional & cond = conditionals.back();
                    
                     Log(LOG_IfElse)
                       CERR << "cond[" << conditionals.size() << "]:"
                               "\n ├── then:  " << cond.if_THEN
                            << "\n ├── else:  " << cond.if_ELSE
                            << "\n └── endif: " << pc
                            << endl;
          
                     if (cond.if_ELSE  == (cond.if_THEN + 1))   // empty THEN
                        {
                          cause = "empty →→ ... ←→ (aka. empty THEN)";
                        cause_line = get_line(pc);
                        cause_PC.low = pc - 1;
                        cause_PC.high = pc;
                          goto error;
                        }
          
                     if (pc == (cond.if_ELSE + 1))   // empty ELSE
                        {
                          cause = "empty ←→ ... ←← (aka. empty ELSE)";
                          cause_line = get_line(pc);
                          cause_PC.low = pc - 1;
                          cause_PC.low = pc - 1;
                          cause_PC.high = pc;
                          goto error;
                        }
          
                     if (cond.if_ELSE < 0)   // simple if ... endif
                        {
                          body[cond.if_THEN].set_int_val(pc + 1);
                        }
                     else                    // if ... else ... endif
                        {
                          body[cond.if_THEN].set_int_val(cond.if_ELSE + 1);
                          body[cond.if_ELSE].set_int_val(pc + 1);
                        }
                     conditionals.pop_back();
                   }
                   LOG_IfElse && pc && body[pc-1].get_Class() == TC_END &&
                    CERR << "\n*** WARNING: TC_END before TOK_IF_END" << endl;
          
                   continue;
          
              default:
                   continue;
            }
       }

   if (conditionals.size())
      {
        cause = "→→ without ←← (aka. IF without ENDIF)";
        cause_line = get_line(conditionals.back().if_THEN);
        cause_PC.low = conditionals.back().if_THEN;
        goto error;
      }

   Log(LOG_IfElse)
      {
        CERR << "final body[" << body.size() << "]:" << endl;
        print_token(CERR, 3);
      }
   return false;   // OK.

error:
   MORE_ERROR() << cause;
   CERR <<  cause << ". First observed on line ["
        << cause_line << "]";
   if (cause_PC.low  != Function_PC_invalid)   CERR << ", PC=" << cause_PC.low;
   if (cause_PC.high != Function_PC_invalid)   CERR << "-" << cause_PC.high;

   CERR << "." << endl;

   Log(LOG_IfElse)
      {
        CERR << "incorrect body at " <<  LOC "]:" << endl;
        print_token(CERR, 3);
      }
   return true;
}
//----------------------------------------------------------------------------
Token
Executable::execute_body() const
{
StateIndicator & si = *Workspace::SI_top();

   try                 { return si.run();                               }
   catch (Error err)   { return Token(TOK_ERROR, err.get_error_code()); }
   catch (Token tok)   { return tok;                                    }
   catch (...)         { return Token(TOK_ERROR, E_SYSTEM_ERROR);       }

   // not reached
   FIXME;
}
//----------------------------------------------------------------------------
void
Executable::print_token(ostream & out, int details) const
{
   out << endl
       <<  "Function body " << &body
       << " [" << body.size() << " token]:" << endl;

   body.print(out, details);
}
//----------------------------------------------------------------------------
ostream &
Executable::print_range(ostream & out, Function_PC2 from_to) const
{
   for (int pc = from_to.high; pc >= from_to.low; --pc)
       {
         out << "    [" << pc << "] " << body[pc] << endl;
       }
   return out;
}
//----------------------------------------------------------------------------
void
Executable::print_text(ostream & out) const
{
   loop(l, text.size())   out << text[l] << endl;
}
//----------------------------------------------------------------------------
UCS_string
Executable::statement_text(Function_PC pc) const
{
   if (pc >= Function_PC(body.size()))
      {
        Assert(pc == Function_PC(body.size()));
        pc = Function_PC(body.size() - 1);
      }

   if (pc > 0 && body[pc].get_Class() == TC_RETURN)   pc = Function_PC(pc - 1);
   if (pc > 0 && body[pc].get_Class() == TC_END)      pc = Function_PC(pc - 1);

const Function_Line line = get_line(pc);
const UCS_string & line_txt = text[line];

   // count statement ends before pc
   //
int stats_before = 0;
   for (Function_PC p = line_start(line); p < pc; ++p)
       {
         // a TOK_STOP_LINE is immediately followd by a TC_END, but the
         // TC_END has no ◊ in line_txt. We therefore discount stats_before.
         //
         if (body[p].get_tag() == TOK_STOP_LINE)          --stats_before;

         if (body[p].get_Class() == TC_END)               ++stats_before;
         else if (body[p].get_tag() == TOK_RETURN_EXEC)   ++stats_before;
       }

int tidx = 0;
   while(tidx < line_txt.size())
      {
        if (stats_before == 0)   break;
        if ( Avec::is_diamond(line_txt[tidx++])   // ◊ xxx
           )   --stats_before;
      }

   // skip leading spaces
   //
   while (line_txt[tidx] == ' ')   ++tidx;

UCS_string ret;
   while (tidx < line_txt.size())
      {
        if (Avec::is_diamond(line_txt[tidx]))    break;
        ret.append(line_txt[tidx++]);
      }

   // skip trailing spaces
   //
   while (ret.size() && ret.back() <= ' ')   ret.pop_back();

   return ret;
}
//----------------------------------------------------------------------------
VoidCount
Executable::remove_TOK_VOID()
{
   return Parser::remove_TOK_VOID(body);
}
//----------------------------------------------------------------------------
void
Executable::set_error_info(Error & error, Function_PC2 body_from_to) const
{
   /* called rarely, so we have time.
   
      error: the error whose info shall be set
      body_from_to: the body region containing the the error.
     
      NOTE:
     
      body_from_to     are the absolute positions (PCs) in the body (!),
                       i.e. the offsets from the first token of the body, while
     
      after_from_to    are the offsets of the first and last token in
                       the failed statement (!) after any optimization, and
     
      before_from_to   are the offsets of the first and last token in
                         the failed statement (before amy optimization).
    */

   if (0)   print_range(CERR, body_from_to);

   // for value errors we point to the failed symbol itself.
   //
   if (error.get_error_code() == E_VALUE_ERROR)
      body_from_to.low = body_from_to.high;

   Log(LOG_prefix__location_info)
      {
        Q1(body.size())
        Q1(body_from_to.low)
        Q1(body_from_to.high)
        if (body.size())   Q1(body[0])
      }

   // decrement body_from_to.high if it points to the end of
   // the function.
   //
   if (body[body_from_to.high].get_Class() == TC_RETURN &&
       body_from_to.high > body_from_to.low)   --body_from_to.high;

Function_PC start = get_statement_start(body_from_to.low);
Function_PC end = get_statement_end(body_from_to.high);

   Assert(start   <= body_from_to.low);
   Assert(body_from_to.low  <= body_from_to.high);
   if (body_from_to.high > end)   body_from_to.high = end;

   Log(LOG_prefix__location_info)
      {
        Q1(start)
        Q1(body_from_to.low)
        Q1(body_from_to.high)
        Q1(end)
      }

   // extract the token of the failed statement and adjust the PCs so that
   // they refer to the failed statement (as opposed to body).
   //
const Token_string stat_after(body, start, end - start);

const Function_PC2 after_from_to(body_from_to.low  - start,
                                 body_from_to.high - start );

   // undo any optimizations that were performed with the failed line
   //
Token_string stat_before;
   reparse(stat_before, body_from_to.low);
Function_PC2 before_from_to = after_from_to;

   // transform the range 'after_from'_to in the (possibly optimized) body back
   // to the range 'before_from'_in the body before optimization.
   //
bool optimized = false;

   /*
    compute the vector 'before_ranges', whose items correspond to
    stat_before, so that if stat_after[j] is the optimization of
    (before_ranges = J)/stat_before.

      That is, for example:

       stat_after is:  [ a0  a1     a2     a3 ... ] and
                         │   │      │      │
                         │   │   ┌──┼──┐   │
                         │   │   │  │  │   │
       stat_before is: [ b0  b1  b2 b2 b2  b3 ... ]  and then:
                         ^       ^

       before_ranges should be:  0 1 2 2 2 3
   */
basic_string<int>ranges_before;   ranges_before.reserve(stat_before.size());

   for (Function_PC PC_before = Function_PC_0, PC_after  = Function_PC_0;
        int(ranges_before.size()) < stat_before.size();
          ++PC_before, ++PC_after) 
      {
        if (stat_after[PC_after].get_tag() == TOK_APL_VALUE4)
           {
             // optimized value. Only set in
             // Parser::optimize_short_primitives() to replace
             // e.g. A⍴B with small results. The monadic case is not (yet)
             // handled properly !!!
             //
             ranges_before.push_back(PC_after);   // A
             ranges_before.push_back(PC_after);   // ⍴
             ranges_before.push_back(PC_after);   // B
             optimized = true;
           }
        else
           {
             ranges_before.push_back(PC_after);   // (A⍴B)
           }
      }

   rev_loop(PC_before, ranges_before.size())
      {
        const Function_PC after = Function_PC(ranges_before[PC_before]);
        if (after == after_from_to.low)
           before_from_to.low  = Function_PC(PC_before);
        if (after == after_from_to.high)
           before_from_to.high = Function_PC(PC_before);

        // skip subsequent items within the same range
        while (PC_before > 0 &&
           ranges_before[PC_before - 1] == after)   --PC_before;
      }

   // if the failed statenment was optimized then we want to display the
   // statement BEFORE the optimization.
   //
   if (optimized)
      set_error_info(error, stat_before, before_from_to);
   else
      set_error_info(error, stat_after, after_from_to);
}
//--------------------------------------------------------------------------
void
Executable::set_error_info(Error & error,
                           const Token_string & failed_statement, 
                           Function_PC2 range) const
{
   /* if the error was caused by some function being executed, say f in
      A f B then the right arg B of the function f was OK and we skip 
      Thus:     A f B
                ^   ^

      becomes:  A f B
                ^ ^

      IOW: a Value B can never be wrong.
    */
   if (failed_statement[range.low].get_Class() == TC_VALUE)
      {
         ++range.low;
      }

   if (error.get_error_code() == E_SYNTAX_ERROR)
      {
        Prefix::adjust_right_caret(range, failed_statement);
        error.add_MORE_indicator(Workspace::more_error().size());
      }

   Log(LOG_prefix__location_info)
      {
        Q1(range.low)
        Q1(range.high)
        Q1(failed_statement.size())
      }

   // Line 2: statement
   //

int len_left = 0;      // characters before the left caret
int len_between = 0;   // distance between the left and the right caret

   // every statement is stored right to left. In order to print it
   // left to right we have to move backwards from the end.
   //
UCS_string message_2(error.get_error_line_2());
   rev_loop(q, failed_statement.size())
       {
         // avoid duplicate ∘ in ∘.f
         //
         if (failed_statement[q].get_tag() == TOK_JOT && q > 0 &&
             failed_statement[q - 1].get_tag() == TOK_OPER2_OUTER)   continue;

         // Note: Token::error_info returns -len if it inserts a space.
         // Such an inserted space counts for the previous token.
         // The previous token is q + 1 since we count down.
         //
         int len = failed_statement[q].error_info(message_2);

         if (len < 0)   // space inserted, len is negative
            {
              if ((q + 1) > range.high)       len_left++;
              else if ((q + 1) > range.low)   len_between++;
              len = -len;
            }

         if (q > range.high)       len_left += len;
         else if (q > range.low)   len_between += len;
       }

   {
     const UTF8_string utf(message_2);
     error.set_error_line_2(utf.c_str());
   }

   // Line 3: carets
   //
   error.set_left_caret(error.get_left_caret() + len_left);

   if (range.high != range.low)   // two carets
      {
        error.set_right_caret(error.get_left_caret() + len_between);
      }
}
//----------------------------------------------------------------------------
void
Executable::reparse(Token_string & original, Function_PC low_PC) const
{
   // compute the failed line and the failed statement in the line by
   // counting the end-of-statement and the end-of-line token before low_PC.
   //
int line = 1;
int statement = 0;
   loop(pc, low_PC)
       {
         const TokenTag tag = body[pc].get_tag();
         if (tag == TOK_END)         // end of statement
            {
              ++statement;
            }
         else if (tag == TOK_ENDL)   // end of line
            {
              line++;
              statement = 0;
            }
       }

   // Caution: ⍎ and ◊ executables have only line 0.
   //
   if (get_parse_mode() != PM_FUNCTION)   line = 0;

   Assert(line <= text.size());
const UCS_string & failed_line = text[line];

   // extract the failed statement text from the failed_line
   //
UCS_string failed_statement;
   {
     int l = 0;

     loop(f, failed_line.size())
         {
           if (failed_line[f] == UNI_DIAMOND)   { ++l;   continue; }
           if (l > statement)   break;      // subsequent line
           if (l < statement)   continue;   // previous line
           failed_statement += failed_line[f];
         }
   }

   // reparse the failed_statement before the optimization
   // into orig (in APL order)
const Parser parser(get_parse_mode(), LOC, false);
Token_string orig;
const ErrorCode ec = parser.parse(failed_statement, orig, false);
   Assert(ec == E_NO_ERROR);

   // revert orig, starting after label (if eny)
   //
const int end = orig.size() > 1 && orig[1].get_tag() == TOK_COLON ? 2 : 0;
   for (int j = orig.size() - 1; j >= end; --j)   original.push_back(orig[j]);
}
//----------------------------------------------------------------------------
Function_PC
Executable::get_statement_end(Function_PC pc) const
{
   while (body[pc].get_Class() != TC_END &&
          body[pc].get_tag() != TOK_RETURN_EXEC)   ++pc;
   return pc;
}
//----------------------------------------------------------------------------
Function_PC
Executable::get_statement_start(Function_PC pc) const
{
   // this function is used in error reporting so it should
   // not Assert() and the like as to avoid infinite recursion.
   //
   // given pc, return the start of the statement to which pc belongs

   if (pc >= Function_PC(body.size()))   pc = Function_PC(body.size() - 1);

   // if we are at the end of the statement, move back.
   //
   if (int(pc) < 2)   return Function_PC_0;
   if (body[pc].get_Class() == TC_RETURN)   --pc;
   if (body[pc].get_Class() == TC_END)      --pc;

   for (; int(pc) > 0; --pc)
      {
        if (body[pc-1].get_Class() == TC_END)          return pc;
        if (body[pc-1].get_tag() == TOK_RETURN_EXEC)   return pc;
      }

   return Function_PC_0;
}
//----------------------------------------------------------------------------
void
Executable::unmark_all_values() const
{
   loop (b, body.size())
      {
        const Token & tok = body[b];
        if (tok.get_ValueType() == TV_VAL)
           {
             Value_P value = tok.get_apl_val();
             if (+value)   value->unmark();
           }

        if (tok.get_ValueType() == TV_FUN)
           {
             cFunction_P fun = tok.get_function();
             const UserFunction * ufun = fun->get_func_ufun();
             if (ufun && ufun->is_lambda())
                {
                  ufun->unmark_all_values();
                }
           }
      }
}
//----------------------------------------------------------------------------
int
Executable::show_owners(const char * prefix, ostream & out,
                        const Value & value) const
{
int count = 0;

   loop (b, body.size())
      {
        const Token & tok = body[b];
        if (tok.get_ValueType() != TV_VAL)      continue;

        if (Value::is_or_contains(tok.get_apl_val().get(), &value))
           {
             out << prefix << get_name() << "[" << b << "]" << endl;
             ++count;
           }
      }

   return count;
}
//----------------------------------------------------------------------------
void
Executable::setup_lambdas()
{
   // quick check if this body contains any lambdas. Parser::match_par_bra()
   // should have checked for unbalanced { } so we simply look for } (which
   // is the first to occur since the body is reversed.
   //
   if (!body_has_curly())   return;   // no { ... } in this body

   // undo the token reversion of the body so that the body token and the
   // function text run in the same direction.
   //
   reverse_each_statement(body);

int lambda_num = 0;
   loop(b, body.size())
       {
         if (body[b].get_tag() != TOK_L_CURLY)   continue;   // not {

         ShapeItem end = -1;
         int curly_level = 1;   // since body[b] is {
         for (ShapeItem b1 = b + 1; b1 < ShapeItem(body.size()); ++b1)
             {
               switch(body[b1].get_tag())
                  {
                    case TOK_L_CURLY:
                         ++curly_level;
                         continue;

                    case TOK_R_CURLY:
                         if (0 == --curly_level)
                            {
                              end = b1;
                              b1 = body.size();
                            }
                         continue;

                    default:
                         continue;
                  }
             }
         Assert(end != -1);
         b = setup_one_lambda(b, end, ++lambda_num);
       }

   // redo the token reversion of the body so that the body token and the
   // text run in the same direction.
   //
   reverse_each_statement(body);

   Parser::match_par_bra(body, true);
   remove_TOK_VOID();
}
//----------------------------------------------------------------------------
ShapeItem
Executable::setup_one_lambda(ShapeItem b, ShapeItem bend, int lambda_num)
{
   // called with this->body in forward (APL-) order, i.e  { ... }
   //
   Assert(body[b].get_tag()    == TOK_L_CURLY);
   Assert(body[bend].get_tag() == TOK_R_CURLY);

   body[b++].clear(LOC);    // invalidate the leading  {
   body[bend].clear(LOC);   // invalidate the trailing }

Token_string lambda_body;
const Fun_signature signature = compute_lambda_body(lambda_body, b, bend);

   /* strip off the trailing local variables.

      The function body ends with ENDL RETURN.
      To strip off a local variable we have to:

      body[-4]        body[-3]        body[-2]        body[-1] 
      ─────────────────────────────────────────────────────────────
      ';'             SYMBOL          ENDL            RETURN
                                      │               │
      ┌─────────────── ← ─────────────┘               │
      │               ┌─────────────── ← ─────────────┘
      │               │
      ENDL            RETURN
    */
basic_string<Symbol *> local_vars;   // collector for local variables
   while (lambda_body.size() >= 4)
      {
        const size_t semi = lambda_body.size() - 4;   // maybe ';' SYMBOL ?
        if (lambda_body[semi]  .get_tag() != TOK_SEMICOL)   break;   // no
        if (lambda_body[semi+1].get_Class() != TC_SYMBOL)   break;   // no
        local_vars.push_back(lambda_body[semi + 1].get_sym_ptr());
        lambda_body[semi]     = lambda_body[semi + 2];
        lambda_body[semi + 1] = lambda_body[semi + 3];
        lambda_body.resize(semi + 2);   // leave ENDL and RETURN_SYMBOL
      }

const UCS_string lambda_text = extract_lambda_text(signature, lambda_num - 1);

   reverse_each_statement(lambda_body);
   reverse_all_token(lambda_body);

  {
    basic_string<Symbol *> local_vars;
    while (lambda_body.size() > 4)
       {
         const size_t semi = lambda_body.size() - 4;
         if (lambda_body[semi]    .get_tag() != TOK_SEMICOL)   break;
         if (lambda_body[semi + 1].get_tag() != TOK_SYMBOL)    break;
         local_vars.push_back(lambda_body[semi + 1].get_sym_ptr());
         lambda_body[semi]     = lambda_body[semi + 2];
         lambda_body[semi + 1] = lambda_body[semi + 3];
         lambda_body.resize(semi + 2);   // leave ENDL and RETURN_SYMBOL
       }
   }

UserFunction * ufun = new UserFunction(signature, lambda_num,
                                       lambda_text, lambda_body, local_vars);

   ufun->increment_refcount(LOC);

   // put a token for the lambda at the place where the { was.
   // That replaces, for example, (in forward notation):
   //
   // A←{ ... }   by:
   // A←UFUN      UFUN being a user-defined function with body { ... }
   //
Token tok_ufun = ufun->get_token();
   body[bend].move(tok_ufun, LOC);

   return bend;
}
//----------------------------------------------------------------------------
Fun_signature
Executable::compute_lambda_body(Token_string & lambda_body,
                                ShapeItem b, ShapeItem bend)
{
int signature = SIG_FUN;
int level = 0;

   /* find the insertion point for λ←. for single statement lambda the
      insertion point is the first body token lambda_body[b].
      For a multi-statement lambda it is the last ◊ before lambda_body[bend].
    */

ShapeItem insertion_point = b;
   for (ShapeItem j = b; j < bend; ++j)
       {
         if (body[j].get_tag() == TOK_END)   insertion_point = j + 1;
       }

   for (; b < bend; ++b)
       {
         Token t;
         t.move(body[b], LOC);
         body[b].clear(LOC);   // invalidate in main body

         // figure the signature by looking for ⍺, ⍶, ⍵, ⍹, and χ
         // and complain about ◊ and →
         switch(t.get_tag())
            {
              case TOK_ALPHA:   if (!level)   signature |= SIG_A;   // no break
              case TOK_OMEGA:   if (!level)   signature |= SIG_B;         break;
              case TOK_CHI:     if (!level)   signature |= SIG_X;         break;
              case TOK_OMEGA_U: if (!level)   signature |= SIG_RO;  // no break
              case TOK_ALPHA_U: if (!level)   signature |= SIG_LO;        break;

              case TOK_DIAMOND: DEFN_ERROR;
              case TOK_BRANCH:  DEFN_ERROR;
              case TOK_ESCAPE:  DEFN_ERROR;

              case TOK_L_CURLY: ++level;   break;
              case TOK_R_CURLY: --level;   break;
              default: break;
            }

         if (b == insertion_point)
            {
              // insert  λ ← tokens
              //
              Symbol * sym_Z = &Workspace::get_v_LAMBDA();
              lambda_body.push_back(Token(TOK_LAMBDA, sym_Z));
              lambda_body.push_back(Token(TOK_ASSIGN1));

              signature |= SIG_Z;
            }

         lambda_body.push_back(t);
       }

   if ((signature & SIG_B) == 0 &&   // niladic
       (signature & (SIG_A | SIG_LO | SIG_RO | SIG_X)))
      {
        MORE_ERROR() <<
           "niladic lambda with axis. left argument, or function argument(s)";
        DEFN_ERROR;
      }

   if ((signature & SIG_LO) && (signature & SIG_X))   // dyadic oper with axis
      {
        MORE_ERROR() << "invalid lambda operator (dyadic with axis)";
        DEFN_ERROR;
      }

   // if the lambda has at least one token, then it supposedly returns λ.
   // Otherwise the lambda is empty (and result-less)
   //
   if (signature & SIG_Z)   // result
      {
        const int64_t trace = 0;
        lambda_body.push_back(Token(TOK_ENDL, trace));

        Token ret_lambda(TOK_RETURN_SYMBOL, &Workspace::get_v_LAMBDA());
        lambda_body.push_back(ret_lambda);
      }
   else
      {
        Token ret_void(TOK_RETURN_VOID);
        lambda_body.push_back(ret_void);
      }

   return Fun_signature(signature);
}
//----------------------------------------------------------------------------
UCS_string
Executable::extract_lambda_text(Fun_signature signature, int skip) const
{
   // this->text is the text of this Executable. Extract the text of
   // the skip'th lambda in this->text (and return it).
   //
UCS_string lambda_text;

int level = 0;   // {/} nesting level
bool copying = false;
ShapeItem tidx = 0;    // the current line in text[]
ShapeItem tcol = 0;    // the current column in text[tidx];
bool in_single_quotes = false;
bool in_double_quotes = false;

   // skip over the first skip lambdas and copy the next one to lambda_text
   //
   for (;;)
       {
         if (tidx >= text.size())
            {
              Q1(copying)   Q1(tidx)   Q1(skip)   FIXME;
            }

         const UCS_string & line = text[tidx];

         if (tcol >= line.size())   // end of line: wrap to next line
            {
              ++tidx;     // next line
              tcol = 0;   // first column
              in_single_quotes = false;
              in_double_quotes = false;
              continue;
            }

         const Unicode uni = line[tcol++];
         if (in_single_quotes)
            {
              if (uni == UNI_SINGLE_QUOTE)   in_single_quotes = false;
              if (copying)   lambda_text.append(uni);
              continue;
            }

         if (in_double_quotes)
            {
              if (uni == UNI_DOUBLE_QUOTE     // either \" or string end
                 && !(tcol >= 2 && line[tcol - 2] == UNI_BACKSLASH))
                 in_double_quotes = false;
              if (copying)   lambda_text.append(uni);
              continue;
            }

         // at this point uni is outside strings
         //
         if (uni == UNI_COMMENT || uni == UNI_NUMBER_SIGN)
            {
              ++tidx;
              tcol = 0;
              in_single_quotes = false;
              in_double_quotes = false;
              continue;
            }

         if (copying)   lambda_text.append(uni);

         switch(uni)
            {
              case UNI_SINGLE_QUOTE:             // start of a new '...' string
                   in_single_quotes = true;
                   continue;

              case UNI_DOUBLE_QUOTE:       // start of a new "..." string
                   in_double_quotes = true;
                   continue;

              case UNI_L_CURLY:            // start of a new { ... }
                   if (level++ == 0 && skip == 0)   copying = true;
                   continue;

              case UNI_R_CURLY:            // end of { ... }
                   if (--level)   continue;      // but not top-level }
                   --skip;                       // next {...} at top-level
                   if (!copying)  continue;

                   lambda_text.pop_back();       // the last }
                   goto out;

              default: continue;
            }
       }

out:
   
   if (signature & SIG_Z)  // insert λ ←
      {
        ShapeItem insertion_point = 0;
        rev_loop(j, lambda_text.size())
           {
             if (lambda_text[j] == UNI_DIAMOND)
                {
                  insertion_point = j + 1;
                  while (lambda_text[insertion_point] <= UNI_SPACE)
                         ++insertion_point;
                  break;
                }
           }
        UCS_string ret;
        loop(j, lambda_text.size())
            {
              if (j == insertion_point)
                 {
                   ret += UNI_LAMBDA;       // insert λ
                   ret += UNI_LEFT_ARROW;   // insert ←
                 }
              ret += lambda_text[j];
            }
        return ret;
      }
    return lambda_text;
}
//----------------------------------------------------------------------------
void
Executable::reverse_each_statement(Token_string & tos)
{
const ShapeItem last = tos.size();
   for (ShapeItem from = 0, to = 1; to < tos.size(); ++to)
       {
         if (tos[to].get_Class() == TC_END)
            {
              // found ia TC_END token within tos. Reverse the tokens
              // from...to to - 1, but leave the TC_END token as is.
              //
              if (from != to)   tos.reverse_from_to(from, to - 1);
              from = to + 1;   // continue after the TC_END token.
            }
         else if (to == (last - 1))
            {
              // reached the end of tos. from...to is the last statement
              // in tos. revert then entire statement (no TC_END to preserve).
              //
              tos.reverse_from_to(from, to);
              break;
            }
       }
}
//----------------------------------------------------------------------------
void
Executable::reverse_all_token(Token_string & tos)
{
   for (Token * t1 = &tos[0], * t2 = &tos[tos.size() - 1]; t1 < t2;)
       t1++->swap_token(*t2--);
}
//----------------------------------------------------------------------------
void
Executable::increment_refcount(const char * loc)
{
   Assert1(get_exec_ufun());
   Assert(get_exec_ufun()->is_lambda());

   ++refcount;

// CERR << "*** increment_refcount() of " << get_name()
//      << " to " << refcount << " at " << loc << endl;A
}
//----------------------------------------------------------------------------
void
Executable::decrement_refcount(const char * loc)
{
UserFunction * ufun = get_exec_ufun();
   Assert1(ufun);
   Assert(ufun->is_lambda());

   if (refcount <= 0)
      {
        CERR << "*** Warning: refcount of " << get_name() << " is " << refcount
             << ":" << endl;
        print_text(CERR);
        FIXME;
      }

   --refcount;

// CERR << "*** decrement_refcount() of " << get_name()
//      << " to " << refcount << " at " << loc << endl;


   if (refcount <= 0)
      {
//      CERR << "*** lambda died" << endl;
//      clear_body();
        delete ufun;
      }
}
//============================================================================
ExecuteList *
ExecuteList::fix(const UCS_string & data, const char * loc)
{
   // clear errors that may have occured before
   if (Error * err = Workspace::get_error())   err->clear_error_code();

ExecuteList * fun = new ExecuteList(data, loc);

   Log(LOG_UserFunction__fix)
      {
        CERR << "fix pmode=execute list:" << endl << data
             << " addr " << voidP(fun) << endl
             << "------------------- ExecuteList::fix() --" << endl;
      }

   {
     Error * err = Workspace::get_error();
     if (err && err->get_error_code())
        {
          Log(LOG_UserFunction__fix)
             {
                CERR << "fix pmode=execute list failed with error "
                     << Error::error_name(err->get_error_code()) << endl;
             }

          err->set_parser_loc(0);
          delete fun;
          return 0;
        }
   }

   try
      {
        fun->parse_body_line(Function_Line_0, data, false, false, loc, false);
      }
   catch (Error err)
      {
        if (Error * werr = Workspace::get_error())   *werr = err;

        delete fun;
        return 0;
      }

   fun->setup_lambdas();

   Log(LOG_UserFunction__fix)
      {
        CERR << "fun->body.size() is " << fun->body.size() << endl;
      }

   // for ⍎ we do not append TOK_END, but only TOK_RETURN_EXEC.
   fun->body.push_back(Token(TOK_RETURN_EXEC));
   if (fun->compute_if_else_targets())
      {
        delete fun;
        return 0;
      }

   Log(LOG_UserFunction__fix)   fun->print(CERR);
   return fun;
}
//============================================================================
StatementList *
StatementList::fix(const UCS_string & data, const char * loc)
{
StatementList * fun = new StatementList(data, loc);

   Log(LOG_UserFunction__fix)
      {
        CERR << "fix pmode=statement list:" << endl << data << endl
             << " addr " << voidP(fun) << endl
             << "------------------- StatementList::fix() --" << endl;
      }

   if (Error * err = Workspace::get_error())   err->set_parser_loc(0);

   try
      {
        fun->parse_body_line(Function_Line_0, data, false, false, loc, false);
        fun->setup_lambdas();
      }
   catch (Error & e)
      {
        Log(LOG_UserFunction__fix)
           CERR << "parse_body_line(line 0) failed" << endl;
        delete fun;
        throw e;
      }

   Log(LOG_UserFunction__fix)
      {
        CERR << "fun->body.size() is " << fun->body.size() << endl;
      }

   fun->body.push_back(Token(TOK_RETURN_STATS));
   if (fun->compute_if_else_targets())
      {
        delete fun;
        return 0;
      }

   Log(LOG_UserFunction__fix)   fun->print(CERR);
   return fun;
}
//============================================================================
