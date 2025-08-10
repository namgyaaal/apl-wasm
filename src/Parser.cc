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

#include "Bif_F12_COMMA.hh"
#include "Bif_F12_PARTITION_PICK.hh"
#include "CharCell.hh"
#include "ComplexCell.hh"
#include "Common.hh"
#include "FloatCell.hh"
#include "IndexExpr.hh"
#include "IntCell.hh"
#include "Output.hh"
#include "Parser.hh"
#include "PointerCell.hh"
#include "PrintOperator.hh"
#include "Symbol.hh"
#include "SystemLimits.hh"
#include "SystemVariable.hh"
#include "Value.hh"
#include "Tokenizer.hh"
#include "Workspace.hh"

//----------------------------------------------------------------------------
ErrorCode
Parser::parse(const UCS_string & input, Token_string & tos,
              bool optimize) const
{
   // convert input characters into token
   //
Token_string tos1;

   {
     Tokenizer tokenizer(pmode, LOC, macro);
     if (const ErrorCode ec = tokenizer.tokenize(input, tos1))   return ec;
   }

   // special case: single token (to speed up ⍎)
   //
   if (tos1.size() == 1)
      {
        const ErrorCode err = parse_statement(tos1, optimize);
        if (err == E_NO_ERROR)   tos.push_back(tos1[0]);
        return err;
      }

   return parse(tos1, tos, optimize);
}
//----------------------------------------------------------------------------
ErrorCode
Parser::parse(const Token_string & input, Token_string & tos,
              bool optimize) const
{
   Log(LOG_parse)
      {
        CERR << "parse 1 [" << input.size() << "]: ";
        input.print(CERR, true);
        CERR << endl;
      }

   // split input line into statements (separated by ◊)
   //
std::basic_string<Token_string *> statements;
   {
     Token_string * stat = new Token_string();
     int curly_depth = 0;
     loop(idx, input.size())
        {
          const Token & tok = input[idx];
          switch(tok.get_tag())
             {
               case TOK_L_CURLY:
                    ++curly_depth;
                    stat->push_back(tok);
                    break;

               case TOK_R_CURLY:
                    if (curly_depth)   --curly_depth;
                    else               return E_UNBALANCED_R_CURLY;
                    stat->push_back(tok);
                    break;

               case TOK_DIAMOND:
                    if (curly_depth)   // ◊ inside { ... }
                       {
               //        return E_ILLEGAL_DIAMOND;  // single statement { ... }
                         stat->push_back(tok);      // multi  statement { ... }
                       }
                    else               // normal ◊
                       {
                         statements.push_back(stat);
                         stat = new Token_string();
                       }
                    break;
          default:
               stat->push_back(tok);
             }
        }
     statements.push_back(stat);
   }

   loop(s, statements.size())
      {
        Token_string * stat = statements[s];
        if (const ErrorCode err = parse_statement(*stat, optimize))
           {
             while (size_t(s) < statements.size())
               {
                 stat = statements[s++];
                 delete stat;
               }
             return err;
           }

        if (s)   tos.push_back(Token(TOK_DIAMOND));

        loop(t, stat->size())
           {
             tos.push_back(Token());
             tos[tos.size() - 1].move((*stat)[t], LOC);
           }
        delete stat;
      }

   return E_NO_ERROR;
}
//----------------------------------------------------------------------------
ErrorCode
Parser::parse_statement(Token_string & tos, bool optimize)
{
   // 1. convert (X) into X and ((X...)) into (X...)
   //
   remove_nongrouping_parantheses(tos);
   remove_TOK_VOID(tos);

   Log(LOG_parse)
      {
        CERR << "parse 2 [" << tos.size() << "]: ";
        tos.print(CERR, true);
      }

   // 2. convert groups like '(' val val...')' into single APL values
   // A group is defined as 2 or more values enclosed in '(' and ')'.
   // For example: (1 2 3) or (4 5 (6 7) 8 9)
   //
   for (int progress = true; progress;)
      {
        progress = collect_groups(tos);
        if (progress)   remove_TOK_VOID(tos);
      }

   Log(LOG_parse)
      {
        CERR << "parse 3 [" << tos.size() << "]: ";
        tos.print(CERR, true);
      }

   // 3. convert vectors like 1 2 3 or '1' 2 '3' into single APL values
   //
   if (collect_constants(tos))           remove_TOK_VOID(tos);

   // parse_statement() is normally called with optimize == true; However,
   // Executable::reparse() calls it with optimize == false in order to
   // restore the body before it was optimized.
   //
   // To disable optimizations you must set the enable arg(s) in
   // Performance.def to 0 (as opposed to setting optimize to false.
   //
   if (optimize)
      {
        if (DO_FT_LITERAL_AXIS && optimize_literal_axes(tos))
           remove_TOK_VOID(tos);

        while (DO_FT_SHORT_PRIMITIVE && optimize_short_primitives(tos))
           remove_TOK_VOID(tos);
      }

   // special case: single APL value (to speed up ⍎)
   //
   if (tos.size() == 1)
      {
        Log(LOG_parse)   CERR << "parse 3a: single value " << tos[0] << endl;;
        return E_NO_ERROR;
      }

   Log(LOG_parse)
      {
        CERR << "parse 4 [" << tos.size() << "]: ";
        tos.print(CERR, true);
      }

   // 5. mark symbol left of ← as LSYMB
   //
   mark_lsymb(tos);
   Log(LOG_parse)
      {
        CERR << "parse 5 [" << tos.size() << "]: ";
        tos.print(CERR, true);
      }

   // 6. replace bitwise functions ⊤∧, ⊤∨, ⊤⍲, and ⊤⍱ by their bitwise variant
   //
   if (optimize)
      {
        optimize_static_patterns(tos);
        Log(LOG_parse)
           {
             CERR << "parse 6 [" << tos.size() << "]: ";
             tos.print(CERR, true);
           }
      }

   // 7. update the distances between ( and ), [ and ], or { and }. After
   //    that, tos.size() must not be changed anymore.
   //
   {
     const ErrorCode ec = match_par_bra(tos, false);
     if (ec != E_NO_ERROR)
        {
          loop(t, tos.size())   tos[t].clear(LOC);
          return ec;
        }
   }

   return E_NO_ERROR;
}
//----------------------------------------------------------------------------
bool
Parser::collect_constants(Token_string & tos)
{
bool progress = false;
   Log(LOG_collect_constants)
      {
        CERR << "collect_constants [" << tos.size() << " token] in: ";
        tos.print(CERR, true);
      }

   // convert several items in vector notation into a single APL value
   //
   loop (t, tos.size())
      {
        ShapeItem to;
        switch(tos[t].get_tag())
           {
             case TOK_APL_VALUE1:
             case TOK_APL_VALUE3:
             case TOK_APL_VALUE4:
             case TOK_CHARACTER:
             case TOK_COMPLEX:
             case TOK_INTEGER:
             case TOK_REAL:
                  break;          // continue below

             default: continue;   // nex token
           }

        // at this point, t is the first item. Collect subsequenct items.
        //
        for (to = t + 1; to < tos.size(); ++to)
            {
#if 1
              // Note: ISO 13751 gives an example with 1 2 3[2] ↔ 2
              // 
              // In contrast, the binding rules stated in IBM's apl2lrm.pdf
              // would give 3[2] ↔ RANK ERROR.
              // 
              // We follow apl2lrm; to enable ISO behavior write #if 0 above.
              // 

              // if tos[to + 1] is [ then [ binds stronger than
              // vector notation and we stop collecting.
              //
              if ((to + 1) < tos.size() &&
                  tos[to + 1].get_tag() == TOK_L_BRACK)   break;
#endif
              const TokenTag tag = tos[to].get_tag();

              if (tag == TOK_CHARACTER)    continue;
              if (tag == TOK_INTEGER)      continue;
              if (tag == TOK_REAL)         continue;
              if (tag == TOK_COMPLEX)      continue;
              if (tag == TOK_APL_VALUE1)   continue;
              if (tag == TOK_APL_VALUE3)   continue;
              if (tag == TOK_APL_VALUE4)   continue;

              // no more values: stop collecting
              //
              break;
            }

        create_value(tos, t, to - t);
        if (to > (t + 1))   progress = true;
      }

   Log(LOG_collect_constants)
      {
        CERR << "collect_constants [" << tos.size() << " token] out: ";
        tos.print(CERR, true);
      }

   return progress;
}
//----------------------------------------------------------------------------
bool
Parser::collect_groups(Token_string & tos)
{
   Log(LOG_collect_constants)
      {
        CERR << "collect_groups [" << tos.size() << " token] in: ";
        tos.print(CERR, true);
      }

int opening = -1;
   loop(t, tos.size())
       {
         switch(tos[t].get_tag())
            {
              case TOK_CHARACTER:
              case TOK_INTEGER:
              case TOK_REAL:
              case TOK_COMPLEX:
              case TOK_APL_VALUE1:
              case TOK_APL_VALUE3:
              case TOK_APL_VALUE4:
                   continue;

              case TOK_L_PARENT:
                   // we remember the last opening '(' so that we know where
                   // the group started when we see a ')'. This causes the
                   // deepest ( ... ) to be grouped first.
                   opening = t;
                   continue;

              case TOK_R_PARENT:
                   if (opening == -1)      continue;       // ')' without '('
                   if (opening == t - 1)   SYNTAX_ERROR;   // '(' ')'

                   tos[opening].clear(LOC);  // invalidate '('
                   tos[t].clear(LOC);        // invalidate ')'
                   create_value(tos, opening + 1, t - opening - 1);

                   // we removed parantheses, so we close the value.
                   //
                   if (tos[opening + 1].get_Class() == TC_VALUE)
                      tos[opening + 1].ChangeTag(TOK_APL_VALUE1);

                   Log(LOG_collect_constants)
                      {
                        CERR << "collect_groups [" << tos.size()
                             << " token] out: ";
                        tos.print(CERR, true);
                      }
                   return true;

              default: // nothing group'able
                   opening = -1;
                   continue;
            }
       }

   return false;
}
//----------------------------------------------------------------------------
int
Parser::find_closing_bracket(const Token_string & tos, int pos)
{
   Assert(tos[pos].get_tag() == TOK_L_BRACK);

int others = 0;

   for (ShapeItem p = pos + 1; p < tos.size(); ++p)
       {
         Log(LOG_find_closing)
            CERR << "find_closing_bracket() sees " << tos[p] << endl;

         if (tos[p].get_tag() == TOK_R_BRACK)
            {
             if (others == 0)   return p;
             --others;
            }
         else if (tos[p].get_tag() == TOK_R_BRACK)   ++others;
       }

   SYNTAX_ERROR;
}
//----------------------------------------------------------------------------
int
Parser::find_opening_bracket(const Token_string & tos, int pos)
{
   Assert(tos[pos].get_tag() == TOK_R_BRACK);

int others = 0;

   for (int p = pos - 1; p >= 0; --p)
       {
         Log(LOG_find_closing)
            CERR << "find_opening_bracket() sees " << tos[p] << endl;

         if (tos[p].get_tag() == TOK_L_BRACK)
            {
             if (others == 0)   return p;
             --others;
            }
         else if (tos[p].get_tag() == TOK_R_BRACK)   ++others;
       }

   SYNTAX_ERROR;
}
//----------------------------------------------------------------------------
int
Parser::find_closing_parent(const Token_string & tos, int pos)
{
   Assert1(tos[pos].get_Class() == TC_L_PARENT);

int others = 0;

   for (ShapeItem p = pos + 1; p < tos.size(); ++p)
       {
         Log(LOG_find_closing)
            CERR << "find_closing_bracket() sees " << tos[p] << endl;

         if (tos[p].get_Class() == TC_R_PARENT)
            {
             if (others == 0)   return p;
             --others;
            }
         else if (tos[p].get_Class() == TC_R_PARENT)   ++others;
       }

   SYNTAX_ERROR;
}
//----------------------------------------------------------------------------
int
Parser::find_opening_parent(const Token_string & tos, int pos)
{
   Assert(tos[pos].get_Class() == TC_R_PARENT);

int others = 0;

   for (int p = pos - 1; p >= 0; --p)
       {
         Log(LOG_find_closing)
            CERR << "find_opening_bracket() sees " << tos[p] << endl;

         if (tos[p].get_Class() == TC_L_PARENT)
            {
             if (others == 0)   return p;
             --others;
            }
         else if (tos[p].get_Class() == TC_R_PARENT)   ++others;
       }

   SYNTAX_ERROR;
}
//----------------------------------------------------------------------------
void
Parser::remove_nongrouping_parantheses(Token_string & tos)
{
   //
   // 1. replace ((X...)) by: (X...)
   // 2. replace (X)      by: X for a single token X,
   //
   for (bool progress = true; progress; progress = false)
       {
         loop(t, tos.size() - 2)
             {
               if (tos[t].get_Class() != TC_L_PARENT)   continue;

               const int closing = find_closing_parent(tos, t);

               // check for case 1.
               //
               if (tos[t + 1].get_Class() == TC_L_PARENT)
                  {
                    // tos[t] is (( ...
                    //
                    const int closing_1 = find_closing_parent(tos, t + 1);
                    if (closing == (closing_1 + 1))
                       {
                         // tos[closing_1] is )) ...
                         // We have case 1. but not for example ((...)(...))
                         // remove redundant tos[t] and tos[closing] because
                         // ((...)) are not "not separating"
                         //
                         progress = true;
                         tos[t].clear(LOC);
                         tos[closing].clear(LOC);
                         continue;
                       }
                  }

               // check for case 2.
               //
               if (closing != (t + 2))   continue;


               // case 2. We have tos[t] = ( X ) ...
               //
               // (X) : "not grouping" if X is a scalar. 
               // If X is non-scalar, enclose it
               //
               progress = true;
               tos[t + 2].move(tos[t + 1], LOC);

               // we "remember" the nongrouping parantheses to disambiguate
               // e,g, SYM/xxx from (SYM)/xxx
               //
               if (tos[t + 2].get_tag() == TOK_SYMBOL)
                  tos[t + 2].ChangeTag(TOK_P_SYMB);
               tos[t + 1].clear(LOC);
               tos[t].clear(LOC);
               ++t;   // skip tos[t + 1]
             }
       }
}
//----------------------------------------------------------------------------
void
Parser::optimize_static_patterns(Token_string & tos)
{
   /* Replace:

       ∧∧, ∨∨, ⍲⍲, and ⍱⍱    with their bitwise token variant,
       ⎕FIO.function_name    with ⎕FIO[X] (via subfun_to_axis(function_name))
    */

bool TOK_VOID_inserted = false;
   loop(t, tos.size() - 1)   // -1 since we need tos[t + 1]
       {
         if (tos[t].get_tag() == TOK_OPER2_INNER    &&   // some f.g
             t                                      &&   // f exists
             tos[t-1].get_Class() == TC_FUN2        &&   // f is function
             tos[t-1].get_function()                &&   // f is valid
             tos[t-1].get_function()->has_subfuns() &&   // f is ⎕FIO or ⎕CR
             tos[t+1].get_tag() == TOK_SYMBOL)           // g is (sub-) name
            {
              const Symbol * symbol = tos[t+1].get_sym_ptr();   // subfun name
              cFunction_P fun = tos[t-1].get_function();
              const sAxis axis = fun->subfun_to_axis(symbol->get_name());
              if (axis != -1)   // subfunction is valid
                 {
                   Value_P function_axis = IntScalar(axis, LOC);
                   Token tok_axis(TOK_AXIS, function_axis);
                   tos[t++].move(tok_axis, LOC);      // replace . with [X]
                   tos[t++].clear(LOC);               // invalidate g
                   TOK_VOID_inserted = true;
                 }
              continue;
            }

         if (tos[t].get_tag() != TOK_F12_ENCODE)   continue;

         switch(tos[t+1].get_tag())
            {
              case TOK_F2_AND:
                   tos[t] = Token(TOK_F2_AND_B, &Bif_F2_AND_B::fun);
                   tos[++t].clear(LOC);
                   TOK_VOID_inserted = true;
                   continue;

              case TOK_F2_OR:
                   tos[t] = Token(TOK_F2_OR_B, &Bif_F2_OR_B::fun);
                   tos[++t].clear(LOC);
                   TOK_VOID_inserted = true;
                   continue;

              case TOK_F2_NAND:
                   tos[t] = Token(TOK_F2_NAND_B,&Bif_F2_NAND_B::fun);
                   tos[++t].clear(LOC);
                   TOK_VOID_inserted = true;
                   continue;

              case TOK_F2_NOR:
                   tos[t] = Token(TOK_F2_NOR_B, &Bif_F2_NOR_B::fun);
                   tos[++t].clear(LOC);
                   TOK_VOID_inserted = true;
                   continue;

              case TOK_F2_EQUAL:
                   tos[t] = Token(TOK_F2_EQUAL_B, &Bif_F2_EQUAL_B::fun);
                   tos[++t].clear(LOC);
                   TOK_VOID_inserted = true;
                   continue;

              case TOK_F2_UNEQU:
                   tos[t] = Token(TOK_F2_UNEQ_B, &Bif_F2_UNEQ_B::fun);
                   tos[++t].clear(LOC);
                   TOK_VOID_inserted = true;
                   continue;

              default: break;
            }
       }

   if (TOK_VOID_inserted)   remove_TOK_VOID(tos);
}
//----------------------------------------------------------------------------
bool
Parser::check_if_value(const Token_string & tos, int pos)
{
   // figure if tos[pos] (the token left of /. ⌿. \. or ⍀) is the end of a
   // function (and then return false) or the end of a value (and then
   // return true).
   //
   switch(tos[pos].get_Class())
      {
        case TC_ASSIGN:     // e.g. ←/       (actually syntax error)
        case TC_R_ARROW:    // e.g. →/       (actually syntax error)
        case TC_L_BRACK:    // e.g. [/       (actually syntax error)
        case TC_END:        // e.g.  /       (actually syntax error)
        case TC_L_PARENT:   // e.g. (/       (actually syntax error)
        case TC_VALUE:      // e.g. 5/
        case TC_RETURN:     // e.g.  /       (actually syntax error)
        case TC_OPER2:      // e.g. ./
             return true;   // tos[pos] is at the end of a value

        case TC_R_BRACK:    // e.g. +[1]/2 or 2/[1]2
             {
               const int pos1 = find_opening_bracket(tos, pos);
               if (pos1 == 0)   return true;   // this is a syntax error
               return check_if_value(tos, pos1 - 1);
             }

        case TC_R_PARENT:   // e.g. (2+3)/2 or 1 2 3 (2+3)/2
             {
               if (pos == 0)   return true;   // (actually syntax error)
               return check_if_value(tos, pos - 1);
             }

        case TC_SYMBOL:     // e.g. A/2 or FOO/2
             if (tos[pos].get_tag() == TOK_ALPHA)     return true;
             if (tos[pos].get_tag() == TOK_CHI)       return true;
             if (tos[pos].get_tag() == TOK_OMEGA)     return true;
             if (tos[pos].get_tag() == TOK_ALPHA_U)   return false;
             if (tos[pos].get_tag() == TOK_OMEGA_U)   return false;
             return (tos[pos].get_tag() == TOK_P_SYMB);   // if value

        default: break;
      }

   return false;   // tos[pos] is at the end of a function
}
//----------------------------------------------------------------------------
Assign_state
Parser::get_assign_state(Token_string & tos, ShapeItem pos)
{
   // this function is called by an optimization for static [] or [N] cases.
   // The benefit is somewhat marginal, we therefore leave the more
   // complicated cases (i.e. those involving ←) to the prefix parser
   // and return ASS_unknown instead of a more precise result.
   //
   while (pos < tos.size())
       {
         if (tos[pos++].get_Class() == TC_ASSIGN)   return ASS_unknown;
       }

   return ASS_none;
}
//----------------------------------------------------------------------------
bool
Parser::optimize_literal_axes(Token_string & tos)
{
   // stupid macOS warns about DO_FT_LITERAL_AXIS && DO_FT_LITERAL_INDEX,
   // so we split it.
   //
   if (!DO_FT_LITERAL_AXIS)    return false;
   if (!DO_FT_LITERAL_INDEX)   return false;

   // replace [ ] or [ N ] by their complete index or axis, as to relieve
   // the Prefix parser.
bool progress = false;

   loop(src, tos.size() - 1)
       {
         if (tos[src].get_tag() != TOK_L_BRACK)   continue;

         const Assign_state assign_state = get_assign_state(tos, src);
         if (assign_state == ASS_unknown)   continue;

         // case 1: [ ] 
         //
         const Token & T1 = tos[src + 1];
         if (T1.get_tag() == TOK_R_BRACK)   // empty index [ ]
            {
              IndexExpr * idx = new IndexExpr(assign_state, LOC);
              Log(LOG_delete)
                 CERR << "new    " << voidP(idx) << " at " LOC << endl;
              new (&tos[src++]) Token(TOK_INDEX, *idx);   // replace [
              new (&tos[src]) Token(TOK_VOID);            // replace ]
              OptmizationStatistics::count(OPTI_FT_LITERAL_AXIS);
              progress = true;
              continue;
            }

         /* case 2: [N]. This case has two subcases:

            2a. f[B] with (system-) function f, or
            2b. V[B] with unknown V (most likely a SYMBOL or a Value

            case 2a. is fairly frequent with f being ⎕FIO or ⎕CR.
          */
         if ((src + 2) < tos.size()          &&   // tos[src+2] exists
             tos[src + 2].get_tag() == TOK_R_BRACK &&   // and is ]
             T1.get_Class() == TC_VALUE            &&   // value N
             T1.get_apl_val()->is_int_scalar())
            {
              /* at this point we either have a function axis f[N] or else a
                 vector index V[N]. In some cases (system functions, APL
                 primitives) we can decide that here; if not then we have
                 to defer the decision to the Prefix parser.
               */
              if (src > 0 && tos[src - 1].is_function())
                 {
                   Value_P function_axis = T1.get_apl_val();
                   Token tok_axis(TOK_AXIS, function_axis);
                   new (&tos[src++]) Token(TOK_VOID);   // invalidate [
                   tos[src++].move(tok_axis, LOC);
                   new (&tos[src])   Token(TOK_VOID);   // invalidate ]
                   OptmizationStatistics::count(OPTI_FT_LITERAL_AXIS);
                   progress = true;
                 }
              else
                 {
                   IndexExpr * idx = new IndexExpr(assign_state, LOC);
                   idx->add_index(T1.get_apl_val());
                   
                   // T1 is an axis [ N ].
                   new (&tos[src++]) Token(TOK_VOID);   // invalidate [
                   new (&tos[src++]) Token(TOK_INDEX, *idx);
                   new (&tos[src])   Token(TOK_VOID);   // invalidate ]
                   OptmizationStatistics::count(OPTI_FT_LITERAL_INDEX);
                   progress = true;
                 }
            }
       }

   return progress;
}
//----------------------------------------------------------------------------
bool
Parser::optimize_short_primitives(Token_string & tos)
{
   if (DONT_FT_SHORT_PRIMITIVE)   return false;

   // replace APL primitives with short literal results and arguments,
   // such as 4⍴0 with their result. The scope of tos is one statement.
   //
bool progress = false;
   if (tos.size() < 3)   return false;   // too short to optimize

// CERR << endl << "tos: ";   tos.print(CERR, false);

   // create a list of 'terminal literals' from where the optimization
   //  may restart. For example:
   //
   //  (2⍴5) ⍴ 6
   //
   // The optimization starts at "6" (end of statement) and restarts at ")".
   //
basic_string<ShapeItem> ends;
   ends.push_back(tos.size());

   // tos is in forward (aka. APL) order. We move backwards from the end
   //
   rev_loop(pc, tos.size())
       {
         if (pc < 3)   break;   // to few tokens remaining in tos

         // at this point we have at least:
         //
         // in APL order:
         //
         // END A F B   (dyadic function call)
         // END F B     (monadic function call)
         //
         // resp. in reverse order:
         //
         // B F A END   (dyadic function call)
         // B F END     (monadic function call)
         //
         // That is A, F, and B are valid, and END must be checked
         // for the dyadic cases. Below we refer to the dyadic END as Q and
         // to the monadic END as AQ (since its position is A).
         //
         if ((1 << tos[pc].get_Class()) & TCG_R_PAR_BRA)   ends.push_back(pc);
       }

   loop(e, ends.size())
       {
         // set shortcuts for relevant positions (PCs) in tos. The rev_loop()
         // above has checked that the tokens at src_B, src_F, and src_AQ are
         // valid, while src_Q may or may not be valid (and must be checked).
         //
         const ShapeItem src_Q  =                  ends[e] - 4;
         const ShapeItem src_AQ = src_Q  + 1;   // ends[e] - 3
         const ShapeItem src_F  = src_AQ + 1;   // ends[e] - 2
         const ShapeItem src_B  = src_F  + 1;   // ends[e] - 1

         Token & tok_B = tos[src_B];
         if (tok_B.get_Class() != TC_VALUE)   continue;   // no Value B

         Token & tok_F = tos[src_F];
         if (tok_F.get_Class() != TC_FUN2)    continue;   // no function F
         cFunction_P fun = tok_F.get_function();

         // check if the function call can only be monadic and rule out strand
         // notation cases. For example (by Elias Mårtenson and verified with
         // IBM APL2):
         //
         // X←2 ◊ X 1 ⍴ 10 11
         //       │ │ │ │
         //       │ └─┴─┴──── dyadic ⍴, but a 1 ⍴ 10 11 optimization would be
         //       └────────── incorrect because X MAY bind stronger than ⍴.
         //
         const Token & tok_Q = tos[src_Q];

         const bool is_dyadic =
               tos[src_AQ].get_Class() == TC_VALUE &&        // AQ is a Value,
               src_Q >= 0                          &&        // Q is valid, and
               ! ((1 << tok_Q.get_Class()) & TCG_MAY_GLUE);  // Q is not sticky

         const bool is_monadic =
               ((1 << tos[src_AQ].get_Class()) & TCG_NO_A);   // A not a Value

         Value_P B = tok_B.get_apl_val();
         if (is_dyadic)
            {
              Token & tok_A = tos[src_AQ];
              if (fun == &Bif_F12_RHO::fun)   // dyadic A⍴B
                 {
                    // NOTE: we use Bif_F12_RHO::do_reshape() instead of
                    //       Bif_F12_RHO::eval_AB() as to bypass the A⍴B
                    //       optimization in eval_AB() (which does not
                    //       work well here)
                    //
                    const Shape sh_A(*tok_A.get_apl_val(), /* qio */ 0);
            
                    if (sh_A.fits_into(cfg_SHORT_VALUE_LENGTH_WANTED))
                       {
                         Token tZ = Bif_F12_RHO::fun.do_reshape(sh_A, *B);
                         tok_A.clear(LOC);   // set A to TOK_VOID
                         tok_F.clear(LOC);   // set F to TOK_VOID
                         tok_B.clear(LOC);   // set F to TOK_VOID
                         new (&tok_B) Token(TOK_APL_VALUE4,   // optimized
                                            tZ.get_apl_val());
                         OptmizationStatistics::count(OPTI_FT_SHORT_PRIMITIVE);
                         progress = true;
                       }
                 }
            }
         else if (is_monadic)
            {
              if (fun == &Bif_F12_COMMA::fun)            // monadic ,B
                 {
                   const Shape new_shape(B->element_count());
                   B->set_shape(new_shape);
                   tok_F.clear(LOC);   // set , to TOK_VOID
                   OptmizationStatistics::count(OPTI_FT_SHORT_PRIMITIVE);
                   progress = true;
                 }
              else if (fun == &Bif_F12_COMMA1::fun)      // monadic ⍪B
                 {
                   // 1↑⍴⍪B  ←→  1↑⍴B
                   // 1↓⍴⍪B  ←→  ×/1↓⍴B
                   //
                   const Shape & sh_B = B->get_shape();
                   const ShapeItem rows = B->get_rank() ? B->get_shape_item(0)
                                                        : 1;
                   ShapeItem low_volume = 1;   // ×/1↓⍴B
                   for (ShapeItem r = 1; r < sh_B.get_rank(); ++r)
                       low_volume *= sh_B.get_shape_item(r);
                   const Shape new_shape(rows, low_volume);
                   B->set_shape(new_shape);

                   tok_F.clear(LOC);   // set ⍪ to TOK_VOID
                   OptmizationStatistics::count(OPTI_FT_SHORT_PRIMITIVE);
                   progress = true;
                 }
              else if (fun == &Bif_F12_PARTITION::fun)   // monadic ⊂B
                 {
                   if (!B->is_simple_scalar())   // otherwise ⊂B is B
                      {
                        Value_P Z(LOC);
                        Z->next_ravel_Pointer(B.get());
                        Z->check_value(LOC);
                        tok_B.clear(LOC);   // set B to TOK_VOID
                        new (&tok_B) Token(TOK_APL_VALUE4, Z);
                      }
                   tok_F.clear(LOC);   // set F to TOK_VOID
                   OptmizationStatistics::count(OPTI_FT_SHORT_PRIMITIVE);
                   progress = true;
                 }
            }
         // otherwise the arity can not be determined statically (and will be
         // decided at runtime in Prefix.cc)
       }

   return progress;
}
//----------------------------------------------------------------------------
VoidCount
Parser::remove_TOK_VOID(Token_string & tos)
{
ShapeItem dst = 0;

   loop(src, tos.size())
       {
         if (tos[src].get_tag() == TOK_VOID)   continue;   // ignore (skip)
         if (src != dst)   tos[dst].move(tos[src], LOC);
         ++dst;
       }

const VoidCount ret = VoidCount(tos.size() - dst);
   tos.resize(dst);
   return ret;
}
//----------------------------------------------------------------------------
ErrorCode
Parser::match_par_bra(Token_string & tos, bool backwards)
{
std::basic_string<ShapeItem> stack;
   loop(s, tos.size())
       {
         const ShapeItem t = backwards ? (tos.size() - 1) - s : s;
         ErrorCode ec;   // anticipated error code, not used in most cases
         TokenClass tc_peer;
         switch(tos[t].get_Class())
           {
             // for [, (, or {, push the position onto stack.
             //
             case TC_L_BRACK:   // NOTE: also includes TOK_SEMICOL, so we check
                  if (tos[t].get_tag() != TOK_L_BRACK)   continue;
                  /* fall through */
             case TC_L_PARENT:
             case TC_L_CURLY:
                  stack.push_back(t);
                  continue;

             case TC_R_BRACK:
                  ec = E_UNBALANCED_R_BRACKET;   // for empty stack below
                  tc_peer = TC_L_BRACK;
                  break;

             case TC_R_PARENT:
                  ec = E_UNBALANCED_R_PARENT;    // for empty stack below
                  tc_peer = TC_L_PARENT;
                  break;

             case TC_R_CURLY:
                  ec = E_UNBALANCED_R_CURLY;     // for empty stack below
                  tc_peer = TC_L_CURLY;
                  break;

             default:
                  continue;
           }

          // at this point, a closing ), ], or } was detected
          //
          if (stack.size() == 0)   return ec;

           const ShapeItem t1 = stack.back();
           stack.pop_back();

          if (tos[t1].get_Class() != tc_peer)   return ec;

          const ShapeItem diff = (t > t1) ? t - t1 : t1 - t;
          tos[t].set_int_val2(diff);
          tos[t1].set_int_val2(diff);
       }

   // at this point all [ ( or { should have been matced (and therefore
   // stack shuld be empty. If not:  return syntax error of the outer token
   //
   if (stack.size())
      {
        const TokenClass outer = tos[stack[0]].get_Class();
        if (outer == TC_L_BRACK)    return E_UNBALANCED_L_BRACKET;
        if (outer == TC_L_PARENT)   return E_UNBALANCED_L_PARENT;
        if (outer == TC_L_CURLY)    return E_UNBALANCED_L_CURLY;
        FIXME;
      }

   return E_NO_ERROR;
}
//----------------------------------------------------------------------------
void
Parser::create_value(Token_string & tos, int pos, int count)
{
   Log(LOG_create_value)
      {
        CERR << "create_value(" << __LINE__ << ") tos[" << tos.size()
             <<  "]  pos " << pos << " count " << count << " in:";
        tos.print(CERR, true);
      }

   if (count == 1)   create_scalar_value(tos[pos]);
   else              create_vector_value(tos, pos, count);

   Log(LOG_create_value)
      {
        CERR << "create_value(" << __LINE__ << ") tos[" << tos.size()
             <<  "]  pos " << pos << " count " << count << " out:";
        tos.print(CERR, true);
      }
}
//----------------------------------------------------------------------------
void
Parser::create_scalar_value(Token & output)
{
   switch(output.get_tag())
      {
        case TOK_CHARACTER:
             {
               Value_P scalar(LOC);

               scalar->next_ravel_Char(output.get_char_val());
               scalar->check_value(LOC);
               Token tok(TOK_APL_VALUE3, scalar);
               output.move(tok, LOC);
             }
             return;

        case TOK_INTEGER:
             {
               Token tok(TOK_APL_VALUE3, IntScalar(output.get_int_val(), LOC));
               output.move(tok, LOC);
             }
             return;

        case TOK_REAL:
             {
               Token tok(TOK_APL_VALUE3,
                         FloatScalar(output.get_flt_val(), LOC));
               output.move(tok, LOC);
             }
             return;

        case TOK_COMPLEX:
             {
               Token tok(TOK_APL_VALUE3,
                         ComplexScalar(output.get_cpx_real(),
                                       output.get_cpx_imag(), LOC));
               output.move(tok, LOC);
             }
             return;

        case TOK_APL_VALUE1:
        case TOK_APL_VALUE3:
             return;

        default: break;
      }

   CERR << "Unexpected token " << output.get_tag() << ": " << output << endl;
   Assert(0 && "Unexpected token");
}
//----------------------------------------------------------------------------
void
Parser::create_vector_value(Token_string & tos, int pos, int count)
{
Value_P Z(count, LOC);

   loop(l, count)
       {
         Token & tok = tos[pos + l];

         switch(tok.get_tag())
            {
              case TOK_CHARACTER:
                   Z->next_ravel_Char(tok.get_char_val());
                   tok.clear(LOC);   // invalidate token
                   break;

              case TOK_INTEGER:
                   Z->next_ravel_Int(tok.get_int_val());
                   tok.clear(LOC);   // invalidate token
                   break;

              case TOK_REAL:
                   Z->next_ravel_Float(tok.get_flt_val());
                   tok.clear(LOC);   // invalidate token
                   break;

              case TOK_COMPLEX:
                   Z->next_ravel_Complex(tok.get_cpx_real(),
                                         tok.get_cpx_imag());
                   tok.clear(LOC);   // invalidate token
                   break;

            case TOK_APL_VALUE1:
            case TOK_APL_VALUE3:
            case TOK_APL_VALUE4:
                 Z->next_ravel_Value(tok.get_apl_val().get());
                 tok.clear(LOC);   // invalidate token
                 break;

              default: FIXME;
            }
       }

   Z->check_value(LOC);
Token tok(TOK_APL_VALUE3, Z);

   tos[pos].move(tok, LOC);

   Log(LOG_create_value)
      {
        CERR << "create_value [" << tos.size() << " token] out: ";
        tos.print(CERR, true);
      }
}
//----------------------------------------------------------------------------
/// in tos, (re-) mark the symbols left of ← as left symbols
void
Parser::mark_lsymb(Token_string & tos)
{
   loop(ass, tos.size())
      {
        if (tos[ass].get_Class() != TC_ASSIGN)   continue;

        // found ← in VAR VAR)← move backwards.
        // Before that we handle the special case of vector specification,
        // i.e. (SYM SYM ... SYM) ← value
        //
        if (ass >= 3 && tos[ass - 1].get_Class() == TC_R_PARENT &&
                        tos[ass - 2].get_Class() == TC_SYMBOL   &&
                        tos[ass - 3].get_Class() == TC_SYMBOL)
           {
             // first make sure that this is really a vector specification.
             //
             const int syms_to = ass - 2;
             int syms_from = ass - 3;
             bool is_vector_spec = true;
             bool is_selective_spec = false;
             for (int a1 = ass - 4; a1 >= 0; --a1)
                 {
                   const TokenClass tc = tos[a1].get_Class();
                   if (tc == TC_L_PARENT)   // end of (...)←
                      {
                        syms_from = a1 + 1;
                        break;
                      }
                   if (tc == TC_SYMBOL)     continue; 
                   if ((1 << tc) & TCG_FUN12_OPER12)
                      {
                        is_selective_spec = true;
                        is_vector_spec = false;
                        break;
                      }

                   // something else
                   //
                   is_vector_spec = false;
                   break;
                 }

             if (is_selective_spec == is_vector_spec)   // none or both
                {
                  MORE_ERROR() <<
                      "Left of )← seems to be neither a vector "
                      "specification nor a selective specvification";
                  LEFT_SYNTAX_ERROR;
                }

             // if this is a vector specification, then mark all symbols
             // inside ( ... ) as TOK_LSYMB2.
             //
             if (is_vector_spec)
                {
                  for (int a1 = syms_from; a1 <= syms_to; ++a1)
                      tos[a1].ChangeTag(TOK_LSYMB2);
                }

             continue;
           }

        int bracks = 0;
        for (int prev = ass - 1; prev >= 0; --prev)
            {
              switch(tos[prev].get_Class())
                 {
                   case TC_R_BRACK: ++bracks;     break;
                   case TC_L_BRACK: --bracks;     break;
                   case TC_SYMBOL:  if (bracks)   break;   // inside [ ... ]
                                    if (tos[prev].get_tag() == TOK_SYMBOL)
                                       {
                                         tos[prev].ChangeTag(TOK_LSYMB);
                                       }
                                    // it could be that a system variable
                                    // is assigned, e.g. ⎕SVE←0. In that case
                                    // (actually always) we simply stop here.
                                    //

                                    /* fall-through */

                   case TC_END:     prev = 0;   // stop prev loop
                                    break;

                   default: break;
                 }
            }
      }
}
//----------------------------------------------------------------------------
