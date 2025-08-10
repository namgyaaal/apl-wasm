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
#include "Bif_F12_FORMAT.hh"
#include "Bif_F12_INDEX_OF.hh"
#include "Bif_F12_INTERVAL_INDEX.hh"
#include "Bif_F12_PARTITION_PICK.hh"
#include "Bif_F12_SORT.hh"
#include "Bif_F12_TAKE_DROP.hh"
#include "Bif_OPER1_COMMUTE.hh"
#include "Bif_F12_DOMINO.hh"
#include "Bif_OPER1_EACH.hh"
#include "Bif_OPER2_INNER.hh"
#include "Bif_OPER2_OUTER.hh"
#include "Bif_OPER2_POWER.hh"
#include "Bif_OPER2_RANK.hh"
#include "Bif_OPER1_REDUCE.hh"
#include "Bif_OPER1_SCAN.hh"
#include "CharCell.hh"
#include "Common.hh"
#include "ComplexCell.hh"
#include "FloatCell.hh"
#include "IntCell.hh"
#include "Output.hh"
#include "PointerCell.hh"
#include "Symbol.hh"
#include "SystemLimits.hh"
#include "SystemVariable.hh"
#include "Tokenizer.hh"
#include "Value.hh"
#include "Workspace.hh"

//----------------------------------------------------------------------------
inline ostream & operator << (ostream & out, const Unicode_source & src)
   { loop(s, src.rest_len())   out << src[s];   return out; }
//----------------------------------------------------------------------------
/** convert \b UCS_string input into a Token_string tos.
*/
ErrorCode
Tokenizer::tokenize(const UCS_string & input, Token_string & tos) const
{
size_t rest_2 = 0;
   try {
         do_tokenize(input, tos, rest_2);
       }
   catch (Error err)
       {
         const int caret_1 = input.size();
         const int caret_2 = input.size() - rest_2;
         err.set_error_line_2(input, caret_1, caret_2);
         return err.get_error_code();
       }

   return E_NO_ERROR;
}
//----------------------------------------------------------------------------
/** convert \b UCS_string input into a Token_string tos. The tokenization
    stops at the end of input (= end of line), or when a comment (⍝ or #)
    is detected.
*/
void
Tokenizer::do_tokenize(const UCS_string & input, Token_string & tos,
                       size_t & rest_2) const
{
   Log(LOG_tokenize)
      CERR << "tokenize: input[" << input.size() << "] is: «"
           << input << "»" << endl;

Unicode_source src(input);
   for(;;)
      {
        rest_2 = src.rest_len();
        if (rest_2 == 0)   break;   // end of input

        Unicode uni = *src;
        if (uni == UNI_COMMENT)       break;   // ⍝ comment: discared the rest
        if (uni == UNI_NUMBER_SIGN)   break;   // # comment: discared the rest

        const Token tok = Avec::uni_to_token(uni, LOC);   // may normalize uni

        Log(LOG_tokenize)
           {
             Unicode_source s1(src, 0, 24);
             CERR << "  tokenize(" <<  src.rest_len() << " chars) sees [tag "
                  << tok.tag_name() << " «" << uni << "»] " << s1;
             if (src.rest_len() != s1.rest_len())   CERR << " ...";
             CERR << endl;
           }

        switch(tok.get_Class())
            {
              case TC_END:   // chars without APL meaning
                   rest_2 = src.rest_len();
                   {
                     Log(LOG_error_throw)
                     CERR << endl << "throwing "
                          << Error::error_name(E_NO_TOKEN)
                          << " in  Tokenizer" << endl;

                     char cc[20];
                     SPRINTF(cc, "U+%4.4X (", uni);
                     MORE_ERROR() << "Tokenizer: No token for Unicode "
                                  <<  cc << uni << ")\nInput was: " << input;
                     Error error(E_NO_TOKEN, LOC);
                     throw error;
                   }
                   break;

              case TC_RETURN:
              case TC_LINE:
              case TC_VALUE:
              case TC_INDEX:
                   CERR << "Offending token: " << tok.get_tag()
                        << " (" << tok << ")" << endl;
                   if (tok.get_tag() == TOK_CHARACTER)
                      CERR << "Unicode: " << UNI(tok.get_char_val()) << endl;
                   rest_2 = src.rest_len();
                   Error::throw_parse_error(E_NON_APL_CHAR, LOC, loc);
                   break;

              case TC_VOID:
                   // Avec::uni_to_token returns TC_VOID for non-apl characters
                   //
                   rest_2 = src.rest_len();
                   UERR << "Unknown APL character: " << uni
                        << " (" << UNI(uni) << ")" << endl;
                   Error::throw_parse_error(E_NON_APL_CHAR, LOC, loc);
                   break;

              case TC_SYMBOL:
                   if (Avec::is_quad(uni))
                      {
                         tokenize_quad(src, tos);
                      }
                   else if (uni == UNI_QUOTE_Quad)
                      {
                        ++src;
                        tos.push_back(Token(TOK_Quad_QUOTE,
                                         &Workspace::get_v_Quad_QUOTE()));
                      }
                   else if (uni == UNI_ALPHA)   // ⍺
                      {
                        ++src;
                        tos.push_back(Token(TOK_ALPHA,
                                         &Workspace::get_v_ALPHA()));
                      }
                   else if (uni == UNI_ALPHA_UNDERBAR)   // ⍶
                      {
                        ++src;
                        tos.push_back(Token(TOK_ALPHA_U,
                                            &Workspace::get_v_ALPHA_U()));
                      }
                   else if (uni == UNI_CHI)   // χ
                      {
                        ++src;
                        tos.push_back(Token(TOK_CHI,
                                            &Workspace::get_v_CHI()));
                      }
                   else if (uni == UNI_LAMBDA)   // λ
                      {
                        // this could be λ like in λ← ...
                        // or λ1 or λ2 or ... as in ... ⍺ λ1 ⍵
                        //
                        if (src.rest_len() > 1 &&
                            Avec::is_digit(src[1]))   // λn
                           {
                             tokenize_symbol(src, tos);
                           }
                        else   // λ
                           {
                             ++src;
                             tos.push_back(Token(TOK_LAMBDA,
                                           &Workspace::get_v_LAMBDA()));
                           }
                      }
                   else if (uni == UNI_OMEGA)   // ⍵
                      {
                        ++src;
                        tos.push_back(Token(TOK_OMEGA,
                                            &Workspace::get_v_OMEGA()));
                      }
                   else if (uni == UNI_OMEGA_UNDERBAR)   // ⍹
                      {
                        ++src;
                        tos.push_back(Token(TOK_OMEGA_U,
                                            &Workspace::get_v_OMEGA_U()));
                      }
                   else   // A-Z, a-z
                      {
                        tokenize_symbol(src, tos);
                      }
                   break;

              case TC_FUN0:
              case TC_FUN12:
              case TC_OPER1:
                   tokenize_function(src, tos);
                   break;

              case TC_OPER2:
                   if (tok.get_tag() == TOK_OPER2_INNER && src.rest_len())
                      {
                        // tok is a dot. This could mean that . is either
                        //
                        // the start of a number:     e.g. +.3
                        // or an operator:            e.g. +.*
                        // or a syntax error:         e.g. Done.
                        //
                        if (src.rest_len() == 1)   // syntax error
                           Error::throw_parse_error(E_SYNTAX_ERROR, LOC, loc);

                        Unicode uni_1 = src[1];
                        const Token tok_1 = Avec::uni_to_token(uni_1, LOC);
                        if ((tok_1.get_tag() & TC_MASK) == TC_NUMERIC)
                           tokenize_number(src, tos, rest_2);
                        else
                           tokenize_function(src, tos);
                      }
                   else
                      {
                        tokenize_function(src, tos);
                      }
                   if (tos.size() >= 2 &&
                       tos.back().get_tag() == TOK_OPER2_INNER &&
                       tos[tos.size() - 2].get_tag() == TOK_JOT)
                      {
                        new (&tos.back()) Token(TOK_OPER2_OUTER,
                                                &Bif_OPER2_OUTER::fun);
                      }

                   break;

              case TC_R_ARROW:   // →
                   ++src;        // skip →
                   if (src.rest_len() == 0)                      // single →
                      {
                        tos.push_back(Token(TOK_ESCAPE));
                      }
                   else if (rest_2 &&
                            *src == UNI_RIGHT_ARROW)   // double → (→→)
                      {
                        ++src;
                        tos.push_back(Token(TOK_IF_THEN, int64_t(0)));
                      }
                   else
                      {
                        tos.push_back(tok);
                      }
                   break;

              case TC_ASSIGN:    // ←
                   ++src;        // skip ←
                   if (rest_2 > 1 && *src == UNI_LEFT_ARROW)         // ←←
                      {
                        ++src;
                        tos.push_back(Token(TOK_IF_END, int64_t(0)));
                      }
                   else if (rest_2 > 1 && *src == UNI_RIGHT_ARROW)   // →→
                      {
                        ++src;
                        tos.push_back(Token(TOK_IF_ELSE, int64_t(0)));
                      }
                   else
                      {
                        const bool sym = tos.size() >= 1 &&
                                   tos[tos.size() - 1].get_tag() == TOK_SYMBOL;
                        const bool dia = tos.size() > 1 &&
                                   tos[tos.size() - 2].get_tag() == TOK_DIAMOND;
                        const bool col = tos.size() > 1 &&
                                   tos[tos.size() - 2].get_tag() == TOK_COLON;
                     
                      /* change tos.get_tag() from  TOK_R_ARROW to TOK_ASSIGN1
                         in the following cases:

                           SYM ←   (at the start of line),        or
                         ◊ SYM ←   (at the start of statement),   or
                         : SYM ←   (at the start of statement after label)

                         or else leave it as is (.
                       */
                      if (sym && ((tos.size() == 2) || dia || col))
                         {
                           tos.push_back(TOK_ASSIGN1);
                         }
                      else
                         {
                           tos.push_back(tok);
                         }
                      }
                   break;

              case TC_L_PARENT:
              case TC_R_PARENT:
              case TC_L_BRACK:
              case TC_R_BRACK:
              case TC_L_CURLY:
              case TC_R_CURLY:
                   ++src;
                   tos.push_back(tok);
                   break;

              case TC_DIAMOND:
                   ++src;
                   tos.push_back(tok);
                   break;

              case TC_COLON:
                   if (pmode != PM_FUNCTION)
                      {
                        rest_2 = src.rest_len();
                        if (pmode == PM_EXECUTE)
                           Error::throw_parse_error(E_ILLEGAL_COLON_EXEC,
                                                    LOC, loc);
                        else
                           Error::throw_parse_error(E_ILLEGAL_COLON_STAT,
                                                    LOC, loc);
                      }

                   ++src;
                   tos.push_back(tok);
                   break;

              case TC_NUMERIC:
                   tokenize_number(src, tos, rest_2);
                   break;

              case TC_SPACE:
              case TC_NEWLINE:
                   ++src;
                   break;

              case TC_QUOTE:
                   if (tok.get_tag() == TOK_QUOTE1)
                      tokenize_string1(src, tos, rest_2);
                   else
                      tokenize_string2(src, tos, rest_2);
                   break;

              default:
                   CERR << "Input: " << input << endl
                        << "uni:   " << uni << endl
                        << "Token = " << tok.get_tag() << endl;

                   if (tok.get_Id() != ID_No_ID)
                      {
                        CERR << ", Id = " << Id(tok.get_tag() >> 16);
                      }
                   CERR << endl;
                   Assert(0 && "Should not happen");
            }
      }

   Log(LOG_tokenize)
      {
        CERR << "tokenize() done (no error)." << endl
             << "   └── tos[" << tos.size() << "] is:";
        loop(t, tos.size())   CERR << " " << tos[t];
        CERR << endl;
      }
}
//----------------------------------------------------------------------------
void
Tokenizer::tokenize_function(Unicode_source & src, Token_string & tos) const
{
   Log(LOG_tokenize)   CERR << "tokenize_function(" << src << ")" << endl;

const Unicode uni = src.get();
const Token tok = tokenize_function(uni);
   tos.push_back(tok);
}
//----------------------------------------------------------------------------
Token
Tokenizer::tokenize_function(Unicode uni)
{
const Token tok = Avec::uni_to_token(uni, LOC);

#define sys(t, f) \
   case TOK_ ## t: return Token(tok.get_tag(), &Bif_ ## f::fun);   break;

   switch(tok.get_tag())
      {
        case TOK_F0_ZILDE:
             // new ⍬ style: tokenize ⍬ as constant
             //
             return Token(TOK_APL_VALUE1, Idx0(LOC));

             // old ⍬ style: tokenize ⍬ as niladic function
             //
             // return Token(tok.get_tag(), &Bif_F0_ZILDE::fun);  break;

        sys(F1_EXECUTE,    F1_EXECUTE)

        sys(F2_AND,             F2_AND)
        sys(F2_EQUAL,           F2_EQUAL)
        sys(F2_FIND,            F2_FIND)
        sys(F2_GREATER,         F2_GREATER)
        sys(F2_INDEX,           F2_INDEX)
        sys(F2_LESS,            F2_LESS)
        sys(F2_LEQU,            F2_LEQU)
        sys(F2_MEQU,            F2_MEQU)
        sys(F2_NAND,            F2_NAND)
        sys(F2_NOR,             F2_NOR)
        sys(F2_OR,              F2_OR)
        sys(F2_UNEQU,           F2_UNEQU)

        sys(F12_BINOM,          F12_BINOM)
        sys(F12_CIRCLE,         F12_CIRCLE)
        sys(F12_COMMA,          F12_COMMA)
        sys(F12_COMMA1,         F12_COMMA1)
        sys(F12_DECODE,         F12_DECODE)
        sys(F12_DIVIDE,         F12_DIVIDE)
        sys(F12_DOMINO,         F12_DOMINO)
        sys(F12_DROP,           F12_DROP)
        sys(F12_ELEMENT,        F12_ELEMENT)
        sys(F12_ENCODE,         F12_ENCODE)
        sys(F12_EQUIV,          F12_EQUIV)
        sys(F12_FORMAT,         F12_FORMAT)
        sys(F12_INDEX_OF,       F12_INDEX_OF)
        sys(F12_INTERVAL_INDEX, F12_INTERVAL_INDEX)
        sys(F2_INTER,           F2_INTER)
        sys(F2_LEFT,            F2_LEFT)
        sys(F12_LOGA,           F12_LOGA)
        sys(F12_MINUS,          F12_MINUS)
        sys(F12_NEQUIV,         F12_NEQUIV)
        sys(F12_PARTITION,      F12_PARTITION)
        sys(F12_PICK,           F12_PICK)
        sys(F12_PLUS,           F12_PLUS)
        sys(F12_POWER,          F12_POWER)
        sys(F12_RHO,            F12_RHO)
        sys(F2_RIGHT,           F2_RIGHT)
        sys(F12_RND_DN,         F12_RND_DN)
        sys(F12_RND_UP,         F12_RND_UP)
        sys(F12_ROLL,           F12_ROLL)
        sys(F12_ROTATE,         F12_ROTATE)
        sys(F12_ROTATE1,        F12_ROTATE1)
        sys(F12_SORT_ASC,       F12_SORT_ASC)
        sys(F12_SORT_DES,       F12_SORT_DES)
        sys(F12_STILE,          F12_STILE)
        sys(F12_TAKE,           F12_TAKE)
        sys(F12_TRANSPOSE,      F12_TRANSPOSE)
        sys(F12_TIMES,          F12_TIMES)
        sys(F12_UNION,          F12_UNION)
        sys(F12_WITHOUT,        F12_WITHOUT)

        sys(JOT,           JOT)

        sys(OPER1_COMMUTE, OPER1_COMMUTE)
        sys(OPER1_EACH,    OPER1_EACH)
        sys(OPER2_POWER,   OPER2_POWER)
        sys(OPER2_RANK,    OPER2_RANK)
        sys(OPER1_REDUCE,  OPER1_REDUCE)
        sys(OPER1_REDUCE1, OPER1_REDUCE1)
        sys(OPER1_SCAN,    OPER1_SCAN)
        sys(OPER1_SCAN1,   OPER1_SCAN1)

        sys(OPER2_INNER,   OPER2_INNER)

        default: break;
      }

   // CAUTION: cannot print entire token here because Avec::uni_to_token()
   // inits the token tag but not any token pointers!
   //
   CERR << endl << "Token = " << tok.get_tag() << endl;
   Assert(0 && "Missing Function");

#undef sys
   return tok;
}
//----------------------------------------------------------------------------
void
Tokenizer::tokenize_quad(Unicode_source & src, Token_string & tos) const
{
   Log(LOG_tokenize)
      CERR << "tokenize_quad(" << src.rest_len() << " chars)"<< endl;

   src.get();               // discard (possibly alternative) ⎕
UCS_string ucs(UNI_Quad_Quad);
   Assert(ucs[0]);

   if (src.rest_len() > 0)   ucs.append(src[0]);
   if (src.rest_len() > 1)   ucs.append(src[1]);
   if (src.rest_len() > 2)   ucs.append(src[2]);
   if (src.rest_len() > 3)   ucs.append(src[3]);
   if (src.rest_len() > 4)   ucs.append(src[4]);

int len = 0;
const Token t = Workspace::get_quad(ucs, len);
   src.skip(len - 1);
   tos.push_back(t);
}
//----------------------------------------------------------------------------
/** tokenize a single quoted string.
 ** If the string is a single character, then we
 **  return a TOK_CHARACTER. Otherwise we return TOK_APL_VALUE1.
 **/
void
Tokenizer::tokenize_string1(Unicode_source & src, Token_string & tos,
                            size_t & rest_2) const
{
   Log(LOG_tokenize)   CERR << "tokenize_string1(" << src << ")" << endl;

const Unicode uni = src.get();
   Assert(Avec::is_single_quote(uni));

UCS_string string_value;
bool got_end = false;

   while (src.has_more())
       {
         const Unicode uni = src.get();

         if (Avec::is_single_quote(uni))
            {
              // a single ' is the end of the string, while a double '
              // (i.e. '') is a single '. 
              //
              if ((src.rest_len() == 0) || !Avec::is_single_quote(*src))
                 {
                   got_end = true;
                   break;
                 }

              string_value.append(UNI_SINGLE_QUOTE);
              ++src;      // skip the second '
            }
         else if (uni == UNI_CR)
            {
              continue;
            }
         else if (uni == UNI_LF)
            {
              rest_2 = src.rest_len();
              Error::throw_parse_error(E_NO_STRING_END, LOC, loc);
            }
         else
            {
              string_value.append(uni);
            }
       }

   if (!got_end)   Error::throw_parse_error(E_NO_STRING_END, LOC, loc);

   if (string_value.size() == 1)   // scalar
      {
        tos.push_back(Token(TOK_CHARACTER, string_value[0]));
      }
   else
      {
        tos.push_back(Token(TOK_APL_VALUE1, Value_P(string_value, LOC)));
      }
}
//----------------------------------------------------------------------------
/** tokenize a double quoted string, i.e.  "..." or «...».
 ** If the string is a single character, then we
 **  return a TOK_CHARACTER, otherwise TOK_APL_VALUE1.
 **
 ** for special cases ««« and »»» do nothing.
 **/
void
Tokenizer::tokenize_string2(Unicode_source & src, Token_string & tos,
                            size_t & rest_2) const
{
   Log(LOG_tokenize)   CERR << "tokenize_string2(" << src << ")" << endl;

   // remember the leading ", «. or »

const Unicode first = src.get();
Unicode last = Invalid_Unicode;   // no last
   if (first == UNI_DOUBLE_QUOTE)
      {
        last = UNI_DOUBLE_QUOTE;
      }
   else if (first == UNI_LEFT_DAQ)
      {
        last = UNI_RIGHT_DAQ;
      }
   else if (first == UNI_RIGHT_DAQ)
      {
        if (src.rest_len() >= 2        &&
            src[0] == UNI_RIGHT_DAQ &&
            src[1] == UNI_RIGHT_DAQ)   // special case: «««
           {
BACKTRACE
             ++src;   ++src;   // discard second and third »
             return;
           }
      }
   else    // internal error
      {
        FIXME;
      }

UCS_string string_value;
bool got_end = false;

   while (src.has_more())
       {
         const Unicode uni = src.get();

         if (uni == last)
            {
              got_end = true;
              break;
            }

         if (uni == UNI_CR)          // ignore CR
            {
              continue;
            }
         else if (uni == UNI_LF)          // end of line before "
            {
              rest_2 = src.rest_len();
              Error::throw_parse_error(E_NO_STRING_END, LOC, loc);
            }
         else if (uni == UNI_BACKSLASH)   // backslash
            {
              const Unicode uni1 = src.get();
              switch(uni1)
                 {
                   case '0':  string_value.append(UNI_NUL);         break;
                   case 'a':  string_value.append(UNI_BEL);         break;
                   case 'b':  string_value.append(UNI_BS);          break;
                   case 't':  string_value.append(UNI_HT);          break;
                   case 'n':  string_value.append(UNI_LF);          break;
                   case 'v':  string_value.append(UNI_VT);          break;
                   case 'f':  string_value.append(UNI_FF);          break;
                   case 'r':  string_value.append(UNI_CR);          break;
                   case '[':  string_value.append(UNI_ESC);         break;
                   case '"':  string_value.append(UNI_DOUBLE_QUOTE);break;
                   case '\\': string_value.append(UNI_BACKSLASH);   break;
                   default:   string_value.append(uni);
                              string_value.append(uni1);
                 }
            }
         else
            {
              string_value.append(uni);
            }
       }

   if (!got_end)   Error::throw_parse_error(E_NO_STRING_END, LOC, loc);

   else
      {
        tos.push_back(Token(TOK_APL_VALUE1, Value_P(string_value, LOC)));
      }
}
//----------------------------------------------------------------------------
void
Tokenizer::tokenize_number(Unicode_source & src, Token_string & tos,
                           size_t & rest_2) const
{
   Log(LOG_tokenize)   CERR << "tokenize_number(" << src << ")" << endl;

   // numbers:
   // real
   // real 'J' real
   // real 'D' real   // magnitude + angle in degrees
   // real 'R' real   // magnitude + angle in radian

const Int_or_Double real_val = tokenize_real(src);
   if (!real_val.is_valid)
      {
        rest_2 = src.rest_len();
        Error::throw_parse_error(E_BAD_NUMBER, LOC, loc);
      }

   if (src.skip_if(UNI_J) || src.skip_if(UNI_j))   // e.g. 3J4
      {
        const Int_or_Double imag_val = tokenize_real(src);
        if (!imag_val.is_valid)   // not a complex number
           {
             --src;   // undo skip 'J'
             if (real_val.is_double)
                {
                  tos.push_back(Token(TOK_REAL, real_val.value.APL_flt));
                  Log(LOG_tokenize)
                     CERR << "  tokenize_number: real "
                          << real_val.value.APL_flt << endl;
                }
             else
                {
                  tos.push_back(Token(TOK_INTEGER, real_val.value.APL_int));
                  Log(LOG_tokenize)
                     CERR << "  tokenize_number: integer "
                          << real_val.value.APL_int << endl;
                }
             goto done;
           }

        tos.push_back(Token(TOK_COMPLEX, real_val.get_double(),
                                         imag_val.get_double()));
        Log(LOG_tokenize)
           CERR << "  tokenize_number: complex " << real_val.get_double()
                << "J" << imag_val.get_double() << endl;
      }
   else if (src.skip_if(UNI_D) || src.skip_if(UNI_d))   // e.g. 1D90 → 0J1
      {
        const Int_or_Double degrees = tokenize_real(src);
        if (!degrees.is_valid)   // not a complex number
           {
             --src;   // undo skip 'D'
             if (real_val.is_double)
                {
                 tos.push_back(Token(TOK_REAL, real_val.value.APL_flt));
                  Log(LOG_tokenize)
                     CERR << "  tokenize_number: real "
                          << real_val.value.APL_flt << endl;

                }
             else
                {
                  tos.push_back(Token(TOK_INTEGER, real_val.value.APL_int));
                  Log(LOG_tokenize)
                     CERR << "  tokenize_number: integer "
                          << real_val.value.APL_int << endl;
                }
             goto done;
           }

        // real_flt is the magnitude and the angle is in degrees.
        //
        const APL_Float real = real_val.get_double()
                             * cos(M_PI*degrees.get_double() / 180.0);
        const APL_Float imag = real_val.get_double()
                             * sin(M_PI*degrees.get_double() / 180.0);
        tos.push_back(Token(TOK_COMPLEX, real, imag));
        Log(LOG_tokenize)   CERR << "  tokenize_number: complex " << real
                                 << "J" << imag << endl;
      }
   else if (src.skip_if(UNI_R) || src.skip_if(UNI_r))   // 1r3.141592654 → ¯1J0
      {
        const Int_or_Double radian = tokenize_real(src);
        if (!radian.is_valid)   // not a complex number
           {
             --src;   // undo skip 'R'
             if (radian.is_double)
                {
                  tos.push_back(Token(TOK_REAL, real_val.value.APL_flt));
                  Log(LOG_tokenize)
                     CERR << "  tokenize_number: real "
                          << real_val.value.APL_flt << endl;
                }
             else
                {
                  tos.push_back(Token(TOK_INTEGER, real_val.value.APL_int));
                  Log(LOG_tokenize)
                     CERR << "  tokenize_number: integer "
                          << real_val.value.APL_int << endl;
                }
             goto done;;
           }

        // real_flt is the magnitude and the angle is in radian.
        //
        APL_Float real = real_val.get_double() * cos(radian.get_double());
        APL_Float imag = real_val.get_double() * sin(radian.get_double());
        tos.push_back(Token(TOK_COMPLEX, real, imag));
        Log(LOG_tokenize)   CERR << "  tokenize_number: complex " << real
                                 << "J" << imag << endl;
      }
   else   // neither XjY not XdY nor XrY
      {
        if (real_val.is_double)
           {
             tos.push_back(Token(TOK_REAL, real_val.value.APL_flt));
             Log(LOG_tokenize)
                CERR << "  tokenize_number: real "
                     << real_val.value.APL_flt << endl;
           }
        else
           {
             tos.push_back(Token(TOK_INTEGER, real_val.value.APL_int));
             Log(LOG_tokenize)
                CERR << "  tokenize_number: integer "
                     << real_val.value.APL_int << endl;
           }
      }

done:
   // ISO 13751 requires a space between two numeric scalar literals (page 42),
   // but not between a numeric scalar literal and an identifier.
   //
   // IBM APL2 requires a space in both cases. For example, 10Q10 is tokenized
   // as  10Q 10
   //
   // We follow ISO. The second numeric literal cannot start with 0-9 because
   // that would have been eaten by the first literal. Therefor the only cases
   // remaining to be checked are a numeric scalar literal followed by ¯
   // or by . (page 42)
   //
   if (src.has_more())
      {
        if (*src == UNI_OVERBAR || *src == '.')
           Error::throw_parse_error(E_BAD_NUMBER, LOC, loc);
      }
}
//----------------------------------------------------------------------------
Tokenizer::Int_or_Double
Tokenizer::tokenize_hex(Unicode_source & src)
{
   src.get();   // skip $
   if (!Avec::is_hex_digit(*src))   return Int_or_Double();   // no hex after $

APL_Integer hex_val = 0;
   while (src.has_more())
         {
           int digit;
           if      (*src <  UNI_0)   break;
           else if (*src <= UNI_9)   digit = src.get() - UNI_0;
           else if (*src <  UNI_A)   break;
           else if (*src <= UNI_F)   digit = 10 + src.get() - UNI_A;
           else if (*src <  UNI_a)   break;
           else if (*src <= UNI_f)   digit = 10 + src.get() - UNI_a;
           else                            break;
           hex_val = hex_val << 4 | digit;
         }

   return Int_or_Double(APL_Integer(hex_val));
}
//----------------------------------------------------------------------------

Tokenizer::Int_or_Double
Tokenizer::tokenize_real(Unicode_source & src)
{
   // hexadecimal ?
   //
   if (src.rest_len() > 1 && *src == UNI_DOLLAR_SIGN)
      return tokenize_hex(src);

enum { MAX_TOKENIZE_DIGITS = 19 };   // == atrlen("9223372036854775807")

UTF8_string int_digits;     // the digits left of . (if any)
UTF8_string fract_digits;   // the digits right of . (if any)
UTF8_string expo_digits;    // the digits  right of E (if any)
bool need_float = false;
bool mant_negative = false;   // mantissa is negative
bool expo_negative = false;   // exponent is negative
bool skipped_0 = false;     // some leading 0 in the integer part were skipped
bool dot_seen = false;      // the decimal . was seen


   /* 1. split src into integer, fractional, and exponent parts,
         thereby removing (skipping):

      1a. a leading sign of the integer part,         (sets mant_negative)
      1b. leading zeros of the integer part,
      1c. the . between the integer and fractional parts,
      1d. trailing zeros of the fractional part,
      1e. the E between the fractional and exponent parts, and/or
      1f. a sign of the exponent part                 (sets expo_negative)
    */
   if (src.has_more() && *src == UNI_OVERBAR)   // 1a.
      {
        mant_negative = true;
        ++src;
      }

   // 1b. discard leading zeros in the integer part
   //
   while (src.skip_if(UNI_0))   { skipped_0 = true; }

   // integer part
   //
   while (src.has_more() && Avec::is_digit(*src))   int_digits += src.get();

   // fractional part...
   //
   if (src.skip_if(UNI_FULLSTOP))   // fract part present
      {
        dot_seen = true;
        while (src.has_more() && Avec::is_digit(*src))
           {
             fract_digits += src.get();
           }

        while (fract_digits.size() && fract_digits.back() == UNI_0)   // 1d.
           {
             fract_digits.pop_back();
             skipped_0 = true;
           }
      }

   // APL syntax requires at least one (integer or fractional) digit
   //
   if (int_digits.size()   == 0  &&   // empty integer part, and
       fract_digits.size() == 0)      // empty fractional part
      {
        // mantisssa is empty. This is either a syntax error (by the user)
        // or caused by discarding leading 0s in int_digits. If the mantissa
        // is 0 here, then the exponent does not matter.
        //
        return skipped_0 ? Int_or_Double(APL_Integer(0))   // valid 0
                         : Int_or_Double();                // syntax error
      }

   // exponent part (but could also be a name starting with E or e)
   //
   if (src.rest_len() >= 2 &&              // at least E and a digit or ¯
       (*src == UNI_E || *src == UNI_e))   // and maybe exponent
      {
        expo_negative = (src[1] == UNI_OVERBAR);
        if (expo_negative       &&
            src.rest_len() >= 3 &&
            Avec::is_digit(src[2]))                    // E¯nnn
           {
             need_float = true;
             ++src;                        // skip e/E
             ++src;                        // skip ¯
             while (src.has_more() && Avec::is_digit(*src))
                  expo_digits += src.get();
           }
        else if (Avec::is_digit(src[1]))               // Ennn
           {
             need_float = true;
             ++src;                        // skip e/E
             while (src.has_more() && Avec::is_digit(*src))
                  expo_digits += src.get();
           }
      }

   // second dot (which id a syntax error)?
   if (src.skip_if(UNI_FULLSTOP) && dot_seen)   return Int_or_Double();

   // set expo to the optional Ennn (if any) in iii.fff.Ennn)
   //
   Log(LOG_tokenize)
      {
        Q1(int_digits.size());
        Q1(int_digits);
        Q1(fract_digits);
        Q1(expo_digits);
      }

   // construct a C string according to the APL string
   //
char buffer[int_digits.size() + fract_digits.size() + expo_digits.size() + 20];
char * b = buffer;
   if (mant_negative)   *b++ = '-';
   loop(i, int_digits.size())   *b++ = int_digits[i];

   if (int_digits.size() > MAX_TOKENIZE_DIGITS)   // very big mantissa
      {
        need_float = true;
        fract_digits.clear();   // ignore fract digits
      }
   else if (int_digits.size() == MAX_TOKENIZE_DIGITS &&
            int_digits[0] == UNI_9 &&
            int_digits[1] >= UNI_2)   // 9200000000000000000 or more
      {
        // the max. APL integer is 0x7FFFFFFFFFFFFFFF aka. 9223372036854775807
        // with 19 deciman digits. int_digits has 19  and starts with 92. The
        // 2 in 92 somewhat simplifies rounding up of aby fractional digits.
        // It thus may or may not fit into an APL integer.
        // This happens rarely so we can afford some extra effort as to figure
        // precisely whether int_digits is real or integer).
        //
        if (fract_digits.size() && fract_digits[0] >= UNI_5)   // round up
           {
             for (int d = int_digits.size();;)
                 {
                   const UTF8 digit = int_digits[--d];
                   if (digit == UNI_9)   // propagate carry
                      {
                        int_digits[d] = UNI_0;       // 9 → 0 and continue
                      }
                   else
                      {
                        int_digits[d] = digit + 1;   // ++N and stop
                        break;
                      }
                 }
           }
        fract_digits.clear();

        const UTF8_string max_int("9223372036854775807");
        loop(j, MAX_TOKENIZE_DIGITS)
            {
              if (int_digits[j] < max_int[j])   break;   // small (int is OK).
              if (int_digits[j] > max_int[j])            // float needed
                 {
                   need_float = true;
                   // float precisionm is lower than integer precision, we can
                   // therefore discard any fractional digits.
                   fract_digits.clear();   // ignore fract digits
                   break;
                 }
            }
      }

   if (fract_digits.size())
      {
         need_float = true;
         *b++ = '.';
         loop(f, fract_digits.size())   *b++ = fract_digits[f];
      }

   if (expo_digits.size())
      {
         need_float = true;
         *b++ = 'e';
         if (expo_negative)   *b++ = '-';
         loop(e, expo_digits.size())   *b++ = expo_digits[e];
      }
   *b = 0;

   if (need_float)
      {
        return Int_or_Double(APL_Float(strtod(buffer, 0)));
      }
   else
      {
        errno = 0;
        const long long  result = strtoll(buffer, 0, 10);   // may set errno
        if (errno)   // strtoll() failed (int_digits too large)
           return Int_or_Double(APL_Float(strtod(buffer, 0)));
        else
           return Int_or_Double(APL_Integer(result));
      }
}
//----------------------------------------------------------------------------
void
Tokenizer::tokenize_symbol(Unicode_source & src, Token_string & tos) const
{
   Log(LOG_tokenize)   CERR << "tokenize_symbol() : " << src.rest_len() << endl;

UCS_string symbol;
   if (macro)
      {
        symbol.append(UNI_MUE);
        symbol.append(UNI_MINUS);
      }
   symbol.append(src.get());

   while (src.has_more())
       {
         const Unicode uni = *src;
         if (!Avec::is_symbol_char(uni))   break;
         symbol.append(uni);
         ++src;
       }

   if (symbol.size() > 2 && symbol[1] == UNI_DELTA  &&
       (symbol[0] == UNI_S || symbol[0] == UNI_T))
      {
        // S∆ or T∆

        while (src.has_more() && *src <= UNI_SPACE)   src.get();   // spaces
        UCS_string symbol1(symbol, 2, symbol.size() - 2);   // without S∆/T∆
        Value_P AB(symbol1, LOC);
        cFunction_P ST = 0;
        if (symbol[0] == UNI_S) ST = &Quad_STOP::fun;
        else                    ST = &Quad_TRACE::fun;

        const bool assigned = (src.rest_len() && *src == UNI_LEFT_ARROW);
        if (assigned)   // dyadic: AB ∆fun
           {
             src.get();                                // skip ←
             Log(LOG_tokenize)
                CERR << "Stop/Trace assigned: " << symbol1 << endl;
             tos.push_back(Token(TOK_APL_VALUE1, AB));   // left argument of ST
             tos.push_back(Token(TOK_FUN2, ST));
           }
        else
           {
             Log(LOG_tokenize)
                CERR << "Stop/Trace referenced: " << symbol1 << endl;
             tos.push_back(Token(TOK_FUN2, ST));
             tos.push_back(Token(TOK_APL_VALUE1, AB));   // right arg of ST
           }

        return;
      }

Symbol * sym = Workspace::lookup_symbol(symbol);
   Assert(sym);
   tos.push_back(Token(TOK_SYMBOL, sym));
}
//----------------------------------------------------------------------------

