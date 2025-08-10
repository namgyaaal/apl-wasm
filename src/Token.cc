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

#include "IndexExpr.hh"
#include "Output.hh"
#include "PrintOperator.hh"
#include "Symbol.hh"
#include "Token.hh"
#include "Workspace.hh"

//----------------------------------------------------------------------------
Token::Token(TokenTag t, IndexExpr & idx)
{
   /* construct a token for an index expressions. There are 3 cases which
      differ by tag and by value type:

      1. t indicates a partial index. Partial indices are used after the
         trailing ] but before the leading [ was parsed (remember that we
         parse right to left). Tag is TOK_PINDEX / TV_INDEX

      2. t indicates a complete token (after parsing [). There are 2 sub-cases:

      2a. idx has rank 1: [ axis ]. IndexExpr is converted to a (single) value,
          possibly 0 to indicate an elided index []. Tag is TOK_AXIS / TV_VAL

      2b. idx is [i1;i2...]: Tag is TOK_INDEX / TV_INDEX.

      In case 1. and 2b. the tag == t; only in case 2a. is the caller's
      t == TOK_INDEX replaced with tok == TOK_AXIS.
    */

   if (t == TOK_PINDEX)   // case 1. (partial index   ...] )
      {
        // this token is a partial index in the prefix parser.
        // use it as is.
        //
        tag = TOK_PINDEX;
        value.index_val = &idx;        // type is TV_INDEX
      }
   else                   // case 2. (complete index [ ... ] )
      {
        Assert(t == TOK_INDEX);
        if (idx.get_rank() < 2)   // case 2a. [idx] or []
           {
             tag = TOK_AXIS;
             value.apl_val = idx.extract_axis();   // type is TV_VAL
           }
        else                           // [idx; ...]
           {
             tag = TOK_INDEX;
             value.index_val = &idx;   // type is TV_INDEX
           }
      }
}
//----------------------------------------------------------------------------
ostream &
operator << (ostream & out, TokenTag tag)
{
   if (tag > 0xFFFF)   out << HEX(tag);
   else                out << HEX4(tag);

   return out;
}
//----------------------------------------------------------------------------
ostream &
operator << (ostream & out, TokenClass tc)
{
#define tcc(x) case x: out << #x;   break;
   switch(tc)
      {
        tcc(TC_INVALID)
        tcc(TC_END)
        tcc(TC_RETURN)
        tcc(TC_VALUE)
        tcc(TC_INDEX)
        tcc(TC_PINDEX)
        tcc(TC_SYMBOL)
        tcc(TC_FUN0)
        tcc(TC_FUN12)
        tcc(TC_OPER1)
        tcc(TC_OPER2)
        tcc(TC_R_PARENT)
        tcc(TC_L_PARENT)
        tcc(TC_R_BRACK)
        tcc(TC_L_BRACK)
        tcc(TC_R_ARROW)
        tcc(TC_ASSIGN)
        tcc(TC_LINE)
        tcc(TC_VOID)
        tcc(TC_DIAMOND)
        tcc(TC_NUMERIC)
        tcc(TC_SPACE)
        tcc(TC_NEWLINE)
        tcc(TC_COLON)
        tcc(TC_QUOTE)
        tcc(TC_OFF)
        tcc(TC_SI_LEAVE)

        default: break;
      }
#undef tcc

   return out;
}
//----------------------------------------------------------------------------
ostream &
operator << (ostream & out, const Token & token)
{
   if (token.get_tag() == TOK_CHARACTER)
      {
        out << "CHAR«" << token.get_char_val() << "»";
        return out;
      }

   if (token.get_tag() == TOK_APL_VALUE1 ||
       token.get_tag() == TOK_APL_VALUE2 ||
       token.get_tag() == TOK_APL_VALUE3)
      {
        if      (token.get_tag() == TOK_APL_VALUE1)   out << "VALUE1";
        else if (token.get_tag() == TOK_APL_VALUE2)   out << "VALUE2";
        else if (token.get_tag() == TOK_APL_VALUE3)   out << "VALUE3";
        else                                          out << "VALUE???";
        Value_P value = token.get_apl_val();
        Assert(+value);
        const APL_types::Depth depth = value->compute_depth();
        out << "«";
        for (APL_types::Depth d = 0; d < depth; ++d)   out << "≡";

        if (value->get_rank())   out << value->get_shape();

        const PrintContext pctx(PR_APL);
        PrintBuffer pb(*value, pctx, 0);
        bool more = pb.get_row_count() > 1;
        if (pb.get_row_count() > 0)
           {
             UCS_string ucs = pb.get_line(0).no_pad();
             if (ucs.size() > 20)
                {
                  ucs.resize(20);
                  more = true;
                }
             out << ucs;
           }
        if (more)   out << "...";
        return out << "»";
      }

   if (token.get_tag() == TOK_ERROR)
      {
        return out << Error::error_name(ErrorCode(token.get_int_val()));
      }

   if (token.get_tag() == TOK_BRANCH)
      {
        return out << token.get_Id() << token.get_int_val();
      }

   if (token.get_tag() == TOK_NOBRANCH)
      {
        return out << token.get_Id() << token.get_int_val();
      }

   if (token.get_Id() > ID_No_ID2)   return out << token.get_Id();

   switch(token.get_Class())
      {
        case TC_END:
             return out << "END";

        case TC_RETURN:
             if (token.get_tag() == TOK_RETURN_EXEC)
                return out << "RETURN ⍎";
             if (token.get_tag() == TOK_RETURN_STATS)
                return out << "RETURN ◊";
             if (token.get_tag() == TOK_RETURN_VOID)
                return out << "RETURN ∇FUN";
             if (token.get_tag() == TOK_RETURN_SYMBOL)
                return out << "RETURN Z←FUN";
             return out << "RETURN ???";

        case TC_VALUE:    return token.print_value(out);

        case TC_INDEX:
        case TC_PINDEX:
             if (token.get_tag() == TOK_INDEX)
                return out << token.get_index_val();
             if (token.get_tag() == TOK_AXIS)
                {
                  out << "[";
                  if (const Value * val = token.get_axes().get())
                     out << *val;
                  return out << "]";
                }
             FIXME;

        case TC_SYMBOL:
             if (token.get_tag() == TOK_LSYMB)
                {
                  token.get_sym_ptr()->print(out << "'");
                  out << UNI_LEFT_ARROW;
                }
             else if (token.get_tag() == TOK_LSYMB2)
                {
                  token.get_sym_ptr()->print(out << "'(... ");
                  out << ")←";
                }
             else
                {
                  token.get_sym_ptr()->print(out << "'");
                }
             return out;

        case TC_FUN0:
        case TC_FUN12:
        case TC_OPER1:
        case TC_OPER2:    return token.print_function(out);
        case TC_LINE:     return out << "\n[" << token.get_fun_line() << "] ";
        case TC_NUMERIC:  return out << "Numeric";
        case TC_NEWLINE:  return out << "{LFeed}";

        default: break;
      }

   return out <<  "{-unknown Token " << token.get_tag() << "-}";
}
//----------------------------------------------------------------------------
void
Token::ChangeTag(TokenTag new_tag)
{
   Assert((tag & TV_MASK) == (new_tag & TV_MASK));
   // tag is ia const TokenTag, so we cheat a little here.
   const_cast<TokenTag &>(tag) = new_tag;
}
//----------------------------------------------------------------------------
int
Token::value_use_count() const
{
   if (!is_apl_val())    return 0;            // token is not a TV_VALUE token
   if (!value.apl_val)   return -98;          // it is, but its Value * is -
   return value.apl_val->get_owner_count();   // non-zero Value *
}
//----------------------------------------------------------------------------
void
Token::release_apl_val(const char * loc)
{
   if (is_apl_val())   value.apl_val.reset();
}
//----------------------------------------------------------------------------
void
Token::extract_apl_val(const char * loc)
{
   if (is_apl_val())   value.apl_val.reset();
}
//----------------------------------------------------------------------------
Value *
Token::extract_and_keep(const char * loc)
{
   if (!is_apl_val())   return 0;

Value * ret = value.apl_val.get();
   value.apl_val.clear_pointer(loc);
   return ret;
}
//----------------------------------------------------------------------------
ostream &
Token::print_function(ostream & out) const
{
   switch(tag)
      {
        case TOK_Quad_TC:
        case TOK_Quad_TS:
        case TOK_Quad_UL:
        case TOK_Quad_WA:
        case TOK_Quad_AF:
        case TOK_Quad_CR:
        case TOK_Quad_DL:
        case TOK_Quad_EC:
        case TOK_Quad_EX:
        case TOK_Quad_SVQ:
        case TOK_Quad_SVR:
        case TOK_Quad_SVS:
        case TOK_Quad_UCS:
        case TOK_Quad_AT:
        case TOK_Quad_EA:
        case TOK_Quad_EB:
        case TOK_Quad_SVC:
        case TOK_Quad_TF:
        case TOK_Quad_ES:
        case TOK_Quad_FX:
        case TOK_Quad_NA:
        case TOK_Quad_NL:
        case TOK_Quad_SI:
        case TOK_Quad_SVO:      return print_quad(out);

        case TOK_F1_EXECUTE:    return out << UNI_EXECUTE;

        case TOK_F2_LESS:       return out << UNI_LESS;
        case TOK_F2_FIND:       return out << UNI_EPSILON_UBAR;
        case TOK_F2_EQUAL:      return out << UNI_EQUAL;
        case TOK_F2_GREATER:    return out << UNI_GREATER;
        case TOK_F2_AND:        return out << UNI_AND;
        case TOK_F2_AND_B:      return out << UNI_AND << UNI_AND;
        case TOK_F2_OR:         return out << UNI_OR;
        case TOK_F2_OR_B:       return out << UNI_OR << UNI_OR;
        case TOK_F2_INDEX:      return out << UNI_SQUISH_Quad;
        case TOK_F2_LEQU:       return out << UNI_LESS_OR_EQUAL;
        case TOK_F2_MEQU:       return out << UNI_MORE_OR_EQUAL;
        case TOK_F2_UNEQU:      return out << UNI_NOT_EQUAL;
        case TOK_F2_NOR:        return out << UNI_NOR;
        case TOK_F2_NOR_B:      return out << UNI_NOR << UNI_NOR;
        case TOK_F2_NAND:       return out << UNI_NAND;
        case TOK_F2_NAND_B:     return out << UNI_NAND << UNI_NAND;

        case TOK_F12_BINOM:     return out << UNI_EXCLAM;
        case TOK_F12_CIRCLE:    return out << UNI_CIRCLE;
        case TOK_F12_COMMA:     return out << UNI_COMMA;
        case TOK_F12_COMMA1:    return out << UNI_COMMA_BAR;
        case TOK_F12_DECODE:    return out << UNI_UP_TACK;
        case TOK_F12_DIVIDE:    return out << UNI_DIVIDE;
        case TOK_F12_DOMINO:    return out << UNI_Quad_DIVIDE;
        case TOK_F12_DROP:      return out << UNI_DOWN_ARROW;
        case TOK_F12_ELEMENT:   return out << UNI_ELEMENT;
        case TOK_F12_ENCODE:    return out << UNI_DOWN_TACK;
        case TOK_F12_EQUIV:     return out << UNI_EQUIVALENT;
        case TOK_F12_FORMAT:    return out << UNI_FORMAT;
        case TOK_F12_INDEX_OF:  return out << UNI_IOTA;
        case TOK_F12_LOGA:      return out << UNI_LOGARITHM;
        case TOK_F12_MINUS:     return out << UNI_MINUS;
        case TOK_F12_PARTITION: return out << UNI_SUBSET;
        case TOK_F12_PICK:      return out << UNI_SUPERSET;
        case TOK_F12_PLUS:      return out << UNI_PLUS;
        case TOK_F12_POWER:     return out << UNI_STAR_OPERATOR;
        case TOK_F12_RHO:       return out << UNI_RHO;
        case TOK_F12_RND_DN:    return out << UNI_LEFT_FLOOR;
        case TOK_F12_RND_UP:    return out << UNI_LEFT_CEILING;
        case TOK_F12_ROLL:      return out << UNI_QUESTION;
        case TOK_F12_ROTATE:    return out << UNI_CIRCLE_STILE;
        case TOK_F12_ROTATE1:   return out << UNI_CIRCLE_BAR;
        case TOK_F12_SORT_ASC:  return out << UNI_SORT_ASCENDING;
        case TOK_F12_SORT_DES:  return out << UNI_SORT_DECENDING;
        case TOK_F12_STILE:     return out << UNI_DIVIDES;
        case TOK_F12_TAKE:      return out << UNI_UP_ARROW;
        case TOK_F12_TIMES:     return out << UNI_MULTIPLY;
        case TOK_F12_TRANSPOSE: return out << UNI_TRANSPOSE;
        case TOK_F12_WITHOUT:   return out << UNI_TILDE_OPERATOR;

        case TOK_JOT:           return out << UNI_RING_OPERATOR;

        case TOK_OPER1_EACH:
        case TOK_OPER1_REDUCE:
        case TOK_OPER1_REDUCE1:
        case TOK_OPER1_SCAN:
        case TOK_OPER1_SCAN1:
        case TOK_OPER2_OUTER:
        case TOK_OPER2_INNER:   return out << get_Id();

        case TOK_FUN0:          out << "USER-F0 ";
                                get_function()->print(out);
                                return out;
        case TOK_FUN2:          out << "USER-F2 ";
                                get_function()->print(out);
                                return out;
        case TOK_OPER1:         out << "USER-OP1 ";
                                get_function()->print(out);
                                return out;
        case TOK_OPER2:         out << "USER-OP2 ";
                                get_function()->print(out);
                                return out;

        default:                break;
      }

   // unknown tag.

   out << "{ unknown function Token " << tag;

   if (get_Id() != ID_No_ID)   // tag has an Id
      {
        out << ", Id = " << get_Id();
      }
   return out <<  ", }";
}
//----------------------------------------------------------------------------
ostream &
Token::print_value(ostream & out) const
{
   switch(tag)
      {
        case TOK_VARIABLE:  return value.sym_ptr->print(out);
        case TOK_FUN0:      return out <<  "{ fun/0 }";
        case TOK_CHARACTER: return out << value.char_val;
        case TOK_INTEGER:   return out << value.int_vals[0];
        case TOK_REAL:      return out << value.float_vals[0];

        case TOK_COMPLEX:   return out << value.float_vals[0] << "J"
                                       << value.float_vals[1];

        case TOK_APL_VALUE1:
        case TOK_APL_VALUE3:
             {
               const Value * v = value.apl_val.get();
               if (v->get_rank() == 0)   out << "''";
               loop(r, v->get_rank())
                   {
                     if (r)   out <<  " ";
                     out << v->get_shape_item(r);
                   }
               out << UNI_RHO;
               loop(e, v->element_count())
                 {
                   if (e == 4)
                      {
                        out << "...";
                        break;
                      }

                   const PrintContext pctx(PR_APL, 2, 80);
                   out << UCS_string(v->get_cravel(e)
                                       .character_representation(pctx), 0, 80)
                       << " ";
                 }
               return out;
             }

        case TOK_Quad_Quad: return out << UNI_Quad_Quad;

        case TOK_Quad_AI:
        case TOK_Quad_AV:
        case TOK_Quad_CT:
        case TOK_Quad_EM:
        case TOK_Quad_ET:
        case TOK_Quad_FC:
        case TOK_Quad_IO:
        case TOK_Quad_LC:
        case TOK_Quad_LX:
        case TOK_Quad_L:
        case TOK_Quad_PP:
        case TOK_Quad_PR:
        case TOK_Quad_PS:
        case TOK_Quad_PW:
        case TOK_Quad_R:
        case TOK_Quad_RL:
        case TOK_Quad_SVE:
        case TOK_Quad_TZ:
        case TOK_Quad_WA:   return print_quad(out);

        default: break;
      }

   out << "{ unknown value Token " << tag;

   if (get_Id() != ID_No_ID)   // tag has an Id
      {
        out << ", Id = " << get_Id();
      }

   return out <<  ", }";
}
//----------------------------------------------------------------------------
void
Token::show_trace(ostream & out, const UCS_string & fun_name, 
                  Function_Line line) const
{
UCS_string fn = fun_name;
   fn.append_UTF8("[");
   fn.append_number(line);
   fn.append_UTF8("] ");

   out << fn;

   switch(get_tag())
      {
        case TOK_APL_VALUE1:
        case TOK_APL_VALUE2:
        case TOK_APL_VALUE3:
             break;   // continue below

        case TOK_BRANCH:
             out << "→" << get_int_val() << endl;
             return;

        case TOK_NOBRANCH:
             out << "→⍬" << endl;
             return;

        case TOK_ESCAPE:
             out << "→" << endl;
             return;

        case TOK_VOID:
        case TOK_NO_VALUE:
             out << endl;
             return;

        default: Q1(*this)
                 FIXME;
      }

   // print a value

PrintContext pctx = Workspace::get_PrintContext(PR_APL);
const Value & val = *get_apl_val();
   if (val.get_rank() == 0)   // scalar
      {
        pctx.set_style(PR_APL_MIN);
      }
   else if (val.get_rank() == 1)   // vector
      {
        if (val.element_count() == 0 &&   // empty vector
            val.get_cfirst().is_simple_cell())
           {
             out << endl;
             return;
           }
            
        pctx.set_style(PR_APL_MIN);
      }
   else                  // matrix or higher
      {
        pctx.set_style(PrintStyle(pctx.get_style() | PST_NO_FRACT_0));
      }

PrintBuffer pb(val, pctx, 0);
const UCS_string indent(fn.size(), UNI_SPACE);
   loop(l, pb.get_row_count())
      {
        if (l)   out << indent;
        out << pb.get_line(l).no_pad() << endl;
      }

   if (pb.get_row_count() == 0)   out << endl;
}
//----------------------------------------------------------------------------
ostream &
Token::print_quad(ostream & out) const
{
   return out << UNI_Quad_Quad << get_Id();
}
//----------------------------------------------------------------------------
UCS_string
Token::canonical(PrintStyle style) const
{
UCS_string ucs;
   switch(get_Class())
      {
        case TC_ASSIGN:
             ucs.append(UNI_LEFT_ARROW);
             break;

        case TC_R_ARROW:
             ucs.append(UNI_RIGHT_ARROW);
             break;

        case TC_L_BRACK:
             if (get_tag() == TOK_L_BRACK)
                ucs.append(UNI_L_BRACK);
             else if (get_tag() == TOK_SEMICOL)
                ucs.append(UNI_SEMICOLON);
             else
                Assert(0);
             break;

        case TC_R_BRACK:
             ucs.append(UNI_R_BRACK);
             break;

        case TC_END:
        case TC_DIAMOND:
             ucs.append(UNI_DIAMOND);
             break;

        case TC_RETURN:                                                  break;
        case TC_LINE:
             ucs.append(UNI_LF);
             break;

        case TC_VALUE:
             {
               PrintContext pctx(style, DEFAULT_Quad_PP, DEFAULT_Quad_PW);
               PrintBuffer pbuf(*get_apl_val(), pctx, 0);
               if (pbuf.get_row_count() == 0)   return ucs;
               return pbuf.l1();
             }

        case TC_SYMBOL:
             ucs.append(get_sym_ptr()->get_name());
             break;

        case TC_R_PARENT:
        case TC_L_PARENT:
        case TC_R_CURLY:
        case TC_L_CURLY:
             return ID::get_name_UCS(get_Id());

        case TC_FUN0:
        case TC_FUN12:
        case TC_OPER1:
        case TC_OPER2:
             if (get_Id() == ID_No_ID)   return get_function()->get_name();
             return ID::get_name_UCS(get_Id());


        case TC_INDEX:
             if (get_tag() == TOK_AXIS)
                {
                  UCS_string ret;
                  ret << "[";

                  // caution: get_apl_val() may be 0 (for index []).
                  //
                  if (const Value * axis = get_apl_val().get())
                     {
                      ret << ShapeItem(axis->get_cfirst().get_int_value());
                     }
                  ret << "]";
                  return ret;
                }

        default:
             CERR << "Token: " << HEX4(tag) << " " << *this
                  << " at " << LOC << endl;
             Q1(get_Class())
             BACKTRACE
             FIXME;
      }

   return ucs;
}
//----------------------------------------------------------------------------
UCS_string
Token::tag_name() const
{
   switch(get_tag())
      {
#define TD(tag, _tc, _tv, _id) case tag: return UCS_ASCII_string(#tag);
#include "Token.def"
        case TOK_NONE:   return UCS_ASCII_string("???Bad-Tag???");
      }

char cc[40];
   SPRINTF(cc, "0x%X", get_tag());
const UTF8_string utf(cc);
UCS_string ucs(utf);
   return ucs;
}
//----------------------------------------------------------------------------
int
Token::error_info(UCS_string & ucs) const
{
UCS_string canon = canonical(PR_APL_FUN).remove_pad();

const Unicode c1 = ucs.back();
const Unicode c2 = canon.size() ? canon[0] : Invalid_Unicode;

   // conditions when we don't need a space
   //
bool need_space = ! (Avec::no_space_after(c1) || Avec::no_space_before(c2));

   if (need_space)   ucs.append(UNI_SPACE);

   ucs.append(canon);
   return need_space ? -canon.size() : canon.size();
}
//----------------------------------------------------------------------------
const char * 
Token::short_class_name(TokenClass cls)
{
   switch(cls)
      {
        case TC_ASSIGN:    return "←";
        case TC_R_ARROW:   return "→";
        case TC_L_BRACK:   return "[";
        case TC_R_BRACK:   return "]";
        case TC_END:       return "END";
        case TC_FUN0:      return "F0";
        case TC_FUN12:     return "F12";
        case TC_INDEX:     return "IDX";
        case TC_OPER1:     return "OP1";
        case TC_OPER2:     return "OP2";
        case TC_L_PARENT:  return "(";
        case TC_R_PARENT:  return ")";
        case TC_RETURN:    return "RET";
        case TC_SYMBOL:    return "SYM";
        case TC_VALUE:     return "VAL";

        case TC_PINDEX:    return "PIDX";
        case TC_VOID:      return "VOID";

        case TC_OFF:       return "OFF";
        case TC_SI_LEAVE:  return "LEAVE";
        case TC_LINE:      return "LINE";
        case TC_DIAMOND:   return "◊";
        case TC_NUMERIC:   return "NUMB";
        case TC_SPACE:     return "SPACE";
        case TC_NEWLINE:   return "LF";
        case TC_COLON:     return ":";
        case TC_QUOTE:     return "QUOTE";
        case TC_L_CURLY:   return "{";
        case TC_R_CURLY:   return "}";

        case TC_INVALID:   return "INV";
        default:           break;
      }

   return "???";
}
//----------------------------------------------------------------------------
ostream &
operator << (ostream & out, const Token_string & tos)
{
   out << "[" << tos.size() << " token]: ";
   loop(t, tos.size())   CERR << "`" << tos[t] << "  ";
   out << endl;
   out << endl;
   return out;
}
//----------------------------------------------------------------------------
const char *
Token::class_name(TokenClass tc)
{
#define tcn(x) case x: return #x;

   switch(tc)
      {
        tcn( TC_ASSIGN   )
        tcn( TC_R_ARROW  )
        tcn( TC_L_BRACK  )
        tcn( TC_R_BRACK  )
        tcn( TC_END      )
        tcn( TC_FUN0     )
        tcn( TC_FUN12    )
        tcn( TC_INDEX    )
        tcn( TC_OPER1    )
        tcn( TC_OPER2    )
        tcn( TC_L_PARENT )
        tcn( TC_R_PARENT )
        tcn( TC_RETURN   )
        tcn( TC_SYMBOL   )
        tcn( TC_VALUE    )

        tcn( TC_PINDEX   )
        tcn( TC_VOID     )

        tcn( TC_OFF      )
        tcn( TC_SI_LEAVE )
        tcn( TC_LINE     )
        tcn( TC_DIAMOND  )
        tcn( TC_NUMERIC  )
        tcn( TC_SPACE    )
        tcn( TC_NEWLINE  )
        tcn( TC_COLON    )
        tcn( TC_QUOTE    )
        tcn( TC_L_CURLY  )
        tcn( TC_R_CURLY  )

        tcn( TC_INVALID  )
        default: break;
      }

   return "*** Obscure token class ***";
}
//----------------------------------------------------------------------------
void
Token_string::reverse_from_to(ShapeItem from, ShapeItem to)
{
Token * t1 = &at(from);
Token * t2 = &at(to);
   Assert(0 <= from);
   Assert(from <= to);
   Assert(to <= ShapeItem(size()));

   while (t1 < t2)   t1++->swap_token(*t2--);
}
//----------------------------------------------------------------------------
ShapeItem
Token_string::replace_segment(const Token_string & src, ShapeItem pos)
{
   loop(s, src.size())
       {
         at(pos).clear(LOC);
         new (&at(pos++)) Token(src[s], LOC);
       }
   return pos;
}
//----------------------------------------------------------------------------
void
Token_string::print(ostream & out, int details) const
{
const bool PC  = details    & 1;
const bool VAL = details & 2;

   loop(pc, size())
       {
         const Token & tok = at(pc);
         if (PC)   out << "    [PC=" << setw(2) << pc << "] ";
         out << "`" << tok;
         if (VAL)
            {
              switch(tok.get_ValueType())
                 {
                   case TV_INT: out << ":" << tok.get_int_val();   break;
                   case TV_FLT: out << ":" << tok.get_flt_val();   break;
                   default:                                        break;
                 }
            }
         if (PC)   out << endl;
         else      out << "  ";
       }

   out << endl;
}
//----------------------------------------------------------------------------
