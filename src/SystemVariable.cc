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

#include <stdint.h>
#include <stdlib.h>
#include <sys/resource.h>   // for ⎕WA
#include <sys/time.h>

// FreeBSD 8.x lacks utmpx.h
#ifdef HAVE_UTMPX_H
# include <utmpx.h>           // for ⎕UL
#endif

#include "Bif_F12_FORMAT.hh"
#include "CDR.hh"
#include "CharCell.hh"
#include "Command.hh"
#include "Common.hh"
#include "FloatCell.hh"
#include "IndexExpr.hh"
#include "IntCell.hh"
#include "LineInput.hh"
#include "Output.hh"
#include "Parallel.hh"
#include "PointerCell.hh"
#include "Prefix.hh"
#include "ProcessorID.hh"
#include "StateIndicator.hh"
#include "SystemVariable.hh"
#include "UserFunction.hh"
#include "UserPreferences.hh"
#include "Value.hh"
#include "Workspace.hh"

#include "Workspace.icc"

UCS_string Quad_QUOTE::buffer;

ShapeItem Quad_SYL::si_depth_limit = 0;
ShapeItem Quad_SYL::value_count_limit = 0;
ShapeItem Quad_SYL::ravel_count_limit = 0;
ShapeItem Quad_SYL::print_length_limit = 0;

Unicode Quad_AV::qav[Avec::MAX_AV];

//============================================================================
void
SystemVariable::assign(Value_P B, bool clone, const char * loc)
{
   CERR << "SystemVariable::assign() not (yet) implemented for "
        << get_Id() << endl;
   FIXME;
}
//----------------------------------------------------------------------------
void
SystemVariable::assign_indexed(const Value * X, Value_P B)
{
   CERR << "SystemVariable::assign_indexed() not (yet) implemented for "
        << get_Id() << endl;
   FIXME;
}
//----------------------------------------------------------------------------
ostream &
SystemVariable::print(ostream & out) const
{
   return out << get_Id();
}
//----------------------------------------------------------------------------
void
SystemVariable::get_attributes(int mode, Value & Z) const
{
   switch(mode)
      {
        case 1: // valences (always 1 0 0 for variables)
                Z.next_ravel_1();
                Z.next_ravel_0();
                Z.next_ravel_0();
                break;

        case 2: // creation time (always 7⍴0 for variables)
                Z.next_ravel_0();
                Z.next_ravel_0();
                Z.next_ravel_0();
                Z.next_ravel_0();
                Z.next_ravel_0();
                Z.next_ravel_0();
                Z.next_ravel_0();
                break;

        case 3: // execution properties (always 4⍴0 for variables)
                Z.next_ravel_0();
                Z.next_ravel_0();
                Z.next_ravel_0();
                Z.next_ravel_0();
                break;

        case 4: {
                  Value_P val = get_apl_value();
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
//============================================================================
Quad_AV::Quad_AV()
   : RO_SystemVariable(ID_Quad_AV)
{
   Assert1(Avec::MAX_AV == 256);

Value_P AV(Avec::MAX_AV, LOC);

   // Avec::character_table is sorted by ascending Unicodes, while ⎕AV
   // is not. We construct qav (which is sorted by ⎕AV position) from
   // Avec::character_table and assign it to ⎕AV.
   //
   loop(cti, Avec::MAX_AV)
       {
         const int av_pos = Avec::get_av_pos(Avec::CHT_Index(cti));
         const Unicode uni = Avec::unicode(Avec::CHT_Index(cti));

         qav[av_pos] = uni;   // remember unicode for indexed_at()
       }

   loop(av, Avec::MAX_AV)  AV->next_ravel_Char(qav[av]);

   AV->check_value(LOC);

   Symbol::assign(AV, false, LOC);
}
//----------------------------------------------------------------------------
Unicode
Quad_AV::indexed_at(uint32_t pos)
{
   if (pos < Avec::MAX_AV)   return qav[pos];
   return UNI_AV_MAX;
}
//============================================================================
Quad_AI::Quad_AI()
   : RO_SystemVariable(ID_Quad_AI),
     session_start(now()),
     user_wait(0)
{
   Symbol::assign(get_apl_value(), false, LOC);
}
//----------------------------------------------------------------------------
Value_P
Quad_AI::get_apl_value() const
{
const int total_ms = (now() - session_start)/1000;
const int user_ms  = user_wait/1000;

Value_P Z(5, LOC);
   Z->next_ravel_Int(ProcessorID::get_own_ID());
   Z->next_ravel_Int(total_ms - user_ms);
   Z->next_ravel_Int(total_ms);
   Z->next_ravel_Int(user_ms);
   Z->next_ravel_Int(Command::get_APL_expression_count());

   Z->check_value(LOC);
   return Z;
}
//============================================================================
Quad_ARG::Quad_ARG()
   : RO_SystemVariable(ID_Quad_ARG)
{
}
//----------------------------------------------------------------------------
Value_P
Quad_ARG::get_apl_value() const
{
const int argc = UserPreferences::uprefs.expanded_argv.size();

Value_P Z(argc, LOC);
   loop(a, argc)
      {
        const char * arg = UserPreferences::uprefs.expanded_argv[a];
        UTF8_string utf(arg);
        UCS_string ucs(utf);
        Value_P sub(ucs, LOC);
        Z->next_ravel_Pointer(sub.get());
      }

   Z->check_value(LOC);
   return Z;
}
//============================================================================
Quad_CT::Quad_CT()
   : SystemVariable(ID_Quad_CT)
{
   Symbol::assign(FloatScalar(DEFAULT_Quad_CT, LOC), false, LOC);
}
//----------------------------------------------------------------------------
void
Quad_CT::assign(Value_P B, bool clone, const char * loc)
{
   if (!B->is_scalar_or_len1_vector())
      {
        if (B->get_rank() > 1)   RANK_ERROR;
        else                         LENGTH_ERROR;
      }

const Cell & cell = B->get_cfirst();
   if (!cell.is_numeric())             DOMAIN_ERROR;
   if (cell.get_imag_value() != 0.0)   DOMAIN_ERROR;

APL_Float val = cell.get_real_value();
   if (val < 0.0)     DOMAIN_ERROR;

   // APL2 "discourages" the use of ⎕CT > 1E¯9.
   // We round down to MAX_Quad_CT (= 1E¯9) instead.
   //
   if (val > MAX_Quad_CT)
      {
        Symbol::assign(FloatScalar(MAX_Quad_CT, LOC), false, LOC);
      }
   else
      {
        Symbol::assign(B, clone, LOC);
      }
}
//============================================================================
Value_P
Quad_EM::get_apl_value() const
{
const Error * err = 0;

   for (StateIndicator * si = Workspace::SI_top(); si; si = si->get_parent())
       {
         if (StateIndicator::get_error(si).get_error_code() != E_NO_ERROR)
            {
              err = &StateIndicator::get_error(si);
              break;
            }
         if (si->get_parse_mode() == PM_FUNCTION)   break;
       }

   if (err == 0)   // no SI entry with non-zero error code
      {
        // return 3 0⍴' '
        //
        Shape sh(ShapeItem(3), ShapeItem(0));
        Value_P Z(sh, LOC);
        Z->set_proto_Spc();
        Z->check_value(LOC);
        return Z;
      }

PrintBuffer pb;
   pb.append_ucs(err->get_error_line_1());
   pb.append_ucs(err->get_error_line_2());
   pb.append_ucs(err->get_error_line_3());
   pb.pad_to_spaces();   // replace pad char by spaces
Value_P Z(pb, LOC);
   Z->check_value(LOC);
   return Z;
}
//============================================================================
Value_P
Quad_ET::get_apl_value() const
{
Value_P Z(2, LOC);
ErrorCode ec = E_NO_ERROR;

   for (StateIndicator * si = Workspace::SI_top(); si; si = si->get_parent())
       {
         ec = StateIndicator::get_error(si).get_error_code();
         if (ec != E_NO_ERROR)
            {
              Z->next_ravel_Int(Error::error_major(ec));
              Z->next_ravel_Int(Error::error_minor(ec));
              goto done;
            }

         if (si->get_parse_mode() == PM_FUNCTION)   break;
       }

   Z->next_ravel_Int(Error::error_major(E_NO_ERROR));
   Z->next_ravel_Int(Error::error_minor(E_NO_ERROR));

done:
   Z->check_value(LOC);
   return Z;
}
//============================================================================
Quad_FC::Quad_FC() : SystemVariable(ID_Quad_FC)
{
Value_P Z(6, LOC);
   Z->next_ravel_Char(UNI_FULLSTOP);
   Z->next_ravel_Char(UNI_COMMA);
   Z->next_ravel_Char(UNI_STAR_OPERATOR);
   Z->next_ravel_Char(UNI_0);
   Z->next_ravel_Char(UNI_UNDERSCORE);
   Z->next_ravel_Char(UNI_OVERBAR);
   Z->check_value(LOC);

   Symbol::assign(Z, false, LOC);
}
//----------------------------------------------------------------------------
void
Quad_FC::push()
{
   Symbol::push();

Value_P QFC(6, LOC);
   QFC->next_ravel_Char(UNI_FULLSTOP);
   QFC->next_ravel_Char(UNI_COMMA);
   QFC->next_ravel_Char(UNI_STAR_OPERATOR);
   QFC->next_ravel_Char(UNI_0);
   QFC->next_ravel_Char(UNI_UNDERSCORE);
   QFC->next_ravel_Char(UNI_OVERBAR);
   QFC->check_value(LOC);

   Symbol::assign(QFC, false, LOC);
}
//----------------------------------------------------------------------------
void
Quad_FC::assign(Value_P B, bool clone, const char * loc)
{
   if (!B->is_scalar_or_vector())   RANK_ERROR;

ShapeItem value_len = B->element_count();
   if (value_len > 6)   value_len = 6;

   loop(c, value_len)
       if (!B->get_cravel(c).is_character_cell())   DOMAIN_ERROR;

   // new value is correct. 
   //
Unicode fc[6] = { UNI_FULLSTOP, UNI_COMMA,      UNI_STAR_OPERATOR,
                  UNI_0,        UNI_UNDERSCORE, UNI_OVERBAR };

   loop(c, 6)   if (c < value_len)
         fc[c] = B->get_cravel(c).get_char_value();

   // 0123456789,. are forbidden for ⎕FC[4 + ⎕IO]
   //
   if (Bif_F12_FORMAT::is_control_char(fc[4]))
      fc[4] = UNI_SPACE;

UCS_string ucs(fc, 6);
Value_P new_val(ucs, LOC);
   Symbol::assign(new_val, false, LOC);
}
//----------------------------------------------------------------------------
void
Quad_FC::assign_indexed(const IndexExpr & IX, Value_P B)
{
   if (!IX.is_axis())   INDEX_ERROR;

   // at this point we have a one dimensional index. It it were non-empty,
   // then assign_indexed(Value) would have been called instead. Therefore
   // IX must be an elided index (like in ⎕FC[] ← B)
   //
   Assert1(!IX.values[0]);
   assign(B, true, LOC);   // ⎕FC[]←value
}
//----------------------------------------------------------------------------
void
Quad_FC::assign_indexed(const Value * X, Value_P B)
{
   // we don't do scalar extension but require indices to match the value.
   //
ShapeItem ec = X->element_count();
   if (ec != B->element_count())   INDEX_ERROR;

   // ignore extra values.
   //
   if (ec > 6)   ec = 6;

const APL_Integer qio = Workspace::get_IO();
Unicode fc[6];
   {
     Value_P old = get_apl_value();
     loop(e, 6)   fc[e] = old->get_cravel(e).get_char_value();
   }

   loop(e, ec)
      {
        const APL_Integer idx = X->get_cravel(e).get_near_int() - qio;
        if (idx < 0)   continue;
        if (idx > 5)   continue;

        fc[idx] = B->get_cravel(e).get_char_value();
      }

   // 0123456789,. are forbidden for ⎕FC[4 + ⎕IO]
   //
   if (Bif_F12_FORMAT::is_control_char(fc[4]))
      fc[4] = UNI_SPACE;

UCS_string ucs(fc, 6);
Value_P new_val(ucs, LOC);
   Symbol::assign(new_val, false, LOC);
}
//============================================================================
Quad_IO::Quad_IO()
   : SystemVariable(ID_Quad_IO)
{
   Symbol::assign(IntScalar(1, LOC), false, LOC);
}
//----------------------------------------------------------------------------
void
Quad_IO::assign(Value_P B, bool clone, const char * loc)
{
   if (!B->is_scalar_or_len1_vector())
      {
        if (B->get_rank() > 1)   RANK_ERROR;
        else                         LENGTH_ERROR;
      }

   if (B->get_cfirst().get_near_bool())
      Symbol::assign(IntScalar(1, LOC), false, LOC);
   else
      Symbol::assign(IntScalar(0, LOC), false, LOC);
}
//============================================================================
Quad_L::Quad_L()
 : NL_SystemVariable(ID_Quad_L)
{
   Symbol::assign(IntScalar(0, LOC), false, LOC);
}
//----------------------------------------------------------------------------
void
Quad_L::assign(Value_P B, bool clone, const char * loc)
{
StateIndicator * si = Workspace::SI_top_fun();
   if (si == 0)   return;

   // ignore assignments if error was SYNTAX ERROR or VALUE ERROR
   //
   if (StateIndicator::get_error(si).is_syntax_or_value_error())   return;

   si->set_L(B);
}
//----------------------------------------------------------------------------
Value_P
Quad_L::get_apl_value() const
{

   if (StateIndicator * si = Workspace::SI_top_error(true))
      {
        UCS_string function;
        Value_P ret = si->get_L(function);
        if (!ret)
           {
             if (function.size())
                MORE_ERROR() << "⎕L: no left argument of function "
                             << function;
             else
                MORE_ERROR() << "⎕L: no function";
             VALUE_ERROR;
           }

        return  ret;
      }

   MORE_ERROR() << "⎕L: no )SI line with a matching error";
   VALUE_ERROR;
}
//============================================================================
Quad_LC::Quad_LC()
   : RO_SystemVariable(ID_Quad_LC)
{
   Symbol::assign(IntScalar(0, LOC), false, LOC);
}
//----------------------------------------------------------------------------
Value_P
Quad_LC::get_apl_value() const
{

   // count how many function elements Quad-LC has...
   //
int len = 0;
   for (StateIndicator * si = Workspace::SI_top();
        si; si = si->get_parent())
       {
         if (si->get_parse_mode() == PM_FUNCTION)   ++len;
       }

Value_P Z(len, LOC);

   for (StateIndicator * si = Workspace::SI_top();
        si; si = si->get_parent())
       {
         if (si->get_parse_mode() == PM_FUNCTION)
            Z->next_ravel_Int(si->get_line());
       }

   Z->check_value(LOC);
   return Z;
}
//============================================================================
Quad_LX::Quad_LX()
   : NL_SystemVariable(ID_Quad_LX)
{
   Symbol::assign(Str0(LOC), false, LOC);
}
//----------------------------------------------------------------------------
void
Quad_LX::assign(Value_P B, bool clone, const char * loc)
{
   if (B->get_rank() > 1)      RANK_ERROR;
   if (!B->is_char_string())   DOMAIN_ERROR;

   Symbol::assign(B, clone, LOC);
}
//============================================================================
Quad_PP::Quad_PP()
   : SystemVariable(ID_Quad_PP)
{
Value_P Qpp = IntScalar(DEFAULT_Quad_PP, LOC);
   Symbol::assign(Qpp, false, LOC);
}
//----------------------------------------------------------------------------
void
Quad_PP::assign(Value_P B, bool clone, const char * loc)
{
APL_Integer pp = B->get_sole_integer();
   if (pp < MIN_Quad_PP)   DOMAIN_ERROR;
   if (pp > MAX_Quad_PP)   pp = MAX_Quad_PP;

   Symbol::assign(IntScalar(pp, LOC), false, LOC);
}
//============================================================================
Quad_PR::Quad_PR()
   : SystemVariable(ID_Quad_PR)
{
   Symbol::assign(CharScalar(UNI_SPACE, LOC), false, LOC);
}
//----------------------------------------------------------------------------
void
Quad_PR::assign(Value_P B, bool clone, const char * loc)
{
UCS_string ucs = B->get_UCS_ravel();

   if (ucs.size() > 1)   LENGTH_ERROR;

   Symbol::assign(B, clone, LOC);
}
//============================================================================
Quad_PS::Quad_PS()
   : SystemVariable(ID_Quad_PS),
     print_quotients(false),
     style(Command::boxing_format)
{
Value_P Z(2, LOC);
   Z->next_ravel_Int(print_quotients);
   Z->next_ravel_Int(style);
   Z->check_value(LOC);
   Symbol::assign(Z, false, LOC);
}
//----------------------------------------------------------------------------
void
Quad_PS::assign(Value_P B, bool clone, const char * loc)
{
APL_Integer B_quot  = 0;
APL_Integer B_style = 0;

   if (B->get_rank() > 1)        RANK_ERROR;

   if (B->element_count() < 1)   LENGTH_ERROR;
   if (B->element_count() > 2)   LENGTH_ERROR;

   if (!B->get_cfirst().is_near_bool())
      {
        MORE_ERROR() << "Bad quot in ⎕PS←quot style: quot is not near bool";
        DOMAIN_ERROR;
      }

   if (B->element_count() == 1)
      {
        // for compatibility with old workspaces
        //
        B_style = B->get_cfirst().get_near_int();
      }
   else
      {
        B_quot  = B->get_cfirst().get_near_bool();
        B_style = B->get_cravel(1).get_near_int();
      }

   switch(B_style) // boxing format
      {
        case -29:
        case -25: case -24: case -23:
        case -22: case -21: case -20:
        case -9: case  -8: case  -7:
        case -4: case  -3: case  -2:
        case  0:
        case  2: case   3: case   4:
        case  7: case   8: case   9:
        case 20: case  21: case  22:
        case 23: case  24: case  25:
        case 29: break;   // OK

        default: MORE_ERROR() <<
                 "Invalid style in ⎕PS←quot style or ⎕PS←style:\n"
                 "style is not ± 0, 2-4, 7-9, 20-25, or 29";
                  DOMAIN_ERROR;
      }

   // values in B are valid
   //
   Command::boxing_format = B_style;
   print_quotients = B_quot;
   style = B_style;

Value_P B2(2, LOC);
   B2->next_ravel_Int(B_quot);
   B2->next_ravel_Int(B_style);
   B2->check_value(LOC);

   Symbol::assign(B2, false, LOC);
   return;
}
//----------------------------------------------------------------------------
void
Quad_PS::assign_indexed(const Value * X, Value_P B)
{
   if (!(X->is_int_scalar() || X->is_int_vector()))   INDEX_ERROR;
   if (!(B->is_int_scalar() || B->is_int_vector()))   DOMAIN_ERROR;
   if (X->element_count() != B->element_count())      LENGTH_ERROR;

const ShapeItem ec = X->element_count();
const APL_Integer qio = Workspace::get_IO();

APL_Integer Z_quot = print_quotients;
APL_Integer Z_style = style;
   loop(e, ec)
      {
        const APL_Integer x = X->get_cravel(e).get_near_int() - qio;
        const APL_Integer b = B->get_cravel(e).get_near_int();

        if (x == 0)   // display quotients
           {
             if (b < 0)   DOMAIN_ERROR;
             if (b > 1)   DOMAIN_ERROR;
             Z_quot = b;
           }
        else if (x == 1)   switch(b) // boxing format
           {
             case -29:
             case -25: case -24: case -23:
             case -22: case -21: case -20:
             case -9: case  -8: case  -7:
             case -4: case  -3: case  -2:
             case  0:
             case  2: case   3: case   4:
             case  7: case   8: case   9:
             case 20: case  21: case  22:
             case 23: case  24: case  25:
             case 29: Z_style = b;
                  Command::boxing_format = b;
                  break;
 
             default:
                  MORE_ERROR() <<
                       "Invalid print style (try 0, 2-4, 7-9, 20-25, or 29)";
                  DOMAIN_ERROR;
           }
        else
           {
             INDEX_ERROR;
           }
      }

Value_P Z(2, LOC);
   Z->next_ravel_Int(Z_quot);
   Z->next_ravel_Int(Z_style);
   Z->check_value(LOC);
   Symbol::assign(Z, false, LOC);
}
//============================================================================
Quad_PW::Quad_PW()
   : SystemVariable(ID_Quad_PW)
{
   Symbol::assign(IntScalar(UserPreferences::uprefs.initial_pw, LOC),
                  false, LOC);
}
//----------------------------------------------------------------------------
void
Quad_PW::assign(Value_P B, bool clone, const char * loc)
{
const APL_Integer pw = B->get_sole_integer();

   // min. ⎕PW is 30. Ignore smaller values.
   if (pw < MIN_Quad_PW)   return;

   // max val is system specific. Ignore larger values.
   if (pw > MAX_Quad_PW)   return;

   Symbol::assign(IntScalar(pw, LOC), false, LOC);
}
//============================================================================
Quad_Quad::Quad_Quad()
 : SystemVariable(ID_Quad_Quad)
{
}
//----------------------------------------------------------------------------
void
Quad_Quad::assign(Value_P B, bool clone, const char * loc)
{
   // write pending LF from  ⍞ (if any)
   Quad_QUOTE::done(true, LOC);

   B->print(COUT);
}
//----------------------------------------------------------------------------
void
Quad_Quad::resolve_right(Token & token, Function_PC & PC) const
{
   // write pending LF from  ⍞ (if any)
   Quad_QUOTE::done(true, LOC);

   COUT << UNI_Quad_Quad << ":" << endl;

bool eof = false;
UCS_string line;
   InputMux::get_line(LIM_Quad_Quad, Workspace::get_prompt(), line, eof,
                      LineHistory::quad_quad_history);

   line.remove_leading_and_trailing_whitespaces();

Token tok_exec(Bif_F1_EXECUTE::execute_statement(line));
   token.move(tok_exec, LOC);
}
//============================================================================
Quad_QUOTE::Quad_QUOTE()
 : SystemVariable(ID_QUOTE_Quad)
{
   // we assign a dummy value so that ⍞ is not undefined.
   //
Value_P dummy(UCS_ASCII_string(LOC), LOC);
   dummy->set_complete();
   Symbol::assign(dummy, false, LOC);
}
//----------------------------------------------------------------------------
void
Quad_QUOTE::done(bool with_LF, const char * loc)
{
   Log(LOG_cork)
      CERR << "Quad_QUOTE::done(" << with_LF << ") called from " << loc
           << " , buffer = [" << buffer << "]" << endl;

   if (buffer.size())
      {
        if (with_LF)   COUT << endl;
        buffer.clear();
      }
}
//----------------------------------------------------------------------------
void
Quad_QUOTE::assign(Value_P B, bool clone, const char * loc)
{
   // ⍞ ← B. At this point we cannot know whether a another ⍞ ← B will
   // follow and we therefore we don't emit a trailing \n. The trailing \n
   // may be emitted later when Quad_QUOTE::done() above is called.
   //
   Log(LOG_cork)
      CERR << "Quad_QUOTE::assign() called, buffer = ["
           << buffer << "]" << endl;

PrintContext pctx(PR_QUOTE_Quad,         // ⍞ style
                  Workspace::get_PP(),   // user's choice
                  0);                    // no APL line folding

PrintBuffer pb(*B, pctx, /* print directly to COUT*/ 0);
   if (pb.get_row_count() > 1)  // multi line output: flush and restart corking
      {
        loop(y, pb.get_row_count())
           {
             done(true, LOC);
             buffer = pb.get_line(y).no_pad();
             COUT << buffer;
           }
      }
   else if (pb.get_row_count() > 0)   // one line output
      {
        COUT << pb.l1().no_pad() << flush;
        buffer.append(pb.l1());
      }
   else                               // empty output
      {
        // nothing to do
      }

   cout << flush;

   Log(LOG_cork)
      CERR << "Quad_QUOTE::assign() done, buffer = ["
           << buffer << "]" << endl;
}
//----------------------------------------------------------------------------
Value_P
Quad_QUOTE::get_apl_value() const
{
   // Z ← ⍞.

   Log(LOG_cork)
      CERR << "Quad_QUOTE::get_apl_value() called, buffer = ["
           << buffer << "]" << endl;

   // InputMux::get_line() may call done() which calls buffer.clear().
   // We therefore save the prompt before calling InputMux::get_line().
   //
const UCS_string old_buffer = buffer.no_pad();

bool eof = false;
UCS_string line;
   InputMux::get_line(LIM_Quote_Quad, old_buffer, line, eof,
                      LineHistory::quote_quad_history);
   done(false, LOC);   // in case InputMux::get_line() has not called it

   if (interrupt_is_raised())   INTERRUPT

   if (Workspace::get_PR().size() > 0)   // valid prompt replacement char
      {
        const Unicode qpr = Workspace::get_PR()[0];
        // replace the characters in line that were not changed with ⎕PR and
        // leave the characters after the first changed one unchanged.
        loop(i, line.size())
           {
             if (i >= old_buffer.size())     break;   // end of prompt
             if (old_buffer[i] != line[i])   break;   // changed prompt[i]
             line[i] = qpr;          // unchanged prompt[i]: replace with qpr
           }
      }

Value_P Z(line, LOC);
   Z->check_value(LOC);
   return Z;
}
//============================================================================
Quad_R::Quad_R()
 : NL_SystemVariable(ID_Quad_R)
{
   Symbol::assign(IntScalar(0, LOC), false, LOC);
}
//----------------------------------------------------------------------------
void
Quad_R::assign(Value_P B, bool clone, const char * loc)
{
StateIndicator * si = Workspace::SI_top_fun();
   if (si == 0)   return;

   // ignore assignments if error was SYNTAX ERROR or VALUE ERROR
   //
   if (StateIndicator::get_error(si).is_syntax_or_value_error())   return;

   si->set_R(B);
}
//----------------------------------------------------------------------------
Value_P
Quad_R::get_apl_value() const
{

   if (StateIndicator * si = Workspace::SI_top_error(true))
      {
        UCS_string function;
        Value_P ret = si->get_R(function);
        if (!ret)
           {
             if (function.size())
                MORE_ERROR() <<"⎕R: no right argument of function "
                             << function;
             else
                MORE_ERROR() << "⎕R: no function";
             VALUE_ERROR;
           }

        return  ret;
      }

   MORE_ERROR() << "⎕R: no )SI line with a matching error";
   VALUE_ERROR;
}
//============================================================================
void
Quad_SYL::assign(Value_P B, bool clone, const char * loc)
{
   // Quad_SYL is mostly read-only, so we only allow assign_indexed() with
   // certain values.
   //
   if (+B)   SYNTAX_ERROR;

   // this assign is called from the constructor in order to trigger the
   // creation of a symbol for Quad_SYL.
   //
   Symbol::assign(IntScalar(0, LOC), false, LOC);
}
//----------------------------------------------------------------------------
void
Quad_SYL::assign_indexed(const IndexExpr & IDX, Value_P B)
{
   //  must be an array index of the form [something; 2]
   //
   if (IDX.get_rank() != 2)   INDEX_ERROR;

   // The only point of getting X2 is to check that it is not elided
   // but quasi-scalar qio + 1
   //
const Value * X2 = IDX.get_axis_value(1);

const APL_Integer qio = Workspace::get_IO();

   if (!X2)                                          INDEX_ERROR;
   if (X2->element_count() != 1)                     INDEX_ERROR;
   if (!X2->get_cfirst().is_near_int())              INDEX_ERROR;
   if (X2->get_cfirst().get_near_int() != qio + 1)   INDEX_ERROR;

const Value * X1 = IDX.get_axis_value(0);

   assign_indexed(X1, B);
}
//----------------------------------------------------------------------------
void
Quad_SYL::assign_indexed(const Value * X, Value_P B)
{
   // try to assign ⎕SYL[X;2]
   //
   if (!(X->is_int_scalar() || X->is_int_vector()))   INDEX_ERROR;
   if (!(B->is_int_scalar() || B->is_int_vector()))   DOMAIN_ERROR;
   if (X->element_count() != B->element_count())      LENGTH_ERROR;

const ShapeItem ec = X->element_count();
const APL_Integer qio = Workspace::get_IO();

   loop(e, ec)
      {
        const APL_Integer x = X->get_cravel(e).get_near_int() - qio;
        const APL_Integer b = B->get_cravel(e).get_near_int();

        if (x == SYL_SI_DEPTH_LIMIT)   // SI depth limit
           {
             const int depth = Workspace::SI_entry_count();
             if (b && (b < (depth + 4)))   DOMAIN_ERROR;    // limit too low
             si_depth_limit = b;
           }
        else if (x == SYL_VALUE_COUNT_LIMIT)   // value count limit
           {
             if (b && (b < APL_Integer(Value::value_count + 10)))    // too low
                DOMAIN_ERROR;
             value_count_limit = b;
           }
        else if (x == SYL_RAVEL_BYTES_LIMIT)   // ravel bytes limit
           {
             const int cells = b / sizeof(Cell);
             if (cells &&
                 cells < APL_Integer(Value::total_ravel_count + 1000))
                DOMAIN_ERROR;

             ravel_count_limit = cells;
           }
        else if (x == SYL_CURRENT_CORES)   // number of cores
           {
#if PARALLEL_ENABLED
             if (CPU_pool::change_core_count(CoreCount(b), false))
                {
                  MORE_ERROR() << "Bad core count " << b
                               << " in ⎕SYL[" << x << "] ";
                  DOMAIN_ERROR;
                }
#else
             MORE_ERROR() << "PARALLEL_ENABLED not set in ⎕SYL[" << x << "] ";
             DOMAIN_ERROR;
#endif
           }
        else if (x == SYL_PRINT_LIMIT)   // print length limit
           {
             if (b < 1000)   DOMAIN_ERROR;
             print_length_limit = b;
           }
        else if (x == SYL_WA_MARGIN)   // ⎕WA safety margin
           {
             if (b < 1000000)   DOMAIN_ERROR;
             Quad_WA::WA_margin = b;
           }
        else if (x == SYL_WA_SCALE)   // ⎕WA memory scale (45% - 200% %
           {
             if (b <  45)   DOMAIN_ERROR;
             if (b > 200)   DOMAIN_ERROR;
             Quad_WA::WA_scale = b;
           }
        else
           {
             MORE_ERROR() << "Bad ⎕SYL index " << x;
             INDEX_ERROR;
           }
      }
}
//----------------------------------------------------------------------------
Value_P
Quad_SYL::get_apl_value() const
{
const Shape sh(SYL_MAX, 2);
Value_P Z(sh, LOC);

#define syl2(n, e, v) syl1(n, e, v)
#define syl3(n, e, v) syl1(n, e, v)
#define syl1(n, _e, v) { const UTF8_string utf(n);              \
  Z->next_ravel_Pointer(Value_P(UCS_string(utf), LOC).get());   \
  Z->next_ravel_Int(v); }
#include "SystemLimits.def"

   Z->check_value(LOC);
   return Z;
}
//============================================================================
Quad_TC::Quad_TC()
   : RO_SystemVariable(ID_Quad_TC)
{
Value_P Z(3, LOC);
   Z->next_ravel_Char(UNI_BS);
   Z->next_ravel_Char(UNI_CR);
   Z->next_ravel_Char(UNI_LF);
   Z->check_value(LOC);

   Symbol::assign(Z, false, LOC);
}
//============================================================================
Quad_TS::Quad_TS()
   : RO_SystemVariable(ID_Quad_TS)
{
   Symbol::assign(IntScalar(0, LOC), false, LOC);
}
//----------------------------------------------------------------------------
Value_P
Quad_TS::get_apl_value() const
{
const APL_time_us offset_us = 1000000 * Workspace::get_v_Quad_TZ().get_offset();
const YMDhmsu time(now() + offset_us);

Value_P Z(7, LOC);
   Z->next_ravel_Int(time.year);
   Z->next_ravel_Int(time.month);
   Z->next_ravel_Int(time.day);
   Z->next_ravel_Int(time.hour);
   Z->next_ravel_Int(time.minute);
   Z->next_ravel_Int(time.second);
   Z->next_ravel_Int(time.micro / 1000);

   Z->check_value(LOC);
   return Z;
}
//============================================================================
Quad_TZ::Quad_TZ()
   : SystemVariable(ID_Quad_TZ)
{
   offset_seconds = compute_offset();

   // ⎕TZ is the offset in hours between GMT and local time
   //
   if (offset_seconds % 3600 == 0)   // full hour
      Symbol::assign(IntScalar(offset_seconds/3600, LOC), false, LOC);
   else
      Symbol::assign(FloatScalar(offset_seconds/3600, LOC), false, LOC);
}
//----------------------------------------------------------------------------
int
Quad_TZ::compute_offset()
{
   // the GNU/Linux timezone variable conflicts with function timezone() on
   // other systems. timezone.tz_minuteswest in gettimeofday() does not
   // sound reliable either.
   // 
   // We compute localtime() and gmtime() and take the difference (hoping
   // that localtime() and gmtime() are more portable.
   // 

   // choose a reference point in time.
   //
const time_t ref = now()/1000000;

tm * local = localtime(&ref);
const int local_minutes = local->tm_min + 60*local->tm_hour;
const int local_days = local->tm_mday + 31*local->tm_mon + 12*31*local->tm_year;

tm * gmean = gmtime(&ref);
const int gm_minutes    = gmean->tm_min + 60*gmean->tm_hour;
const int gm_days = gmean->tm_mday + 31*gmean->tm_mon + 12*31*gmean->tm_year;
int diff_minutes = local_minutes - gm_minutes;

   if (local_days < gm_days)        diff_minutes -= 24*60;   // local < gmean
   else if (local_days > gm_days)   diff_minutes += 24*60;   // local > gmean

   return 60*diff_minutes;
}
//----------------------------------------------------------------------------
ostream &
Quad_TZ::print_timestamp(ostream & out, APL_time_us when) const
{
const APL_time_us offset = get_offset();
const YMDhmsu time(when + 1000000*offset);

char gmt[40];
   SPRINTF(gmt, "(GMT%+2d)", int(offset/3600));

ostringstream os;
   os << setfill('0') << time.year  << "-"
      << setw(2)      << time.month << "-"
      << setw(2)      << time.day   << "  "
      << setw(2)      << time.hour  << ":"
      << setw(2)      << time.minute << ":"
      << setw(2)      << time.second << " "
      << setfill(' ') << left << setw(8) << gmt;

   return out << os.str();
}
//----------------------------------------------------------------------------
void
Quad_TZ::assign(Value_P B, bool clone, const char * loc)
{
   if (!B->is_scalar())   RANK_ERROR;

   // ignore values outside [-12 ... 14], DOMAIN ERROR for bad types.

const Cell & cell = B->get_cfirst();
   if (cell.is_integer_cell())
      {
        const APL_Integer ival = cell.get_near_int();
        if (ival < -12)   return;
        if (ival > 14)    return;
        offset_seconds = ival*3600;
        Symbol::assign(B, clone, LOC);
        return;
      }

   if (cell.is_float_cell())
      {
        const double hours = cell.get_real_value();
        if (hours < -12.1)   return;
        if (hours > 14.1)    return;
        offset_seconds = int(0.5 + hours*3600);
        Symbol::assign(B, clone, LOC);
        return;
      }

   DOMAIN_ERROR;
}
//============================================================================
Quad_UL::Quad_UL()
   : RO_SystemVariable(ID_Quad_UL)
{
   Symbol::assign(get_apl_value(), false, LOC);
}
//----------------------------------------------------------------------------
Value_P
Quad_UL::get_apl_value() const
{
int user_count = 0;

#ifdef USER_PROCESS
   setutxent();   // rewind utmp file

   for (;;)
       {
         utmpx * entry = getutxent();
         if (entry == 0)   break;   // end of  user accounting database

         if (entry->ut_type == USER_PROCESS)   ++user_count;
       }

   endutxent();   // close utmp file
#else
   user_count = 1;
#endif

Value_P Z(LOC);
   Z->next_ravel_Int(user_count);
   Z->check_value(LOC);
   return Z;
}
//============================================================================
Quad_X::Quad_X()
 : NL_SystemVariable(ID_Quad_X)
{
   Symbol::assign(IntScalar(0, LOC), false, LOC);
}
//----------------------------------------------------------------------------
void
Quad_X::assign(Value_P B, bool clone, const char * loc)
{
StateIndicator * si = Workspace::SI_top_fun();
   if (si == 0)   return;

   // ignore assignments if error was SYNTAX ERROR or VALUE ERROR
   //
   if (StateIndicator::get_error(si).is_syntax_or_value_error())   return;

   si->set_X(B);
}
//----------------------------------------------------------------------------
Value_P
Quad_X::get_apl_value() const
{
   if (StateIndicator * si = Workspace::SI_top_error(true))
      {
        UCS_string function;
        Value_P ret = si->get_X(function);
        if (!ret)
           {
             if (function.size())
                MORE_ERROR() << "⎕X: no axis argument of function "
                             << function;
             else
                MORE_ERROR() << "⎕X: no function";
             VALUE_ERROR;
           }

        return ret;
      }

   MORE_ERROR() << "⎕X: no )SI line with a matching error";
   VALUE_ERROR;
}
//============================================================================
