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

#include <fcntl.h>
#include <errno.h>
#include <iostream>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "Archive.hh"
#include "Bif_F12_COMMA.hh"
#include "Bif_F12_DOMINO.hh"
#include "Bif_F12_FORMAT.hh"
#include "Bif_F12_INDEX_OF.hh"
#include "Bif_F12_PARTITION_PICK.hh"
#include "Bif_F12_SORT.hh"
#include "Bif_F12_TAKE_DROP.hh"
#include "Bif_OPER1_COMMUTE.hh"
#include "Bif_OPER1_EACH.hh"
#include "Bif_OPER1_REDUCE.hh"
#include "Bif_OPER1_SCAN.hh"
#include "Bif_OPER2_INNER.hh"
#include "Bif_OPER2_OUTER.hh"
#include "Bif_OPER2_POWER.hh"
#include "Bif_OPER2_RANK.hh"
#include "Common.hh"
#include "Command.hh"
#include "CharCell.hh"
#include "ComplexCell.hh"
#include "Executable.hh"
#include "FloatCell.hh"
#include "Function.hh"
#include "Heapsort.hh"
#include "IndexExpr.hh"
#include "IntCell.hh"
#include "LvalCell.hh"
#include "Macro.hh"
#include "NativeFunction.hh"
#include "Output.hh"
#include "PointerCell.hh"
#include "PrintOperator.hh"
#include "PrimitiveFunction.hh"
#include "Quad_FFT.hh"
#include "Quad_FX.hh"
#include "Quad_JSON.hh"
#include "Quad_MAP.hh"
#include "Quad_PLOT.hh"
#include "Quad_PNG.hh"
#include "Quad_RVAL.hh"
#include "Quad_TF.hh"
#include "Quad_XML.hh"
#include "StateIndicator.hh"
#include "Symbol.hh"
#include "Token.hh"
#include "UCS_string.hh"
#include "UserFunction.hh"
#include "Value.hh"
#include "ValueHistory.hh"
#include "Workspace.hh"

#include "Workspace.icc"

using namespace std;

/// if there is less than x chars left on the current line then leave
/// char mode and start a new line indented.
#define NEED(x)   if (space < int(x)) \
   { leave_char_mode();   out << "\n";   space = do_indent(); } out

//----------------------------------------------------------------------------
bool
XML_Saving_Archive::xml_allowed(Unicode uni)
{
   if (uni < ' ')    return false;    // control chars and negative
   if (uni == '<')   return false;   // < not allowed
   if (uni == '&')   return false;   // < not allowed
   if (uni == '"')   return false;   // not allowed in "..."
   if (uni > '~')
      {
        // non-ASCII character. This is, in principle, allowed in XML but
        // may not print properly if the font used does not provide the
        // character. We therefore allow characters in ⎕AV and their alternate
        // characters except our own type markers (⁰¹²...)
        //
        if (is_iPAD_char(uni))   return false;
        if (Avec::find_char(uni) != Avec::Invalid_CHT)              return true;
        if (Avec::map_alternative_char(uni) != Avec::Invalid_CHT)   return true;
        return false;    // allowed, but may
      }
   return true;
}
//----------------------------------------------------------------------------
const char *
XML_Saving_Archive::decr(int & counter, const char * str)
{
   counter -= strlen(str);
   return str;
}
//----------------------------------------------------------------------------
int
XML_Saving_Archive::do_indent()
{
const int spaces = indent * INDENT_LEN;
   loop(s, spaces)   out << " ";

   return 72 - spaces;
}
//----------------------------------------------------------------------------
XML_Saving_Archive::Vid
XML_Saving_Archive::find_vid(const Value * val)
{
const void * item = bsearch(val, val_pars, value_count, sizeof(_val_par),
                      _val_par::compare_val_par1);
   if (item == 0)   return INVALID_VID;
   return Vid(reinterpret_cast<const _val_par *>(item) - val_pars);
}
//----------------------------------------------------------------------------
void
XML_Saving_Archive::emit_unicode(Unicode uni, int & space)
{
   if (uni == UNI_LF)
      {
        leave_char_mode();
        out << UNI_PAD_U1 << "A" << "\n";
        space = do_indent();
      }
   else if (!xml_allowed(uni))
      {
        space -= leave_char_mode();
        char cc[40];
        SPRINTF(cc, "%X", uni);
        NEED(1 + strlen(cc)) << UNI_PAD_U1 << decr(space, cc);
        space--;   // PAD_U1
      }
   else 
      {
        NEED(1) << "";
        space -= enter_char_mode();
        out << uni;
        space--;   // uni
      }
}
//----------------------------------------------------------------------------
void
XML_Saving_Archive::save_UCS(const UCS_string & ucs)
{
int space = do_indent();
   out << decr(space, "<UCS uni=\"");
   Assert(char_mode == false);
   indent += 5;
   loop(u, ucs.size())   emit_unicode(ucs[u], space);
   leave_char_mode();
   out << "\"/>" << endl;
   space -= 2;
   indent -= 5;
}
//----------------------------------------------------------------------------
XML_Saving_Archive &
XML_Saving_Archive::save_shape(Vid vid)
{
const Value & v = *val_pars[vid]._val;
const Vid parent_vid = val_pars[vid]._par;

   do_indent();
   out << "<Value flg=\"" << HEX(v.get_flags()) << "\" "
                 "vid=\"" << vid                << "\" "
                 "parent=\"" << parent_vid      << "\" "
                 "rk=\"" << v.get_rank()        << "\"";

   loop (r, v.get_rank())
      {
        out << " sh-" << r << "=\"" << v.get_shape_item(r) << "\"";
      }

   out << "/>" << endl;
   return *this;
}
//----------------------------------------------------------------------------
XML_Saving_Archive &
XML_Saving_Archive::save_Ravel(Vid vid)
{
   Log(LOG_archive)   CERR << "save_Ravel(Vid " << vid << ")" << endl;

const Value & v = *val_pars[vid]._val;
const ShapeItem len = v.nz_element_count();
const Cell * C = &v.get_cfirst();

int space = do_indent();
   if (v.is_packed())   // Value has a packed (non-Cell) ravel
      {
        const uint8_t * bytes = reinterpret_cast<const uint8_t *>(C);
        const ShapeItem byte_count = (len + 7)/8;

        // print the start of the XML element
        {
          char cc[80];   // output line buffer
          SPRINTF(cc, "<Ravel vid=\"%d\" bytes=\"", vid);
          out << cc;
        }

        // print the data of the bytes attribute
        ++indent;
        out << uhex << UNI_PAD_U9;   // emit the following bytes in hex
        if (space < byte_count)   out << endl;

        loop(b, byte_count)   // print with max. 64 bytes per line
            {
              if (b && (b & 0x3F) == 0)   out << "\n    " << UNI_PAD_U9;
              out << (bytes[b] & 0xFF);
            }
        out << nohex << "\"/>" << endl;
        --indent;
      }
   else
      {
        // print the start of the XML element
        {
          char cc[80];   // output line buffer
          SPRINTF(cc, "<Ravel vid=\"%d\" cells=\"", vid);
          out << decr(space, cc);
        }

        // print the data of the 'cells' attribute
        ++indent;
        loop(l, len)   emit_cell(*C++, space);

        space -= leave_char_mode();
        space -= 2;

        out << "\"/>" << endl;
        --indent;
      }

   return *this;
}
//----------------------------------------------------------------------------
void
XML_Saving_Archive::emit_cell(const Cell & cell, int & space)
{
char cc[80];
   switch(cell.get_cell_type())
      {
        case CT_CHAR:   // uses UNI_PAD_U0, UNI_PAD_U1, and UNI_PAD_U2
             emit_unicode(cell.get_char_value(), space);
             break;

        case CT_INT:   // uses UNI_PAD_U3
             space -= leave_char_mode();
             SPRINTF(cc, "%lld", static_cast<long long>(cell.get_int_value()));
             NEED(1 + strlen(cc)) << UNI_PAD_U3 << decr(--space, cc);
             break;

        case CT_FLOAT:   // uses UNI_PAD_U4 or UNI_PAD_U8
             space -= leave_char_mode();
#ifdef cfg_RATIONAL_NUMBERS_WANTED
             {
               const FloatCell & flt =
                                 reinterpret_cast<const FloatCell &>(cell);
               if (const APL_Integer denom = flt.get_denominator())
                  {
                    // a non-zero denominator indicates a rational quotient)
                    //
                    const APL_Integer numer = flt.get_numerator();
                    SPRINTF(cc, "%lld÷%lld", static_cast< long long>(numer),
                                             static_cast<long long>(denom));
                    NEED(1 + strlen(cc)) << UNI_PAD_U8 << decr(--space, cc);
                    break;
                  }
             }
#endif
             SPRINTF(cc, "%.17g", double(cell.get_real_value()));
             NEED(1 + strlen(cc)) << UNI_PAD_U4 << decr(--space, cc);
             break;

        case CT_COMPLEX:   // uses UNI_PAD_U5
             space -= leave_char_mode();
             SPRINTF(cc, "%17gJ%17g", double(cell.get_real_value()),                                                  double(cell.get_imag_value()));
             NEED(1 + strlen(cc)) << UNI_PAD_U5 << decr(--space, cc);
             break;

        case CT_POINTER:   // uses UNI_PAD_U6
             space -= leave_char_mode();
             {
               const Vid vid = find_vid(cell.get_pointer_value().get());
               SPRINTF(cc, "%d", vid);
               NEED(1 + strlen(cc)) << UNI_PAD_U6 << decr(--space, cc);
             }
             break;

        case CT_CELLREF:   // uses UNI_PAD_U7
             {
             space -= leave_char_mode();
             const LvalCell & lv = reinterpret_cast<const LvalCell &>(cell);
             if (lv.get_lval_value())   // lv has a valid target
                {
                  const Value * owner = lv.get_cell_owner();
                  const long long offset = owner->get_offset(&lv);
                  const Vid vid = find_vid(owner);
                  SPRINTF(cc, "%d[%lld]", vid, offset);
                  NEED(1 + strlen(cc)) << UNI_PAD_U7 << decr(--space, cc);
                }
             else     // 0-cell-pointer (from selective assignment)
                {
                  SPRINTF(cc, "0");
                  NEED(2) << UNI_PAD_U7 << "0" << decr(--space, cc);
                }
             }
             break;

        default: Assert(0);
      }
}
//----------------------------------------------------------------------------
void
XML_Saving_Archive::save_functions()
{
   // save build-in functions
   //
   do_indent();
   out << "<!-- system functions and primitives... -->" << endl;
#define sf_def(x, _str, _txt)   save_Function(x::fun);
#define pf_def(x, _str, _txt)   save_Function(x::fun);
#include "SystemVariable.def"

   out << endl;
}
//----------------------------------------------------------------------------
void
XML_Saving_Archive::save_Function(const Function & fun)
{
   do_indent();
   out << "<Function fid=\"" << HEX(&fun) << "\"";

   if (is_saved(&fun))
      {
         out << " ref=\"true\"/>"    << endl;
      }
   else if (fun.is_macro())
      {
        const UserFunction * ufun = fun.get_func_ufun();
        Assert(ufun);

        out << " macro=\"" << ufun->get_macnum() << "\"/>" << endl;
      }
   else if (fun.is_derived())
      {
        CERR << endl <<
"WARNING: The )SI stack contains a derived function. )SAVEing a workspace in\n"
"         such a state is currently not supported and WILL cause problems\n"
"         when )LOADing the workspace. Please perform )SIC (or →) and then\n"
"         )SAVE this workspace again.\n"
"\n"
"         As a precaution for loosing data or function definitions, please\n"
"         also )DUMP this workspace (before )SIC or →).\n"
             << endl;

        const DerivedFunction & dfn = static_cast<const DerivedFunction &>(fun);
        out << " LO-fid=\"" << HEX(dfn.get_LO())     << "\""
               " OPER-fid=\"" << HEX(dfn.get_OPER()) << "\"";

        const Function * ro = dfn.get_RO();
        const Value * axis = dfn.get_AXIS();
        if (ro || axis)
           {
             out << endl;
             do_indent();
             out << "        ";
           }
        if (ro)     out << " RO-fid=\""   << HEX(ro) << "\"";
        if (axis)   out << " AXIS-vid=\"" << HEX(find_vid(axis)) << "\"";

        out << "/>" << endl;
      }
   else if (fun.is_defined())   // defined APL function
      {
        const int * eprops = fun.get_exec_properties();
        const APL_time_us creation_time = fun.get_creation_time();

        out << " creation-time=\"" << creation_time << "\"" << endl
            << "                exec-properties=\""
            << eprops[0] << "," << eprops[1] << ","
            << eprops[2] << "," << eprops[3] << "\"";

        if (fun.is_native())   out << " native=\"1\"";
        out << ">" << endl;
        ++indent;

        save_UCS(fun.canonical(false));

        --indent;
        do_indent();
        out << "</Function>" << endl;
        saved_Functions.push_back(&fun);
      }
   else // primitive APL function
      {
        out << " tag=\"" << fun.get_tag()  << "\""
               "/>   <!-- primitive: " << ID::get_name_UCS(fun.get_Id())
            << " -->" << endl;
      }
}
//----------------------------------------------------------------------------
bool
XML_Saving_Archive::is_saved(const Function * fun) const
{
   loop(f, saved_Functions.size())
       if (fun == saved_Functions[f])   return true;

   return false;
}
//----------------------------------------------------------------------------
int
XML_Saving_Archive::save_Function_name(const Function & fun)
{
   if (fun.is_derived())
      {
        CERR << endl <<
"WARNING: The )SI stack contains a derived function. )SAVEing a workspace in\n"
"         such a state is currently not supported and WILL cause problems\n"
"         when )LOADing the workspace. Please perform )SIC (or →) and then\n"
"         )SAVE this workspace again.\n"
"\n"
"         As an alternative (and to be on the safe side), you should also\n"
"         consider to )DUMP this workspace.\n"
             << endl;
      }

const UserFunction * ufun = fun.get_func_ufun();
   if (ufun)   // user defined function
      {
        const UCS_string & fname = ufun->get_name();
        Symbol * sym = Workspace::lookup_symbol(fname);
        Assert(sym);
        const int sym_depth = sym->get_exec_ufun_depth(ufun);
        out << " ufun-name=\""  << fname     << "\""
            << " symbol-level=\"" << sym_depth << "\"";
        return 2;   // two attributes
      }
   else        // primitive or quad function
      {
        out << " fun-id=\"" << HEX(fun.get_Id());
        return 1;   // one attribute
      }
}

//----------------------------------------------------------------------------
void
XML_Saving_Archive::save_Parser(const StateIndicator & si)
{
const Prefix & prefix = si.current_stack;

   do_indent();
    out << "<Parser size=\""      << prefix.size()
        << "\" assign-pending=\"" << prefix.get_assign_state()
        << "\" action=\""         << prefix.action
        << "\" lookahead-high=\"" << prefix.get_lookahead_PC()
        << "\">" << endl;

   ++indent;

   // write the derived functions cache...
   //
   save_Derived(si.fun_oper_cache);

   // write the lookahead token, starting at the fifo's get position
   //
   loop(s, prefix.size())
      {
        const Token_loc & tloc = prefix.at(prefix.size() - s - 1);
        save_token_loc(tloc);
      }

   // write the remaining token
   //
   save_token_loc(prefix.saved_MISC);

   --indent;

   do_indent();
   out << "</Parser>" << endl;
}
//----------------------------------------------------------------------------
void
XML_Saving_Archive::save_Derived(const DerivedFunctionCache & fns)
{
   loop(f, fns.size())
      {
        const DerivedFunction & derived = fns[f];
        save_Function(derived);
      }
}
//----------------------------------------------------------------------------
void
XML_Saving_Archive::save_symtab(const SymbolTable & symtab)
{
   Log(LOG_archive)   CERR << "save_symtab()" << endl;

std::basic_string<const Symbol *> symbols = symtab.get_all_symbols();

   // remove erased symbols
   //
   for (size_t s = 0; s < symbols.size();)
      {
        const Symbol * sym = symbols[s];
        if (sym->is_erased())
            {
              symbols[s] = symbols.back();
              symbols.pop_back();
              continue;
            }

        ++s;
      }

   out << endl;
   do_indent();
   out << "<!-- Symbol Table -->" << endl;
   do_indent();
   out << "<SymbolTable size=\"" << symbols.size() << "\">" << endl;

   ++indent;

   while (symbols.size() > 0)
      {
        // set idx to the alphabetically smallest name
        //
        int idx = 0;
        for (size_t i = 1; i < symbols.size(); ++i)
            {
              if (symbols[idx]->compare(*symbols[i]) > 0)   idx = i;
            }

        const Symbol * sym = symbols[idx];
        save_Symbol(*sym);

        symbols[idx] = symbols.back();
        symbols.pop_back();
      }

   --indent;

   do_indent();
   out << "</SymbolTable>" << endl << endl;
}
//----------------------------------------------------------------------------
void
XML_Saving_Archive::save_SI_entry(const StateIndicator & si)
{
   Log(LOG_archive)   CERR << "save_SI_entry()" << endl;

const Executable & exec = *si.get_executable();

   do_indent();
   out << "<SI-entry level=\"" << si.get_level()
       << "\" pc=\"" << si.get_PC()
       << "\" line=\"" << exec.get_line(si.get_PC()) << "\""
       <<">" << endl << flush;

   ++indent;
   do_indent();
   switch(exec.get_parse_mode())
      {
        case PM_FUNCTION:
             {
               Symbol * sym = Workspace::lookup_symbol(exec.get_name());
               Assert(sym);
               const UserFunction * ufun = exec.get_exec_ufun();
               Assert(ufun);
               const int sym_depth = sym->get_exec_ufun_depth(ufun);

               if (ufun->is_macro())
                  out << "<UserFunction macro-num=\"" << ufun->get_macnum()
                      << "\"/>" << endl;
               else if (ufun->is_lambda())
                  {
                    out << "<UserFunction lambda-name=\"" << ufun->get_name()
                        << "\">" << endl;
                    ++indent;
                    save_UCS(ufun->canonical(false));
                    --indent;
                    do_indent();
                    out << "</UserFunction>" << endl;
                  }
               else
                  out << "<UserFunction ufun-name=\"" << sym->get_name()
                      << "\" symbol-level=\"" << sym_depth << "\"/>" << endl;
             }
             break;

        case PM_STATEMENT_LIST:
             out << "<Statements>" << endl;
             ++indent;
             save_UCS(exec.get_text(0));
             --indent;
             do_indent();
             out << "</Statements>" << endl;
               break;

        case PM_EXECUTE:
             out << "<Execute>" << endl;
             ++indent;
             save_UCS(exec.get_text(0));
             --indent;
             do_indent();
             out << "</Execute>" << endl;
             break;

          default: FIXME;
      }

   // print the parser states...
   //
   save_Parser(si);

   --indent;

   do_indent();
   out << "</SI-entry>" << endl << endl;
}
//----------------------------------------------------------------------------
void
XML_Saving_Archive::save_Symbol(const Symbol & sym)
{
   Log(LOG_archive)   CERR << "save_Symbol(Vid "
                           << sym.get_name() << ")" << endl;

   do_indent();
   out << "<Symbol name=\"" << sym.get_name() << "\" stack-size=\""
       << sym.value_stack_size() << "\">" << endl;

   ++indent;
   loop(v, sym.value_stack_size())  save_vstack_item(sym[v]);
   --indent;

   do_indent();
   out << "</Symbol>" << endl << endl;
}
//----------------------------------------------------------------------------
void
XML_Saving_Archive::save_user_commands(
               const std::vector<Command::user_command> & cmds)
{
   Log(LOG_archive)   CERR << "save_user_commands()" << endl;
   if (cmds.size() == 0)   return;

   do_indent();
   out << "<Commands size=\"" << cmds.size() << "\">" << endl;

   ++indent;
   loop(u, cmds.size())
      {
        const Command::user_command & ucmd = cmds[u];
        do_indent();
        out << "<Command name=\"" << ucmd.prefix
            << "\" mode=\"" << ucmd.mode
            << "\" fun=\"" <<  ucmd.apl_function << "\"/>" << endl;
      }

   --indent;
   do_indent();
   out << "</Commands>" << endl << endl;
}
//----------------------------------------------------------------------------
void
XML_Saving_Archive::save_token_loc(const Token_loc & tloc)
{
   do_indent();
   out << "<Token pc=\"" << tloc.get_PC()
       << "\" tag=\"" << HEX(tloc.get_token().get_tag()) << "\"";
   emit_token_val(tloc.get_token());

   out << "/>" << endl;
}
//----------------------------------------------------------------------------
void
XML_Saving_Archive::emit_token_val(const Token & tok)
{
   switch(tok.get_ValueType())
      {
        case TV_NONE:  break;

        case TV_CHAR:  Log(LOG_archive)   CERR << "Saving TV_SYM Token" << endl;
                       out << " char=\"" << int(tok.get_char_val()) << "\"";
                       break;

        case TV_INT:   Log(LOG_archive)   CERR << "Saving TV_INT Token" << endl;
                       out << " int=\"" << tok.get_int_val() << "\"";
                       break;

        case TV_FLT:   Log(LOG_archive)   CERR << "Saving TV_FLT Token" << endl;
                       out << " float=\"" << tok.get_flt_val() << "\"";
                       break;

        case TV_CPX:   Log(LOG_archive)   CERR << "Saving TV_CPX Token" << endl;
                       out << " real=\"" << tok.get_cpx_real()
                           << "\" imag=\"" << tok.get_cpx_imag() << "\"";
                       break;

        case TV_SYM:   Log(LOG_archive)   CERR << "Saving TV_SYM Token" << endl;
                       {
                         Symbol * sym = tok.get_sym_ptr();
                         const UCS_string name = sym->get_name();
                         out << " sym=\"" << name << "\"";
                       }
                       break;

        case TV_LIN:   Log(LOG_archive)   CERR << "Saving TV_LIN Token" << endl;
                       out << " line=\"" << tok.get_fun_line() << "\"";
                       break;

        case TV_VAL:   {
                         Log(LOG_archive)
                            CERR << "Saving TV_VAL Token" << endl;

                         const Vid vid = find_vid(tok.get_apl_val().get());
                         out << " vid=\"" << vid << "\"";
                       }
                       break;

        case TV_INDEX: {
                         Log(LOG_archive)
                            CERR << "Saving TV_INDEX Token" << endl;
                         const IndexExpr & idx = tok.get_index_val();
                         const int rank = idx.get_rank();
                         out << " index=\"";
                         loop(i, rank)
                             {
                               if (i)   out << ",";
                               const Value * val = idx.values[i].get();
                               if (val)   out << "vid_" << find_vid(val);
                               else       out << "-";
                                out << "\"";
                             }
                       }
                       break;

        case TV_FUN:   {
                         Log(LOG_archive)
                            CERR << "Saving TV_FUN Token" << endl;

                         cFunction_P fun = tok.get_function();
                         Assert1(fun);
                         save_Function_name(*fun);
                       }
                       out << "\"";
                       break;

        default:       FIXME;

      }
}
//----------------------------------------------------------------------------
void
XML_Saving_Archive::save_vstack_item(const ValueStackItem & vsi)
{
   Log(LOG_archive)   CERR << "    save_vstack_item(name_class "
                           << vsi.get_NC() << ")" << endl;
   switch(vsi.get_NC())
      {
        case NC_UNUSED_USER_NAME:
             do_indent();
             out << "<unused-name/>" << endl;
             break;

        case NC_LABEL:
             do_indent();
             out << "<Label value=\"" << vsi.get_label() << "\"/>" << endl;
             break;

        case NC_VARIABLE:
             do_indent();
             out << "<Variable vid=\"" << find_vid(vsi.get_val_cptr())
                 << "\"/>" << endl;
             break;

        case NC_FUNCTION:
        case NC_OPERATOR:
             save_Function(*vsi.get_function());
             break;

        case NC_SYSTEM_VAR:
             do_indent();
             out << "<Shared-Variable key=\"" << vsi.get_key()
                 << "\"/>" << endl;
             break;

        default: Assert(0);
      }
}
//----------------------------------------------------------------------------
bool
XML_Saving_Archive::_val_par::compare_val_par(const _val_par & A,
                                              const _val_par & B, const void *)
{
   return A._val > B._val;
}
//----------------------------------------------------------------------------
int
XML_Saving_Archive::_val_par::compare_val_par1(const void * key, const void * B)
{
const void * Bv = (reinterpret_cast<const _val_par *>(B))->_val;
   return charP(key) - charP(Bv);
}
//----------------------------------------------------------------------------
XML_Saving_Archive &
XML_Saving_Archive::save()
{
   Log(LOG_archive)   CERR << "save()" << endl;

tm * t;
   {
     timeval now;   gettimeofday(&now, 0);
     time_t seconds = now.tv_sec;
     t = gmtime(&seconds);
   }

const int offset = Workspace::get_v_Quad_TZ().get_offset();   // timezone offset

   // check with: xmllint --valid workspace.xml >/dev/null
   //
   out <<
"<?xml version='1.0' encoding='UTF-8' standalone='yes'?>\n"
"\n"
"<!DOCTYPE Workspace\n"
"[\n"
"    <!ELEMENT Workspace (Function*,Value*,Ravel*,SymbolTable,\n"
"                         Symbol*,Commands?,StateIndicator)>\n"
"    <!ATTLIST Workspace  wsid       CDATA #REQUIRED>\n"
"    <!ATTLIST Workspace  year       CDATA #REQUIRED>\n"
"    <!ATTLIST Workspace  month      CDATA #REQUIRED>\n"
"    <!ATTLIST Workspace  day        CDATA #REQUIRED>\n"
"    <!ATTLIST Workspace  hour       CDATA #REQUIRED>\n"
"    <!ATTLIST Workspace  minute     CDATA #REQUIRED>\n"
"    <!ATTLIST Workspace  second     CDATA #REQUIRED>\n"
"    <!ATTLIST Workspace  timezone   CDATA #REQUIRED>\n"
"    <!ATTLIST Workspace  saving_SVN CDATA #REQUIRED>\n"
"    <!ATTLIST Workspace  syntax     CDATA #IMPLIED>\n"
"\n"
"        <!ELEMENT Value (#PCDATA)>\n"
"        <!ATTLIST Value flg    CDATA #REQUIRED>\n"
"        <!ATTLIST Value vid    CDATA #REQUIRED>\n"
"        <!ATTLIST Value parent CDATA #IMPLIED>\n"
"        <!ATTLIST Value rk     CDATA #REQUIRED>\n"
"        <!ATTLIST Value sh-0   CDATA #IMPLIED>\n"
"        <!ATTLIST Value sh-1   CDATA #IMPLIED>\n"
"        <!ATTLIST Value sh-2   CDATA #IMPLIED>\n"
"        <!ATTLIST Value sh-3   CDATA #IMPLIED>\n"
"        <!ATTLIST Value sh-4   CDATA #IMPLIED>\n"
"        <!ATTLIST Value sh-5   CDATA #IMPLIED>\n"
"        <!ATTLIST Value sh-6   CDATA #IMPLIED>\n"
"        <!ATTLIST Value sh-7   CDATA #IMPLIED>\n"
"\n"
"        <!ELEMENT Ravel (#PCDATA)>\n"
"        <!ATTLIST Ravel vid    CDATA #REQUIRED>\n"
"        <!ATTLIST Ravel bytes  CDATA #IMPLIED>\n"
"        <!ATTLIST Ravel cells  CDATA #IMPLIED>\n"
"\n"
"        <!ELEMENT SymbolTable (Symbol*)>\n"
"        <!ATTLIST SymbolTable size CDATA #REQUIRED>\n"
"\n"
"            <!ELEMENT Symbol (unused-name|Variable|Function|\n"
"                              Label|Shared-Variable)*>\n"
"            <!ATTLIST Symbol name       CDATA #REQUIRED>\n"
"            <!ATTLIST Symbol stack-size CDATA #REQUIRED>\n"
"\n"
"                <!ELEMENT unused-name EMPTY>\n"
"\n"
"                <!ELEMENT Variable (#PCDATA)>\n"
"                <!ATTLIST Variable vid CDATA #REQUIRED>\n"
"\n"
"                <!ELEMENT Function (UCS*)>\n"
"                <!ATTLIST Function fid             CDATA #REQUIRED>\n"
"                <!ATTLIST Function creation-time   CDATA #IMPLIED>\n"
"                <!ATTLIST Function LO-fid          CDATA #IMPLIED>\n"
"                <!ATTLIST Function OPER-fid        CDATA #IMPLIED>\n"
"                <!ATTLIST Function RO-fid          CDATA #IMPLIED>\n"
"                <!ATTLIST Function AXIS-vid        CDATA #IMPLIED>\n"
"                <!ATTLIST Function exec-properties CDATA #IMPLIED>\n"
"                <!ATTLIST Function macro           CDATA #IMPLIED>\n"
"                <!ATTLIST Function tag             CDATA #IMPLIED>\n"
"\n"
"                <!ELEMENT Label (#PCDATA)>\n"
"                <!ATTLIST Label value CDATA #REQUIRED>\n"
"\n"
"                <!ELEMENT Shared-Variable (#PCDATA)>\n"
"                <!ATTLIST Shared-Variable key CDATA #REQUIRED>\n"
"\n"
"        <!ELEMENT UCS (#PCDATA)>\n"
"        <!ATTLIST UCS uni CDATA #REQUIRED>\n"
"\n"
"        <!ELEMENT Commands (Command*)>\n"
"        <!ATTLIST Commands size CDATA #REQUIRED>\n"
"\n"
"            <!ELEMENT Command (#PCDATA)>\n"
"            <!ATTLIST Command name       CDATA #REQUIRED>\n"
"            <!ATTLIST Command mode       CDATA #REQUIRED>\n"
"            <!ATTLIST Command fun       CDATA #REQUIRED>\n"
"\n"
"        <!ELEMENT StateIndicator (SI-entry*)>\n"
"        <!ATTLIST StateIndicator levels CDATA #REQUIRED>\n"
"\n"
"            <!ELEMENT SI-entry ((Execute|Statements|UserFunction),Parser+)>\n"
"            <!ATTLIST SI-entry level     CDATA #REQUIRED>\n"
"            <!ATTLIST SI-entry pc        CDATA #REQUIRED>\n"
"            <!ATTLIST SI-entry line      CDATA #REQUIRED>\n"
"\n"
"                <!ELEMENT Statements (UCS)>\n"
"\n"
"                <!ELEMENT Execute (UCS)>\n"
"\n"
"                <!ELEMENT UserFunction (#PCDATA)>\n"
"                <!ATTLIST UserFunction ufun-name       CDATA #IMPLIED>\n"
"                <!ATTLIST UserFunction macro-num       CDATA #IMPLIED>\n"
"                <!ATTLIST UserFunction lambda-name     CDATA #IMPLIED>\n"
"                <!ATTLIST UserFunction symbol-level    CDATA #IMPLIED>\n"
"\n"
"                <!ELEMENT Parser (Token*,Function*)>\n"
"                <!ATTLIST Parser size           CDATA #REQUIRED>\n"
"                <!ATTLIST Parser assign-pending CDATA #REQUIRED>\n"
"                <!ATTLIST Parser lookahead-high CDATA #REQUIRED>\n"
"                <!ATTLIST Parser action         CDATA #REQUIRED>\n"

"                    <!ELEMENT Token (#PCDATA)>\n"
"                    <!ATTLIST Token pc           CDATA #REQUIRED>\n"
"                    <!ATTLIST Token tag          CDATA #REQUIRED>\n"
"                    <!ATTLIST Token char         CDATA #IMPLIED>\n"
"                    <!ATTLIST Token int          CDATA #IMPLIED>\n"
"                    <!ATTLIST Token float        CDATA #IMPLIED>\n"
"                    <!ATTLIST Token real         CDATA #IMPLIED>\n"
"                    <!ATTLIST Token imag         CDATA #IMPLIED>\n"
"                    <!ATTLIST Token sym          CDATA #IMPLIED>\n"
"                    <!ATTLIST Token line         CDATA #IMPLIED>\n"
"                    <!ATTLIST Token vid          CDATA #IMPLIED>\n"
"                    <!ATTLIST Token index        CDATA #IMPLIED>\n"
"                    <!ATTLIST Token fun-id       CDATA #IMPLIED>\n"
"                    <!ATTLIST Token ufun-name    CDATA #IMPLIED>\n"
"                    <!ATTLIST Token symbol-level CDATA #IMPLIED>\n"
"                    <!ATTLIST Token comment      CDATA #IMPLIED>\n"
"\n"
"]>\n"
"\n"
"    <!-- hour/minute/second is )SAVE time in UTC (aka. GMT).\n"
"         timezone is offset to UTC in seconds.\n"
"         local time is UTC + offset -->\n"
"\n"
"<Workspace wsid=\""       << Workspace::get_WS_name()
     << "\" year=\""       << (t->tm_year + 1900)
     << "\" month=\""      << (t->tm_mon  + 1)
     << "\" day=\""        <<  t->tm_mday << "\"" << endl <<
"           hour=\""       <<  t->tm_hour
     << "\" minute=\""     <<  t->tm_min
     << "\" second=\""     <<  t->tm_sec
     << "\" timezone=\""   << offset << "\"" << endl <<
"           saving_SVN=\"" << ARCHIVE_SVN
     << "\" syntax=\"" << ASX_MAJOR << "."
                       << ASX_MINOR << "."
                       << ASX_OTHER << "\">\n" << endl;

   ++indent;

   Log(LOG_archive)   CERR << "save() saves function..." << endl;
   save_functions();

   // collect all values to be saved. We mark the values to avoid
   // saving of stale values and unmark the used values
   //
   Log(LOG_archive)   CERR << "save() marks values..." << endl;
   Value::mark_all_dynamic_values();

   Log(LOG_archive)   CERR << "save() unmarks values..." << endl;
   Workspace::unmark_all_values();

   for (const DynamicObject * dob = DynamicObject::get_all_values()->get_next();
        dob != DynamicObject::get_all_values(); dob = dob->get_next())
       {
         // WARNING: do not use pValue() here !
         const Value * val = static_cast<const Value *>(dob);

         if (val->is_marked())    continue;   // stale

         ++value_count;
       }

   Log(LOG_archive)
      CERR << "save() allocates values[" << value_count << "]..." << endl;

   try
      {
        val_pars = new _val_par[value_count];
      }
   catch(...)
      {
        MORE_ERROR() <<
           "XML_Saving_Archive::save() could not allocate values["
               << value_count << "]: (lack of memory).\n"
            "Your workspace may be corrupt (possibly due "
               "to a previous WS FULL ?).\n"
            "You should not )SAVE it. Maybe )DUMP the workspace instead?."
           ;
         WS_FULL;
      }
   Log(LOG_archive)   CERR << "save() done allocating values[]..." << endl;

ShapeItem idx = 0;

   for (const DynamicObject * dob = DynamicObject::get_all_values()->get_next();
        dob != DynamicObject::get_all_values(); dob = dob->get_next())
       {
         // WARNING: do not use pValue() here !
         const Value * val = static_cast<const Value *>(dob);

         if (val->is_marked())    continue;   // stale

         val->unmark();
         new (val_pars + idx++) _val_par(val, INVALID_VID);
       }

   Assert(idx == value_count);

   // some people use an excessive number of values. We therefore sort them
   // by the address of the value as to speed up finding them later on
   //
   Heapsort<_val_par>::sort(val_pars, value_count, 0,
                            &_val_par::compare_val_par);
   loop(v, (value_count - 1))
       {
         Assert(&val_pars[v]._val < &val_pars[v + 1]._val);
       }

   // set up parents of values
   //
   loop(p, value_count)   // for every (parent-) value
      {
        const Value & parent = *val_pars[p]._val;
        const ShapeItem ec = parent.nz_element_count();
        loop(e, ec)   // for every ravel cell of the (parent-) value
            {
              const Cell & cP = parent.get_cravel(e);
              if (cP.is_lval_cell())
                 {
                   Log(LOG_archive)
                      CERR << "LVAL CELL in " << p << " at " LOC << endl;
                   continue;
                 }

              if (!cP.is_pointer_cell())   continue;

              // from here on, cp is a PointerCell of the parent...
              //
              const Value * sub = cP.get_pointer_value().get();
              Assert1(sub);
              const Vid sub_idx = find_vid(sub);
              Assert(sub_idx < value_count);

              // check for multiple parents of the same value. This is an
              // error in the old cloning sheme, but not in the new one.
              //
#ifdef NEW_CLONE
              if (false)   // never (new scheme)
#else
              if (values[sub_idx]._par != INVALID_VID)
#endif
                 {
                   // sub already has a parent, which supposedly cannot
                   // happen. print out some more information about this
                   // case.
                   //
                   CERR << "*** Sub-Value "
                        << voidP(sub) << " has two parents."      << endl
                        << "Child: vid=" << sub_idx << ", _val="
                        << val_pars[sub_idx]._val << ", _par="
                        << val_pars[sub_idx]._par                 << endl
                        << "Parent 2: vid=" << p <<  ", _val="
                        << val_pars[p]._val << "_par="
                        << val_pars[p]._par                       << endl
                        << "Call stack:"                          << endl;
                        BACKTRACE
                   CERR << endl << " Running )CHECK..." << endl;
                   UCS_string no_arg;
                   Command::cmd_CHECK(CERR, no_arg);
                   CERR << endl;

#if cfg_VALUE_HISTORY_WANTED
VH_entry::print_history(CERR, *sub, 0);
VH_entry::print_history(CERR, *values[sub_idx]._val, 0);
VH_entry::print_history(CERR, *values[p]._val, 0);
#endif

   CERR << endl <<
"The workspace will be )SAVEd, but using it for anything other than for\n"
" recovering its content (i.e. defined functions or variables) means\n"
" asking for BIG trouble!" << endl;
                 }

              val_pars[sub_idx] = _val_par(val_pars[sub_idx]._val, Vid(p));
            } //   end of loop(e, ec)   (parent ravel cells_
      }   // end of loop(p, value_count) (parent values)


   // save all values (without their ravel)
   //
   do_indent();
   out << "<!-- APL values... -->" << endl;
   loop(vid, value_count)   save_shape(Vid(vid));

   // save ravels of all values
   //
   out << endl;
   do_indent();
   out << "<!-- Ravels of APL values... -->" << endl;
   loop(vid, value_count)   save_Ravel(Vid(vid));

   // save user defined symbols
   //
   save_symtab(Workspace::get_symbol_table());

   // save certain system variables
   //
   do_indent();
   out << "<!-- APL system variables... -->" << endl;

#define rw_sv_def(x, _str, _txt) save_Symbol(Workspace::get_v_ ## x());
#define ro_sv_def(x, _str, _txt) save_Symbol(Workspace::get_v_ ## x());
#include "SystemVariable.def"

   // save user-defined commands (if any)
   //
   save_user_commands(Workspace::get_user_commands());

   // save state indicator
   //
   {
     out << endl;
     do_indent();
     out << "<!-- State Indicator -->" << endl;
     const int levels = Workspace::SI_entry_count();
     do_indent();
     out << "<StateIndicator levels=\"" << levels << "\">" << endl;

     ++indent;

     loop(l, levels)
        {
          for (const StateIndicator * si = Workspace::SI_top();
               si; si = si->get_parent())
              {
                if (si->get_level() == l)
                   {
                     save_SI_entry(*si);
                     break;
                   }
              }
        }

     --indent;

     do_indent();
     out << "</StateIndicator>" << endl << endl;
   }

   --indent;

   do_indent();

   // write closing tag and a few 0's so that string functions
   // can be used on the mmaped file.
   //
   out << "</Workspace>" << endl
       << char(0) << char(0) <<char(0) <<char(0) << endl;

   return *this;
}
//============================================================================
XML_Loading_Archive::XML_Loading_Archive(ostream & _out, const char * _filename,
                                         int & dump_fd)
   : out(_out),
     fd(-1),
     map_start(0),
     map_length(0),
     file_start(0),
     line_start(0),
     line_no(1),
     current_char(UNI_SPACE),
     data(0),
     file_end(0),
     copying(false),
     protection(false),
     reading_vids(false),
     have_allowed_objects(false),
     filename(_filename),
     file_is_complete(false)
{
   Log(LOG_archive)   CERR << "Loading file " << filename << endl;

   fd = open(filename, O_RDONLY);
   if (fd == -1)   return;

struct stat st;
   if (fstat(fd, &st))
      {
        CERR << "fstat() failed: " << strerror(errno) << endl;
        close(fd);
        fd = -1;
        return;
      }

   map_length = st.st_size;
   map_start = mmap(0, map_length, PROT_READ, MAP_SHARED, fd, 0);
   if (map_start == reinterpret_cast<const void *>(-1))
      {
        CERR << "mmap() failed: " << strerror(errno) << endl;
        close(fd);
        fd = -1;
        return;
      }

   // success
   //
   file_start = charP(map_start);
   file_end = utf8P(file_start + map_length);

   reset();

   if (!strncmp(file_start, "#!", 2) ||   // )DUMP file
       !strncmp(file_start, "<!", 2) ||   // )DUMP-HTML file
       !strncmp(file_start, "⍝!", 4))     // a library
      {
        // the file was either written with )DUMP or is a library.
        // Return the open file descriptor (the destructor will unmap())
        //
        dump_fd = fd;
        fd = -1;   // file will be closed via dump_fd
        return;
      }

   if (strncmp(file_start, "<?xml", 5))   // not an xml file
      {
        CERR << "file " << filename << " does not " << endl
             << "have the format of a GNU APL .xml or .apl file" << endl;
        close(fd);
        fd = -1;
        return;
      }
}
//----------------------------------------------------------------------------
XML_Loading_Archive::~XML_Loading_Archive()
{
   if (map_start)   munmap(map_start, map_length);
   if (fd != -1)    close(fd);
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::reset()
{
   line_start = data = utf8P(file_start);
   line_no = 1;
   next_tag(LOC);
}
//----------------------------------------------------------------------------
bool
XML_Loading_Archive::skip_to_tag(const char * tag)
{
   for (;;)
      {
         if (next_tag(LOC))   return true;
         if (is_tag(tag))     break;
      }

   return false;
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::read_vids()
{
   reset();   // skips to <Workspace>
   reading_vids = true;
   read_Workspace(true);
   reading_vids = false;

   reset();
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::where(ostream & out)
{
   out << "line=" << line_no << "+" << (data - line_start) << " '";

   loop(j, 40)   { if (data[j] == 0x0A)   break;   out << data[j]; }
   out << "'" << endl;
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::where_att(ostream & out)
{
   out << "line=" << line_no << "+" << (attributes - line_start) << " '";

   loop(j, 40)
      {
        if (attributes[j] == 0x0A)        break;
        if (attributes + j >= end_attr)   break;
         out << attributes[j];
      }

   out << "'" << endl;
}
//----------------------------------------------------------------------------
bool
XML_Loading_Archive::get_uni()
{
   if (data > file_end)   return true;   // EOF

int len = 0;
   current_char = UTF8_string::toUni(data, len, true);
   data += len;
   if (current_char == 0x0A)   { ++line_no;   line_start = data; }
   return false;
}
//----------------------------------------------------------------------------
bool
XML_Loading_Archive::is_tag(const char * prefix) const
{
   return !strncmp(charP(tag_name), prefix, strlen(prefix));
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::expect_tag(const char * prefix, const char * loc) const
{
   if (!is_tag(prefix))
      {
        CERR << "   Got tag ";
        print_tag(CERR);
        CERR << " when expecting tag " << prefix
             << " at " << loc << "  line " << line_no << endl;
        DOMAIN_ERROR;
      }
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::print_tag(ostream & out) const
{
   loop(t, attributes - tag_name)   out << tag_name[t];
}
//----------------------------------------------------------------------------
const UTF8 *
XML_Loading_Archive::find_attr(const char * att_name, bool optional)
{
const int att_len = strlen(att_name);

   for (const UTF8 * d = attributes; d < end_attr; ++d)
       {
         if (strncmp(att_name, charP(d), att_len))   continue;
         const UTF8 * dd = d + att_len;
         while (*dd <= ' ')   ++dd;   // skip whitespaces
         if (*dd++ != '=')   continue;

         // attribute= found. find value.
         while (*dd <= ' ')   ++dd;   // skip whitespaces
         Assert(*dd == '"');
         return dd + 1;
       }

   // not found
   //
   if (!optional)
      {
         const int offset = attributes - line_start;
         MORE_ERROR() << "mandatory Attribute '" << att_name
              << "' is missing in line " << line_no << ":" << offset
              << " of file " << filename;
         DOMAIN_ERROR;
      }
   return 0;   // not found
}
//----------------------------------------------------------------------------
int64_t
XML_Loading_Archive::find_int_attr(const char * attrib, bool optional, int base)
{
const UTF8 * value = find_attr(attrib, optional);
   if (value == 0)   return -1;   // not found

const int64_t val = strtoll(charP(value), 0, base);
   return val;
}
//----------------------------------------------------------------------------
APL_Float
XML_Loading_Archive::find_float_attr(const char * attrib)
{
const UTF8 * value = find_mandatory_attr(attrib);
const APL_Float val = strtod(charP(value), 0);
   return val;
}
//----------------------------------------------------------------------------
bool
XML_Loading_Archive::next_tag(const char * loc)
{
   /* loc points into a read-only string. Set the variables:

      tag_name:    char after next '<'
      attributes:  char after tag_name + blank
      end_attr:    next >
    */

again:

   // read chars up to (including) '<'
   //
   while (current_char != '<')
      {
         if (get_uni())   return true;   // EOF
      }

   tag_name = data;

   // read char after <
   //
   if (get_uni())   return true;   // EOF

   if (current_char == '?')   goto again;   // processing instruction
   if (current_char == '!')                 // comment
      {
        const char * ctag_name = reinterpret_cast<const char *>(tag_name);
        const char * comment_end = strstr(ctag_name, "-->");
        Assert(comment_end);
        comment_end += 3;
        while (data < reinterpret_cast<const UTF8 *>(comment_end))
           {
             if (get_uni())   return true;   // EOF
           }
        goto again;
      }
   if (current_char == '/')   get_uni();    // / start of end tag

   // read chars before attributes (if any)
   //
   for (;;)
       {
         if (current_char == ' ')   break;
         if (current_char == '/')   break;
         if (current_char == '>')   break; 
         if (get_uni())   return true;   // EOF
       }

   attributes = data;

   // read chars before end of tag
   //
   while (current_char != '>')
      {
         if (get_uni())   return true;   // EOF
      }

   end_attr = data;

/*
   CERR << "See tag ";
   for (const UTF8 * t = tag_name; t < attributes; ++t)   CERR << char(*t);
   CERR << " at " << loc << " line " << line_no << endl;
*/

   return false;
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::read_Workspace(bool silent)
{
   expect_tag("Workspace", LOC);

const int offset   = find_int_attr("timezone", false, 10);
const UTF8 * wsid  = find_mandatory_attr("wsid");

int year  = find_int_attr("year",     false, 10);
int mon   = find_int_attr("month",    false, 10);
int day   = find_int_attr("day",      false, 10);
int hour  = find_int_attr("hour",     false, 10);
int min   = find_int_attr("minute",   false, 10);
int sec   = find_int_attr("second",   false, 10);

   check_compatibility();

   sec  += offset          % 60;
   min  += (offset /   60) % 60;
   hour += (offset / 3600) % 60;
   if      (sec  >= 60)   { sec  -= 60;   ++min;  }
   else if (sec  <  0)    { sec  += 60;   --min;  }

   if      (min  >= 60)   { min  -= 60;   ++hour; }
   else if (min  <   0)   { min  += 60;   --hour; }

   if      (hour >= 24)   { hour -= 24;   ++day;  }
   else if (hour <   0)   { hour += 24;   --day;  }

bool next_month = false;
bool prev_month = false;
   switch(day)
      {
        case 32: next_month = true;
                 break;

        case 31: if (mon == 4 || mon == 6 || mon == 9 || mon == 11)
                    next_month = true;
                 break;

        case 30: if (mon == 2)   next_month = true;
                 break;

        case 29: if (mon != 2)         break;               // not february
                 if (year & 3)         next_month = true;   // not leap year
                 // the above fails if someone loads a workspace that
                 // was saved around midnight on 2/28/2100. Dont do that!
                 break;

        case 0:  prev_month = true;
                 break;

        default: break;
      }

   if      (next_month)   { day = 1; ++mon;  }
   else if (prev_month)
           {
             day = 31;   --mon;
             if (mon == 4 || mon == 6 || mon == 9 || mon == 11)   day = 30;
             else if (mon == 2)                 day = (year & 3) ? 28 : 29;
           }

   if      (mon > 12)   { mon =  1; ++year; }
   else if (mon <  1)   { mon = 12; --year; }

   Log(LOG_archive)   CERR << "read_Workspace() " << endl;

   // quick check that the file is complete
   //
   for (const UTF8 * c = file_end - 12; (c > data) && (c > file_end - 200); --c)
       {
         if (!strncmp(charP(c), "</Workspace>", 12))
            {
              file_is_complete = true;
              break;
            }
       }

   if (!file_is_complete && !copying)
      {
        CERR <<
"*** workspace file " << filename << endl <<
"    seems to be incomplete (possibly caused by a crash on )SAVE?)\n" 
"    You may still be able to )COPY from it.\n"
"\nNOT COPIED" << endl;
        return;
      }

   // the order in which tags are written to the xml file
   //
const char * tag_order[] =
{
  "Function",
  "Value",
  "Ravel",
  "SymbolTable",
  "Symbol",
  "Commands",
  "StateIndicator",
  "/Workspace",
  0
};
const char ** tag_pos = tag_order;

   for (;;)
       {
         next_tag(LOC);

         // make sure that we do not move backwards in tag_order
         //
         if (!is_tag(*tag_pos))   // new tag
            {
              tag_pos++;
              for (;;)
                  {
                     if (*tag_pos == 0)
                        {
                          MORE_ERROR() << "Unexpected Tag "
                                       << UTF8_string(tag_name,
                                                      attributes - tag_name);
                          DOMAIN_ERROR;   // end of list
                        }
                     if (is_tag(*tag_pos))   break;          // found
                     ++tag_pos;
                  }
            }

         if      (is_tag("Function"))         read_Function();
         else if (is_tag("Value"))            read_Value();
         else if (is_tag("Ravel"))            read_Ravel();
         else if (is_tag("SymbolTable"))      read_SymbolTable();
         else if (is_tag("Symbol"))           read_Symbol();
         else if (is_tag("Commands"))         read_Commands();
         else if (copying)                    break;
         else if (is_tag("StateIndicator"))   read_StateIndicator();
         else if (is_tag("/Workspace"))       break;
         else    /* complain */               expect_tag("UNEXPECTED", LOC);
       }

   // loaded workspace can contain stale variables, e.g. shared vars.
   // remove them.
   //
   Value::erase_stale(LOC);

   if (reading_vids)   return;

const char * tz_sign = (offset < 0) ? "" : "+";
   if (!silent)   out
        << "SAVED "
        << setfill('0') << year        << "-"
        << setw(2)      << mon         << "-"
        << setw(2)      << day         << " "
        << setw(2)      << hour        << ":"
        << setw(2)      << min         << ":"
        << setw(2)      << sec         << " (GMT"
        << tz_sign      << offset/3600 << ")"
        << setfill(' ') <<  endl;

   if (!copying)
      {
        const UTF8 * end = wsid;
        while (*end != '"')   ++end;

        const UTF8_string wsname_utf(wsid, end - wsid);
        Workspace::set_WS_name(UCS_string(wsname_utf));
      }

   if (have_allowed_objects && allowed_objects.size())
      {
        CERR << "NOT COPIED:";
        loop(a, allowed_objects.size())   CERR << " " << allowed_objects[a];
        CERR << endl;
      }
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::check_compatibility()
{
   // new compatibility mechanism?
   //
   if (const char * syntax = charP(find_optional_attr("syntax")))   // yes
      {
        unsigned int major = 0;
        unsigned int minor = 0;
        unsigned int other  = 0;
        sscanf(syntax, "%u.%u.%u", &major, &minor, &other);
        if (major == ASX_MAJOR && minor == ASX_MINOR)   return;
        if (major == ASX_MAJOR)
           {
              CERR << "NOTE: this workspaces was )SAVEd with syntax version "
                   << major << "." << minor << "." << other << endl
                   << "but is now )LOADed with a newer version " << ASX_MAJOR
                   << "." << ASX_MINOR << "." << ASX_OTHER << "." << endl
                   << "That should be OK, but new features introduced by the"
                   << " newer version will not be supported." << endl;
           }
        return;
      }

bool mismatch = false;

   // old (SVN-version based) compatibility mechanism

UCS_string saving_SVN;   // from the XML file
UCS_string current_SVN(UTF8_string(ARCHIVE_SVN));
   {
     const UTF8 * saving = find_optional_attr("saving_SVN");
     while (saving && *saving != '"')   saving_SVN.append(Unicode(*saving++));
   }

   if (saving_SVN.size() == 0)   // saved with very old version
      {
        mismatch = true;
        CERR << "WARNING: this workspace was )SAVEd with a VERY "
             << "old SVN version of GNU APL." << endl;
      }
   else if (saving_SVN != current_SVN)   // saved with different version
      {
        mismatch = true;
        CERR << "WARNING: this workspace was )SAVEd with SVN version "
             << saving_SVN << endl <<
        "          but is now being )LOADed with a SVN version "
             << current_SVN << " or greater" << endl;
      }

   if (mismatch)
      {
        CERR << "Expect problems, in particular when the )SI was not clear.\n";
        if (!copying)
           CERR << "In case of problems, please try )COPY instead of )LOAD."
                << endl;
      }
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::read_Value()
{
   expect_tag("Value", LOC);

   // read all mandatory attributes even if they are no used in this pass
   //
const Vid vid    = find_Vid_attr("vid",    false, 10);
const int flags  = find_int_attr("flg",    false, 16);
const Vid parent = find_Vid_attr("parent", false, 10);
const int rk     = find_int_attr("rk",     false, 10);

   Log(LOG_archive)   CERR << "  read_Value() vid=" << vid << endl;

   if (reading_vids)
      {
         parents.push_back(parent);
         return;
      }

Shape sh_value;
   loop(r, rk)
      {
        char sh[20];
        SPRINTF(sh, "sh-%d", int(r));
        const UTF8 * sh_r = find_mandatory_attr(sh);
        sh_value.add_shape_item(atoll(charP(sh_r)));
      }

   /* if:

      1. we do )COPY or )PCOPY, and
      2. vid is not in vids_COPY list,

      then we push 0 (so that indexing with vid still works) and ignore such
      values in read_Ravel.
    */

bool no_copy = false;   // assume the value is needed
   if (copying)
      {
        // if vid is a sub-value then find its topmost owner
        //
        int parent = vid;
        for (;;)
            {
              Assert(parent < int(parents.size()));
              if (parents[parent] == -1)   break;   // topmost owner found
              parent = parents[parent];
            }

        no_copy = true;   // assume the value is not needed
        loop(v, vids_COPY.size())
           {
              if (parent == vids_COPY[v])   // vid is in the list: copy
                 {
                   no_copy = false;
                   break;
                 }
           }
      }

   if (no_copy)
      {
        values.push_back(Value_P());
      }
   else
      {
        Assert(vid == int(values.size()));
        if (flags & VF_packed)
           {
             Value_P val(sh_value, /* constructor allocates */ 0, LOC);
             values.push_back(val);
            }
        else
           {
             Value_P val(sh_value, LOC);
             if (flags & VF_member)   val->set_member();
             values.push_back(val);
           }
      }
}
//----------------------------------------------------------------------------
const UTF8 *
XML_Loading_Archive::read_Cells(Value & Z, const UTF8 * input)
{
   // skip leading whitespace
   while (*input <= ' ')   ++input;

int type_len = 0;
const Unicode type = UTF8_string::toUni(input, type_len, true);
   input += type_len;   // skip type in input

   // result pointer for strtoll()
UTF8 * end = 0;
char **pend = reinterpret_cast<char **>(&end);

   switch (type)
      {
        case UNI_PAD_U0: // end of UNI_PAD_U2,      e.g. ²ASCII⁰
        case '\n':       // end of UNI_PAD_U2 (fix old bug)
             return input;;

        case UNI_PAD_U1: // hex Unicode:            e.g. ¹
        case UNI_PAD_U2: // printable ASCII,        e.g. ²ASCII⁰
             {
               // read_XML_string() needs type, so we restore it.
               input -= type_len;

               UCS_string ucs;
               input = read_XML_string(ucs, input);
               loop(u, ucs.size())
                   {
                     Z.next_ravel_Char(ucs[u]);
                   }
             }
             return input;

        case UNI_PAD_U3: // integer,                e.g. ³
             {
               const APL_Integer val = strtoll(charP(input), pend, 10);
               Z.next_ravel_Int(val);
               input = end;
             }
             return input;

        case UNI_PAD_U4: // real,                   e.g. ⁴6675.79
             {
               const APL_Float val = strtod(charP(input), pend);
               Z.next_ravel_Float(val);
               input = end;
             }
             return input;

        case UNI_PAD_U5: // complex,                e.g. ⁵
             {
               const APL_Float real = strtod(charP(input), pend);
               input = utf8P(end);
               Assert(*end == 'J');
               ++end;
               const APL_Float imag = strtod(*pend, pend);
               Z.next_ravel_Complex(real, imag);
               input = end;
             }
             return input;

        case UNI_PAD_U6: // pointer,                e.g. ⁶1525 (vid)
             {
               const int vid = strtoll(charP(input), pend, 10);
               Assert(vid >= 0);
               Assert(vid < int(values.size()));
               Z.next_ravel_Value(values[vid].get());
               input = end;
             }
             return input;

        case UNI_PAD_U7: // cellref,                e.g. ⁷ 
             if (input[0] == '0')    // 0-cell-pointer
                {
                  Z.next_ravel_Lval(0, 0);
                  input++;
                }
             else
                {
                  const int vid = strtoll(charP(input), pend, 16);
                  Assert(vid >= 0);
                  Assert(vid < int(values.size()));
                  Assert(*end == '[');   ++end;
                  const ShapeItem offset = strtoll(*pend, pend, 16);
                  Assert(*end == ']');   ++end;
                  Value * target = values[vid].get();
                  Z.next_ravel_Lval(&target->get_wravel(offset), target);
                  input = end;
                }
             return input;

        case UNI_PAD_U8: // rational quotient,      e.g. ⁸
             //
             // we should understand rational quotients even if we are
             // not ./configured for them.
             //
             {
               const uint64_t numer = strtoll(charP(input), pend, 10);
               input = utf8P(end);

               // skip ÷ (which is is C3 B7 in UTF8)
               Assert((*end++ & 0xFF) == 0xC3);
               Assert((*end++ & 0xFF) == 0xB7);
               const uint64_t denom = strtoll(*pend, pend, 10);
               Assert(denom > 0);
#ifdef cfg_RATIONAL_NUMBERS_WANTED
               Z.next_ravel_Float(numer, denom);
#else
               Z.next_ravel_Float((1.0*numer)/denom);
#endif
               input = end;
             }
             return input;

        case UNI_PAD_U9: // packed boolean,         e.g. ⁹
             Assert(Z.is_packed());
             for (;;)
                 {
                   const uint8_t c0 = input[0];
                   const uint8_t c1 = input[1];
                   if (c0 <= ' ')    { ++input;   continue; }   // whitespace
                   if (c0 == '"')   return input + 1;   // end of ravel

                   const char cc[3] = { char(c0), char(c1), 0 };
                   char * end = 0;
                   const uint8_t byte = strtoll(cc, &end, 16);
                   input += end - cc;

                   const int converted = end - cc;
                   if (converted == 0)   return input;   // nothing converted
                   Z.next_ravel_Byte(byte);              // > 0 bytes converted
                   if (converted == 1)   return input + 1;
                 }

        default: Q1(type) Q1(line_no) DOMAIN_ERROR;
      }
}
//----------------------------------------------------------------------------
const UTF8 *
XML_Loading_Archive::read_XML_string(UCS_string & ucs, const UTF8 * utf)
{
   /* argument 'utf' is the source for a sequence of characters.
      'utf' comes from find_attr() and is therefore NOT 0-terminated but
      instead terminated by '"'.

      The Unicodes read from 'utf' are stored in result 'ucs'.

      On return, 'utf' points to the terminating '"'.
    */

   while (*utf <= ' ')   ++utf;   // skip leading whitespace

   // char mode is controlled by delimiters:
   //
   // ² (UNI_PAD_U2 = start of char mode),
   // ¹ (UNI_PAD_U1 = start of hex mode = implicit end of char mode), and
   // ⁰ (UNI_PAD_U0 = end of char mode).
   //
   // This function processes a sequence of ²strings⁰ and ¹unicodes.
   // It returns at the end of the XML attribute (") or a the next
   // micro-tag (³ ⁴ ⁵ ⁶ ⁷ ⁸ ⁹)

   for (bool char_mode = false; *utf && (*utf != '"');)
       {
         // get next Unicode and advance utf
         //
         const UTF8 * from = utf;
         int len = 0;   // length of uni
         const Unicode uni = UTF8_string::toUni(utf, len, true);
         utf += len;     // skip uni

          if (char_mode && uni != '\n' && uni != UNI_PAD_U0)
             {
               ucs.append(uni);
               continue;
             }

         if (uni == UNI_PAD_U2)   // start of char_mode
            {
              char_mode = true;
              continue;
            }

         if (uni == UNI_PAD_U1)   // start of hex mode
            {
              char_mode = false;
              char * end = 0;
              const int hex = strtoll(charP(utf), &end, 16);
              ucs.append(Unicode(hex));
              utf = utf8P(end);
              continue;
            }

         if (uni == UNI_PAD_U0)   // end of ²char_mode⁰
            {
              char_mode = false;
              continue;
            }

         if (uni == '\n')   // end of char_mode (fix old bug)
            {
              // due to an old bug, the trailing UNI_PAD_U0 may be missing
              // at the end of the line. We therefore reset char_mode at the
              // end of the line so that workspaces save with that fault can
              // be read.
              //
              char_mode = false;
              continue;
            }

         if (uni == ' ')   // indentation
            {
              continue;
            }

         if (!char_mode)   // outside of ²char_mode⁰
            {
              if (uni == UNI_PAD_U3 ||
                  uni == UNI_PAD_U4 ||
                  uni == UNI_PAD_U5 ||
                  uni == UNI_PAD_U6 ||
                  uni == UNI_PAD_U7 ||
                  uni == UNI_PAD_U8 ||
                  uni == UNI_PAD_U9)    return from;
            }

         FIXME   // not reached
       }

   return utf;
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::read_Ravel()
{
   if (reading_vids)   return;

const int vid = find_int_attr("vid", false, 10);
const UTF8 * cells_utf = find_optional_attr("cells");
   if (!cells_utf)   cells_utf = find_mandatory_attr("bytes");

   Log(LOG_archive)
      CERR << "    read_Ravel() vid=" << vid
           << ", XML line " << line_no << " - ";

   Assert(vid < int(values.size()));
Value_P Z = values[vid];

   if (!Z)
      {
        Log(LOG_archive) CERR << "NO (vids_COPY or static)" << endl;
        return;   // )COPY with vids_COPY or static value
      }

   if (Z->is_packed())   // so it can't be short and ravel is a utf8_t *
      {
        read_Cells(*Z, cells_utf);
        Log(LOG_archive) CERR << "YES (packed)" << endl;
        return;
      }

   if (Z->element_count() == 0)   // then Z->has_more() is 0 and can't be used
      {
        Value_P Z0(LOC);   // a scalar to read the prototype
        read_Cells(*Z0, cells_utf);   // prototype
        Z->set_default(*Z0, LOC);
        Log(LOG_archive) CERR << "YES (empty)" << endl;
      }
   else
      {
        while (Z->more())
            {
              cells_utf = read_Cells(*Z, cells_utf);
           // while (*cells_utf <= ' ')   ++cells_utf;   // trailing whitespace
            }
        Log(LOG_archive) CERR << "YES (" << Z->element_count()
                              << " items)" << endl;
         }
   Z->check_value(LOC);
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::read_unused_name(int d, Symbol & symbol)
{
   Log(LOG_archive)   CERR << "      [" << d << "] unused name" << endl;

   if (d == 0)   return;   // Symbol::Symbol has already created the top level

   symbol.push();
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::read_Variable(int d, Symbol & symbol)
{
const int vid = find_int_attr("vid", false, 10);
   Assert(vid < int(values.size()));

   Log(LOG_archive)   CERR << "      [" << d << "] read_Variable() vid=" << vid
                           << " name=" << symbol.get_name() << endl;

   // some system variables are saved for troubleshooting purposes, but
   // should not be loaded...
   //
   if (symbol.is_readonly())                     return;
   if (symbol.get_name().starts_iwith("⎕NLT"))   return;   // extinct
   if (symbol.get_Id() == ID_Quad_SVE)           return;
   if (symbol.get_Id() == ID_Quad_SYL)           return;
   if (symbol.get_Id() == ID_Quad_PS)            return;

   if (vid == -1)   // stale variable
      {
        Log(LOG_archive)   CERR << "      " << symbol.get_name()
                                << " looks like a stale variable" << endl;
        return;
      }

   if (!values[vid])   return;   // value filtered out

   while (symbol.value_stack_size() <= d)   symbol.push();
// if (d != 0)   symbol.push();

   try
      {
        symbol.assign(values[vid], true, LOC);
      }
   catch (...)
      {
        CERR << "*** Could not assign value " << *values[vid]
             << "    to variable " << symbol.get_name() << " ***" << endl;
      }
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::read_Label(int d, Symbol & symbol)
{
const int value = find_int_attr("value", false, 10);
   if (d == 0)   symbol.pop();
   symbol.push_label(Function_Line(value));
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::read_Function()
{
const Fid fid = find_Fid_attr("fid", false, 16);
const TokenTag primitive_tag = TokenTag(find_int_attr("tag", true, 16));
   Assert(primitive_tag != TOK_NONE);
   cFunction_P pfun = ID::get_system_function(primitive_tag);
   Assert(pfun);
   add_fid_function(fid, pfun, LOC);
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::read_Function(int d, Symbol & symbol)
{
const Fid fid = find_Fid_attr("fid", false, 16);
const Macro::Macro_num macnum =
                       Macro::Macro_num(find_int_attr("macro", true, 10));
   if (macnum != Macro::MAC_NONE)   // function is a macro
      {
        Macro * macro = Macro::get_macro(macnum);
        Assert(macro);
        symbol.push_function(macro);
        add_fid_function(fid, macro, LOC);
        return;
      }

const TokenTag primitive_tag = TokenTag(find_int_attr("tag", true, 16));
   if (primitive_tag != TOK_NONE)   // function is an APL primitive
      {
        cFunction_P prim = ID::get_system_function(Id(primitive_tag >> 16));
        Assert(prim);
        symbol.push_function(prim);
        add_fid_function(fid, prim, LOC);
        return;
      }

const Fid LO_fid = find_Fid_attr("LO-fid", true, 16);
   if (LO_fid != -1)   // derived function
      {
        const Fid OPER_fid = find_Fid_attr("OPER-fid", false, 16);
        const Fid RO_fid = find_Fid_attr("RO-fid", true, 16);
        const Vid AXIS_vid = find_Vid_attr("AXIS-vid", true, 16);

        symbol.push();   // placeholder (for now)
        add_fid_function(fid, 0, LOC);

        _derived_todo td = { 0, symbol.top_of_stack()->get_function_P(),
                             fid, LO_fid, OPER_fid, RO_fid, AXIS_vid, LOC };
        derived_todos.push_back(td);
        return;
      }

   if (find_optional_attr("ref"))   // function pointer
      {
        cFunction_P fun = find_function(fid);
        Assert(fun);
        symbol.push_function(fun);
        return;
      }

const int native                = find_int_attr("native", true, 10);
const APL_time_us creation_time = find_int_attr("creation-time", true, 10);
int eprops[4] = { 0, 0, 0, 0 };

   if (const UTF8 * ep = find_optional_attr("exec-properties"))
      {
        sscanf(charP(ep), "%d,%d,%d,%d",
               eprops, eprops + 1, eprops + 2, eprops+ 3);
      }

   Log(LOG_archive)
      CERR << "      [" << d << "] read_Function(" << symbol.get_name()
           << ") native=" << native << endl;

   next_tag(LOC);
   expect_tag("UCS", LOC);
const UTF8 * utf = find_mandatory_attr("uni");
   next_tag(LOC);
   expect_tag("/Function", LOC);

UCS_string text;
    read_XML_string(text, utf);

   if (native == 1)
      {
        NativeFunction * nfun = NativeFunction::fix(text, symbol.get_name());
        if (nfun)   // fix succeeded
           {
             nfun->set_creation_time(creation_time);
             if (d == 0)   symbol.pop();
             symbol.push_function(nfun);
             add_fid_function(fid, nfun, LOC);
           }
        else        // fix failed
           {
             CERR << "   *** loading of native function " << text
                  << " failed" << endl << endl;
             if (d == 0)   symbol.pop();
             symbol.push();
           }
      }
   else
      {
        int err = 0;
        UTF8_string filename_utf(filename);
        UCS_string creator_UCS(filename_utf);
        creator_UCS.append(UNI_COLON);
        creator_UCS.append_number(line_no);
        UTF8_string creator(creator_UCS);

        UserFunction * ufun = 0;
        if (text[0] == UNI_LAMBDA)
           {
             ufun = UserFunction::fix_lambda(symbol, text);
             ufun->increment_refcount(LOC);   // since we pushed it below
           }
        else
           {
             ufun = UserFunction::fix(text, err, false, LOC, creator, false);
           }

        if (d == 0)   symbol.pop();
        if (ufun)
           {
             ufun->set_creation_time(creation_time);
             symbol.push_function(ufun);
             add_fid_function(fid, ufun, LOC);
             ufun->set_exec_properties(eprops);
           }
        else
           {
             CERR << "    ⎕FX " << symbol.get_name() << " failed: "
                  << Workspace::more_error() << endl;
             symbol.push();
             add_fid_function(fid, 0, LOC);
           }
      }
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::read_Derived(StateIndicator & si, int lev)
{
   Log(LOG_archive)   CERR << "  read_Derived()" << endl;

const Fid fid      = find_Fid_attr("fid", false, 16);   // in the )SAVEing WS
const Fid LO_fid   = find_Fid_attr("LO-fid",   false, 16);
const Fid OPER_fid = find_Fid_attr("OPER-fid", false, 16);
const Fid RO_fid   = find_Fid_attr("RO-fid",   true,  16);
const Vid AXIS_vid = find_Vid_attr("AXIS-vid", true,  10);
Function * derived = si.fun_oper_cache.get(LOC);

   if (LO_fid == NO_FID)
      {
        CERR << "got non-derived function when expecting a derived one" << endl;
        return;
      }

_derived_todo td = { derived, 0, fid, LO_fid, OPER_fid, RO_fid,
                     AXIS_vid, LOC };
   add_fid_function(fid, derived, LOC);
   derived_todos.push_back(td);
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::read_Shared_Variable(int d, Symbol & symbol)
{
// const SV_key key = find_int_attr("key", false, 10);
   if (d != 0)   symbol.push();

   CERR << "WARNING: workspace was )SAVEd with a shared variable "
        << symbol.get_name() << endl
        << " (shared variables are not restored by )LOAD or )COPY)" << endl;

   // symbol.share_var(key);
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::read_SymbolTable()
{
const int size = find_int_attr("size", false, 10);

   Log(LOG_archive)   CERR << "  read_SymbolTable()" << endl;

   loop(s, size)
      {
        next_tag(LOC);
        read_Symbol();
      }

   next_tag(LOC);
   expect_tag("/SymbolTable", LOC);
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::read_Symbol()
{
   expect_tag("Symbol", LOC);

const UTF8 * name = find_mandatory_attr("name");
const UTF8 * name_end = name;
   while (*name_end != '"')   ++name_end;

UTF8_string name_UTF(name, name_end - name);
UCS_string  name_UCS(name_UTF);
   if (name_UCS.size() == 0)
      {
        CERR << "*** Warning: empty Symbol name in XML archive " << filename
             << " around line " << line_no << endl;
        skip_to_tag("/Symbol");
        return;
      }

   Log(LOG_archive)   CERR << "    read_Symbol() name=" << name_UCS << endl;

   // ⎕NLT and ⎕PT were removed, but could lurk around in old workspaces.
   // ⎕PW and ⎕TZ are session variables that must not )LOADed (but might be
   // )COPYd)
   //
   if (name_UTF == UTF8_string("⎕NLT") || name_UTF == UTF8_string("⎕PT"))
      {
        Log(LOG_archive)   CERR << "        skipped at " << LOC << endl;
        skip_to_tag("/Symbol");
        return;
      }

const int depth = find_int_attr("stack-size", false, 10);

   // lookup symbol, trying ⎕xx first
   //
Symbol * symbol;
   if (name_UCS == ID::get_name_UCS(ID_LAMBDA))
      symbol = &Workspace::get_v_LAMBDA();
   else if (name_UCS == ID::get_name_UCS(ID_ALPHA))
      symbol = &Workspace::get_v_ALPHA();
   else if (name_UCS == ID::get_name_UCS(ID_ALPHA_U))
      symbol = &Workspace::get_v_ALPHA_U();
   else if (name_UCS == ID::get_name_UCS(ID_CHI))
      symbol = &Workspace::get_v_CHI();
   else if (name_UCS == ID::get_name_UCS(ID_OMEGA))
      symbol = &Workspace::get_v_OMEGA();
   else if (name_UCS == ID::get_name_UCS(ID_OMEGA_U))
      symbol = &Workspace::get_v_OMEGA_U();
   else
      symbol = Workspace::lookup_existing_symbol(name_UCS);

   // we do NOT copy if:
   //
   // 1. )PCOPY and the symbol exists, or 
   // 2.  there is an object list and this symbol is not contained in the list
   //
const bool is_protected = symbol && protection;
const bool is_selected = allowed_objects.contains(name_UCS);
bool no_copy = is_protected || (have_allowed_objects && !is_selected);

   if (reading_vids)
      {
        // we prepare vids for )COPY or )PCOPY, so we do not create a symbol
        // and care only for the top level
        //
        if (no_copy || (depth == 0))
           {
             Log(LOG_archive)   CERR << "        skipped at " << LOC << endl;
             skip_to_tag("/Symbol");
             return;
           }

        // we have entries and copying is allowed
        //
        next_tag(LOC);
        if (is_tag("Variable"))
           {
             const Vid vid = find_Vid_attr("vid", false, 10);
             vids_COPY.push_back(vid);
           }
        skip_to_tag("/Symbol");
        return;
      }

   // in a )COPY without dedicated objects only
   // ⎕CT, ⎕FC, ⎕IO, ⎕LX, ⎕PP, ⎕PR, and ⎕RL shall be copied
   //
   if (!have_allowed_objects       &&   // no dedicated object list
        copying                    &&   // )COPY
        (name_UCS == ID::get_name_UCS(ID_Quad_CT) ||
         name_UCS == ID::get_name_UCS(ID_Quad_FC) ||
         name_UCS == ID::get_name_UCS(ID_Quad_IO) ||
         name_UCS == ID::get_name_UCS(ID_Quad_LX) ||
         name_UCS == ID::get_name_UCS(ID_Quad_PP) ||
         name_UCS == ID::get_name_UCS(ID_Quad_PR) ||
         name_UCS == ID::get_name_UCS(ID_Quad_RL)
        ))
      {
        Log(LOG_archive)   CERR << name_UCS << " not copied at " << LOC << endl;
        no_copy = true;
      }

   // in a )LOAD silently ignore session variables (⎕PW and ⎕TZ)
   //
   if (!copying &&   // )LOAD
        (name_UCS == ID::get_name_UCS(ID_Quad_PW) ||
         name_UCS == ID::get_name_UCS(ID_Quad_TZ)))
      {
        skip_to_tag("/Symbol");
        return;
      }

   if (copying)
      {
        if (no_copy || (depth == 0))
           {
             skip_to_tag("/Symbol");
             return;
           }
      }

   // remove this symbol from allowed_objects so that we can print NOT COPIED
   // at the end for all objects that are still in the list.
   //
   loop(a, allowed_objects.size())
      {
        if (allowed_objects[a] == name_UCS)
             {
               allowed_objects[a] = allowed_objects.back();
               allowed_objects.pop_back();
               break;
           }
      }

   if (symbol == 0)
      {
        symbol = Workspace::lookup_symbol(name_UCS);
      }
   Assert(symbol);

   loop(d, depth)
      {
        // for )COPY skip d > 0
        //
        if (copying && (d > 0))
           {
             skip_to_tag("/Symbol");
             return;
           }

        next_tag(LOC);
        if      (is_tag("unused-name"))       read_unused_name(d, *symbol);
        else if (is_tag("Variable"))          read_Variable(d, *symbol);
        else if (is_tag("Function"))          read_Function(d, *symbol);
        else if (is_tag("Label"))             read_Label(d, *symbol);
        else if (is_tag("Shared-Variable"))   read_Shared_Variable(d, *symbol);
      }

   Assert(symbol->value_stack_size() == depth);
   next_tag(LOC);
   expect_tag("/Symbol", LOC);
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::read_Commands()
{
const int size = find_int_attr("size", false, 10);

   Log(LOG_archive)   CERR << "  read_Commands()" << endl;

   loop(s, size)
      {
        next_tag(LOC);
        read_Command();
      }

   next_tag(LOC);
   expect_tag("/Commands", LOC);
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::read_Command()
{
   expect_tag("Command", LOC);

const UTF8 * name = find_mandatory_attr("name");
const UTF8 * name_end = name;
   while (*name_end != '"')   ++name_end;
UTF8_string name_UTF(name, name_end - name);
UCS_string  name_UCS(name_UTF);

const UTF8 * fun = find_mandatory_attr("fun");
const UTF8 * fun_end = fun;
   while (*fun_end != '"')   ++fun_end;
UTF8_string fun_UTF(fun, fun_end - fun);
UCS_string  fun_UCS(fun_UTF);

const int mode = find_int_attr("mode", false, 10);

Command::user_command ucmd = { name_UCS, fun_UCS, mode };
   Workspace::get_user_commands().push_back(ucmd);
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::read_StateIndicator()
{
   if (copying)
      {
        skip_to_tag("/StateIndicator");
        return;
      }

   Log(LOG_archive)   CERR << "read_StateIndicator()" << endl;

const int levels = find_int_attr("levels", false, 10);

   loop(l, levels)
      {
        next_tag(LOC);
        expect_tag("SI-entry", LOC);

        try
           {
             read_SI_entry(l);
           }
        catch (...)
           {
             CERR <<
"\n"
"*** SORRY! An error occured while reading the )SI stack of the )SAVEd\n"
"    workspace. The )SI stack was reconstructed to the extent possible.\n"
"    We stronly recommend to perform )SIC and then )DUMP the workspace under\n"
"    a different name.\n" << endl;

             // skip rest of <StateIndicator>
             //
             skip_to_tag("/StateIndicator");
             return;
           }

        // the parsers loop eats the terminating /SI-entry
      }

   instantiate_derived_functions(true);
   instantiate_derived_functions(false);

   next_tag(LOC);
   expect_tag("/StateIndicator", LOC);
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::read_SI_entry(int lev)
{
const int level = find_int_attr("level", false, 10);
const int pc = find_int_attr("pc", false, 10);

   Log(LOG_archive)   CERR << "    read_SI_entry() level=" << level << endl;

const Executable * exec = 0;
   next_tag(LOC);
   if      (is_tag("Execute"))        exec = read_Execute();
   else if (is_tag("Statements"))     exec = read_Statement();
   else if (is_tag("UserFunction"))   exec = read_UserFunction();
   else    Assert(0 && "Bad tag at " LOC); 

   Assert(lev == level);
   Assert(exec);

   Workspace::push_SI(exec, LOC);
StateIndicator * si = Workspace::SI_top();
   Assert(si);
   si->get_prefix().goto_PC(Function_PC(pc));
   read_Parser(*si, lev);

   for (;;)
       {
         // skip old EOC tags
         //
         next_tag(LOC);
         if (is_tag("/SI-entry"))   break;
       }
}
//----------------------------------------------------------------------------
const Executable *
XML_Loading_Archive::read_Execute()
{
   next_tag(LOC);
   expect_tag("UCS", LOC);

const UTF8 * utf = find_mandatory_attr("uni");
UCS_string text;
   read_XML_string(text, utf);
   next_tag(LOC);
   expect_tag("/Execute", LOC);

ExecuteList * exec = ExecuteList::fix(text, LOC);
   Assert(exec);
   return exec;
}
//----------------------------------------------------------------------------
const Executable *
XML_Loading_Archive::read_Statement()
{
   next_tag(LOC);
   expect_tag("UCS", LOC);
const UTF8 * utf = find_mandatory_attr("uni");
UCS_string text;
   read_XML_string(text, utf);

   next_tag(LOC);
   expect_tag("/Statements", LOC);

StatementList * exec = StatementList::fix(text, LOC);
   Assert(exec);
   return exec;
}
//----------------------------------------------------------------------------
const Executable *
XML_Loading_Archive::read_UserFunction()
{
const int macro_num = find_int_attr("macro-num", true, 10);
   if (macro_num != -1)
      return Macro::get_macro(Macro::Macro_num(macro_num));

const UTF8 * lambda_name = find_optional_attr("lambda-name");
   if (lambda_name)   return read_lambda(lambda_name);

const int level     = find_int_attr("symbol-level", false, 10);
const UTF8 * name   = find_mandatory_attr("ufun-name");
const UTF8 * n  = name;
   while (*n != '"')   ++n;
UTF8_string name_UTF(name, n - name);
UCS_string name_UCS(name_UTF);

Symbol * symbol = Workspace::lookup_symbol(name_UCS);
   Assert(symbol);
   Assert(level >= 0);
   Assert(level < symbol->value_stack_size());
ValueStackItem & vsi = (*symbol)[level];
   Assert(vsi.get_NC() == NC_FUNCTION || vsi.get_NC() == NC_OPERATOR);
cFunction_P fun = vsi.get_function();
   Assert(fun);
const UserFunction * ufun = fun->get_func_ufun();
   Assert(fun == ufun);

   return ufun;
}
//----------------------------------------------------------------------------
Executable *
XML_Loading_Archive::read_lambda(const UTF8 * lambda_name)
{
UCS_string lambda = read_UCS();

Symbol dummy(ID_No_ID);
UserFunction * ufun = UserFunction::fix_lambda(dummy, lambda);
   Assert(ufun);
   ufun->increment_refcount(LOC);   // since we push it below

   next_tag(LOC);
   expect_tag("/UserFunction", LOC);
   return ufun;
}
//----------------------------------------------------------------------------
UCS_string
XML_Loading_Archive::read_UCS()
{
   skip_to_tag("UCS");
const UTF8 * utf = find_mandatory_attr("uni");
UCS_string text;
   read_XML_string(text, utf);
   return text;
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::read_Parser(StateIndicator & si, int lev)
{
   next_tag(LOC);
   expect_tag("Parser", LOC);

   Log(LOG_archive)   CERR << "        read_Parser() level=" << lev << endl;

const int stack_size = find_int_attr("size",           false, 10);
const int ass_state  = find_int_attr("assign-pending", false, 10);
const int lah_high   = find_int_attr("lookahead-high", false, 10);
const int action     = find_int_attr("action",         false, 10);

Prefix & parser = si.current_stack;

   parser.set_assign_state(Assign_state(ass_state));
   parser.action = R_action(action);
   parser.set_lookahead_PC(Function_PC(lah_high));

   // read derived functions cache
   //
   for (;;)
       {
         next_tag(LOC);
         if (is_tag("/Parser"))   break;
         if (is_tag("Token"))     break;

          read_Derived(si, lev);
       }

   // read parser token
   //
   for (;;)
       {
         next_tag(LOC);
         if (is_tag("/Parser"))   break;

         if (is_tag("Token"))
            {
              Token_loc tl;
              read_Token(tl);

              if (parser.size() < stack_size)   parser.push(tl);
              else                    parser.saved_MISC.copy(tl, LOC);
            }
       }

   Log(LOG_archive)
      {
         CERR << "        ";
         parser.print_stack(CERR, LOC);
      }

   expect_tag("/Parser", LOC);
}
//----------------------------------------------------------------------------
bool
XML_Loading_Archive::read_Token(Token_loc & tloc)
{
   expect_tag("Token", LOC);

   tloc.set_PC(Function_PC(find_int_attr("pc", false, 10)));

const TokenTag tag = TokenTag(find_int_attr("tag", false, 16));

   switch(tag & TV_MASK)   // cannot call get_ValueType() yet
      {
        case TV_NONE: 
               new (&tloc.get_token()) Token(tag);
             break;

        case TV_CHAR:
             {
               const Unicode uni = Unicode(find_int_attr("char", false, 10));
               new (&tloc.get_token()) Token(tag, uni);
             }
             break;

        case TV_INT:   
             {
               const int64_t ival = find_int_attr("int", false, 10);
               new (&tloc.get_token()) Token(tag, ival);
             }
             break;

        case TV_FLT:   
             {
               const APL_Float val = find_float_attr("float");
               new (&tloc.get_token()) Token(tag, val);
             }
             break;

        case TV_CPX:
             {
               const APL_Float real = find_float_attr("real");
               const APL_Float imag = find_float_attr("imag");
               new (&tloc.get_token()) Token(tag, real, imag);
             }
             break;

        case TV_SYM:   
             {
               const UTF8 * sym_name = find_mandatory_attr("sym");
               const UTF8 * end = sym_name;
               while (*end != '"')   ++end;
               UTF8_string name_UTF(sym_name, end - sym_name);
               UCS_string name_UCS(name_UTF);

               Symbol * symbol = Avec::is_quad(name_UCS[0])
                               ? Workspace::lookup_existing_symbol(name_UCS)
                               : Workspace::lookup_symbol(name_UCS);
               new (&tloc.get_token()) Token(tag, symbol);
             }
             break;

        case TV_LIN:   
             {
               const int ival = find_int_attr("line", false, 10);
               new (&tloc.get_token()) Token(tag, Function_Line(ival));
             }
             break;

        case TV_VAL:   
             {
               const int vid = find_int_attr("vid", false, 10);
               Assert(vid < int(values.size()));
               new (&tloc.get_token()) Token(tag, values[vid]);
             }
             break;

        case TV_INDEX: 
             {
               const UTF8 * vids = find_mandatory_attr("index");
               IndexExpr & idx = *new IndexExpr(ASS_none, LOC);
               while (*vids != '"')
                  {
                    if (*vids == ',')   ++vids;
                    if (*vids == '-')   // elided index
                       {
                         idx.add_index(Value_P());
                       }
                    else                // value
                       {
                         char * end = 0;
                         Assert1(*vids == 'v');   ++vids;
                         Assert1(*vids == 'i');   ++vids;
                         Assert1(*vids == 'd');   ++vids;
                         Assert1(*vids == '_');   ++vids;
                         const int vid = strtoll(charP(vids), &end, 10);
                         Assert(vid < int(values.size()));
                         idx.add_index(values[vid]);
                         vids = utf8P(end);
                       }
                  }
               new (&tloc.get_token()) Token(tag, idx);
             }
             break;

        case TV_FUN:
             {
               cFunction_P fun = read_Function_name();
               Assert(fun);
               new (&tloc.get_token()) Token(tag, fun);
             }
             break;

        default: FIXME;
      }

   return true;
}
//----------------------------------------------------------------------------
cFunction_P
XML_Loading_Archive::read_Function_name()
{
const UTF8 * fun_name = find_optional_attr("ufun-name");

   if (fun_name)   // user defined function
      {
        const int level = find_int_attr("symbol-level", false, 10);
        const UTF8 * end = fun_name;
        while (*end != '"')   ++end;
        UTF8_string name_UTF(fun_name, end - fun_name);
        UCS_string name_UCS(name_UTF);
        if (name_UCS == ID::get_name_UCS(ID_LAMBDA))
           {
             Assert(level == -1);
             return find_lambda(name_UCS);
           }

        const Symbol & symbol = *Workspace::lookup_symbol(name_UCS);

        Assert(level >= 0);
        Assert(level < symbol.value_stack_size());
        const ValueStackItem & vsi = symbol[level];
        Assert(vsi.get_NC() == NC_FUNCTION);
        cFunction_P fun = vsi.get_function();
        Assert(fun);
        return fun;
      }

const int fun_id = find_int_attr("fun-id", true, 16);
   if (fun_id != -1)
      {
        cFunction_P sysfun = ID::get_system_function(Id(fun_id));
        Assert(sysfun);
        return sysfun;
      }

   // not found. This can happen when the function is optional.
   //
   return 0;
}
//----------------------------------------------------------------------------
XML_Loading_Archive::fun_map *
XML_Loading_Archive::find_fun_map(Fid fid)
{
   loop(f, fid_to_function.size())
       {
         if (fid_to_function[f].old_fid == fid)   return &fid_to_function[f];
       }

   return 0;
}
//----------------------------------------------------------------------------
cFunction_P
XML_Loading_Archive::find_function(Fid fid)
{
   if (fun_map * map = find_fun_map(fid))   return map->new_fun;
   return 0;
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::add_fid_function(Fid fid, cFunction_P new_fun,
                                      const char * loc)
{
   if (fun_map * map = find_fun_map(fid))   // fid exists
      {
        if (map->new_fun)   // function also exists.
           {
             // function already exists. Setting it multiple times is OK
             // as long as the pointers are the same (e.g. pointer to
             // primitive APL functions).
             //
             if (new_fun == map->new_fun)   return;

             CERR << "*** OVERRIDING fid " << HEX(fid)
                  << " (from " << map->loc << ") at " << loc << "***" << endl;
           }
        else
           {
             map->new_fun = new_fun;
             map->loc = loc;
           }

        return;
      }

   // new fid/function
   //
const fun_map fm =  { fid, new_fun, loc };
   fid_to_function.push_back(fm);
}
//----------------------------------------------------------------------------
void
XML_Loading_Archive::instantiate_derived_functions(bool allocate)
{
   /* derived functions are allocated i 2 phases:

      phase 1: allocate=true and todo.cache set: create the function
      phase 2: allocate=false and todo.symptr set: store pointer
    */

   loop(d, derived_todos.size())
      {
        _derived_todo & todo = derived_todos[d];
        Assert(todo.fid != -1);

        cFunction_P fun = find_function(todo.fid);
        Assert(fun);

        if (!allocate)
           {
             if (todo.symptr)   *todo.symptr = fun;
             continue;
           }

        if (!todo.cache)   continue;

        Assert(todo.LO_fid   != -1);
        cFunction_P LO = find_function(todo.LO_fid);
        Assert(LO);

        Assert(todo.OPER_fid != -1);
        cFunction_P OPER = find_function(todo.OPER_fid);
        Assert(OPER);
        Token tok_LO(TOK_FUN2, LO);

        if (todo.RO_fid   != -1)   // dyadic operator
           {
             Assert(todo.RO_fid != -1);
             cFunction_P RO = find_function(todo.RO_fid);
             Assert(RO);
             Token tok_RO(TOK_FUN2, RO);

             if (todo.AXIS_vid == -1)   // dyadic operator without axis
                {
                  new (todo.cache) DerivedFunction(tok_LO, OPER, tok_RO, LOC);
                }
             else                       // dyadic operator with axis
                {
                  Value_P val_X = values[todo.AXIS_vid];
                  new (todo.cache) DerivedFunction(tok_LO, OPER, val_X,
                                                   tok_RO, LOC);
                }
           }
        else                       // monadic operator
           {
             if (todo.AXIS_vid == -1)   // monadic operator without axis
                {
                  new (todo.cache) DerivedFunction(tok_LO, OPER, LOC);
                }
             else                       // monadic operator with axis
                {
                  Value_P val_X = values[todo.AXIS_vid];
                  new (todo.cache) DerivedFunction(tok_LO, OPER, val_X, LOC);
                }
           }
      }
}
//----------------------------------------------------------------------------
cFunction_P
XML_Loading_Archive::find_lambda(const UCS_string & lambda)
{
const StateIndicator & si = *Workspace::SI_top();
const Executable & exec = *si.get_executable();
const Token_string & body = exec.get_body();

   loop(b, body.size())
      {
        const Token & tok = body[b];
        if (tok.get_ValueType() == TV_SYM)
           {
             const Symbol * sym = tok.get_sym_ptr();
             Assert(sym);
             loop(v, sym->value_stack_size())
                {
                  const ValueStackItem & vs = (*sym)[v];
                  if (vs.get_NC() & NC_FUN_OPER)
                     {
                       if (vs.get_function()->get_name() == lambda)
                          {
                            return vs.get_function();
                          }
                     }
                }
             continue;   // not found
           }
        else if (tok.get_ValueType() != TV_FUN)   continue;

        cFunction_P fun = tok.get_function();
        Assert1(fun);
        const UserFunction * ufun = fun->get_func_ufun();
        if (!ufun)   continue;   // not a user defined function

        const UCS_string & fname = ufun->get_name();
        if (fname == lambda)   return fun;
      }

   CERR << "find_lambda() failed for " << lambda
        << " at )SI level=" << si.get_level() << endl;
   return 0;
}
//============================================================================

