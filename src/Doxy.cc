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
#include <errno.h>
#include <sys/stat.h>

#include "Bif_F12_TAKE_DROP.hh"
#include "Doxy.hh"
#include "Heapsort.hh"
#include "Logging.hh"
#include "NativeFunction.hh"
#include "PrintOperator.hh"
#include "StateIndicator.hh"
#include "UserFunction.hh"
#include "UTF8_string.hh"
#include "Workspace.hh"

using namespace std;

#define BRLF "<BR>\r\n"
#define CRLF "\r\n"

//----------------------------------------------------------------------------
/// a helper for sorting Symbol * by symbol name
static bool
symcomp(const Symbol * const & s1, const Symbol * const & s2, const void *)
{
   return s2->get_name().compare(s1->get_name()) == COMP_LT;
}
//----------------------------------------------------------------------------
Doxy::Doxy(ostream & cout, const UCS_string & dest_dir)
   : out(cout),
     root_dir(dest_dir),
     errors(0)
{
   ws_name = Workspace::get_WS_name();
   if (ws_name.compare(UCS_ASCII_string("CLEAR WS")) == COMP_EQ)
      {
        ws_name = UCS_ASCII_string("CLEAR-WS");
      }
   else if (Avec::is_digit(ws_name[0]))
      {
         // workspace name starts with a library reference number. Skip the
         // library reference number and the whitespace directly after it
         //
         int pos = 1;   // first blank after the library reference number
         while (pos < ws_name.size() && Avec::is_white(ws_name[pos]))   ++pos;
         ws_name = UCS_string(ws_name, pos, ws_name.size() - pos);
      }

   root_dir.append_ASCII("/");
   root_dir.append_UTF8(UTF8_string(ws_name));

   Log(LOG_command_DOXY)
      out << "Creating output directory " << root_dir << endl;

   errno = 0;
   if (mkdir(root_dir.c_str(), 0777))
      {
        const char * why = strerror(errno);

        // EEXIST says "File exists" which is a little misleading. We
        // therefore say ""Directory already exists".
        //
        if (errno == EEXIST)   why = "Directory already exists";

        CERR << "Cannot create fresh destination directory "
             << root_dir << ": " << why << endl;
        ++errors;
        DOMAIN_ERROR;
      }
   write_css();
}
//----------------------------------------------------------------------------
void
Doxy::write_css()
{
UTF8_string css_filename(root_dir);
   css_filename.append_ASCII("/apl_doxy.css");

   Log(LOG_command_DOXY)
      out << "Writing style sheet file " << css_filename << endl;

ofstream css(css_filename.c_str());
   css <<
"/* GNU APL Doxy css file */"                                             CRLF
"BODY, TABLE.h1tab"                                                       CRLF
"  { background-color: #D0D0FF; }"                                        CRLF
"TABLE.h1tab"                                                             CRLF
"  { width: 100%; border: 0;"                                             CRLF
"    margin-left:auto; margin-right:auto; }"                              CRLF
"TD.h1tab { border: 0; }"                                                 CRLF
"TABLE    { background-color: #FFFFFF; }"                                 CRLF
"H1, H3, TH, .center"                                                     CRLF
"   { text-align: center; }"                                              CRLF
"H3       { width: 100%; text-align: center; }"                           CRLF
"TH, TD   { padding-left: 0.5em; padding-right: 0.5em; }"                 CRLF
".code    { font-family: monospace; }"                                    CRLF
"TABLE, TH, TD { border: 1px solid black; }"                              CRLF
".onefuntab { margin-left:auto; margin-right:auto; }"                     CRLF
".funtab, .vartab, .sitab"                                                CRLF
"  { margin-left:auto; margin-right:auto;"                                CRLF
"    border-collapse: collapse; }"                                        CRLF
".cg1, .cg2"                                                              CRLF
"  { margin:auto; display: block; }"                                      CRLF
".doxy_comment { width: 40%; }"                                           CRLF;

   css.close();
}
//----------------------------------------------------------------------------
void
Doxy::gen()
{
const SymbolTable & symtab = Workspace::get_symbol_table();

std::basic_string<const Symbol *> all_symbols = symtab.get_all_symbols();
std::basic_string<const Symbol *> variables;

   loop(a, all_symbols.size())
      {
        const Symbol * sym = all_symbols[a];
        if (sym->is_erased())                continue;
        if (sym->get_name()[0] == UNI_MUE)   continue;   // macro

        bool is_function = false;
        bool is_variable = false;
        loop(si, sym->value_stack_size())
            {
              switch((*sym)[si].get_NC())
                 {
                   case NC_VARIABLE: is_variable = true;   break;
                   case NC_FUNCTION: is_function = true;   break;
                   case NC_OPERATOR: is_function = true;   break;
                   default: ;
                 }
            }
        if (is_function)
           all_functions.push_back(sym);


        if (is_variable)
           variables.push_back(sym);
      }

   if (all_functions.size() > 1)
      Heapsort<const Symbol *>::
              sort(&all_functions[0], all_functions.size(), 0, symcomp);

   if (variables.size() > 1)
      Heapsort<const Symbol *>::
              sort(&variables[0], variables.size(), 0, symcomp);

UTF8_string index_filename(root_dir);
   index_filename.append_ASCII("/index.html");

   Log(LOG_command_DOXY)
      out << "Writing top-level HTML file " << index_filename << endl;

ofstream page(index_filename.c_str());
   page <<
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\""                     CRLF
"                      \"http://www.w3.org/TR/html4/strict.dtd\">"        CRLF
"<HTML>"                                                                  CRLF
"  <HEAD>"                                                                CRLF
"    <TITLE>Documentation of " << ws_name << "</title> "                  CRLF
"    <META http-equiv=\"content-type\" "                                  CRLF
"          content=\"text/html; charset=UTF-8\">"                         CRLF
"   <LINK rel='stylesheet' type='text/css' href='apl_doxy.css'>"          CRLF
" </HEAD>"                                                                CRLF
" <BODY>"                                                                 CRLF
"  <H1>Documentation of workspace " << ws_name << "</H1><HR>"             CRLF;

   make_call_graph(all_functions);
   functions_table(all_functions, page);
   variables_table(variables, page);
   SI_table(page);

   set_call_graph_root(0);
const UCS_string alias = UTF8_string("all_functions");
   if (write_call_graph(0, alias, false) == 0)
      {
        page <<
"   <H3>Global Function Call Graph</H3>"                                  CRLF
"    <IMG class=cg1 usemap=#CG src=cg_" << alias << ".png>"               CRLF;

         // copy the cmap file for the IMG (generated by dot) into the page
         {
           UTF8_string cmapx_filename(root_dir);
           cmapx_filename.append_ASCII("/cg_");
           cmapx_filename.append_UTF8(UTF8_string(alias));
           cmapx_filename.append_ASCII(".cmapx");
           if (FILE * cmap = fopen(cmapx_filename.c_str(), "r"))
              {
                char buffer[400];
                for (;;)
                    {
                      const char * s = fgets(buffer, sizeof(buffer), cmap);
                      if (s == 0)   break;
                      buffer[sizeof(buffer) - 1] = 0;
                      page << "    " << buffer;
                    }
                fclose(cmap);
              }
           else
              {
                CERR << "*** Cannot open " << cmapx_filename
                     << ": " << strerror(errno) << endl;
              }

           Log(LOG_command_DOXY) {} else unlink(cmapx_filename.c_str());
         }
      }

   page <<
" </BODY>"                                                                CRLF
"</HTML>"                                                                 CRLF;
   page.close();
}
//----------------------------------------------------------------------------
void
Doxy::functions_table(const std::basic_string<const Symbol *> & functions,
                      ofstream & page)
{
   if (functions.size() == 0)   return;   // no functions in the workspace

   page <<
"   <H3>Defined Functions</H3>"                                           CRLF
"   <TABLE class=funtab>"                                                 CRLF
"     <TR>"                                                               CRLF
"      <TH>Function"                                                      CRLF
"      <TH>L"                                                             CRLF
"      <TH>SI"                                                            CRLF
"      <TH>Lines"                                                         CRLF
"      <TH>Header"                                                        CRLF
"      <TH class=doxy_comment>Doxy Comments"                              CRLF;

size_t total_lines = 0;
   loop(f, functions.size())
       {
         functions_table_entry(functions[f], page, total_lines);
       }

   // summary line
   //
   page <<
"      <TR><TD><TD colspan=2 class='code center'>Total<TD class='code center'>"
        << total_lines <<                                                 CRLF
"   </TABLE>"                                                             CRLF;
}
//----------------------------------------------------------------------------
void
Doxy::functions_table_entry(const Symbol * fun_sym, ofstream & page,
                            size_t & total_lines)
{
   loop(si, fun_sym->value_stack_size())
       {
         cFunction_P fun = fun_sym->get_function(si);
         const UserFunction * ufun = fun->get_func_ufun();
         if (fun == 0)   continue;

         const int si_level = fun_sym->get_SI_level(fun);

         // colunm 1: function name/link
         //
         page <<
"     <TR>"                                                               CRLF
"      <TD class=code>";

         int line_count = 0;
         if (fun->is_native())
            {
              native_page(fun, fun_sym->get_name());
              page << fun_anchor(fun_sym->get_name()) << " (native)"      CRLF;
            }
         else if (fun->is_lambda())
            {
              line_count = 1;
              function_page(ufun, fun_sym->get_name());
              page << fun_anchor(fun_sym->get_name()) <<                  CRLF;
            }
         else if (ufun)
            {
              line_count = ufun->get_text_size();
              const UserFunction * ufun = fun->get_func_ufun();
              function_page(ufun, ufun->get_name());
              page << fun_anchor(fun_sym->get_name()) <<                  CRLF;
            }
         else
            {
              page << fun_sym->get_name() <<                              CRLF;
            }

         // column 2: local stack depth
         //
         page <<
"      <TD class='code center'>" << si <<                                 CRLF;

              // column 3: SI level
              //
              if (si_level)   page <<
"      <TD class='code center'>" << si_level <<                           CRLF;
              else            page <<
"      <TD class='code center'>G"                                         CRLF;

              // column 4: number of lines
              //
              page <<
"      <TD class='code center'>" << line_count <<                         CRLF;

              total_lines += line_count;
              // column 5: function header (if any)
              //
              page <<
"      <TD class=code>";
              if (fun->is_native())
                 page << reinterpret_cast<const NativeFunction *>
                                               (fun)->get_so_path();
              else if (fun->is_lambda())
                 {
                   Assert(ufun);
                   const UCS_string & text = ufun->get_text(1);
                   page << "{" << UCS_string(text, 2, text.size() - 2)
                        << "}"                                            CRLF;
                 }
              else if (ufun)
                 bold_name(page, ufun);
              else
                 page << "-";
              page <<                                                     CRLF;

         // column 6: Doxygen comments
         //
         page <<
"      <TD class=doxy_comment>";
         if (fun->is_lambda())
            {
              page << "(named λ)"                                         CRLF;
            }
         else if (ufun && !fun->is_native())
            {
              loop(l, ufun->get_text_size())
                  {
                    UCS_string text = ufun->get_text(l);
                    text.remove_leading_and_trailing_whitespaces();
                    if (text.size() >= 2 &&
                        text[0] == UNI_COMMENT &&
                        text[1] == UNI_COMMENT)
                       page << text.to_HTML(2, false) <<                  CRLF;
                  }
            }
         else
            {
              page << "-"                                                 CRLF;
            }
       }
}
//----------------------------------------------------------------------------
void
Doxy::variables_table(const std::basic_string<const Symbol *> & variables,
                       ofstream & page)
{
   if (variables.size() == 0)   return;

   page <<
"   <H3>Variables</H3>"                                                   CRLF
"   <TABLE class=vartab>"                                                 CRLF
"     <TR>"                                                               CRLF
"      <TH>Variable"                                                      CRLF
"      <TH>L"                                                             CRLF
"      <TH>SI"                                                            CRLF
"      <TH>⍴⍴"                                                            CRLF
"      <TH>⍴"                                                             CRLF
"      <TH>≡"                                                             CRLF
"      <TH>Type"                                                          CRLF
"      <TH>↑ϵ"                                                            CRLF;
   loop(v, variables.size())
      {
        const Symbol & var_sym = *variables[v];
        loop(si, var_sym.value_stack_size())
            {
              if (var_sym[si].get_NC() != NC_VARIABLE)   continue;

              const Value * value = var_sym[si].get_val_cptr();
              const int si_level  = var_sym.get_SI_level(*value);
              Value_P elem = Bif_F12_ELEMENT::do_eval_B(value);
              Value_P first = Bif_F12_TAKE::first(*elem.get());
              page <<
"     <TR>"                                                               CRLF
"      <TD class=code>" << var_sym.get_name() <<                          CRLF
"      <TD class='code center'>" << si <<                                 CRLF;
              if (si_level)   page <<
"      <TD class='code center'>" << si_level <<                           CRLF;
              else            page <<
"      <TD class='code center'>G"                                         CRLF;
              page <<
"      <TD class='code center'>" << value->get_rank() <<                  CRLF
"      <TD class='code center'>";
              if (value->get_rank())
                 {
                   loop(r, value->get_rank())
                       page << " " << value->get_shape_item(r);
                   page <<                                                CRLF;
                 }
              else
                 {
                  page << "⍬"                                             CRLF;
                 }

              page <<                                                     CRLF
"      <TD class='code center'>" << value->compute_depth() <<             CRLF;

              const CellType ct = value->deep_cell_types();
                 page <<
"      <TD class=code>";
              if ((ct & CT_NUMERIC) && (ct & CT_CHAR)) page << "Mixed"    CRLF;
              else if (ct & CT_NUMERIC)                page << "Numeric"  CRLF;
              else if (ct & CT_CHAR)                  page << "Character" CRLF;
              else                                     page << "? ? ?"    CRLF;

              page <<                                                     CRLF
"      <TD class='code center'>" << *first <<                             CRLF;
            }
      }
   page <<
"   </TABLE>"                                                             CRLF;
}
//----------------------------------------------------------------------------
void
Doxy::SI_table(ofstream & page)
{
   // collect SI entries in reverse order...
   //
std::basic_string<const StateIndicator *> stack;

   for (const StateIndicator * si = Workspace::SI_top();
        si; si = si->get_parent())
      {
        stack.push_back(si);
      }

   if (stack.size() == 0)   return;

   page <<
"   <H3>SI Stack</H3>"                                                    CRLF
"   <TABLE class=sitab>"                                                  CRLF
"     <TR>"                                                               CRLF
"      <TH>SI"                                                            CRLF
"      <TH>T"                                                             CRLF
"      <TH>Fun[line]"                                                     CRLF
"      <TH>Statement"                                                     CRLF
"      <TH>Local"                                                         CRLF
"      <TH>Error"                                                         CRLF;

   loop(d, stack.size())
       {
         const StateIndicator * si = stack[stack.size() - d - 1];
         const Executable * exec = si->get_executable();
         Assert(exec);
         const UserFunction * ufun = exec->get_exec_ufun();   // possibly 0!

         page <<
"     <TR>"                                                               CRLF
"      <TD class='code center'>" << si->get_level() <<                    CRLF
"      <TD class='code center'>" << si->get_parse_mode_name() <<          CRLF;
          if (si->get_parse_mode() == PM_FUNCTION)
             {
               Assert(ufun);
               Function_PC PC = si->get_prefix().get_range_high();
               if (const ErrorCode ec =
                         StateIndicator::get_error(si).get_error_code())
                  {
                    // get_error_line_2() is something like fun[line] statement.
                    // find the space after fun[line].
                    //
                    int spc2 = 0;
                    const UCS_string el2(UTF8_string(
                          StateIndicator::get_error(si).get_error_line_2()));
                    loop(e, el2.size())
                        {
                          if (el2[e] == ']')
                             {
                               spc2 = e + 1;
                               break;
                             }
                        }

                    int spe2 = spc2;
                    while (spe2 < el2.size() &&
                           el2[spe2] == UNI_SPACE)   ++spe2;
                    const int spl2 = el2.size() - spe2;
                    const UCS_string & el3 =
                          StateIndicator::get_error(si).get_error_line_3();

                    // put fun[line] into one <TD> and the failed statement into
                    // the next <TD>
                    //
                    page <<
"      <TD class=code>"  << UCS_string(el2, 0, spc2) <<                   CRLF
"      <TD class=code>"  << UCS_string(el2, spe2, spl2) <<                BRLF
"           "               << el3.to_HTML(spe2, true) <<                 CRLF
"      <TD class=code>>";   if (ufun)   ufun->print_local_vars(page);
                    page <<                                               CRLF
"      <TD class=code>"  << Error::error_name(ec) <<                      CRLF;
                  }
               else
                  {
                    page <<
"      <TD class=code>"  << ufun->get_name_and_line(PC) <<                CRLF
"      <TD class=code>"  << exec->statement_text(PC) <<                   CRLF
"      <TD class=code>";   if (ufun)   ufun->print_local_vars(page);
                    page <<                                               CRLF;
                  }
             }
          else if (si->get_parse_mode() == PM_STATEMENT_LIST)
             {
               page <<
"      <TD class='code center'>-"                                         CRLF
"      <TD class=code>" << exec->get_text(0) <<                           CRLF;
             }
          else                          // PM_EXECUTE
             {
               page <<
"      <TD class='code center'>-"                                         CRLF
"      <TD class=code>" << exec->get_text(0) <<                           CRLF;
             }
       }
   page <<
"   </TABLE>"                                                             CRLF;
}
//----------------------------------------------------------------------------
void
Doxy::native_page(cFunction_P fun, const UCS_string & alias)
{
   Assert(fun->is_native());

const NativeFunction * nat = reinterpret_cast<const NativeFunction *>(fun);
UCS_string Z(UTF8_string("    <TR><TD class=center>"));
   if (nat->has_result())
      {
        Z += UNI_Z;
        Z += UNI_LEFT_ARROW;
      }

UTF8_string fun_filename(root_dir);
   fun_filename.append_ASCII("/f_");
   fun_filename.append_UTF8(UTF8_string(alias));
   fun_filename.append_ASCII(".html");

   Log(LOG_command_DOXY)
      out << "Writing native function HTML file " << fun_filename << endl;

ofstream page(fun_filename.c_str());

   page <<
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\""                     CRLF
"                      \"http://www.w3.org/TR/html4/strict.dtd\">"        CRLF
"<HTML>"                                                                  CRLF
"  <HEAD>"                                                                CRLF
"    <TITLE>Documentation of function" << alias << "</TITLE> "            CRLF
"    <META http-equiv=\"content-type\" "                                  CRLF
"          content=\"text/html; charset=UTF-8\">"                         CRLF
"   <LINK rel='stylesheet' type='text/css' href='apl_doxy.css'>"          CRLF
" </HEAD>"                                                                CRLF
" <BODY>"                                                                 CRLF
"  <TABLE class=h1tab><TR>"                                               CRLF
"   <TD class=h1tab width=10%><H3>↑ <A href=index.html>UP</A></H3>"       CRLF
"   <TD class=h1tab width=80%><H1>Native Function " << alias << "</H1>"   CRLF
"   <TD class=h1tab width=10%>"                                           CRLF
"  </TABLE><HR>"                                                          CRLF
"   <H3>Definition</H3>"                                                  CRLF
"<P class=center>shared library: " << nat->get_so_path() <<               BRLF
                                                                          BRLF
"   <H3>Supported signatures</H3>"                                        CRLF
                                                                          BRLF
"  <TABLE class=funtab>"                                                  CRLF;

   if (nat->f_eval_)       page << Z << alias <<                          CRLF;
   if (nat->f_eval_B)      page << Z << alias << " B"                     CRLF;
   if (nat->f_eval_AB)     page << Z << "A "     << alias << " B"         CRLF;
   if (nat->f_eval_LB)     page << Z <<   "(LO " << alias << ") B"        CRLF;
   if (nat->f_eval_ALB)    page << Z << "A (LO " << alias << ") B"        CRLF;
   if (nat->f_eval_LRB)    page << Z <<   "(LO " << alias << " RO) B"     CRLF;
   if (nat->f_eval_ALRB)   page << Z << "A (LO " << alias << " RO) B"     CRLF;
   if (nat->f_eval_XB)     page << Z             << alias << "[X] B"      CRLF;
   if (nat->f_eval_AXB)    page << Z << "A "     << alias << "[X] B"      CRLF;
   if (nat->f_eval_LXB)    page << Z <<   "(LO " << alias << ")[X] B"     CRLF;
   if (nat->f_eval_ALXB)   page << Z << "A (LO " << alias << ") B"        CRLF;
   if (nat->f_eval_LRXB)   page << Z <<   "(LO " << alias << " RO)[X] B"  CRLF;
   if (nat->f_eval_ALRXB)  page << Z << "A (LO " << alias << " RO)[X] B"  CRLF;

   page <<
"  </TABLE>"                                                              CRLF;

   swap_caller_calee();
   set_call_graph_root(fun);
   if (write_call_graph(fun, alias, true) == 0)
      {
        page <<
"   <H3>Caller Graph (defined functions calling native function "
             << alias << ")</H3>"                                         CRLF
"     <IMG class=cg2 usemap=#GC src=gc_" << alias << ".png>"              CRLF;

         // copy the cmap file for the IMG (generated by dot) into the page
         {
           UTF8_string cmapx_filename(root_dir);
           cmapx_filename.append_ASCII("/gc_");
           cmapx_filename.append_UTF8(UTF8_string(alias));
           cmapx_filename.append_ASCII(".cmapx");
           FILE * cmap = fopen(cmapx_filename.c_str(), "r");
           Assert(cmap);
           char buffer[400];
           for (;;)
               {
                 const char * s = fgets(buffer, sizeof(buffer), cmap);
                 if (s == 0)   break;
                 buffer[sizeof(buffer) - 1] = 0;
                 page << "    " << buffer;
               }
           fclose(cmap);
           Log(LOG_command_DOXY) {} else unlink(cmapx_filename.c_str());
         }
      }
   swap_caller_calee();   // restore

   page <<
" </BODY>"                                                                CRLF
" </HTML>"                                                                CRLF;

   page.close();
}
//----------------------------------------------------------------------------
void
Doxy::function_page(const UserFunction * ufun, const UCS_string & alias)
{
UTF8_string fun_filename(root_dir);
   fun_filename.append_ASCII("/f_");
   fun_filename.append_UTF8(UTF8_string(alias));
   fun_filename.append_ASCII(".html");

   Log(LOG_command_DOXY)
      out << "Writing function HTML file " << fun_filename << endl;

ofstream page(fun_filename.c_str());
   page <<
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\""                     CRLF
"                      \"http://www.w3.org/TR/html4/strict.dtd\">"        CRLF
"<HTML>"                                                                  CRLF
"  <HEAD>"                                                                CRLF
"    <TITLE>Documentation of function" << alias << "</TITLE> "            CRLF
"    <META http-equiv=\"content-type\" "                                  CRLF
"          content=\"text/html; charset=UTF-8\">"                         CRLF
"   <LINK rel='stylesheet' type='text/css' href='apl_doxy.css'>"          CRLF
" </HEAD>"                                                                CRLF
" <BODY>"                                                                 CRLF
"  <TABLE class=h1tab><TR>"                                               CRLF
"   <TD class=h1tab width=10%><H3>↑ <A href=index.html>UP</A></H3>"       CRLF
"   <TD class=h1tab width=80%><H1>Function " << alias << "</H1>"          CRLF
"   <TD class=h1tab width=10%>"                                           CRLF
"  </TABLE><HR>"                                                          CRLF;

   page <<
"   <H3>Definition</H3>"                                                  CRLF
"   <TABLE class=onefuntab><TR><TD><PRE>"                                 CRLF
"    ∇ ";
   loop(l, ufun->get_text_size())
       {
         const UCS_string & line(ufun->get_text(l));
         if (l > 99)       page << "[" << l << "]";
         else if (l > 9)   page << "[" << l << "] ";
         else if (l > 0)   page << "[" << l << "]  ";
         page << line.to_HTML(0, false) <<                                CRLF;
       }
   page <<
"    ∇</PRE>"                                                             CRLF
"  </TABLE>"                                                              CRLF;

   set_call_graph_root(ufun);
   if (write_call_graph(ufun, alias, false) == 0)
      {
        page << "   <H3>Call Graph (defined functions called from function "
             << alias << ")</H3>"                                         CRLF
                "    <IMG class=cg1 usemap=#CG src=cg_"
                      << alias << ".png>"                                 CRLF;

        // copy the cmap file for the IMG (generated by dot) into the page
        {
          UTF8_string cmapx_filename(root_dir);
          cmapx_filename.append_ASCII("/cg_");
          cmapx_filename.append_UTF8(UTF8_string(alias));
          cmapx_filename.append_ASCII(".cmapx");
          FILE * cmap = fopen(cmapx_filename.c_str(), "r");
          if (cmap == 0)
             {
               CERR << "cannot open " << cmapx_filename << ": "
                    << strerror(errno) << endl;
               return;
             }
          char buffer[400];
          for (;;)
              {
                const char * s = fgets(buffer, sizeof(buffer), cmap);
                if (s == 0)   break;
                buffer[sizeof(buffer) - 1] = 0;
                page << "    " << buffer;
              }
          fclose(cmap);
          Log(LOG_command_DOXY) {} else unlink(cmapx_filename.c_str());
        }
      }
   swap_caller_calee();

   set_call_graph_root(ufun);
   if (write_call_graph(ufun, alias, true) == 0)
      {
        page << "   <H3>Caller Graph (defined functions calling function "
             << alias << ")</H3>"                                         CRLF
             << "<IMG class=cg2 usemap=#GC src=gc_"
             << alias << ".png>"                                          CRLF;

         // copy the cmap file for the IMG (generated by dot) into the page
         {
           UTF8_string cmapx_filename(root_dir);
           cmapx_filename.append_ASCII("/gc_");
           cmapx_filename.append_UTF8(UTF8_string(alias));
           cmapx_filename.append_ASCII(".cmapx");
           FILE * cmap = fopen(cmapx_filename.c_str(), "r");
           Assert(cmap);
           char buffer[400];
           for (;;)
               {
                 const char * s = fgets(buffer, sizeof(buffer), cmap);
                 if (s == 0)   break;
                 buffer[sizeof(buffer) - 1] = 0;
                 page << "    " << buffer;
               }
           fclose(cmap);
           Log(LOG_command_DOXY) {} else unlink(cmapx_filename.c_str());
         }
      }

   swap_caller_calee();   // restore

   page <<
" </BODY>"                                                                CRLF
" </HTML>"                                                                CRLF;

   page.close();
}
//----------------------------------------------------------------------------
void
Doxy::bold_name(ostream & of, const UserFunction * ufun) const
{
const UserFunction_header & header = ufun->get_header();
const char * bold = "<span style='font-weight: bold'>";

   if (header.Z())   of << header.Z()->get_name() << "←";
   if (header.A())   of << header.A()->get_name() << " ";
   if (header.LO())   // operator
      {
        of << "(" << header.LO()->get_name() << " ";
        of << bold << header.get_name() << "</span>";
        if (header.RO())   of << " " << header.RO()->get_name();
      }
   else               // function
      {
        of << bold << header.get_name() << "</span>";
      }

   if (header.B())   of << " " << header.B()->get_name();
}
//----------------------------------------------------------------------------
void
Doxy::make_call_graph(const std::basic_string<const Symbol *> & all_fns)
{
   loop(f, all_fns.size())
      {
        const Symbol & fun_sym = *(all_fns[f]);
        loop(si, fun_sym.value_stack_size())
            {
              if (cFunction_P fun = fun_sym.get_function(si))
                 {
                   if (const UserFunction * ufun = fun->get_func_ufun())
                      add_fun_to_call_graph(&fun_sym, ufun);
                 }
            }
      }
}
//----------------------------------------------------------------------------
void
Doxy::add_fun_to_call_graph(const Symbol * caller_sym,
                            const UserFunction * ufun)
{
   Log(LOG_command_DOXY)
      out << "   add (caller-) Symbol " << caller_sym->get_name() << endl;

const Token_string & body = ufun->get_body();

   // scan the body of ufun for symbol Token and filter those that are
   // functions. This is rather heuriostic and therefore not reliable.
   //
   // It can fail if the same name is used for both functions and variables.
   //
   loop(b, body.size())
      {
        const Token & tok = body[b];
        if (tok.get_Class() != TC_SYMBOL)   continue;

        const Symbol * callee_ptr = tok.get_sym_ptr();
        Assert(callee_ptr);

        // assume (maybe incorrectly) that local symbol of ufun are variables.
        //
        if (ufun->localizes(callee_ptr))   continue;

        const Symbol & callee_sym = *callee_ptr;
        loop(si, callee_sym.value_stack_size())
            {
              if (cFunction_P fun = callee_sym.get_function(si))
                 {
                   Assert(fun);
                   const UserFunction * callee = fun->get_func_ufun();
                   if (!(callee || fun->is_native()))   continue;

                    // ignore multiple calls of the same callee from the
                    // same caller...
                    //
                    bool have_call = false;
                    loop(cg, call_graph.size())
                        {
                          if (call_graph[cg].caller == ufun &&
                              call_graph[cg].callee == fun)
                             {
                               have_call = true;
                               break;   // loop(cg, ...
                             }
                        }

                    if (!have_call)
                       {
                         const fcall_edge edge(ufun, caller_sym->get_name(),
                                               fun,   callee_sym.get_name());

                         call_graph.push_back(edge);
                         Log(LOG_command_DOXY)
                            out << "    " << caller_sym->get_name()
                                << " calls " << fun->get_name() << endl;
                       }
                 }
            }
      }
}
//----------------------------------------------------------------------------
void
Doxy::set_call_graph_root(cFunction_P fun)
{
const int MAX = 2 * call_graph.size();   // MAX means unreachable
UCS_string root_name(UTF8_string("all"));
   if (fun)   root_name = fun->get_name();

   if (call_graph.size() == 0)
      {
        // there are no calls (all functions are leafs). If the call graph
        // root is the top-level (i.e. fun == 0), then add all nodes
        // to nodes.
        //
        if (fun == 0)
           {
             nodes.clear();
             aliases.clear();
             loop(f, all_functions.size())
                 {
                   const Symbol * sym = all_functions[f];
                   cFunction_P fun = sym->get_function();
                   nodes.push_back(fun);
                   aliases.push_back(fun->get_name());
                 }
             return;
           }
      }

   // 1. set all edges starting at fun to 0 and all others unreachable
   //
   loop(cg, call_graph.size())
       {
        fcall_edge & edge = call_graph[cg];
        if (edge.caller == fun)   edge.value = 0;     // root
        else if (fun == 0)        edge.value = 1;     // no root, all edges
        else                       edge.value = MAX;   // unreachable
       }

bool progress = true;
   for (int depth = 0; progress; ++depth)
       {
         progress = false;
         loop(f, call_graph.size())
             {
               const fcall_edge & from = call_graph[f];
               if (from.value != depth)   continue;

               // 'from' starts at level depth.
               // Mark all edges 'to'at level depth + 1
               //
               loop(t, call_graph.size())
                  {
                    fcall_edge & to = call_graph[t];
                    if (from.callee != to.caller)   continue;
                    if (to.value < (depth + 1))     continue;
                    to.value = depth + 1;
                    progress = true;
                  }
             }
       }

   Log(LOG_command_DOXY)
      loop(e, call_graph.size())
          {
            const fcall_edge & edge = call_graph[e];
            if (edge.value == MAX)   continue;   // not reachable

            out << "edge " << edge.caller->get_name() << " → "
                << edge.callee->get_name()
                << " has distance " << edge.value
                << " from " << root_name << endl;
          }

   // create a list of nodes that are reachable
   //
   nodes.clear();
   aliases.clear();
   loop(e, call_graph.size())
       {
         const fcall_edge & edge = call_graph[e];
         if (edge.value == MAX)   continue;   // not reachable

         bool caller_is_new = true;
         bool callee_is_new = true;
         loop(n, nodes.size())
             {
               if (nodes[n] == edge.caller)   caller_is_new = false;
               if (nodes[n] == edge.callee)   callee_is_new = false;
             }

          if (caller_is_new)
             {
               nodes.push_back(edge.caller);
               aliases.push_back(edge.caller_name);
             }
          if (callee_is_new)
             {
               nodes.push_back(edge.callee);
               aliases.push_back(edge.callee_name);
             }
       }

   Log(LOG_command_DOXY)
      {
        loop(n, nodes.size())
        out << "Node: " << nodes[n]->get_name()
            << " alias: " << aliases[n] << endl;
      }
}
//----------------------------------------------------------------------------
int
Doxy::write_call_graph(cFunction_P fun, const UCS_string & alias, bool caller)
{
UTF8_string cg_filename(root_dir);
   if (caller)   cg_filename.append_ASCII("/gc_");
   else          cg_filename.append_ASCII("/cg_");
   cg_filename.append_UTF8(UTF8_string(alias));
UTF8_string png_filename(cg_filename);
UTF8_string cmapx_filename(cg_filename);
   cg_filename.append_ASCII(".gv");
   png_filename.append_ASCII(".png");
   cmapx_filename.append_ASCII(".cmapx");

   Log(LOG_command_DOXY)
      out << "Writing function call graph .gv file " << cg_filename << endl;

ofstream gv(cg_filename.c_str());

const char * node_attributes = " shape=rect"
                               " fontcolor=blue"
                               " fillcolor=\"#D0D0D0\"";
const char * node0_attributes = " shape=rect"
                               " style=filled"
                               " fillcolor=\"#D0D0D0\"";

   if (caller)   gv << "digraph GC" << endl;
   else          gv << "digraph CG" << endl;

   gv << "{"       << endl
      << "  graph [rankdir=\"LR\"];" << endl;

   if (nodes.size() == 0)   // no edges, single node
      {
        gv << "n0 [label=" << alias << node0_attributes << "];" << endl;
      }
   else   // normal graph, > 0 edges
      {
        // create nodes
        //
        loop(nrev, nodes.size())
            {
              // graphviz allocates nodes in reverse order, so we write them
              // in reverse order (as to display them ordered alphabetically).
              //
              const ShapeItem n = nodes.size() - nrev - 1;
              const UCS_string & alias = aliases[n];
              if (nodes[n] == fun)   // the root
                 gv << "n" << n << " [label= " << alias
                    << node0_attributes << "];" << endl;
              else
                 gv << "n" << n << " [label= <<u>" << alias << "</u>>"
                    << node_attributes << " URL=\"f_" << alias << ".html\"];"
                    << endl;
            }

        // create edges
        //
        loop(e, call_graph.size())
           {
             const fcall_edge & edge = call_graph[e];
             if (edge.value >= int(call_graph.size()))   // not reachable
                continue;
             const int n0 = node_ID(edge.caller);
             const int n1 = node_ID(edge.callee);
             Assert(n0 != -1);
             Assert(n1 != -1);
             if (caller)   gv << "n" << n1 << " -> n" << n0 << endl;
             else          gv << "n" << n0 << " -> n" << n1 << endl;
           }
      }

   gv << "}" << endl;

   gv.close();

int ret = gv_to_png(cg_filename.c_str(), png_filename.c_str(), false);
   if (ret == 0)
      ret = gv_to_png(cg_filename.c_str(), cmapx_filename.c_str(), true);
   return ret;
}
//----------------------------------------------------------------------------
int Doxy::node_ID(cFunction_P fun)
{
   loop(n, nodes.size())
      if (nodes[n] == fun)   return n;

   return -1;
}
//----------------------------------------------------------------------------
void
Doxy::swap_caller_calee()
{
   loop(cg, call_graph.size())
       {
         const fcall_edge edge = call_graph[cg];
         call_graph[cg].caller      = edge.callee;
         call_graph[cg].caller_name = edge.callee_name;
         call_graph[cg].callee      = edge.caller;
         call_graph[cg].callee_name = edge.caller_name;
       }
}
//----------------------------------------------------------------------------
int
Doxy::gv_to_png(const char * gv_filename, const char * out_filename, bool cmapx)
{
   // convert .gv file gv_filename to .png file out_filename using the 'dot'
   // program

const char * out_type = cmapx ? "cmapx" : "png";
char cmd[200];
   SPRINTF(cmd, "dot -T%s %s -o%s", out_type, gv_filename, out_filename);

   errno = 0;
FILE * dot = popen(cmd, "r");
   if (dot == 0)
      {
        if (errno)
           CERR << "Could not run 'dot': " << strerror(errno) << endl;
        else
           CERR << "Could not run 'dot'. Maybe install package 'graphviz'?"
                << endl;
        ++errors;
        return 1;
      }

char buffer[1000];
   for (;;)
       {
         const int len = fread(buffer, 1, sizeof(buffer), dot);
         if (len == 0)   break;
         buffer[len] = 0;
         CERR << "dot says: " << buffer;
       }
   pclose(dot);

   // if LOG_command_DOXY then do not remove temporary files
   Log(LOG_command_DOXY)
      {
        out << "  converted " << gv_filename << " to " << out_filename << endl;
      }
   else
      {
        if (cmapx)   unlink(gv_filename);   // no longer needed
      }
   return 0;
}
//----------------------------------------------------------------------------
UCS_string
Doxy::fun_anchor(const UCS_string & name)
{
UCS_string anchor = UTF8_string("<A href=f_");
   anchor.append(name);
   anchor.append_ASCII(".html>");
   anchor.append(name);
   anchor.append_ASCII("</A>");
   return anchor;
}

