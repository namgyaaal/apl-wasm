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

#include "Command.hh"
#include "InputFile.hh"
#include "LineInput.hh"
#include "Logging.hh"
#include "Nabla.hh"
#include "Output.hh"
#include "PrintOperator.hh"
#include "Symbol.hh"
#include "UserFunction.hh"
#include "UserPreferences.hh"
#include "Workspace.hh"

//----------------------------------------------------------------------------
void
Nabla::edit_function(const UCS_string & cmd)
{
Nabla nabla(cmd);
   nabla.edit();
}
//----------------------------------------------------------------------------
Nabla::Nabla(const UCS_string & cmd)
   : defn_line_no(InputFile::current_line_no()),
     fun_symbol(0),
     ecmd(ECMD_NOP),
     edit_from(-1),
     edit_to(-1),
     function_existed(false),
     modified(false),
     do_close(false),
     locked(false),
     current_line(1),
     first_command(cmd)
{
   Workspace::more_error().clear();
}
//----------------------------------------------------------------------------
void
Nabla::throw_edit_error(const char * why)
{
   COUT << "DEFN ERROR+" << endl
        << "      " << first_command << endl
        << "      " << UCS_string(first_command.size() - 1, UNI_SPACE)
        << "^" << endl;

   if (Workspace::more_error().size() == 0)
      {
        MORE_ERROR() << why;
      }

   Error::throw_define_error(fun_header, first_command, why);
}
//----------------------------------------------------------------------------
void
Nabla::edit()
{
   if (const char * error = start())
      {
        Log(LOG_verbose_error)   if (Workspace::more_error().size() == 0)
           {
             UERR << "Bad ∇-open '" << first_command
                  << "' : '" << error << "'" << endl;
           }
        throw_edit_error(error);
      }

   // editor loop
   //
int control_D_count = 0;
   Log(LOG_nabla)   UERR << "Nabla(" << fun_header << ")..." << endl;
try_again:
   while (!do_close)
       {
         const UCS_string prompt = current_line.print_prompt(0);
         bool eof = false;
         UCS_string line;
         if (UserPreferences::uprefs.raw_cin)
            {
              LineHistory lh(10);
              InputMux::get_line(LIM_Nabla, prompt, line, eof, lh);
            }
         else
            {
              LineHistory lh(*this);
              InputMux::get_line(LIM_Nabla, prompt, line, eof, lh);
            }

         if (eof)   // end-of-input (^D) pressed
            {
              ++control_D_count;
              if (control_D_count < 5)
                 {
                    COUT << "^D" << endl;
                    continue;
                 }
               COUT << endl << "      *** end of input" << endl;
               Command::cmd_OFF(5);
            }

         if (const char * loc = parse_oper(line, false))
            {
              UERR << "??? " << loc << endl;
              continue;
            }

         if (const char * loc = execute_oper())
            {
              UERR << "∇-command failed: " << loc << endl;
              Output::set_color_mode(Output::COLM_INPUT);
            }
       }

   Log(LOG_nabla)
      {
        UERR << "done: '" << fun_header << "'" << endl;
        loop(l, lines.size())   UERR << lines[l].text << endl;
      }

UCS_string fun_text;
   loop(l, lines.size())
      {
        fun_text.append(lines[l].text);
        fun_text.append(UNI_LF);
      }

   // maybe copy function into the history
   //
   if (!UserPreferences::uprefs.raw_cin &&
       !InputFile::running_script() &&
       ((UserPreferences::uprefs.nabla_to_history ==  /* always     */ 2) ||
        ((UserPreferences::uprefs.nabla_to_history == /* if changed */ 1) &&
       modified)))
      {
        // create a history entry that can be re-entered and replace
        // the last history line (which contained some ∇foo ...)
        //
        {
          UCS_string line_0(UTF8_string("    "));
          line_0.append(UNI_NABLA);
          line_0.append(lines[0].text);
          line_0.remove_trailing_whitespaces();
          LineInput::replace_history_line(line_0);
        }

        for (size_t l = 1; l < lines.size(); ++l)
            {
              UCS_string line_l(UTF8_string("["));
              line_l.append_number(l);
              line_l.append_UTF8("]  ");
              while (line_l.size() < 6)   line_l.append(UNI_SPACE);
              line_l.append(lines[l].text);
              line_l.remove_trailing_whitespaces();

              LineInput::add_history_line(line_l);
           }

        UCS_string line_N(UTF8_string("   "));
        line_N.append(UNI_NABLA);
        LineInput::add_history_line(line_N);
      }

int error_line = 0;
char creator[APL_PATH_MAX+20];
   SPRINTF(creator, "%s:%d", InputFile::current_filename(), defn_line_no)
const UTF8_string creator_utf8(creator);

UserFunction * ufun = UserFunction::fix(fun_text, error_line, false,
                                        LOC, creator_utf8, true);

   if (ufun == 0)   // UserFunction::fix() failed
      {
        const UCS_string & MORE = Workspace::more_error();
        if (InputFile::running_script())
           {
             // the ∇-editor runs from a script, therefore warning the user
             // interactively and asking to fix the fault makes no sense. We
             // therefore exit with DEFN_ERROR, so that the script does not
             // hang in a endless try again loop.
             //
             UTF8_string more_utf8(MORE);
             throw_edit_error(more_utf8.c_str());
           }

        if (error_line == -1)   /// unknown error line
           {
             COUT <<
             MORE << 
"\nFatal error in defined function.\n"
"    You may want to change faulty line(s)\n"
"    or cancel editing entirely with:   [→]∇\n";
           }
        else
           {
             COUT <<
             MORE << "\nFatal error in defined function line [" << error_line
                  << "]. To fix this you may want to:\n"
                     "    change the faulty line with:    ["
                  << error_line << "] ..., or \n"
                     "    delete the faulty line with:    [∆"
                  << error_line << "], or\n"
                     "    cancel editing entirely with:   [→]∇.";
           }
        do_close = false;
        goto try_again;
      }

   if (locked)
      {
         const int exec_properties[4] = { 1, 1, 1, 1 };
         ufun->set_exec_properties(exec_properties);
      }

   // set stop and trace vectors
   //
std::basic_string<Function_Line> stop_vec;
std::basic_string<Function_Line> trace_vec;
   loop(l, lines.size())
       {
         if (lines[l].stop_flag)    stop_vec.push_back(Function_Line(l));
         if (lines[l].trace_flag)   trace_vec.push_back(Function_Line(l));
       }

   ufun->set_trace_stop(stop_vec,  true);
   ufun->set_trace_stop(trace_vec, false);
}
//----------------------------------------------------------------------------
const char *
Nabla::start()
{
   // cmd should be something like:
   //
   // ∇FUN
   // ∇FUN[⎕]
   // ∇FUN[⎕]∇
   // etc.
   //
UCS_string::iterator c(first_command);

   // skip leading spaces
   //
   c.skip_white();

   // skip leading nabla.
   //
   if (c.has_more() && c.next() != UNI_NABLA)   return "Bad ∇-command (no ∇)";

   // skip leading spaces
   //
   c.skip_white();

   // function header.
   //
   while (c.has_more() && c.lookup() != UNI_L_BRACK)
         fun_header.append(c.next());

   /* at this point there could be an axis specification [X] that
      could be confused with an operation like [⎕]. We take the first char
      after the [ to decide if there is an axis or a ∇-operation like [⎕].

      For example:

          ∇FOO[X] B : [ starts axis
          ∇FOO[⎕]   : [ starts ∇operation [⎕] (display)
    */
   if (c.has_more() && c.lookup() == UNI_L_BRACK)   // [
      {
        c.next();   // the [
        c.skip_white();
        if (c.has_more() && Avec::is_first_symbol_char(c.lookup()))   // axis
           {
             fun_header.append(UNI_L_BRACK);   //  copy the [
             while (c.has_more() && c.lookup() != UNI_L_BRACK)
                   fun_header.append(c.next());
           }
        else                                                      // ∇-command
           {
             c.un_next();
           }
      }

UserFunction_header hdr(fun_header, false);
   if (hdr.get_error())
      {
         static char cc[200];
         SPRINTF(cc, "Bad function header: %s", hdr.get_error_info());
         return cc;
      }

   fun_symbol = Workspace::lookup_symbol(hdr.get_name());
   Assert(fun_symbol);

   if (fun_header.size() == 0)   return "no function name";

   // optional operation
   //
   if (c.has_more() && c.lookup() == UNI_L_BRACK)
      {
        UCS_string oper;
        Unicode cc;
        do
          {
            if (!c.has_more())   return "no ] in ∇-command";
            oper.append(cc = c.next());
          } while (cc != UNI_R_BRACK);

        if (const char * loc = parse_oper(oper, true))   return loc;   // error
      }

   switch(fun_symbol->get_NC())
      {
        case NC_UNUSED_USER_NAME:   // open a new function
             function_existed = false;
             modified = true;
             {
               // a new function must not have a command
               //
               if (ecmd != ECMD_NOP)   return "∇-command in new function";

               const char * open_loc = open_new_function();
               if (open_loc)   return open_loc;
             }
             break;

        case NC_FUNCTION:
        case NC_OPERATOR:   // open an existing function
             function_existed = true;
             if (InputFile::running_script())   // script
                {
                  const char * open_loc = open_new_function();
                  if (open_loc)   return open_loc;
                  break;   // continue below
                }

             // interactive
             {
               if (hdr.has_vars())
                  {
                       // an existing function was opened with a header that
                       // contains more than the function name.
                       //
                       return "attempt to ∇-open existing function with "
                              "new function header";
                  }

               const char * open_loc = open_existing_function();
               if (open_loc)   return open_loc;
             }
             break;

        default:
             return "attempt to ∇-open a variable at " LOC;
      }

   // at this point the (new or existing) function was successfully opened.
   // That means that at least the header is present (lines.size() > 0)

   // immediate close (only show command is allowed here),
   // e.g. ∇fun[⎕]∇
   //
   if (c.has_more() && c.lookup() == UNI_NABLA)
      {
        if (ecmd != ECMD_SHOW)   return "illegal command between ∇ ... ∇";
        if (const char * loc = execute_oper())
           UERR << "execute_oper() failed at " << loc << endl;
        do_close = true;
        return 0;
      }

   if (const char * loc = execute_oper())
      UERR << "execute_oper() failed at " << loc << endl;

   return 0;   // no error
}
//----------------------------------------------------------------------------
const char *
Nabla::parse_oper(UCS_string & oper, bool initial)
{
   Log(LOG_nabla)
      UERR << "parsing oper '" << oper << "'" << endl;

   // skip trailing spaces
   //
   oper.remove_trailing_whitespaces();
   if (oper.size() > 0 && oper.back() == UNI_NABLA)
      {
        do_close = true;
        oper.pop_back();
        while (oper.size() > 0 && oper.back() <= ' ')   oper.pop_back();
      }
   else if (oper.size() > 0 && oper.back() == UNI_DEL_TILDE)
      {
        do_close = true;
        locked = true;
        oper.pop_back();
        while (oper.size() > 0 && oper.back() <= ' ')   oper.pop_back();
      }

   current_text.clear();
   ecmd = ECMD_NOP;

   if (oper.size() == 0 && do_close)   return 0;

UCS_string::iterator c(oper);
Unicode cc = c.has_more() ? c.next() : Invalid_Unicode;
UCS_string text = oper;
   while (cc == ' ')   cc = c.next();   // skip leading whitespace

   // we expect one of the following:
   //
   // [⎕] [n⎕] [⎕m] [n⎕m] [⎕n-m]                    (show)
   //     [n∆] [∆m] [n∆m] [∆n-m] [∆n1 n2 ...]       (delete)
   // [→]                                           (escape)
   // [n]                                           (goto)
   // text                                          (override text)

   if (cc != UNI_L_BRACK)   // override text
      {
        ecmd = ECMD_EDIT;
        edit_from = current_line;
        current_text = text;
//      for (; c.has_more(); cc = c.next())   current_text.append(cc);
        return 0;
      }

   // a loop over multiple commands, like
   // [2⎕4] [∆5]
   //
   // only the last command is executed; the previous commands are discarded/
   //
command_loop:

   // at this point, [ was seen and skipped

   ecmd = ECMD_NOP;
   edit_from.clear();   // set to missing
   edit_to.clear();     // set to missing
   got_minus = false;   // set to missing

   // set optional edit_from (if present)
   //
   if (c.has_more() && (Avec::is_digit(c.lookup()) ||    // N.M
                    c.lookup() == UNI_FULLSTOP))     //  .M
      {
        edit_from = parse_lineno(c);
      }

   // operation, which is one of:
   //
   // [⎕   show
   // []   edit
   // [∆   delete
   // [→   abandon
   //
   if (!c.has_more())   return "Bad ∇-command";
   switch (c.lookup())
      {
        case UNI_Quad_Quad:
        case UNI_Quad_Quad1:    ecmd = ECMD_SHOW;     c.next();   break;
        case UNI_R_BRACK:       ecmd = ECMD_EDIT;                 break;
        case UNI_DELTA:         ecmd = ECMD_DELETE;   c.next();   break;
        case UNI_RIGHT_ARROW:   ecmd = ECMD_ESCAPE;   c.next();   break;

        default: UERR << "Bad edit op '" << c.lookup() << "'"
                      << " in line " << current_line << endl;
                 return "Bad ∇-command";
      }

   // don't allow delete or escape in the first command
   if (initial)
      {
        if (ecmd == ECMD_DELETE)   return "Bad initial ∇-command ∆";
        if (ecmd == ECMD_ESCAPE)   return "Bad initial ∇-command →";
      }

again:
   // set optional edit_to (if present)
   //
   if (c.has_more() && Avec::is_digit(c.lookup()))   edit_to = parse_lineno(c);

   if (c.has_more() && c.lookup() == UNI_MINUS)   // range
      {
        if (got_minus)   return "error: second -  in ∇-range";
        got_minus = true;
        edit_from = edit_to;   // shift
        c.next();   // consume the -
        goto again;
      }

   if (c.has_more() && c.next() != UNI_R_BRACK)   return "missing ] in ∇-range";

   // at this point we have parsed an editor command, like:
   //
   // [from ⎕ to]
   // [from ∆ to]
   // [from]

   c.skip_white();

   if (c.has_more() &&
       c.lookup() == UNI_L_BRACK)   // another command: ignore the previous one
      {
         c.next();   // eat the [
         goto command_loop;
      }

   // copy the rest to current_text. Set do_close if ∇ or ⍫ is seen
   // unless inside strings.
   //
   while (c.has_more())
      {
        switch(cc = c.next())
           {
             case UNI_NABLA:           // ∇
                  do_close = true;
                  return 0;

             case UNI_DEL_TILDE:       // ⍫
                  locked = true;
                  do_close = true;
                  return 0;

             case UNI_DOUBLE_QUOTE:  // "
                  current_text.append(cc);
                  for (;;)
                      {
                        if (!c.has_more())   // premature end of input
                           {
                             current_text.append(UNI_DOUBLE_QUOTE);
                             return 0;
                           }
                        cc = c.next();

                        current_text.append(cc);
                        if (cc == UNI_DOUBLE_QUOTE)   break; // string end
                        if (cc == UNI_BACKSLASH)      // \x
                           {
                             if (!c.has_more())   // premature end of input
                                {
                                  current_text.append(UNI_BACKSLASH);
                                  current_text.append(UNI_DOUBLE_QUOTE);
                                  return 0;
                                }
                             cc = c.next();
                             current_text.append(cc);
                           }
                      }
                  break;

             case UNI_SINGLE_QUOTE:    // '
                  current_text.append(cc);
                  for (;;)
                      {
                        // no need to care for ''. Since we only copy, we can
                        // handle ' ... '' ... ' like two adjacent strings
                        // instead of a string containing a (doubled) quote.
                        //
                        if (!c.has_more())   // premature end of input
                           {
                             current_text.append(UNI_SINGLE_QUOTE);
                             return 0;
                           }
                        cc = c.next();
                        current_text.append(cc);
                        if (cc == UNI_SINGLE_QUOTE)      break;   // string end
                      }
                  break;

             default:
                current_text.append(cc);
           }
      }

   return 0;   // OK
}
//----------------------------------------------------------------------------
LineLabel
Nabla::parse_lineno(UCS_string::iterator & c)
{
LineLabel ret(0);

   while (c.has_more() && Avec::is_digit(c.lookup()))
      {
        ret.ln_major *= 10;
        ret.ln_major += c.next() - UNI_0;
      }

   if (c.has_more() && c.lookup() == UNI_FULLSTOP)
      {
        c.next();   // eat the .
        while (c.has_more() && Avec::is_digit(c.lookup()))
              ret.ln_minor.append(c.next());
      }

   return ret;
}
//----------------------------------------------------------------------------
const char *
Nabla::open_new_function()
{
   Log(LOG_nabla)
      UERR << "creating new function '" << fun_symbol->get_name() 
           << "' with header '" << fun_header << "'" << endl;

   lines.push_back(FunLine(0, fun_header));
   return 0;
}
//----------------------------------------------------------------------------
const char *
Nabla::open_existing_function()
{
   Log(LOG_nabla)
      UERR << "opening existing function '" << fun_symbol->get_name()
           << "'" << endl;

   if (const char * why = fun_symbol->cant_be_defined())   return why;

   // this function must only be called when editing functions interactively
   //
   if (InputFile::running_script())
      return "∇-edit existing function from a script";

cFunction_P function = fun_symbol->get_function();
   Assert(function);

   if (function->get_exec_properties()[0])
      return "function is locked";

   if (function->is_native())
      return "function is native";

   if (function->is_lambda())
      return "function is a lambda";

   if (Workspace::is_called(fun_symbol->get_name()))
      return "function is used, pendent or suspended";

const UserFunction * ufun = function->get_func_ufun();
   if (ufun == 0)
      return "function is not editable at " LOC;

const UCS_string ftxt = function->canonical(false);
   Log(LOG_nabla)   UERR << "existing function is:\n" << ftxt << endl;

UCS_string_vector tlines;
   ftxt.to_vector(tlines);

   Assert(tlines.size());
   fun_header = tlines[0];
   loop(t, tlines.size())
       {
         FunLine fl(t, tlines[t]);

         // set stop and trace flags
         //
         loop(st, ufun->get_stop_lines().size())
             {
               if (t == ufun->get_stop_lines()[st])   // stop set
                  {
                    fl.stop_flag = true;
                    break;
                  }
             }

         loop(tr, ufun->get_trace_lines().size())
             {
               if (t == ufun->get_trace_lines()[tr])   // trace set
                  {
                    fl.trace_flag = true;
                    break;
                  }
             }

         lines.push_back(fl);
       }

   current_line = LineLabel(tlines.size());

   return 0;
}
//----------------------------------------------------------------------------
const char *
Nabla::execute_oper()
{
   if (ecmd == ECMD_NOP)
      {
        Log(LOG_nabla)
           UERR << "Nabla::execute_oper(NOP)" << endl;
        return 0;
      }

const bool have_from = edit_from.ln_major != -1;
const bool have_to = edit_to.ln_major != -1;

   if (lines.size())
      {
        if (!have_from && ecmd != ECMD_DELETE)   edit_from = lines[0].label;
        if (!have_to)     edit_to = lines[lines.size() - 1].label;
      }

   if (ecmd == ECMD_SHOW)     return execute_show();
   if (ecmd == ECMD_DELETE)   return execute_delete();
   if (ecmd == ECMD_EDIT)     return execute_edit();
   if (ecmd == ECMD_ESCAPE)   return execute_escape();

   UERR << "edit command " << ecmd
        << " from " << edit_from << " to " << edit_to << endl;
   FIXME;

   return LOC;
}
//----------------------------------------------------------------------------
const char *
Nabla::execute_show()
{
   Log(LOG_nabla)
      UERR << "Nabla::execute_oper(SHOW) from " << edit_from
           << " to " << edit_to << " line-count " << lines.size() << endl;

int idx_from = find_line(edit_from);
int idx_to   = find_line(edit_to);

const LineLabel user_edit_to = edit_to;

   if (idx_from == -1)   edit_from = lines[idx_from = 0].label;
   if (idx_to == -1)     edit_to   = lines[idx_to = lines.size() - 1].label;

   Log(LOG_nabla)
      UERR << "Nabla::execute_oper(SHOW) from "
           << edit_from << " to " << edit_to << endl;

   if (idx_from == 0)                     // then print header line
      COUT << "    ∇" << endl;
   for (int e = idx_from; e <= idx_to; ++e)   lines[e].print(COUT);
   if (idx_to == int(lines.size() - 1))   // then print last line
      COUT << "    ∇" << endl;

   if (user_edit_to.valid())   // eg. [⎕42] or [2⎕42]
      {
        current_line = user_edit_to;
        if (line_exists(current_line))   current_line.next();
        if (line_exists(current_line))
           {
             // user_edit_to and its next line exist. Increase minor length
             // That is, if both [8] and [9] exist then use [8.1]
             //
             current_line = user_edit_to;
             current_line.insert();
           }
      }
   else                      // eg. [42⎕] or [⎕]
      {
        current_line = lines.back().label;
        current_line.next();
      }

   Log(LOG_nabla)
      UERR << "Nabla::execute_oper(SHOW) done with current_line '"
           << current_line << "'" << endl;

   return 0;
}
//----------------------------------------------------------------------------
const char *
Nabla::execute_delete()
{
   // for delete we want exact numbers.
   //
const int idx_to = find_line(LineLabel(edit_to));
   if (idx_to == -1)   return "Bad line number N in [M∆N] ";

   modified = true;

   if (edit_from == -1)   // [∆N] : delete single line
      {
        lines.erase(lines.begin() + idx_to);
        return 0;
      }

   // [N∆M] : delete multiple lines
   //
const int idx_from = find_line(LineLabel(edit_from));
   if (idx_from == -1)       return "Bad line number M in [M∆N] ";
   if (idx_from >= idx_to)   return "M ≥ N in [M∆N] ";

   loop(j, 1 + idx_to - idx_from)   lines.erase(lines.begin() + idx_from);
   current_line = lines.back().label;
   return 0;
}
//----------------------------------------------------------------------------
const char *
Nabla::execute_edit()
{
   // if the user has not specified a line then edit at current_line
   //
   if (edit_from.ln_major == -1)   edit_from = current_line;

   current_line = edit_from;

   // if the user has specified a line label without a text then we are done
   //
   if (current_text.size() == 0)   // empty line (we MAY need it)
      {
        if (!UserPreferences::uprefs.multi_line_strings_3)
           return 0;   // we do not
        if (out_of_order)
           return 0;   // we do not;
      }

   // check that current_text is valid
   //
   if (current_line.is_header_line_number())   return edit_header_line();
   else                                        return edit_body_line();
}
//----------------------------------------------------------------------------
const char *
Nabla::edit_header_line()
{
   // parse the header and check that it is valid
   //
UserFunction_header header(current_text, false);
   if (header.get_error() != E_NO_ERROR)
      {
        CERR << "BAD FUNCTION HEADER";
        COUT << endl;
        return 0;
      }

   // check if the function name has changed
   //
   Assert(fun_symbol);
const UCS_string & old_name = fun_symbol->get_name();
const UCS_string & new_name = header.get_name();
   if (old_name != new_name)
      {
        // the name has changed. This is OK if the new name can be edited.
        //
        // the old symbol shall not be ⎕FXed when closing the editor, so we
        // can simply forget it and continue with the new function
        //
        // We need to check that it is not a variable or an existing function.
        //
        Symbol * sym = Workspace::lookup_symbol(new_name);   // create if needed
        Assert(sym);
        if (sym->get_NC() != NC_UNUSED_USER_NAME)
           {
             CERR << "BAD FUNCTION HEADER";
             COUT << endl;
             return 0;
           }
      }

   modified = true;

   lines[0].text = current_text;
   current_line.next();
   return 0;
}
//----------------------------------------------------------------------------
const char *
Nabla::edit_body_line()
{
UCS_string parse_text = current_text;   // a copy that can be modified.
   if (UserPreferences::uprefs.multi_line_strings_3)
      {
        // figure the multi-line status from all lines before current_line
        bool multi = false;
        loop(i, lines.size())
            {
              if (current_line == lines[i].label)   break;   // line replace
              if (current_line <  lines[i].label)   break;   // after current
              if (-1 == lines[i].text.multi_pos(multi))   continue;
              multi = ! multi;
            }

        if (multi)   // line belongs to a multi-line string (incl. """ or »»»)
           {
             parse_text.clear();
           }
       else          // APL code outside multi-line strings (or start of one)
           {
             const int pos = parse_text.multi_pos(multi);
             if (pos != -1)   // start of multi-line string (""" or «««)
                {
                  // for the sole purpose of parsing: replace the start of the
                  // multi-line string (""" or »»») with the empty string ''.
                  //
                  parse_text.resize(pos);
                  parse_text.append_ASCII("''");
                }
           }
      }

   if (parse_text.size())
      {
        const Parser parser(PM_FUNCTION, LOC, false);
        Token_string in;

        ErrorCode ec = parser.parse(parse_text, in, true);
        if (ec == E_NO_STRING_END && UserPreferences::uprefs.multi_line_strings)
           {
             ec = E_NO_ERROR;
             Workspace::more_error().clear();
           }

        if (ec)
           {
             CERR << "SYNTAX ERROR";
             if (Workspace::more_error().size())
                {
                  CERR << "+" << endl << Workspace::more_error();
                }
             COUT << endl;
             return 0;
           }
      }

   // some users prefer the removal of leading and trailing whitespace
   //
   if (UserPreferences::uprefs.discard_indentation)
      current_text.remove_leading_and_trailing_whitespaces();

   modified = true;

const int idx_from = find_line(edit_from);

   Assert(lines.size() > 0);
   if (idx_from == -1)   // new line
      {
        // find the largest label before edit_from (if any)
        //
        int before_idx = -1;
        loop(i, lines.size())
            {
              if (lines[i].label < edit_from)   before_idx = i;
              else                              break;
            }

        FunLine fl(edit_from, current_text);
        lines.insert(lines.begin() + before_idx + 1, fl);
      }
   else
      {
        // replace line
        //
        lines[idx_from].text = current_text;
      }

   current_line.next();

   return 0;
}
//----------------------------------------------------------------------------
const char *
Nabla::execute_escape()
{
   // the user has entered [→].
   //
   // Note that fun_symbol and fun_symbol->get_function() may both be valid
   // even though the function is "fresh". We use function_existed imstead.
   //
   lines.clear();


   if (function_existed)   // existing function
      {
        Assert(fun_symbol);
        const Function * fun = fun_symbol->get_function();
        Assert(fun);
        const UserFunction * ufun = fun->get_func_ufun();
        Assert(ufun);
        loop(l, ufun->get_text_size())
            {
              const UCS_string & fun_line = ufun->get_text(l);
              lines.push_back(FunLine(l, fun_line));
            }
      }
   else       // new function: only restore the header
      {
        lines.push_back(FunLine(0, fun_header));
        current_line = LineLabel(1);
      }

   return 0;
}
//----------------------------------------------------------------------------
int
Nabla::find_line(const LineLabel & lab) const
{
   if (lab.ln_major == -1)   return -1;

   loop(l, lines.size())   if (lab == lines[l].label)   return l;

   return -1;   // not found.
}
//----------------------------------------------------------------------------
void
Nabla::FunLine::print(ostream & out) const
{
   label.print(out);

   // print a space unless text is a label or a comment
   //
   if (!text.is_comment_or_label())   out << " ";
   out << text << endl;
}
//----------------------------------------------------------------------------
void
LineLabel::print(ostream & out) const
{
UCS_string ucs(UTF8_string("["));
   ucs.append_number(ln_major);
   if (ln_minor.size())
      {
        ucs.append_UTF8(".");
        ucs.append(ln_minor);
      }
   ucs.append_UTF8("]");

   while (ucs.size() < 5)   ucs.append_UTF8(" ");
   out << ucs;
}
//----------------------------------------------------------------------------
UCS_string
LineLabel::print_prompt(int min_size) const
{
UCS_string ret(UTF8_string("["));
   ret.append_number(ln_major);

   if (ln_minor.size())
      {
        ret.append(Unicode('.'));
        loop(s, ln_minor.size())   ret.append(Unicode(char(ln_minor[s])));
      }

   ret.append_UTF8("] ");
   while (ret.size() < min_size)   ret.append(UNI_SPACE);
   return ret;
}
//----------------------------------------------------------------------------
void
LineLabel::next()
{
   if (ln_minor.size() == 0)   // full number: add 1
      {
        ++ln_major;
        return;
      }

   // fract number: increment last fract digit
   //
const Unicode cc = ln_minor[ln_minor.size() - 1];
   if (cc != UNI_9)   ln_minor[ln_minor.size() - 1] = Unicode(cc + 1);
   else                     ln_minor.append(UNI_1);
}
//----------------------------------------------------------------------------
void
LineLabel::insert()
{
   ln_minor.append(UNI_1);
}
//----------------------------------------------------------------------------
bool
LineLabel::operator ==(const LineLabel & other) const
{
   return (ln_major == other.ln_major) && (ln_minor == other.ln_minor);
}
//----------------------------------------------------------------------------
bool
LineLabel::operator <(const LineLabel & other) const
{
   if (ln_major != other.ln_major)   return ln_major < other.ln_major;
 return  ln_minor.compare(other.ln_minor) < 0;
}
//----------------------------------------------------------------------------
ostream &
operator <<(ostream & out, const LineLabel & lab)
{
   lab.print(out);
   return out;
}
//----------------------------------------------------------------------------
UCS_string
Nabla::FunLine::get_label_and_text() const
{
UCS_string ret = label.print_prompt(6);
   ret.append(text);

   return ret;
}
//----------------------------------------------------------------------------
UCS_string
Nabla::get_label_and_text(int line, bool & is_current) const
{
const FunLine & fl = lines[line];
   is_current = fl.label == current_line;
   return fl.get_label_and_text();
}
//----------------------------------------------------------------------------

