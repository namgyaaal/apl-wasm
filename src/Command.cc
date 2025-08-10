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
#include <limits.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/stat.h>

#include "Common.hh"

#include "CharCell.hh"
#include "ComplexCell.hh"
#include "Command.hh"
#include "Doxy.hh"
#include "Executable.hh"
#include "FloatCell.hh"
#include "IO_Files.hh"
#include "IndexExpr.hh"
#include "InputFile.hh"
#include "IntCell.hh"
#include "LineInput.hh"
#include "Nabla.hh"
#include "NativeFunction.hh"
#include "Output.hh"
#include "Parser.hh"
#include "Prefix.hh"
#include "Quad_FX.hh"
#include "Quad_TF.hh"
#include "Security.hh"
#include <signal.h>
#include "StateIndicator.hh"
#include "Svar_DB.hh"
#include "Symbol.hh"
#include "Tokenizer.hh"
#include "UserFunction.hh"
#include "UserPreferences.hh"
#include "ValueHistory.hh"
#include "Value.hh"
#include "Workspace.hh"

#include "Workspace.icc"

bool Command::auto_MORE = false;

int Command::boxing_format = 0;
ShapeItem Command::APL_expression_count = 0;

UCS_string_vector Command::copy_once_table;

//----------------------------------------------------------------------------
void
Command::process_line()
{
UCS_string prefix;   // for new-style multiline strings
UCS_string accu;     // for new-style multiline strings
UCS_string prompt = Workspace::get_prompt();
bool multiline = false;
int count = 0;
   for (;;)
       {
         UCS_string line;
         bool eof = false;
         InputMux::get_line(LIM_ImmediateExecution, prompt,
                      line, eof, LineInput::get_history());

         if (eof) CERR << "EOF at " << LOC << endl;

         const ShapeItem multi = line.multi_pos(multiline);
         if (multi != -1)   /// line is START or END of multi-line string
            {
              multiline = ! multiline;
              if (multiline)    // START of multi-line string
                  {
                    count = 0;
                    prefix = line;
                    prefix.resize(multi);   // discard trailing """ ff.

                    prompt.prepend(UNI_RIGHT_ARROW);
                    accu.append(UNI_SPACE);
                  }
              else              // END of multi-line string
                  {
                    accu.pop_back();   // trailing " "
                    if (accu.size() == 1)   accu.append_ASCII(" \"\"");

                    if (count == 0)        // no string
                       {
                         const UTF8_string empty("0⍴⊂\"\"");   // 0⍴⊂""
                         prefix << UCS_string(empty);
                         process_line(prefix, 0);
                       }
                    else if (count == 1)   // a single "string" would not nest
                       {
                         const UTF8_string encl("(,⊂");  // enclose accu...
                         prefix << UCS_string(encl) << accu << ")";
                         process_line(prefix, 0);
                       }
                    else                   // true multi-string
                       {
                         prefix << accu;
                         process_line(prefix, 0);
                       }
                    return;
                  }
            }
         else if (multiline)   // inside multi-line
            {
              ++count;
              accu.append_ASCII("\"");
              accu.append(line.do_escape(true));
              accu.append_ASCII("\" ");
            }
         else                   // normal input line
            {
              process_line(line, 0);
              return;
            }
       }
}
//----------------------------------------------------------------------------
void
Command::process_line(UCS_string & line, ostream * out)
{
   line.remove_leading_whitespaces();
   if (line.size() == 0)           return;   // empty input line

   switch(line[0])
      {
         case UNI_R_PARENT:      // regular command, e.g. )SI
              if (out == 0)   out = &COUT;
              do_APL_command(*out, line);
              if (line.size())   break;
              return;

         case UNI_R_BRACK:       // debug command, e.g. ]LOG
              if (out == 0)   out = &CERR;
              do_APL_command(*out, line);
              if (line.size())   break;
              return;

         case UNI_NABLA:               // e.g. ∇FUN
              Nabla::edit_function(line);
              return;

         case UNI_NUMBER_SIGN:   // e.g. # comment
         case UNI_COMMENT:             // e.g. ⍝ comment
              return;

        default: break;
      }

   ++APL_expression_count;
   do_APL_expression(line);
}
//----------------------------------------------------------------------------
bool
Command::do_APL_command(ostream & out, UCS_string & line)
{
   if (line.contains(UNI_COMMENT))   // unlikely, but valid
      {
        loop(l, line.size())   // find ⍝
            {
              if (line[l] == UNI_COMMENT)   // found ⍝
                 {
                   line.resize(l);
                   line.remove_trailing_whitespaces();
                   break;
                 }
            }
      }

const UCS_string orig_line(line);   // the original line

   // split line into command and command arguments
   //
UCS_string cmd;   // the command without arguments
const size_t len = line.copy_black(cmd, 0);   // line w/o leading/trailing ws

UCS_string arg(line, len, line.size() - len);
UCS_string_vector args = split_arg(arg);
   line.clear();

   // clear the )MORE info, unless the command itself is )MORE
   //
   if (!cmd.starts_iwith(")MORE"))   // command is not )MORE.
      {
        // clear )MORE info unless cmd itself is )MORE
        //
        Workspace::more_error().clear();
      }

#define cmd_def(cmd_str, code, garg, _hint)                          \
   if (cmd.starts_iwith(cmd_str))                                    \
      { if (check_params(out, cmd_str, args.size(), garg))   return true; \
        code; return false; }
#include "Command.def"

   // check for user defined commands...
   //
   loop(u, Workspace::get_user_commands().size())
       {
         if (cmd.starts_iwith(Workspace::get_user_commands()[u].prefix))
            {
              do_USERCMD(out, line, orig_line, cmd, args, u);
              return true;
            }
       }

     out << "BAD COMMAND" << endl;
     return false;
}
//----------------------------------------------------------------------------
bool
Command::check_params(ostream & out, const char * command, int argc,
                      const char * args)
{
   // allow everything for ]USERCMD
   //
   if (!strcmp(command, "]USERCMD"))   return false;

   // analyze args to figure the number of parametes expected.
   //
int mandatory_args = 0;
int opt_args = 0;
int brackets = 0;
bool in_param = false;
bool many = false;

UTF8_string args_utf(args);
UCS_string args_ucs(args_utf);
   loop (a, args_ucs.size())   switch(args_ucs[a])
       {
         case '[': ++brackets;   in_param = false;   continue;
         case ']': --brackets;   in_param = false;   continue;
         case '|':               in_param = false;
              if (brackets)   --opt_args;
              else            --mandatory_args;
              continue;
         case '.':
              if (a < (args_ucs.size() - 2) &&
                  args_ucs[a + 1] == '.'    &&
                  args_ucs[a + 2] == '.')   many = true;
              continue;
         case '0' ... '9':
         case '_':
         case 'A' ... 'Z':
         case 'a' ... 'z':
         case UNI_OVERBAR:
         case '-':
              if (!in_param)   // start of a name or range
                 {
                   if (brackets)   ++opt_args;
                   else            ++mandatory_args;
                   in_param = true;
                 }
              continue;

         case ' ': in_param = false;
              continue;

         default: Q1(args_ucs[a])   Q1(int(args_ucs[a]))
       }

   if (argc < mandatory_args)   // too few parameters
      {
        out << "BAD COMMAND+" << endl;
        MORE_ERROR() << "missing parameter(s) in command " << command
                     << ". Usage:\n"
                     << "      " << command << " " << args;
        return true;
      }

   if (many)   return false;

   if (argc > (mandatory_args + opt_args))   // too many parameters
      {
        out << "BAD COMMAND+" << endl;
        MORE_ERROR() << "too many (" << argc << ") parameter(s) in command "
                     << command << ". Usage:\n"
                     << "      " << command << " " << args;
        return true;
      }

   return false;   // OK
}
//----------------------------------------------------------------------------
void
Command::do_APL_expression(UCS_string & line)
{
   Workspace::more_error().clear();

Executable * statements = 0;
   try
      {
        statements = StatementList::fix(line, LOC);
      }
   catch (Error err)
      {
        bool plus = Workspace::more_error().size();   // assume + needed
        const char * error_name = Error::error_name(err.get_error_code());
        if (strchr(error_name, UNI_PLUS))   plus = false;   // not needed
        if (Workspace::more_error().size() &&
               Workspace::more_error().back() == UNI_PLUS)
           plus = false; // dito.

        UERR << error_name;
        if (plus)   UERR << UNI_PLUS;
        UERR << endl;
        if (err.get_error_line_2().size())
           {
             COUT << "      " << err.get_error_line_2() << endl
                  << "      " << err.get_error_line_3() << endl;
           }

        err.print(UERR, LOC);
        delete statements;
        return;
      }
   catch (...)
      {
        CERR << "*** Command::process_line() caught other exception ***"
             << endl;
        delete statements;
        cmd_OFF(0);
      }

   if (statements == 0)   // StatementList::fix() failed
      {
        COUT << "main: Parse error";
        if (Workspace::more_error().size())   COUT << "+";
        else                                  COUT << ".";
        COUT << endl;
        return;
      }

   // At this point, the user command was parsed correctly.
   // check for Escape (→)
   //
   {
     const Token_string & body = statements->get_body();
     if (body.size() == 3                &&
         body[0].get_tag() == TOK_ESCAPE &&
         body[1].get_Class() == TC_END   &&
         body[2].get_tag() == TOK_RETURN_STATS)
        {
          // remove all SI entries up to (including) the next immediate
          // execution context
          //
          for (bool goon = true; goon;)
              {
                StateIndicator * si = Workspace::SI_top();
                if (si == 0)   break;   // SI empty

                goon = si->get_parse_mode() != PM_STATEMENT_LIST;
                si->escape();   // pop local vars of user defined functions
                Workspace::pop_SI(LOC);
              }

          delete statements;
          return;
        }
   }

// statements->print(CERR);

   // push a new context for the statements.
   //
   Workspace::push_SI(statements, LOC);
   finish_context();
}
//----------------------------------------------------------------------------
void
Command::finish_context()
{
   for (;;)
       {
         //
         // NOTE that the entire SI may change while executing this loop.
         // We should therefore avoid references to SI entries.
         //
         Token token = Workspace::SI_top()->get_executable()->execute_body();

// Q1(token)

         // start over if execution has pushed a new SI entry
         //
         if (token.get_tag() == TOK_SI_PUSHED)   continue;

check_EOC:
         if (Quad_FIO::benchmark_cycles_from)   // a benchmark is ongoing
            {
              if (StateIndicator * si = Workspace::SI_top()->get_parent())
                 {
                   if (si->is_safe_execution_start())
                      {
                        // at this point token is the result of a defined
                        // function which is being benchmarked. Replace the
                        // result by the number of CPU cycles executed since
                        // the start of the benchmark.
                        //
                        const uint64_t to = cycle_counter();
                        const uint64_t from = Quad_FIO::benchmark_cycles_from;
                        const uint64_t diff = to - from;
                        Value_P val = IntScalar(diff, LOC);
                        token.~Token();   // free the value (if any)
                        new (&token) Token(TOK_APL_VALUE1, val);
                        si->clear_safe_execution();
                      }
                 }
            }
         else if (Workspace::SI_top()->is_safe_execution_start())
            {
              if (Quad_FIO::benchmark_cycles_from)
                 {
                   // ⎕FIO[-1] has started a cycle measurement. Read the CPU
                   // cycle counter, compute the cycle difference, stop the
                   // measurement, and return the the cycle difference as int
                   // scalar...
                   //
                   const uint64_t to = cycle_counter();
                   const uint64_t diff = to - Quad_FIO::benchmark_cycles_from;
                   token = Token(TOK_APL_VALUE1, IntScalar(diff, LOC));
                   Quad_FIO::benchmark_cycles_from = 0;
                 }
              else
                 {
                   Quad_EC::eoc(token);
                 }
            }

         // the far most frequent cases are TC_VALUE and TOK_VOID
         // so we handle them first.
         //
         if (token.get_Class() == TC_VALUE || token.get_tag() == TOK_VOID )
            {
              if (Workspace::SI_top()->get_parse_mode() == PM_STATEMENT_LIST)
                 {
                   if (attention_is_raised())
                      {
                        clear_attention_raised(LOC);
                        clear_interrupt_raised(LOC);
                        ATTENTION;
                      }

                   break;   // will return to calling context
                 }

              Workspace::pop_SI(LOC);

              // we are back in the calling SI. There should be a TOK_SI_PUSHED
              // token at the top of stack. Replace it with the result from
              //  the called (just poped) SI.
              //
              {
                Prefix & prefix =
                         Workspace::SI_top()->get_prefix();
                Assert(prefix.at0().get_tag() == TOK_SI_PUSHED);

                new (&prefix.tos().get_token()) Token(token);
              }
              if (attention_is_raised())
                 {
                   clear_attention_raised(LOC);
                   clear_interrupt_raised(LOC);
                   ATTENTION;
                 }

              continue;
            }

         if (token.get_tag() == TOK_BRANCH)
            {
              const Function_Line line = Function_Line(token.get_int_val());
              if (line == Function_Retry                                     &&
                  Workspace::SI_top()->get_parse_mode() == PM_STATEMENT_LIST &&
                  Workspace::SI_top()->get_parent())
                 {
                   Workspace::pop_SI(LOC);
                   Workspace::SI_top()->retry(LOC);
                   continue;
                 }

              StateIndicator * si = Workspace::SI_top_fun();

              if (si == 0)
                 {
                    MORE_ERROR() <<
                    "branch back into function (→N) without suspended function";
                    SYNTAX_ERROR;   // →N without function,
                 }

              // pop contexts above defined function
              //
              while (si != Workspace::SI_top())   Workspace::pop_SI(LOC);

              si->goon(line, LOC);
              continue;
            }

         if (token.get_tag() == TOK_ESCAPE)
            {
              // remove all SI entries up to (including) the next immediate
              // execution context
              //
              for (bool goon = true; goon;)
                  {
                    StateIndicator * si = Workspace::SI_top();
                    if (si == 0)   break;   // SI empty

                    goon = si->get_parse_mode() != PM_STATEMENT_LIST;
                    si->escape();   // pop local vars of user defined functions
                    Workspace::pop_SI(LOC);
                  }
              return;
            }

         if (token.get_tag() == TOK_ERROR)
            {
              if (token.get_int_val() == E_COMMAND_PUSHED)
                 {
                   Workspace::pop_SI(LOC);
                   UCS_string pushed_command = Workspace::get_pushed_Command();
                   process_line(pushed_command, 0);
                   pushed_command.clear();
                   Workspace::push_Command(pushed_command);   // clear in
                   return;
                 }

              // clear attention and interrupt flags
              //
              clear_attention_raised(LOC);
              clear_interrupt_raised(LOC);

              // check for safe execution mode. Unroll all SI entries that
              // have the same safe_execution_count, except the last
              // unroll the SI stack.
              //
              if (Workspace::SI_top()->get_safe_execution_count())
                 {
                   // SI_top() is in save execution mode. Pop it and all
                   // callers with the same get_safe_execution_count().
                   //
                   // after pop'ing the SI entries only the original SI
                   // (which has set the save execution mode) shall remain
                   // the SI stack.
                   //
                   StateIndicator * si = Workspace::SI_top();
                   const int sex_level = si->get_safe_execution_count();
                   while (si->get_parent() && sex_level ==
                          si->get_parent()->get_safe_execution_count())
                      {
                        si = si->get_parent();
                        Workspace::pop_SI(LOC);
                      }

                    goto check_EOC;
                  }

              // if suspend is not allowed then pop all SI entries that
              // don't allow suspend
              //
              if (Workspace::SI_top()->get_executable()->cannot_suspend())
                 {
                    Error err = StateIndicator::get_error(Workspace::SI_top());
                    while (Workspace::SI_top()->get_executable()
                                              ->cannot_suspend())
                       {
                         Workspace::pop_SI(LOC);
                       }

                   if (Workspace::SI_top())
                      {
                        StateIndicator::get_error(Workspace::SI_top()) = err;
                      }
                 }

              if (Workspace::get_error()->get_print_loc() == 0)   // not printed
                 {
                   Workspace::get_error()->print(CERR, LOC);
                 }
              else
                 {
                    // CERR << "ERROR printed twice" << endl;
                 }

              if (Workspace::SI_top()->get_level() == 0)
                 {
                   Value::erase_stale(LOC);
                   IndexExpr::erase_stale(LOC);
                 }
              return;
            }

         // we should not come here.
         //
         Q1(token)  Q1(token.get_Class())  Q1(token.get_tag())  FIXME;
       }

   // pop the context for the statements
   //
   Workspace::pop_SI(LOC);
}
//----------------------------------------------------------------------------
void
Command::cmd_XTERM(ostream & out, const UCS_string & arg)
{
const char * term = getenv("TERM");
   if (!strncmp(term, "dumb", 4) && arg.starts_iwith("ON"))
      {
        out << "impossible on dumb terminal" << endl;
      }
   else if (arg.starts_iwith("OFF") || arg.starts_iwith("ON"))
      {
        Output::toggle_color(arg);
      }
   else if (arg.size() == 0)
      {
        out << "]COLOR/XTERM ";
        if (Output::color_enabled()) out << "ON"; else out << "OFF";
        out << endl;
      }
   else
      {
        out << "BAD COMMAND" << endl;
        return;
      }
}
//----------------------------------------------------------------------------
UCS_string_vector
Command::split_arg(const UCS_string & arg)
{
UCS_string_vector result;
   for (size_t idx = 0; ; )
      {
        UCS_string token;
        idx = arg.copy_black(token, idx);
        if (token.size() == 0)   return result;

        result.push_back(token);
      }
}
//----------------------------------------------------------------------------
void
Command::cmd_BOXING(ostream & out, const UCS_string & arg)
{
int format = arg.atoi();

   if (arg.size() == 0)
      {
        out << "]BOXING ";
        if (boxing_format == 0) out << "OFF";
        else out << boxing_format;
        out << endl;
        return;
      }

   if (arg.starts_iwith("OFF"))   format = 0;
   switch (format)
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
        case 29:
                 boxing_format = format;
                 return;
      }

   out << "BAD ]BOXING PARAMETER+" << endl;
   MORE_ERROR() << "Parameter " << arg << " is not valid for command ]BOXING.\n"
      "  Valid parameters are OFF, N, and -N with\n"
      "  N ϵ { 2, 3, 4, 7, 8, 9, 20, 21, 22, 23, 24, 25, 29 }";
}
//----------------------------------------------------------------------------
bool
Command::val_val::compare_val_val(const val_val & A,
                                  const val_val & B, const void *)
{
   return A.child > B.child;
}
//----------------------------------------------------------------------------
int
Command::val_val::compare_val_val1(const void * key, const void * B)
{
const void * Bv = reinterpret_cast<const val_val *>(B)->child;
   return charP(key) - charP(Bv);
}
//----------------------------------------------------------------------------
void
Command::cmd_CHECK(ostream & out, const UCS_string & arg)
{
bool show_OK = true;   // assume more verbose output
   if (arg.size())   // check with argument (supposedly BRIEF).
      {
        UCS_string arg0(arg);
        arg0.remove_leading_and_trailing_whitespaces();
        const UCS_string brief(UTF8_string("BRIEF"));
        show_OK = arg0.compare(brief) != COMP_EQ;   // not BRIEF
        if (show_OK)   // still show_OK
           {
             out << arg << "? Expecting BRIEF." << endl;
             return;

           }
      }
   // 1. erase stale functions from failed ⎕EX
   //
   {
     bool erased = false;
     if (const int stale = Workspace::cleanup_expunged(CERR, erased))
        {
          out << "WARNING - " << stale << " stale functions ("
               << (erased ? "" : "not ") << "erased)" << endl;
        }
     else if (show_OK)
        {
          out << "OK      - no stale functions" << endl;
        }
   }

   // 2. print stale values (if any)
   //
   {
     if (const int stale = Value::print_stale(CERR))
        {
          out << "ERROR   - " << stale << " stale values" << endl;
          IO_Files::apl_error(LOC);
        }
     else if (show_OK)
        {
          out << "OK      - no stale values" << endl;
        }
   }

   // 3. print stale index expressions (if any)
   {
     if (const int stale = IndexExpr::print_stale(CERR))
        {
          out << "ERROR   - " << stale << " stale indices" << endl;
          IO_Files::apl_error(LOC);
        }
     else if (show_OK)
        {
          out << "OK      - no stale indices" << endl;
        }
   }

   // 4. discover duplicate parents. In the old clone() scheme every nested
   //    value has (at most) one parent. In the new clone() scheme, however,
   //    a nested value can be reused and have multiple parents.
   {
#ifndef NEW_CLONE   // old clone
     // 4a. create a { parent = 0, value } vector<val_val> of all values
     //
     std::vector<val_val> val_vals;
     ShapeItem duplicate_parents = 0;
     for (const DynamicObject * obj =
                DynamicObject::get_all_values()->get_next();
          obj != DynamicObject::get_all_values(); obj = obj->get_next())
         {
           const Value * val = static_cast<const Value *>(obj);

           val_val vv = { 0, val };   // no parent
           val_vals.push_back(vv);
         }

     // 4b. sort vector<val_val> val_vals by address so we can bsearch it.
     //
     Heapsort<val_val>::sort(&val_vals.front(), val_vals.size(), 0,
                             &val_val::compare_val_val);
     loop(v, (val_vals.size() - 1))
         Assert(&val_vals[v].child < &val_vals[v + 1].child);

      // 4c. set parents of pointer cells
      //
      loop(v, val_vals.size())   // for every .child (acting as parent here)
          {
            const Value * val = val_vals[v].child;
            const ShapeItem ec = val->nz_element_count();
            loop(e, ec)   // for every ravel cell of the (parent-) value
                {
                  const Cell & cP = val->get_cravel(e);
                  if (!cP.is_pointer_cell())   continue;   // not a parent

                  const Value * sub = cP.get_pointer_value().get();
                  Assert1(sub);

                  val_val * vvp = reinterpret_cast<val_val *>
                       (bsearch(sub, &val_vals.front(), val_vals.size(),
                                sizeof(val_val), val_val::compare_val_val1));
                  Assert(vvp);
                  if (vvp->parent == 0)   // child has no parent (OK)
                     {
                       vvp->parent = val;
                       continue;
                     }

                  // the child already has a parent (which is bad).
                  // print its history to help figuring why.
                  //
                  ++duplicate_parents;
                  out << "Value * vvp=" << voidP(vvp) << " already has parent "
                      << voidP(vvp->parent) << " when checking Value * val="
                      << voidP(vvp) << endl;

                  out << "History of the child:" << endl;
                  VH_entry::print_history(out, *vvp->child, LOC);
                  out << "History of the first parent:" << endl;
                  VH_entry::print_history(out, *vvp->parent, LOC);
                  out << "History of the second parent:" << endl;
                  VH_entry::print_history(out, *val, LOC);
                  out << endl;
               }
          }

     if (duplicate_parents)
          {
            out << "ERROR   - " << duplicate_parents
                << " duplicate parents" << endl;
            IO_Files::apl_error(LOC);
          }
#endif
          {
            if (show_OK)
               out << "OK      - no duplicate parents" << endl;
          }
   }

   // 5. discover strange Cells and counters.
   //
   Value::check_all_Cells(out);
}
//----------------------------------------------------------------------------
void
Command::cmd_CONTINUE(ostream & out)
{
UCS_string wsname(UTF8_string("CONTINUE"));
   Workspace::wsid(out, wsname, LIB0, false);     // )WSID CONTINUE
   Workspace::save_WS(out, LIB0, wsname, true);   // )SAVE
   cmd_OFF(0);                                    // )OFF
}
//----------------------------------------------------------------------------
void
Command::cmd_COPY(ostream & out, UCS_string_vector & args, bool protection)
{
LibRef libref = LIB0;   // library reference number to copy from, default is 0

   if (args.size() == 0)   // at least workspace name is required
      {
        out << "BAD COMMAND+" << endl;
        MORE_ERROR() << "missing workspace name in command )COPY or )PCOPY";
        return;
      }

   // process and skip the optional library number
   {
     const Unicode l = args.front()[0];
     if (Avec::is_digit(l))
        {
          libref = LibRef(l - '0');
          args.erase(0);
        }
   }

UCS_string wsname = args.front();
   args.erase(0);
   Workspace::copy_WS(out, libref, wsname, args, protection);
}
//----------------------------------------------------------------------------
void
Command::cmd_COPY_ONCE(ostream & out, UCS_string_vector & args)
{
   if (args.size() > 2)   // too many arguments
      {
        MORE_ERROR() << "too many parameters in command ]COPY_ONCE";
        return;
      }

LibRef libref = LIB0;   // library reference number to copy from, default is 0
   if (args.size() == 0)   // no argument means: display current table
      {
        if (copy_once_table.size() == 0)
           {
             out << "There were no )COPY_ONCE workspaces copied." << endl;
             return;
           }

        out << "There were " << copy_once_table.size()
            << " )COPY_ONCE workspaces copied:" << endl;
        UCS_string_vector usv;
        loop(row, copy_once_table.size())
            {
              const UCS_string & src = copy_once_table[row];
              UCS_string ws(UNI_SPACE);
              ws += src.front();      // wsname
              ws += UNI_SPACE;
              ws.append(UCS_string(src, 2, src.size() - 2));
              ws += UNI_SPACE;
              usv.push_back(ws);
            }
        usv.print_table(out, 1);
        return;
      }

   // process and skip the optional library number
   //
   if (args.size() == 2)
      {
        const UCS_string & arg0 = args.front();   // first argument
        const Unicode l = arg0.front();           // first character
        if (Avec::is_digit(l))
           {
             libref = LibRef(l - '0');
             args.erase(0);
           }
      }

   Assert(args.size() == 1);   // only wsname left
const UCS_string wsname(args.front());
   args.erase(0);

   // lib_wsname is the name in the copy_once_table
   //
UCS_string lib_wsname(Unicode(libref + UNI_0));
   lib_wsname += UNI_UNDERSCORE;
   lib_wsname.append(wsname);

   // silently return if wsname is already contained in the copy_once_table
   //
   if (copy_once_table.contains(lib_wsname))   return;

   // add it to the table;
   //
   out << "NEW )COPY_ONCE workspace: ";
   if (libref)   out << libref << " ";
   out << wsname << endl;

   copy_once_table.push_back(lib_wsname);
   Workspace::copy_WS(out, libref, wsname, args, false);
}
//----------------------------------------------------------------------------
void
Command::clear_copy_once_table()
{
   if (const size_t count = copy_once_table.size())
      {
        copy_once_table.clear();
        CERR << ")COPY_ONCE table cleared (" << count << " entries)" << endl;
      }
}
//----------------------------------------------------------------------------
void
Command::cmd_DOXY(ostream & out, const UCS_string_vector & args)
{
UTF8_string root("/tmp");
   if (args.size())   root = UTF8_string(args.front());

   try
     {
       Doxy doxy(out, root);
       doxy.gen();

       if (doxy.get_errors())
          out << "Command ]DOXY failed (" << doxy.get_errors() << " errors)"
              << endl;
      else
         out << "Command ]DOXY finished successfully." << endl
             << "    The generated documentation was stored in directory "
             << doxy.get_root_dir() << endl
             << "    You may now browse it from file://"
             << doxy.get_root_dir()
             << "/index.html" << endl;
     }
   catch (...) {}
}
//----------------------------------------------------------------------------
void
Command::cmd_DROP(ostream & out, const UCS_string_vector & lib_ws)
{
   // Command is:
   //
   // )DROP wsname
   // )DROP libnum wsname

   // lib_ws.size() is 1 or 2. If 2 then the first is the lib number
   //
LibRef libref = LIB_NONE;
UCS_string wname = lib_ws.back();
   if (lib_ws.size() == 2)   libref = LibRef(lib_ws.front()[0] - '0');

UTF8_string filename = LibPaths::get_lib_filename(libref, wname, true,
                                                  ".xml", ".apl");

const int result = unlink(filename.c_str());
   if (result)
      {
        out << wname << " NOT DROPPED: " << strerror(errno) << endl;
        MORE_ERROR() << "could not unlink file " << filename;
      }
   else
      {
        Workspace::get_v_Quad_TZ().print_timestamp(out, now()) << endl;
      }
}
//----------------------------------------------------------------------------
void
Command::cmd_DUMP(ostream & out, const UCS_string_vector & args,
                  bool html, bool silent)
{
   CHECK_SECURITY(disable_SAVE_command);

   // Command is:
   //
   // )DUMP
   // )DUMP workspace
   // )DUMP lib workspace

   if (args.size() > 0)   // workspace or lib workspace
      {
        LibRef lib;
        UCS_string wsname;
        if (resolve_lib_wsname(out, args, lib, wsname))   return;   // error
        Workspace::dump_WS(out, lib, wsname, html, silent);
        return;
      }

   // )DUMP: use )WSID unless it is CLEAR WS
   //
LibRef wsid_lib = LIB0;
UCS_string wsid_name = Workspace::get_WS_name();
   if (Avec::is_digit(wsid_name.front()))   // wsid contains a libnum
      {
        wsid_lib = LibRef(wsid_name.front() - '0');
        wsid_name.erase(0);
        wsid_name.remove_leading_whitespaces();
      }

   if (wsid_name.compare(UCS_ASCII_string("CLEAR WS")) == COMP_EQ)
      { 
        // don't dump CLEAR WS
        //
        COUT << "NOT DUMPED: THIS WS IS CLEAR WS" << endl;
        MORE_ERROR() <<
        "the workspace was not dumped because 'CLEAR WS' is a special\n"
        "workspace name that cannot be dumped. "
        "First create WS name with )WSID <name>."; 
        return;
      }

   Workspace::dump_WS(out, wsid_lib, wsid_name, html, silent);
}
//----------------------------------------------------------------------------
void
Command::cmd_ERASE(ostream & out, UCS_string_vector & args)
{
   Workspace::erase_symbols(out, args);
}
//----------------------------------------------------------------------------
void
Command::cmd_KEYB(ostream & out)
{
   // maybe print user-supplied keyboard layout file
   //
   if (UserPreferences::uprefs.keyboard_layout_file.size())
      {
        FILE * layout =
               fopen(UserPreferences::uprefs.keyboard_layout_file.c_str(), "r");
        if (layout == 0)
           {
             out << "Could not open "
                 << UserPreferences::uprefs.keyboard_layout_file
                 << ": " << strerror(errno) << endl
                 << "Showing default layout instead" << endl;
           }
        else
           {
             out << "User-defined Keyboard Layout:\n";
             for (;;)
                 {
                    const int cc = fgetc(layout);
                    if (cc == EOF)   break;
                    out << char(cc);
                 }
             out << endl;
             return;
           }
      }

   out << "US Keyboard Layout:\n"
                              "\n"
"╔════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦═════════╗\n"
"║ ~  ║ !⌶ ║ @⍫ ║ #⍒ ║ $⍋ ║ %⌽ ║ ^⍉ ║ &⊖ ║ *⍟ ║ (⍱ ║ )⍲ ║ _! ║ +⌹ ║         ║\n"
"║ `◊ ║ 1¨ ║ 2¯ ║ 3< ║ 4≤ ║ 5= ║ 6≥ ║ 7> ║ 8≠ ║ 9∨ ║ 0∧ ║ -× ║ =÷ ║ BACKSP  ║\n"
"╠════╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦══════╣\n"
"║       ║ Q  ║ W⍹ ║ E⍷ ║ R  ║ T⍨ ║ Y¥ ║ U  ║ I⍸ ║ O⍥ ║ P⍣ ║ {⍞ ║ }⍬ ║  |⊣  ║\n"
"║  TAB  ║ q? ║ w⍵ ║ eϵ ║ r⍴ ║ t∼ ║ y↑ ║ u↓ ║ i⍳ ║ o○ ║ p⋆ ║ [← ║ ]→ ║  \\⊢  ║\n"
"╠═══════╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩══════╣\n"
"║ (CAPS   ║ A⍶ ║ S« ║ D» ║ F  ║ G  ║ H  ║ J⍤ ║ K  ║ L⌷ ║ :≡ ║ \"≢ ║         ║\n"
"║  LOCK)  ║ a⍺ ║ s⌈ ║ d⌊ ║ f_ ║ g∇ ║ h∆ ║ j∘ ║ kλ ║ l⎕ ║ ;⍎ ║ '⍕ ║ RETURN  ║\n"
"╠═════════╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═════════╣\n"
"║             ║ Z  ║ Xχ ║ C¢ ║ V  ║ B£ ║ N  ║ M  ║ <⍪ ║ >⍙ ║ ?  ║          ║\n"
"║  SHIFT      ║ z⊂ ║ x⊃ ║ c∩ ║ v∪ ║ b⊥ ║ n⊤ ║ m| ║ ,⍝ ║ .⍀ ║ /⌿ ║  SHIFT   ║\n"
"╚═════════════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩══════════╝\n"
   << endl;
}
//----------------------------------------------------------------------------
void
Command::cmd_PSTAT(ostream & out, const UCS_string & arg)
{
#ifndef cfg_PERFORMANCE_COUNTERS_WANTED
   out << "\n"
<< "Command ]PSTAT is not available, since performance counters were not\n"
"configured for this APL interpreter. To enable performance counters (which\n"
"will slightly decrease performance), recompile the interpreter as follows:"

<< "\n\n"
"   ./configure PERFORMANCE_COUNTERS_WANTED=yes (... "
<< "other configure options"
<< ")\n"
"   make\n"
"   make install (or try: src/apl to test without installing)\n"
"\n"

<< "above the src directory."
<< "\n";

   return;
#endif

   if (arg.starts_iwith("CLEAR"))
      {
        out << "Performance counters cleared" << endl;
        Performance::reset_all();
        return;
      }

   if (arg.starts_iwith("SAVE"))
      {
        const char * filename = "./PerformanceData.def";
        ofstream outf(filename, ofstream::out);
        if (!outf.is_open())
           {
             out << "opening " << filename
                 << " failed: " << strerror(errno) << endl;
             return;
           }

        out << "Writing performance data to file " << filename << endl;
        Performance::save_data(out, outf);
        return;
      }

Pfstat_ID iarg = PFS_ALL;
   if (arg.size() > 0)   iarg = Pfstat_ID(arg.atoi());

   Performance::print(iarg, out);
}
//----------------------------------------------------------------------------
void
Command::primitive_help(ostream & out, const char * arg, int arity,
                        const char * prim,  const char * name,
                        const char * brief, const char * descr)
{
   if (strcmp(arg, prim))   return;

   switch(arity)
      {
        case -6: out << "   " << name << ":   " << brief << endl
                     << "    " << descr << endl;            return;

        case -5: out << "   quasi-dyadic operator:"
                        "   Z ← A (∘ . G) B";               break;
        case -4: out << "   dyadic primitive operator:"
                        "   Z ← A (F . G) B";               break;
        case -3: out << "   dyadic primitive operator:"
                        "   Z ← (F " << prim << " G) B";    break;
        case -2: out << "   monadic primitive operator:"
                        "  Z ← A (F " << prim << ") B";     break;
        case -1: out << "   monadic primitive operator:"
                        "  Z ← (F " << prim << ") B";       break;
        case  0: out << "   niladic primitive function:"
                        " Z ← " << prim;                    break;
        case  1: out << "   monadic primitive function:"
                        " Z ← " << prim << " B";            break;
        case  2: out << "   dyadic primitive function:"
                        "  Z ← A " << prim << " B";         break;
        case  3: out << "   monadic primitive function (with axis):"
                        "  Z ← " << prim << "[X] B";      break;

        default: FIXME;
      }

   if (*name)   out << "  ("  << name  <<  ")";
   out << endl;
   if (*brief)  out << "    " << brief << endl;

   if (descr)   out << descr << endl;
}
//----------------------------------------------------------------------------

/// return the length differece between a UCS_string and its UTF8 encoding
static inline int
len_diff(const char * txt)
{
int ret = 0;
   while (const char cc = *txt++)   if ((cc & 0xC0) == 0x80)   ++ret;
   return ret;
}

void
Command::cmd_HELP(ostream & out, const UCS_string & _arg)
{
   // map alternate APL characters to standard ones
   //
UCS_string arg;
   loop(a, _arg.size())   arg += Avec::make_standard(_arg[a]);

   if (arg.size() > 0 && Avec::is_first_symbol_char(arg.front()))
      {
        // help for a user defined name
        //
        CERR << "symbol '" << arg << "' ";
        Symbol * sym = Workspace::lookup_existing_symbol(arg);
        if (sym == 0)
           {
             CERR << "does not exist in the current workspace" << endl;
             return;
           }

        if (sym->is_erased())
           {
             CERR << "is erased." << endl;
             return;
           }

        ValueStackItem * vs = sym->top_of_stack();
        if (vs == 0)
           {
             CERR << " has no stack." << endl;
             return;
           }

        switch(vs->get_NC())
           {
             case NC_INVALID:
                  CERR << "has no valid name class" << endl;
                  return;

             case NC_UNUSED_USER_NAME:
                  CERR << "is an unused name" << endl;
                  return;

             case NC_LABEL:
                  CERR << "is a label (line " << vs->get_label()
                       << ")" << endl;
                  return;

             case NC_VARIABLE:
                  {
                    CERR << "is a variable:" << endl;
                    Value_P val = sym->get_apl_value();
                    if (+val)   val->print_properties(CERR, 4, true);
                  }
                  CERR << endl;
                  return;

             case NC_FUNCTION:
                  {
                    cFunction_P fun = sym->get_function();
                    Assert(fun);
                    if (fun->is_native())
                       {
                         const NativeFunction *nf =
                               reinterpret_cast<const NativeFunction *>(fun);
                         CERR << "is a native function implemented in "
                              << nf->get_so_path() << endl
                              << "    load state: " << (nf->is_valid() ?
                                 "OK (loaded)" : "error") << endl;
                         return;
                       }

                    CERR << "is a ";
                    if      (fun->get_fun_valence() == 2)   CERR << "dyadic";
                    else if (fun->get_fun_valence() == 1)   CERR << "monadic";
                    else                                    CERR << "niladic";
                    CERR << " defined function:" << endl;

                    const UserFunction * ufun = fun->get_func_ufun();
                    Assert(ufun);
                    ufun->help(CERR);
                  }
                  return;

             case NC_OPERATOR:
                  {
                    cFunction_P fun = sym->get_function();
                    Assert(fun);
                    CERR << "is a ";
                    if (fun->get_oper_valence() == 2)   CERR << "dyadic";
                    else                                CERR << "monadic";
                    CERR << " defined operator:" << endl;

                    const UserFunction * ufun = fun->get_func_ufun();
                    Assert(ufun);
                    ufun->help(CERR);
                  }
                  return;

             case NC_SYSTEM_VAR:
                  CERR << "is a shared variable" << endl;
                  return;

             default:
                  FIXME;
           }

        return;
      }

   {
     bool prim = arg.size() == 1;   // standard (1-character) APL primitive
     if (arg.size() == 2)
        prim = arg.front() == UNI_DOWN_TACK ||
              (arg.front() == UNI_COMMENT && arg[1] == UNI_COMMENT);

     if (prim)
        {
          UTF8_string arg_utf(arg);
          const char * arg_cp = arg_utf.c_str();

#define help_def(ar, prim, name, title, descr)              \
   primitive_help(out, arg_cp, ar, prim, name, title, descr);
#include "Help.def"

         return;
        }
   }

   enum { COL2 = 40 };   ///< where the second column starts

UCS_string_vector commands;
   commands.reserve(60);

   out << left << "APL Commands:" << endl;
#define cmd_def(cmd_str, _cod, arg, _hint) \
   { UCS_string c(UTF8_string(cmd_str " " arg));   commands.push_back(c); }
#include "Command.def"

bool left_col = true;
   loop(c, commands.size())
      {
        const UCS_string & cmd = commands[c];
        if (left_col)
           {
              out << "      " << setw(COL2 - 2) << cmd;
              left_col = false;
           }
        else
           {
              out << cmd << endl;
              left_col = true;
           }
      }

  if (Workspace::get_user_commands().size())
     {
       out << endl << "User defined commands:" << endl;
       loop(u, Workspace::get_user_commands().size())
           {
             out << "      " << Workspace::get_user_commands()[u].prefix
                 << " [args]  calls:  ";
             if (Workspace::get_user_commands()[u].mode)
                out << "tokenized-args ";
 
             out << Workspace::get_user_commands()[u].apl_function
                 << " quoted-args" << endl;
           }
     }

   out << endl << "System variables:" << endl
       << "      " << setw(COL2)
       << "⍞       Character Input/Output"
       << "⎕       Evaluated Input/Output" << endl;
   left_col = true;

#define ro_sv_def(x, _str, txt)                                            \
   { const UCS_string & ucs = Workspace::get_v_ ## x().get_name();         \
     if (left_col)   out << "      " << setw(8) << ucs << setw(30) << txt; \
     else            out << setw(8) << ucs << txt << endl;                 \
        left_col = !left_col; }
#define rw_sv_def(x, _str, txt)                                            \
   { const UCS_string & ucs = Workspace::get_v_ ## x().get_name();         \
     if (left_col)   out << "      " << setw(8) << ucs << setw(30) << txt; \
     else            out << setw(8) << ucs << txt << endl;                 \
        left_col = !left_col; }
#include "SystemVariable.def"

   out << endl << "System functions:" << endl;
   left_col = true;
#define ro_sv_def(x, _str, _txt)
#define rw_sv_def(x, _str, _txt)
#define sf_def(_q, str, txt)                                              \
   if (left_col)   out << "      ⎕" << setw(7) << str << setw(30 +        \
                                        len_diff(txt)) << txt;            \
   else            out << "⎕" << setw(7) << str << txt << endl;           \
   left_col = !left_col;
#include "SystemVariable.def"
}
//----------------------------------------------------------------------------
void
Command::cmd_HISTORY(ostream & out, const UCS_string & arg)
{
   if (arg.size())   // )HISTORY  CLEAR (or else line filter)
      {
        if (arg.starts_iwith("CLEAR"))   LineInput::clear_history(out);
        else                             LineInput::print_history(out, arg);
      }
   else              // )HISTORY (with no argument/filter)
      {
        UCS_string no_filter;
        LineInput::print_history(out, no_filter);
      }
}
//----------------------------------------------------------------------------
void
Command::cmd_HOST(ostream & out, const UCS_string & arg)
{
   if (UserPreferences::uprefs.safe_mode)
      {
        out << 
"This interpreter was started in \"safe mode\" (command line option --safe,\n"
"see ⎕ARG). The APL command )HOST is not permitted in safe mode." << endl;
        return;
      }

UTF8_string host_cmd(arg);
FILE * pipe = popen(host_cmd.c_str(), "r");
   if (pipe == 0)   // popen() failed
      {
        out << ")HOST command failed: " << strerror(errno) << endl;
        return;
      }

   // reset SIGCHLD to its default so that pclose() works as expected
   //
   signal(SIGCHLD, SIG_DFL);
   for (;;)
       {
         const int cc = fgetc(pipe);
         if (cc == EOF)   break;
         out << char(cc);
       }

   errno = 0;
const int result = pclose(pipe);
   Log(LOG_verbose_error)
      {
        if (result)   CERR << "NOTE: pclose(" << arg << ") says: errno="
                           << errno << " (" << strerror(errno) << ")" << endl;
      }

   out << endl << result << ' ' << endl;
   signal(SIGCHLD, SIG_IGN);
}
//----------------------------------------------------------------------------
void
Command::cmd_IN(ostream & out, UCS_string_vector & args, bool protection)
{
   // Command is:
   //
   // IN filename [objects...]

UCS_string fname = args.front();
   args.front() = args.back();
   args.pop_back();

UTF8_string filename = LibPaths::get_lib_filename(LIB_NONE, fname, true,
                                                  ".atf", 0);

FILE * in = fopen(filename.c_str(), "r");
   if (in == 0)   // open failed: try filename.atf unless already .atf
      {
        UTF8_string fname_utf8(fname);
        CERR << ")IN " << fname_utf8.c_str()
             << " failed: " << strerror(errno) << endl;

        char cc[200];
        SPRINTF(cc, "command )IN: could not open file %s for reading: %s",
                    fname_utf8.c_str(), strerror(errno));
        Workspace::more_error() << cc;
        return;
      }

UTF8 buffer[80];
int idx = 0;

transfer_context tctx(protection);

   for (;;)
      {
        const int cc = fgetc(in);
        if (cc == EOF)   break;
        if (idx == 0 && cc == 0x0A)   // optional LF
           {
             // CERR << "CRLF" << endl;
             continue;
           }

        if (idx < 80)
           {
              if (idx < 72)   buffer[idx++] = cc;
              else            buffer[idx++] = 0;
             continue;
           }

        if (cc == 0x0D || cc == 0x15)   // ASCII or EBCDIC
           {
             tctx.is_ebcdic = (cc == 0x15);
             tctx.process_record(buffer, args);

             idx = 0;
             ++tctx.recnum;
             continue;
           }

        CERR << "BAD record charset (neither ASCII nor EBCDIC)" << endl;
        break;
      }

   fclose(in);
}
//----------------------------------------------------------------------------
void
Command::cmd_LOAD(ostream & out, UCS_string_vector & args,
                  UCS_string & quad_lx, bool silent)
{
   // Command is:
   //
   // LOAD wsname
   // LOAD libnum wsname

LibRef lib;
UCS_string wsname;
   if (resolve_lib_wsname(out, args, lib, wsname))   return;   // error

   Workspace::load_WS(out, lib, wsname, quad_lx, silent);
}
//----------------------------------------------------------------------------
void
Command::cmd_LIBS(ostream & out, const UCS_string_vector & args)
{
   // Command is:
   //
   // )LIB N path         (set libdir N to path)
   // )LIB path           (set libroot to path)
   // )LIB                (display root and path states)
   //
   if (args.size() == 2)   // set individual dir
      {
        const UCS_string & libref_ucs = args.front();
        const int libref = libref_ucs.front() - '0';
        if (libref_ucs.size() != 1 || libref < 0 || libref > 9)
           {
             CERR << "Invalid library reference " << libref_ucs << "'" << endl;
             return;
           }

        UTF8_string path(args[1]);
        LibPaths::set_lib_dir(LibRef(libref), path.c_str(),
                              LibPaths::LibDir::CSRC_CMD);
        out << "LIBRARY REFERENCE " << libref << " SET TO " << path << endl;
        return;
      }

   if (args.size() == 1)   // set root
      {
        UTF8_string utf(args.front());
        LibPaths::set_APL_lib_root(utf.c_str());
        out << "LIBRARY ROOT SET TO " << args.front() << endl;
        return;
      }

   out << "Library root: " << LibPaths::get_APL_lib_root() << 
"\n"
"\n"
"Library reference number to (absolute) path mapping:\n"
"\n"
"╔═══╤═════╤═════════════╤══════════════════════════════════════════════════════╗\n"
"║Ref│Conf │State (errno)│ Path to the directory containing the workspace files ║\n"
"╟───┼─────┼─────────────┼──────────────────────────────────────────────────────╢\n";

   loop(d, 10)
       {
          out << "║ " << d << " │";
          UTF8_string path = LibPaths::get_lib_dir(LibRef(d));
          switch(LibPaths::get_cfg_src(LibRef(d)))
             {
                case LibPaths::LibDir::CSRC_NONE:      out << "NONE │" << endl;
                                                       continue;
                case LibPaths::LibDir::CSRC_ENV:       out << "ENV  │";   break;
                case LibPaths::LibDir::CSRC_PWD:       out << "PWD  │";   break;
                case LibPaths::LibDir::CSRC_PREF_SYS:  out << "PSYS │";   break;
                case LibPaths::LibDir::CSRC_PREF_HOME: out << "PUSER│";   break;
                case LibPaths::LibDir::CSRC_CMD:       out << "CMD  │";   break;
             }

        if (DIR * dir = opendir(path.c_str()))
           { out << " present     │ ";   closedir(dir); }
        else
           {
             char cc[10];
             SPRINTF(cc, "(%u)", errno);
             out << " missing " << setw(4) << cc << "│ ";
           }

        out << left << setw(53) << path.c_str() << "║\n";
      }

   out <<
"╚═══╧══╤══╧═════════════╧══════════════════════════════════════════════════════╝\n"
"       │\n"
"       ├── NONE:  found no method to compute the library path\n"
"       ├── CMD:   the path was set with )LIBS N path\n"
"       ├── ENV:   the path came from environment variable $APL_LIB_ROOT\n"
"       ├── PSYS:  the path came from the system preferences in file\n"
"       │                   " << apl_DIR__sysconf << "/gnu-apl.d/preferences\n"
"       ├── PUSER: the path came from user preferences in file\n"
"       │                   $HOME/.config/gnu-apl or $HOME/.gnu-apl\n"
"       └── PWD:   the path is relative to current directory $PWD (last resort)"
       << endl;
}
//----------------------------------------------------------------------------
DIR *
Command::open_LIB_dir(UTF8_string & path, ostream & out,
                      const UCS_string_vector & args)
{
   // args can be one of:
   //                              example:
   // 1.                           )LIB
   // 2.  N                        )LIB 1
   // 3.  )LIB directory-name      )LIB /usr/lib/...
   //

UCS_string arg(UNI_0);
   if (args.size())   arg = args.front();

   if (args.size() == 0)                       // case 1.
      {
        path = LibPaths::get_lib_dir(LIB0);
      }
   else if (arg.size() == 1 &&
            Avec::is_digit(Unicode(arg.front())))   // case 2.
      {
        path = LibPaths::get_lib_dir(LibRef(arg.front() - '0'));
      }
   else                                        // case 3.
      {
        path = UTF8_string(arg);
      }

   // follow symbolic links, but not too often (because symbolic links may
   // form an endless loop)...
   //
   loop(depth, 20)
       {
         char buffer[FILENAME_MAX + 1];
         const ssize_t len = readlink(path.c_str(), buffer, FILENAME_MAX);
         if (len <= 0)   break;   // not a symlink

         buffer[len] = 0;
         if (buffer[0] == '/')   // absolute path
            {
              path = UTF8_string(buffer);
            }
          else                   // relative path
            {
              path += '/';
              path.append_UTF8(UTF8_string(buffer));
            }
       }

DIR * dir = opendir(path.c_str());

   if (dir == 0)
      {
        const char * why = strerror(errno);
        out << "IMPROPER LIBRARY REFERENCE '" << arg << "': " << why << endl;

        MORE_ERROR() <<
        "path '" << path << "' could not be opened as directory: " << why;
        return 0;   // error
      }

   return dir;
}
//----------------------------------------------------------------------------
bool
Command::is_directory(const dirent * entry, const UTF8_string & path)
{
#ifdef _DIRENT_HAVE_D_TYPE
   return entry->d_type == DT_DIR;
#endif

UTF8_string filename = path;
UTF8_string entry_name(entry->d_name);
   filename += '/';
   filename.append_UTF8(entry_name);

DIR * dir = opendir(filename.c_str());
   if (dir) closedir(dir);
   return dir != 0;
}
//----------------------------------------------------------------------------
void
Command::LIB_common(ostream & out, const UCS_string_vector & cmd_args, bool dbg)
{
   // check for (and then extract) optional range and sort parameters...
   //
UCS_string_vector args;
const UCS_string * range = 0;
SORT_ORDER sort = SORT_NONE;
   loop(a, cmd_args.size())
      {
        const UCS_string & arg = cmd_args[a];
        if (arg.size() == 5 && arg.starts_iwith("-size"))
           {
             sort = SORT_SIZE;
             continue;
           }

        if (arg.size() == 5 && arg.starts_iwith("-time"))
           {
             sort = SORT_TIME;
             continue;
           }

        // at this point, arg could be a range (e.g. A-F), or a
        // path (e.g. ./file.apl), or simply a WSID. Assume a WSID,
        //
        bool is_range = false;
        bool is_path = false;
        loop(aa, arg.size())
            {
              const Unicode uni = arg[aa];
              if (uni == UNI_MINUS)
                 {
                   if (!is_path)   is_range = true;
                   break;
                 }

              if (!Avec::is_symbol_char(uni))
                 {
                   is_path = true;
                   break;
                 }
            }

         if (is_path)   // arg is a filename
            {
              args.push_back(arg);
            }
         else if (!is_range)   // arg is a WSID
            {
              args.push_back(arg);
            }
         else if (range)   // second non-range arg
            {
              MORE_ERROR() <<
              "multiple range parameters in )LIB or ]LIB command";
              return;
            }
         else
            {
              range = &arg;
            }
      }

UCS_string from;
UCS_string to;
   if (range)
      {
        const bool bad_from_to = parse_from_to(from, to, *range);
        if (bad_from_to)
           {
             CERR << "bad range argument" << endl;
             MORE_ERROR() << "bad range argument " << *range
                  << ", expecting from-to";
             return;
           }
      }

   // 2. open directory
   //
UTF8_string path;
DIR * dir = open_LIB_dir(path, out, args);
   if (dir == 0)   return;

   // 3. collect the WS files and sub-directories in the )LIBS N directory
   //
UCS_string_vector files;
UCS_string_vector directories;

   for (;;)
       {
         const dirent * entry = readdir(dir);
         if (entry == 0)   break;   // directory loop done
         const size_t dlen = strlen(entry->d_name);
         if (entry->d_name[0] == '.')   continue;   // ignore hidden files

         const UTF8_string filename_utf8(entry->d_name);
         UCS_string filename(filename_utf8);

         // check the range of the name (if any)...
         //
         if (from.size() && filename.lexical_before(from))   continue;
         if (to.size() && to.lexical_before(filename))       continue;

         if (is_directory(entry, path))
            {
              filename.append(UNI_SLASH);
              directories.push_back(filename);
              continue;
            }

         if (filename[dlen - 1] == '~')   continue;  // editor backup

         if (dbg)
            {
              files.push_back(filename);
            }
         else
            {
              if (filename_utf8.ends_with(".apl"))
                 {
                   files.push_back(filename);
                 }
              else if (filename_utf8.ends_with(".xml"))
                 {
                   files.push_back(filename);
                 }
            }
       }
   closedir(dir);

   // 4. sort dirctories and files alphabetically
   //
   directories.sort();   // sort directories alphabetically
   files.sort();         // sort files alphabetically

   // 5. print the directories, then the files
   //
   if (sort)   LIB_print_12(out, path, directories, files, sort);
   else        LIB_print_0(out, path, directories, files);
}
//----------------------------------------------------------------------------
void
Command::LIB_print_0(ostream & out, const UTF8_string lib_path,
                     const UCS_string_vector & directories,
                     const UCS_string_vector & files)
{
   // 1. start with directories and append files, removing duplicates
   //    file names (caused by .apl and .xml extensions). After that
   //    all names are in directories with the extensions removed.
   //
UCS_string_vector all_names;
   loop(d, directories.size())   all_names.push_back(directories[d]);
   loop(f, files.size())
      {
        const UCS_string filename(files[f], 0, files[f].size() - 4);
        if (all_names.size() && all_names.back() == filename)
           {
             // this happens when thetre is both an .apl and an .xml file.
             // Skip the second // to avoid duplicate file names. Otherwise
             // append the name to all.
             //
             continue;
           }
        all_names.push_back(filename);
      }

   // At this point, all_names contains all names, with directories
   // before files and duplicate names removed.
   //
        
   // figure column widths
   //
   enum { tabsize = 4 };

std::vector<int> col_widths;
   all_names.compute_column_width(tabsize, col_widths);

   loop(c, all_names.size())
      {
        const size_t col = c % col_widths.size();
        out << all_names[c];
        if (col == size_t(col_widths.size() - 1) ||
              c == ShapeItem(all_names.size() - 1))
           {
             // last column or last item: print newline
             //
             out << endl;
           }
        else
           {
             // intermediate column: print spaces
             //
             const int len = tabsize*col_widths[col] - all_names[c].size();
             Assert(len > 0);
             loop(l, len)   out << " ";
           }
      }
}
//----------------------------------------------------------------------------
void
Command::LIB_print_12(ostream & out, const UTF8_string lib_path,
                      const UCS_string_vector & directories,
                      const UCS_string_vector & files, SORT_ORDER sort)
{
const size_t max_dir = directories.max_width(1, 1);
const size_t max_file = files.max_width(1, 1);
const size_t max_name = max_dir > max_file ? max_dir : max_file;

   // print directories
   //
   loop(d, directories.size())
       {
         const size_t fill = max_name - directories[d].size() + 9;
         out << directories[d] << string(fill, ' ') << "(DIR)" << endl;
       }

   if (files.size() == 0)   return;

   // re-sort files by size or by time
   //
UCS_string_vector sorted_files;
vector<size_t> sorted_props;
vector<int> file_properties;   // the properties for sorting
vector<bool> appended;         // appended[f] is true if file[f] was appended
   loop(f, files.size())
       {
         const size_t property = sort_property(sort, lib_path, files[f]);
         file_properties.push_back(property);
         appended.push_back(false);
       }

   // find the index of the smallest sort_property and append its
   // corresponding name
   //
   Assert(file_properties.size() == size_t(files.size()));
   while (sorted_files.size() < files.size())   // until done
      {
        int smallest_unused = -1;
        loop(f, files.size())
            {
              if (appended[f])   continue;   // already appended
              if (smallest_unused == -1 ||
                  file_properties[f] < file_properties[smallest_unused])
                 {
                  smallest_unused = f;
                 }
            }

        // at this point the smallest not yet appended index was found
        //
        sorted_files.push_back(files[smallest_unused]);
        sorted_props.push_back(file_properties[smallest_unused]);
        appended[smallest_unused] = true;   // mark it as appended
      }

   loop(f, sorted_files.size())
       {
         out << sorted_files[f];
         for (size_t j = sorted_files[f].size() ; j < max_name; ++j)
             out << " ";

         if (sort == SORT_SIZE)
            {
               out << setw(8) << sorted_props[f] << " bytes" << endl;
            }
         else if (sort == SORT_TIME)
            {
               const time_t when = sorted_props[f];
               out << "  " << ctime(&when);   // ctime() does '\n'
            }
         else FIXME;
       }
}
//----------------------------------------------------------------------------
size_t
Command::sort_property(SORT_ORDER sort, const UTF8_string & lib_path,
                       const UCS_string & wsid)
{
   Assert(sort != SORT_NONE);   // 1 or 2

UTF8_string wsid_utf8(wsid);
UTF8_string name(lib_path);   // e.g. /home/workspaces
   name += '/';               //      /home/workspaces/
   name.append(wsid_utf8);    //      /home/workspaces/wsid

   if (access(name.c_str(), R_OK) == 0)   // file is readable
      {
        struct stat st;
        if (stat(name.c_str(), &st) == 0)   // got stat
           {
             if (sort == SORT_SIZE)   return st.st_size;
             if (sort == SORT_TIME)   return st.st_mtime;
             else                          FIXME;
           }
      }

   return 0;   // invalid
}
//----------------------------------------------------------------------------
void
Command::cmd_LIB1(ostream & out, const UCS_string_vector & args)
{
   /* Command is:

    )LIB [N] [RANGE] [sort]
   
    where:

    N is an optional library number (0-9, default 0)
    RANGE is a range for the file names (two ASCII characters A-Z)
    sort is a sorting order: -T (sort by time) or -S (sort by size)
    */

   Command::LIB_common(out, args, false);
}
//----------------------------------------------------------------------------
void
Command::cmd_LIB2(ostream & out, const UCS_string_vector & args)
{
   /* Command is:

    ]LIB [N] [RANGE] [sort]
   
    where:

    N is an optional library number (0-9, default 0)
    RANGE is a range for the file names (two ASCII characters A-Z)
    sort is a sorting order: -sT (sort by time) or -sS (sort by size)
    */

   // The difference between cmd_LIB2 and cmd_LIB2 is the output
   // channel, i.e. )LIB vs. ]LIB.

   Command::LIB_common(out, args, true);
}
//----------------------------------------------------------------------------
void
Command::cmd_LOG(ostream & out, const UCS_string & arg)
{
#ifdef cfg_DYNAMIC_LOG_WANTED

   log_control(arg);

#else

   out <<
"\n"
"Command ]LOG is not available, since dynamic logging was not\n"
"configured for this APL interpreter. To enable dynamic logging (which\n"
"will slightly decrease performance), recompile the interpreter as follows:\n"
"\n"
"   ./configure DYNAMIC_LOG_WANTED=yes (... other configure options)\n"
"   make\n"
"   make install (or try: src/apl to test without installing)\n"
"\n"
"above the src directory."
"\n";

#endif
}
//----------------------------------------------------------------------------
void
Command::cmd_MORE(ostream & out, const UCS_string_vector & args)
{
   if (args.size() > 0)   // optional AUTO ?
      {
        if (!args.front().starts_iwith("AUTO"))   // no
           {
             CERR << "BAD COMMAND+" << endl;
             MORE_ERROR() << "Bad )MORE argument: " << args.front()
                          << ". Use none or AUTO.";
             return;
           }

        // )MORE AUTO...
        //
        if (args.size() > 1)   // optional ON/OFF ?
           {
             if      (args[1].starts_iwith("ON"))    auto_MORE = true;
             else if (args[1].starts_iwith("OFF"))   auto_MORE = false;
             else
                {
                  CERR << "BAD COMMAND+" << endl;
                  MORE_ERROR() << "Bad )MORE AUTO argument: " << args[1]
                               << ". Use none, ON, or OFF.";
                  return;
                }
           }
        else                   // no ON or OFF:
           {
             auto_MORE = ! auto_MORE;   // toggle
           }
        out << "Automatic )MORE is now: "
            << (auto_MORE ? "ON" : "OFF") << endl;
        return;
      }

   if (Workspace::more_error().size() == 0)
      {
        out << "NO )MORE ERROR INFO" << endl;
        return;
      }

   out << Workspace::more_error() << endl;
   return;
}
//----------------------------------------------------------------------------
void
Command::cmd_OFF(int exit_val)
{
   COUT << endl;
   if (!UserPreferences::uprefs.silent)
      {

        timeval end;
        gettimeofday(&end, 0);
        end.tv_sec -= UserPreferences::uprefs.session_start.tv_sec;
        end.tv_usec -= UserPreferences::uprefs.session_start.tv_usec;
        if (end.tv_usec < 1000000)   { end.tv_usec += 1000000;   --end.tv_sec; }
        COUT << "Goodbye." << endl
             << "Session duration: " << (end.tv_sec + 0.000001*end.tv_usec)
             << " seconds " << endl;
      }

   cleanup(true);

   // restore the initial memory rlimit
   //
#ifndef RLIMIT_AS // BSD does not define RLIMIT_AS
# define RLIMIT_AS RLIMIT_DATA
#endif

rlimit rl;
   getrlimit(RLIMIT_AS, &rl);
   rl.rlim_cur = Quad_WA::initial_rlimit;
   setrlimit(RLIMIT_AS, &rl);

   exit(exit_val);
}
//----------------------------------------------------------------------------
void
Command::cmd_OPTIM(ostream & out, const UCS_string & arg)
{
   if (arg.starts_iwith("CLEAR"))
      {
        out << "Optimization counters cleared" << endl;
        OptmizationStatistics::reset_all();
        return;
      }

int ulen;
#define optim(ena, opt, text) ulen = 40 + UTF8_string::bytes_chars(text);   \
   out << left << setw(ulen) << text << right << " : ";                     \
   if (ena) out << setw(6) << OptmizationStatistics::get(OPTI_ ## opt);     \
   else     out << "disabled";                                              \
   out << endl;
#include "Performance.def"
}
//----------------------------------------------------------------------------
void
Command::cmd_NEXTFILE(ostream & out, const UCS_string_vector & args)
{
   loop(a, args.size())
       {
         const UCS_string & arg = args[a];

         // figure HAVE- or NO- prefix and capability
         //
         if (arg.starts_iwith("HAVE-"))
            {
              if (! have_capability(arg.drop(5)))   return;
            }
         else if (arg.starts_iwith("NO-"))
            {
              if (have_capability(arg.drop(3)))   return;
            }
         else
            {
              CERR << "]NEXTFILE: unknown capability '" << arg
                   << "' (ignored).\n"
                      "Capabilities start with HAVE- or with NO- "
                      "(try TAB expansion)."
                   << endl;
              continue;   // next arg
            }
       }

   IO_Files::next_file();
}
//----------------------------------------------------------------------------
bool
Command::have_capability(const UCS_string & capa)
{
const int len = capa.size();
   if (capa.front() == UNI_Quad_Quad)   // ⎕xxx
      {
        const UCS_string capa1 = capa.drop(1);
        if (len == 4 && capa1.starts_iwith("FFT"))       return apl_FFT;
        if (len == 4 && capa1.starts_iwith("PNG"))       return apl_PNG;
        if (len == 3 && capa1.starts_iwith("RE"))        return apl_PCRE;
      }
   else
      {
        if (len == 3 && capa.starts_iwith("GTK"))        return apl_GTK3;
        if (len == 3 && capa.starts_iwith("GUI"))        return apl_GUI;
        if (len == 8 && capa.starts_iwith("POSTGRES"))   return apl_POSTGRES;
        if (len == 7 && capa.starts_iwith("SQLITE3"))    return apl_SQLITE3;
        if (len == 3 && capa.starts_iwith("X11"))        return apl_X11;
      }

   CERR << "]NEXTFILE: Unknown capability '" << capa << "'" << endl;
   return false;
}
//----------------------------------------------------------------------------
void
Command::cmd_PUSHFILE()
{
   CERR <<
"*** Pushing an immediate execution context (leave it with ]NEXTFILE)"
        << endl;

   if (InputFile::files_todo.size())
      InputFile::files_todo.front().set_pushed_pending(true);

InputFile fam("stdin", stdin, false, true, true, no_LX);
   fam.set_pushed_IE();
   InputFile::files_todo.insert(InputFile::files_todo.begin(), fam);
}
//----------------------------------------------------------------------------
void
Command::cmd_OUT(ostream & out, UCS_string_vector & args)
{
UCS_string fname = args.front();
   args.erase(0);

UTF8_string filename = LibPaths::get_lib_filename(LIB_NONE, fname, false,
                                                  ".atf", 0);

FILE * atf = fopen(filename.c_str(), "w");
   if (atf == 0)
      {
        const char * why = strerror(errno);
        out << ")OUT " << fname << " failed: " << why << endl;
        MORE_ERROR() << "command )OUT: could not open file " << fname
                     << " for writing: " << why;
        return;
      }

uint64_t seq = 1;   // sequence number for records written
   Workspace::write_OUT(atf, seq, args);

   fclose(atf);
}
//----------------------------------------------------------------------------
bool
Command::check_name_conflict(ostream & out, const UCS_string & cnew,
                             const UCS_string cold)
{
int len = cnew.size();
        if (len > cold.size())   len = cold.size();

   loop(l, len)
      {
        int c1 = cnew[l];
        int c2 = cold[l];
        if (c1 >= 'a' && c1 <= 'z')   c1 -= 0x20;   // uppercase
        if (c2 >= 'a' && c2 <= 'z')   c2 -= 0x20;   // uppercase
        if (l && (c1 != c2))   return false;   // OK: different
     }

   out << "BAD COMMAND+" << endl;
   MORE_ERROR() << "conflict with existing command name in command ]USERCMD";

   return true;
}
//----------------------------------------------------------------------------
bool
Command::check_redefinition(ostream & out, const UCS_string & cnew,
                            const UCS_string fnew, const int mnew)
{
   loop(u, Workspace::get_user_commands().size())
     {
       const UCS_string cold = Workspace::get_user_commands()[u].prefix;
       const UCS_string fold = Workspace::get_user_commands()[u].apl_function;
       const int mold = Workspace::get_user_commands()[u].mode;

       if (cnew != cold)   continue;

       // user command name matches; so must mode and function
       if (mnew != mold || fnew != fold)
         {
           out << "BAD COMMAND" << endl;
           MORE_ERROR() <<
           "conflict with existing user command definition in command ]USERCMD";
         }
       return true;
     }

   return false;
}
//----------------------------------------------------------------------------
void
Command::cmd_SAVE(ostream & out, const UCS_string_vector & args)
{
   CHECK_SECURITY(disable_SAVE_command);

   // )SAVE
   // )SAVE workspace
   // )SAVE lib workspace

   if (args.size() > 0)   // workspace or lib workspace
      {
        LibRef lib;
        UCS_string wsname;
        if (resolve_lib_wsname(out, args, lib, wsname))   return;   // error
        Workspace::save_WS(out, lib, wsname, false);
        return;
      }

   // )SAVE without arguments: use )WSID unless CLEAR WS
   //
LibRef wsid_lib = LIB0;
UCS_string wsid_name = Workspace::get_WS_name();
   if (Avec::is_digit(wsid_name.front()))   // wsid contains a libnum
      {
        wsid_lib = LibRef(wsid_name.front() - '0');
        wsid_name.erase(0);
        wsid_name.remove_leading_whitespaces();
      }

   if (wsid_name.compare(UCS_ASCII_string("CLEAR WS")) == COMP_EQ)
      {
        // don't save CLEAR WS
        COUT << "NOT SAVED: THIS WS IS CLEAR WS" << endl;
        MORE_ERROR() <<
        "the workspace was not saved because 'CLEAR WS' is a special\n"
        "workspace name that cannot be saved. "
        "First create WS name with )WSID <name>."; 
        return;
      }

   Workspace::save_WS(out, wsid_lib, wsid_name, true);
}
//----------------------------------------------------------------------------
bool
Command::resolve_lib_wsname(ostream & out, const UCS_string_vector & args,
                            LibRef &lib, UCS_string & wsname)
{
   Assert(args.size() > 0);
   if (args.size() == 1)   // name without libnum
      {
        lib = LIB0;
        wsname = args.front();
        return false;   // OK
      }

   if (!(args.front().size() == 1 && Avec::is_digit(args.front()[0])))
      {
        out << "BAD COMMAND+" << endl;
        MORE_ERROR() << "invalid library reference '" << args.front() << "'";
        return true;   // error
      }

   lib = LibRef(args.front()[0] - '0');
   wsname = args[1];
   return false;   // OK
}
//----------------------------------------------------------------------------
void
Command::cmd_USERCMD(ostream & out, const UCS_string & cmd,
                     UCS_string_vector & args)
{
   // ]USERCMD
   // ]USERCMD REMOVE-ALL
   // ]USERCMD REMOVE        ]existing-command
   // ]USERCMD ]new-command  APL-fun
   // ]USERCMD ]new-command  APL-fun  mode
   // ]USERCMD ]new-command  { ... }
   //
   if (args.size() == 0)
      {
        if (Workspace::get_user_commands().size())
           {
             loop(u, Workspace::get_user_commands().size())
                {
                  out << Workspace::get_user_commands()[u].prefix << " → ";
                  if (Workspace::get_user_commands()[u].mode)   out << "A ";
                  out << Workspace::get_user_commands()[u].apl_function << " B"
                      << " (mode " << Workspace::get_user_commands()[u].mode
                      << ")" << endl;
                }
           }
        return;
      }

  if (args.size() == 1 && args.front().starts_iwith("REMOVE-ALL"))
     {
       Workspace::get_user_commands().clear();
       out << "    All user-defined commands removed." << endl;
       return;
     }

  if (args.size() == 2 && args.front().starts_iwith("REMOVE"))
     {
       loop(u, Workspace::get_user_commands().size())
           {
             if (Workspace::get_user_commands()[u].prefix
                                                  .starts_iwith(args[1]) &&
                 args[1].starts_iwith(Workspace::get_user_commands()[u]
                                                          .prefix))   // same
                {
                  // print first and remove then!
                  //
                  out << "    User-defined command "
                      << Workspace::get_user_commands()[u].prefix
                      << " removed." << endl;
                  Workspace::get_user_commands().
                     erase(Workspace::get_user_commands().begin() + u);
                  return;
                }
           }

       out << "BAD COMMAND+" << endl;
       MORE_ERROR() << "user command in command"
                       " ]USERCMD REMOVE does not exist";
       return;
     }

  // check if the user command is not followed by the string
  if (args.size() == 1)
     {
        out << "BAD COMMAND+" << endl;
        MORE_ERROR() << "user command syntax in ]USERCMD:"
                        " ]new-command  APL-fun  [mode]";
        return;
     }

UCS_string command_name = args.front();
UCS_string apl_fun = args[1];
int mode = 0;

   // check if lambda
   bool is_lambda = false;
   if (apl_fun.front() == '{')
      {
         // looks like the user command is a lambda function.
         UCS_string result;
         // lambdas could contain spaces, collect all arguments in one string
         for (ShapeItem i = 1; i < args.size(); ++i)
            {
               result << args[i];
            }
         // check if lamda-function closed properly
         if (result.back() == '}')
            {
               is_lambda = true;
               apl_fun = result;
               // determine the mode: if both alpha and omega present then
               // assume dyadic, otherwise monadic usage
               mode = (apl_fun.contains(UNI_OMEGA) &&
                       apl_fun.contains(UNI_ALPHA)) ? 1 : 0;
            }
         else
            {
               out << "BAD COMMAND+" << endl;
               MORE_ERROR() << "closing } in lambda function not found";
               return;
            }
      }

   if (args.size() > 3 && !is_lambda)
      {
        out << "BAD COMMAND+" << endl;
        MORE_ERROR() << "too many parameters in command ]USERCMD";
        return;
      }

   // check mode
   if (!is_lambda && args.size() == 3)   mode = args[2].atoi();
   if (mode < 0 || mode > 1)
      {
        out << "BAD COMMAND+" << endl;
        MORE_ERROR() << "unsupported mode " << mode
                     << " in command ]USERCMD (0 or 1 expected)";
        return;
      }

   // check command name
   //
   loop(c, command_name.size())
      {
        bool error = false;
        if (c == 0)   // command must start with ] or )
           {
             if (command_name[c] != ']' && command_name[c] != ')')
                error = true;
           }
        else // and continue with symbol characters
           {
             error = error || !Avec::is_symbol_char(command_name[c]);
           }

        if (error)
           {
             out << "BAD COMMAND+" << endl;
             MORE_ERROR() << " bad user command name in command ]USERCMD";
             return;
           }
      }

   // check conflicts with existing commands
   //
#define cmd_def(cmd_str, _cod, _arg, _hint) \
   if (check_name_conflict(out, UTF8_string(cmd_str), command_name))   return;
#include "Command.def"
   if (check_redefinition(out, command_name, apl_fun, mode))
     {
       out << "    User-defined command "
           << command_name << " installed." << endl;
       return;
     }

   // check APL function name
   // Only needed when not a lambda function
   if (!is_lambda)
      {
         loop(c, apl_fun.size())
            {
               if (!Avec::is_symbol_char(apl_fun[c]))
                  {
                     out << "BAD COMMAND+" << endl;
                     MORE_ERROR() <<
                          "bad APL function name in command ]USERCMD";
                     return;
                  }
            }
      }

user_command new_user_command = { command_name, apl_fun, mode };
   Workspace::get_user_commands().push_back(new_user_command);

   out << "    User-defined command "
       << new_user_command.prefix << " installed." << endl;
}
//----------------------------------------------------------------------------
void
Command::do_USERCMD(ostream & out, UCS_string & apl_cmd,
                    const UCS_string & line, const UCS_string & cmd,
                    UCS_string_vector & args, int uidx)
{
  if (Workspace::get_user_commands()[uidx].mode > 0)   // dyadic
     {
        apl_cmd.append_quoted(cmd);
        apl_cmd.append(UNI_SPACE);
        loop(a, args.size())
           {
             apl_cmd.append_quoted(args[a]);
             apl_cmd.append(UNI_SPACE);
           }
     }

   apl_cmd.append(Workspace::get_user_commands()[uidx].apl_function);
   apl_cmd.append(UNI_SPACE);
   apl_cmd.append_quoted(line);
}
//----------------------------------------------------------------------------
#ifdef cfg_DYNAMIC_LOG_WANTED
void
Command::log_control(const UCS_string & arg)
{
UCS_string_vector args = split_arg(arg);

   if (args.size() == 0 || arg.front() == UNI_QUESTION)  // no arg or '?'
      {
        for (LogId l = LID_MIN; l < LID_MAX; l = LogId(l + 1))
            {
              const char * info = Log_info(l);
              Assert(info);

              const bool val = Log_status(l);
              CERR << "    " << setw(2) << right << l << ": " 
                   << (val ? "(ON)  " : "(OFF) ") << left << info << endl;
            }

        return;
      }

   // at this point args is a sequence of numbers (LIDs) and actions (ON or
   // OFF). We parse args back to front. to acreate a vector of actions
   // which is then executed from front to back.
   //
vector<struct lid_OOT> lid_OOTs;
OOT action = Toggle;
   loop(a, args.size())
       {
         const UCS_string & ucs = args[args.size() - a - 1];
         if      (ucs.starts_iwith("ON"))    action = On;
         else if (ucs.starts_iwith("OFF"))   action = Off;
         else   // remember the LogId and its action and
            {
              const LogId lid = LogId(ucs.atoi());
              const lid_OOT lo = { lid, action };
              if (lid >= LID_MIN && lid <= LID_MAX)
                 lid_OOTs.push_back(lo);
              else
                 CERR << "    Invalid logging facility " << lid
                      << " ignored. Valid logging facilities are: "
                      << LID_MIN << ".." << (LID_MAX - 1) << endl;
            }
       }

   loop(a, lid_OOTs.size())
       {
         const lid_OOT lo = lid_OOTs[lid_OOTs.size() - a - 1];
         const LogId lid = lo.lid;
         const char * info = Log_info(lid);
         bool new_status = !Log_status(lid);   // assume toggle
         if      (lo.on_off_toggle == On)   new_status = true;
         else if (lo.on_off_toggle == Off)  new_status = false;
         Log_control(lid, new_status);

         CERR << "    Logging facility " << lid << ": " << info
              << " is now " << (new_status ? "ON " : "OFF") << endl;
       }
}
#endif
//----------------------------------------------------------------------------
void
Command::transfer_context::process_record(const UTF8 * record,
                                          const UCS_string_vector & objects)
{
const char rec_type = record[0];   // '*', ' ', or 'X'
const char sub_type = record[1];

   if (rec_type == '*')   // comment or similar
      {
        Log(LOG_command_IN)
           {
             const char * stype = " *** bad sub-record of *";
             switch(sub_type)
                {
                  case ' ': stype = " comment";     break;
                  case '(': {
                              stype = " timestamp";
                              YMDhmsu t(now());   // fallback if sscanf() != 7
                              if (7 == sscanf(charP(record + 1),
                                              "(%d %d %d %d %d %d %d)",
                                              &t.year, &t.month, &t.day,
                                              &t.hour, &t.minute, &t.second,
                                              &t.micro))
                                  {
                                    timestamp = t.get();
                                  }
                            }
                            break;
                  case 'I': stype = " imbed";       break;
                }

             CERR << "record #" << setw(3) << recnum << ": '" << rec_type
                  << "'" << stype << endl;
           }
      }
   else if (rec_type == ' ' || rec_type == 'X')   // object
      {
        if (new_record)
           {
             Log(LOG_command_IN)
                {
                  const char * stype = " *** bad sub-record of X";

//                          " -------------------------------------";
                  switch(sub_type)
                     {
                       case 'A': stype = " 2 ⎕TF array ";           break;
                       case 'C': stype = " 1 ⎕TF char array ";      break;
                       case 'F': stype = " 2 ⎕TF function ";        break;
                       case 'N': stype = " 1 ⎕TF numeric array ";   break;
                     }

                  CERR << "record #" << setw(3) << recnum
                       << ": " << stype << endl;
                }

             item_type = sub_type;
           }

        add(record + 1, 71);

        new_record = (rec_type == 'X');   // 'X' marks the final record
        if (new_record)
           {
             if      (item_type == 'A')   array_2TF(objects);
             else if (item_type == 'C')   chars_1TF(objects);
             else if (item_type == 'N')   numeric_1TF(objects);
             else if (item_type == 'F')   function_2TF(objects);
             else                         CERR << "????: " << data << endl;
             data.clear();
           }
      }
   else
      {
        CERR << "record #" << setw(3) << recnum << ": '" << rec_type << "'"
             << "*** bad record type '" << rec_type << endl;
      }
}
//----------------------------------------------------------------------------
uint32_t
Command::transfer_context::get_nrs(UCS_string & name, Shape & shape) const
{
int idx = 1;

   // data + 1 is: NAME RK SHAPE RAVEL...
   //
   while (idx < data.size() && data[idx] != UNI_SPACE)
         name.append(data[idx++]);
   ++idx;   // skip space after the name

int rank = 0;
   while (idx < data.size() &&
          data[idx] >= UNI_0 &&
          data[idx] <= UNI_9)
      {
        rank *= 10;
        rank += data[idx++] - UNI_0;
      }
   ++idx;   // skip space after the rank

   loop (r, rank)
      {
        ShapeItem s = 0;
        while (idx < data.size() &&
               data[idx] >= UNI_0 &&
               data[idx] <= UNI_9)
           {
             s *= 10;
             s += data[idx++] - UNI_0;
           }
        shape.add_shape_item(s);
        ++idx;   // skip space after shape[r]
      }
  
   return idx;
}
//----------------------------------------------------------------------------
void
Command::transfer_context::numeric_1TF(const UCS_string_vector & objects) const
{
UCS_string var_name;
Shape shape;
int idx = get_nrs(var_name, shape);

   if (objects.size() && !objects.contains(var_name))   return;

Symbol * sym = 0;
   if (Avec::is_quad(var_name.front()))   // system variable.
      {
        int len = 0;
        const Token t = Workspace::get_quad(var_name, len);
        if (t.get_ValueType() == TV_SYM)   sym = t.get_sym_ptr();
        else                               Assert(0 && "Bad system variable");
      }
   else                            // user defined variable
      {
        sym = Workspace::lookup_symbol(var_name);
        Assert(sym);
      }
   
   Log(LOG_command_IN)
      {
        CERR << endl << var_name << " rank " << shape.get_rank() << " IS '";
        loop(j, data.size() - idx)   CERR << data[idx + j];
        CERR << "'" << endl;
      }

Token_string tos;
   {
     UCS_string data1(data, idx, data.size() - idx);
     Tokenizer tokenizer(PM_EXECUTE, LOC, false);
     if (tokenizer.tokenize(data1, tos) != E_NO_ERROR)   return;
   }
 
   if (tos.size() != shape.get_volume())   return;

Value_P Z(shape, LOC);
   Z->set_proto_Int();   // prototype

const ShapeItem ec = Z->element_count();
   loop(e, ec)
      {
        const Token & tok = tos[e];
        const TokenTag tag = tok.get_tag();
        if      (tag == TOK_INTEGER)  Z->next_ravel_Int(tok.get_int_val());
        else if (tag == TOK_REAL)     Z->next_ravel_Float(tok.get_flt_val());
        else if (tag == TOK_COMPLEX)  Z->next_ravel_Complex(tok.get_cpx_real(),
                                                            tok.get_cpx_imag());
        else FIXME;
      }
   Z->check_value(LOC);

   Assert(sym);
   sym->assign(Z, false, LOC);
}
//----------------------------------------------------------------------------
void
Command::transfer_context::chars_1TF(const UCS_string_vector & objects) const
{
UCS_string var_name;
Shape shape;
int idx = get_nrs(var_name, shape);

   if (objects.size() && !objects.contains(var_name))   return;

Symbol * sym = 0;
   if (Avec::is_quad(var_name.front()))   // system variable.
      {
        int len = 0;
        const Token t = Workspace::get_quad(var_name, len);
        if (t.get_ValueType() == TV_SYM)   sym = t.get_sym_ptr();
        else                               Assert(0 && "Bad system variable");
      }
   else                            // user defined variable
      {
        sym = Workspace::lookup_symbol(var_name);
        Assert(sym);
      }

   Log(LOG_command_IN)
      {
        CERR << endl << var_name << " shape " << shape << " IS: '";
        loop(j, data.size() - idx)   CERR << data[idx + j];
        CERR << "'" << endl;
      }

Value_P Z(shape, LOC);
const ShapeItem ec = Z->element_count();
   Z->set_proto_Spc();   // prototype

ShapeItem padded = 0;
   loop(e, ec)
      {
        Unicode uni = UNI_SPACE;
        if (e < (data.size() - idx))   uni = data[e + idx];
        else                           ++padded;
        Z->next_ravel_Char(uni);
      }

   if (padded)
      {
        CERR << "WARNING: ATF Record for " << var_name << " is broken ("
             << padded << " spaces added)" << endl;
      }

   Z->check_value(LOC);

   Assert(sym);
   sym->assign(Z, false, LOC);
}
//----------------------------------------------------------------------------
void
Command::transfer_context::array_2TF(const UCS_string_vector & objects) const
{
   // an Array in 2 ⎕TF format
   //
UCS_string data1(&data[1], data.size() - 1);
UCS_string var_or_fun;

   // data1 is: VARNAME←data...
   //
   if (objects.size())
      {
        UCS_string var_name;
        loop(d, data1.size())
           {
             const Unicode uni = data1[d];
             if (uni == UNI_LEFT_ARROW)   break;
             var_name.append(uni);
           }

        if (!objects.contains(var_name))   return;
      }

   var_or_fun = Quad_TF::tf2_inverse(data1);

   if (var_or_fun.size() == 0)
      {
        CERR << "ERROR: inverse 2 ⎕TF failed for '" << data1 << "'" << endl;
      }
}
//----------------------------------------------------------------------------
void
Command::transfer_context::function_2TF(const UCS_string_vector & objects)const
{
int idx = 1;
UCS_string fun_name;

   /// chars 1...' ' are the function name
   while ((idx < data.size()) && (data[idx] != UNI_SPACE))
        fun_name.append(data[idx++]);
   ++idx;

   if (objects.size() && !objects.contains(fun_name))   return;

UCS_string statement;
   while (idx < data.size())   statement.append(data[idx++]);
   statement.append(UNI_LF);

UCS_string fun_name1 = Quad_TF::tf2_inverse(statement);
   if (fun_name1.size() == 0)   // tf2_inverse() failed
      {
        CERR << "inverse 2 ⎕TF failed for the following APL statement: "
             << endl << "    " << statement << endl;
        return;
      }

Symbol * sym1 = Workspace::lookup_existing_symbol(fun_name1);
   Assert(sym1);
   {
     Function * fun1 = const_cast<Function *>(sym1->get_function());
     Assert(fun1);
     fun1->set_creation_time(timestamp);
   }

   Log(LOG_command_IN)
      {
       const YMDhmsu ymdhmsu(timestamp);
       CERR << "FUNCTION '" << fun_name1 <<  "'" << endl
            << "   created: " << ymdhmsu.day << "." << ymdhmsu.month
            << "." << ymdhmsu.year << "  " << ymdhmsu.hour
            << ":" << ymdhmsu.minute << ":" << ymdhmsu.second
            << "." << ymdhmsu.micro << " (" << timestamp << ")" << endl;
      }
}
//----------------------------------------------------------------------------
void
Command::transfer_context::add(const UTF8 * str, int len)
{

#if 0
   // helper function to print the uni_to_cp_map table when given the inverse
   // cp_to_uni_map. Before that the IBM ⎕AV is printed
   //
   Avec::print_inverse_IBM_quad_AV();
   DOMAIN_ERROR;
#endif

const Unicode * cp_to_uni_map = Avec::IBM_quad_AV();
   loop(l, len)
      {
        const UTF8 utf = str[l];
        switch(utf)
           {
             case '^': data.append(UNI_AND);              break;   // ~ → ∼
             case '*': data.append(UNI_STAR_OPERATOR);    break;   // * → ⋆
             case '~': data.append(UNI_TILDE_OPERATOR);   break;   // ~ → ∼
             default:  data.append(Unicode(cp_to_uni_map[utf]));
           }
      }
}
//----------------------------------------------------------------------------
bool
Command::parse_from_to(UCS_string & from, UCS_string & to,
                       const UCS_string & user_arg)
{
   // parse user_arg which is one of the following:
   //
   // 1.   (empty)
   // 2.   FROMTO
   // 3a.  FROM -
   // 3b.       - TO
   // 3c.  FROM - TO
   //
   from.clear();
   to.clear();

int s = 0;
bool got_minus = false;

   // skip spaces before from
   //
   while (s < user_arg.size() && user_arg[s] <= ' ') ++s;

   if (s == user_arg.size())   return false;   // case 1.: OK

   // copy left of - to from
   //
   while (s < user_arg.size()   &&
              user_arg[s] > ' ' &&
              user_arg[s] != '-')  from.append(user_arg[s++]);

   // skip spaces after from
   //
   while (s < user_arg.size() && user_arg[s] <= ' ') ++s;

   if (s < user_arg.size() && user_arg[s] == '-') { ++s;   got_minus = true; }

   // skip spaces before to
   //
   while (s < user_arg.size() && user_arg[s] <= ' ') ++s;

   // copy right of - to from
   //
   while (s < user_arg.size() && user_arg[s] > ' ')  to.append(user_arg[s++]);

   // skip spaces after to
   //
   while (s < user_arg.size() && user_arg[s] <= ' ') ++s;

   if (s < user_arg.size())   return true;   // error: non-blank after to

   if (!got_minus)   to = from;   // case 2.

   if (from.size() == 0 && to.size() == 0) return true;   // error: single -

   // "increment" TO so that we can compare ITEM < TO
   //
   if (to.size())   to.back() = Unicode(to.back() + 1);

   return false;   // OK
}
//----------------------------------------------------------------------------
bool
Command::is_lib_ref(const UCS_string & lib)
{
   if (lib.size() == 1)   // single char: lib number
      {
        if (Avec::is_digit(lib.front()))   return true;
      }

   if (lib.front() == UNI_FULLSTOP)   return true;

   loop(l, lib.size())
      {
        const Unicode uni = lib[l];
        if (uni == UNI_SLASH)       return true;
        if (uni == UNI_BACKSLASH)   return true;
      }

   return false;
}
//----------------------------------------------------------------------------
