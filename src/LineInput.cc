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
#include <signal.h>

#include "Assert.hh"
#include "Command.hh"
#include "Common.hh"
#include "Error.hh"
#include "InputFile.hh"
#include "IO_Files.hh"
#include "LineInput.hh"
#include "Nabla.hh"
#include "Parallel.hh"
#include "SystemVariable.hh"
#include "TabExpansion.hh"
#include "UserPreferences.hh"
#include "Workspace.hh"

struct termios LineInput::initial_termios = { 0 };
int LineInput::initial_termios_errno = 0;

// hooks for external editors (emacs)
extern void (*start_input)();
void (*start_input)() = 0;

extern void (*end_input)();
void (*end_input)() = 0;

LineInput * LineInput::the_line_input = 0;

LineHistory LineHistory::quote_quad_history(10);
LineHistory LineHistory::quad_quad_history(10);
LineHistory LineHistory::quad_INP_history(2);

UCS_string LineEditContext::cut_buffer;

get_line_cb * InputMux::get_line_callback = 0;

//============================================================================

ESCmap ESCmap::the_ESCmap[] =
{
   // normal sequences
  { 3, Output::ESC_CursorUp,      UNI_CursorUp     },
  { 3, Output::ESC_CursorDown,    UNI_CursorDown   },
  { 3, Output::ESC_CursorRight,   UNI_CursorRight  },
  { 3, Output::ESC_CursorLeft,    UNI_CursorLeft   },
  { 3, Output::ESC_CursorEnd,     UNI_CursorEnd    },
  { 3, Output::ESC_CursorHome,    UNI_CursorHome   },
  { 4, Output::ESC_InsertMode,    UNI_InsertMode   },
  { 4, Output::ESC_Delete,        UNI_DELETE },

   // sequences with SHIFT and/or CTRL
  { 6, Output::ESC_CursorUp_1,    UNI_CursorUp     },
  { 6, Output::ESC_CursorDown_1,  UNI_CursorDown   },
  { 6, Output::ESC_CursorRight_1, UNI_CursorRight  },
  { 6, Output::ESC_CursorLeft_1,  UNI_CursorLeft   },
  { 6, Output::ESC_CursorEnd_1,   UNI_CursorEnd    },
  { 6, Output::ESC_CursorHome_1,  UNI_CursorHome   },
  { 6, Output::ESC_InsertMode_1,  UNI_InsertMode   },
  { 6, Output::ESC_Delete_1,      UNI_DELETE },
};

enum { ESCmap_entry_count = sizeof(ESCmap::the_ESCmap) / sizeof(ESCmap) };

//============================================================================
bool
ESCmap::need_more(const char * seq, int len)
{
   loop(e, ESCmap_entry_count)
       {
         if (ESCmap::the_ESCmap[e].has_prefix(seq, len))   return true;
       }

   return false;
}
//----------------------------------------------------------------------------
bool
ESCmap::has_prefix(const char * seq, int seq_len) const
{
   if (seq_len >= len)   return false;
   loop(s, seq_len)
      {
        if ((seq[s] != seqence[s]) && seqence[s])   return false;
      }

   return true;
}
//----------------------------------------------------------------------------
bool
ESCmap::is_equal(const char * seq, int seq_len) const
{
   if (len != seq_len)   return false;
   loop(s, seq_len)
      {
        if ((seq[s] != seqence[s]) && seqence[s])   return false;
      }

   return true;
}
//----------------------------------------------------------------------------
void
ESCmap::refresh_lengths()
{
return;
   loop(e, ESCmap_entry_count)
     the_ESCmap[e].len = strlen(the_ESCmap[e].seqence);
}
//============================================================================
LineHistory::LineHistory(int maxl)
   : current_line(0),
     put(0),
     max_lines(maxl),
     last_search_line(0)
{
   UCS_string u(UTF8_string("xxx"));
   add_line(u);
}
//----------------------------------------------------------------------------
LineHistory::LineHistory(const Nabla & nabla)
   : current_line(0),
     put(0),
     max_lines(1000),
     last_search_line(0)
{
   UCS_string u(UTF8_string("xxx"));
   add_line(u);
int cl = -1;
   loop(l, nabla.get_line_count())
       {
         bool on_current = false;
         const UCS_string lab_text = nabla.get_label_and_text(l, on_current);
         add_line(lab_text);   // overrides this->current_line !
         if (on_current)   cl = l + 1;   // + 1 due to "xxx" above
       }

   if (cl != -1)   current_line = cl;
}
//----------------------------------------------------------------------------
void
LineHistory::read_history(const char * filename)
{
FILE * hist = fopen(filename, "r");
   if (hist == 0)
      {
        Log(LOG_get_line)
           CERR << "Cannot open history file " << filename
                << ": " << strerror(errno) << endl;
        return;
      }

   for (;;)
       {
         char buffer[4000];
         const char * s = fgets(buffer, sizeof(buffer) - 1, hist);
         if (s == 0)   break;   // end of file
         buffer[sizeof(buffer) - 1] = 0;

         int slen = strlen(buffer);
         if (slen && (buffer[slen - 1] == '\n'))   buffer[--slen] = 0;
         if (slen && (buffer[slen - 1] == '\r'))   buffer[--slen] = 0;

         UTF8_string utf(buffer);
         UCS_string ucs(utf);
         add_line(ucs);
       }

   next();
}
//----------------------------------------------------------------------------
void
LineHistory::save_history(const char * filename)
{
   if (hist_lines.size() == 0)   return;

ofstream outf(filename);
   if (!outf.is_open())
      {
        CERR << "Cannot write history file " << filename
             << ": " << strerror(errno) << endl;
         return;
      }

int count = 0;
   for (ShapeItem p = put + 1; p < hist_lines.size(); ++p)
      {
        outf << hist_lines[p] << endl;
        ++count;
      }
   for (int p = 0; p < put; ++p)
      {
        outf << hist_lines[p] << endl;
        ++count;
      }

   Log(LOG_get_line)
      cerr << count << " history lines written to " << filename << endl;
}
//----------------------------------------------------------------------------
void
LineHistory::clear_history(ostream & out)
{
   current_line = 0;
   put = 0;
   hist_lines.clear();
UCS_string u((UTF8_string("xxx")));
   add_line(u);
}
//----------------------------------------------------------------------------
void
LineHistory::print_history(ostream & out, const UCS_string & filter) const
{
   // hist_lines is a ring buffer. first print its tail, then its head.

   for (ShapeItem p = put + 1; p < hist_lines.size(); ++p)   // tail
      {
        if (filter.size() && !hist_lines[p].starts_iwith(filter))   continue;
        out << "      " << hist_lines[p] << endl;
      }
   for (int p = 0; p < put; ++p)                          // head
      {
        if (filter.size() && !hist_lines[p].starts_iwith(filter))   continue;
        out << "      " << hist_lines[p] << endl;
      }
}
//----------------------------------------------------------------------------
void
LineHistory::add_line(const UCS_string & line)
{
   if (max_lines == 0)      return;   // no history
   if (!line.has_black())   return;   // almost empty

   // remove leading blanks
   //
int blanks = 0;
   while (blanks < line.size() && line[blanks] <= UNI_SPACE)   ++blanks;

   // a repeated cut-and-paste of entire lines increases the indentation every
   // time due to the APL input prompt). We therefore limit this effect
   // to 6 blanks.
   //
UCS_string line1;
   if (blanks)
      {
        line1 = UCS_string(line, blanks, line.size() - blanks);
      }
   else
      {
        line1 = line;
      }
   while (line1.back() <= UNI_SPACE)   line1.pop_back();

   if (int(hist_lines.size()) < max_lines)   // append
      {
        hist_lines.push_back(line1);
        put = 0;
      }
   else                            // override
      {
        if (put >= int(hist_lines.size()))   put = 0;   // wrap
        hist_lines[put++] = line1;
      }

   next();   // update current_line
}
//----------------------------------------------------------------------------
void
LineHistory::replace_line(const UCS_string & line)
{
   if (put > 0)   hist_lines[put - 1] = line;
   else           hist_lines.back() = line;
}
//----------------------------------------------------------------------------
const UCS_string *
LineHistory::up()
{
   if (hist_lines.size() == 0)   return 0;   // no history

int new_current_line = current_line - 1;
    if (new_current_line < 0)   new_current_line += hist_lines.size();   // wrap
    if (new_current_line == put)   return 0;

   return &hist_lines[current_line = new_current_line];
}
//----------------------------------------------------------------------------
const UCS_string *
LineHistory::down()
{
   if (hist_lines.size() == 0)     return 0;   // no history
   if (current_line == put)   return 0;

int new_current_line = current_line + 1;
   if (new_current_line >= int(hist_lines.size()))
      new_current_line = 0;   // wrap
   current_line = new_current_line;
   if (current_line == put)   return 0;

   return &hist_lines[current_line];
}
//----------------------------------------------------------------------------
const void
LineHistory::clear_search(void)
{
    cur_search_substr.clear();
}
//----------------------------------------------------------------------------
const void
LineHistory::update_search(UCS_string &cur_line)
{
    cur_search_substr = cur_line;
}
//----------------------------------------------------------------------------
const UCS_string *
LineHistory::search(UCS_string &cur_line)
{
    if( hist_lines.size() == 0 ) return 0;  // no history

    // For now, a simple substring search of hist_lines[]
    int search_start_line = last_search_line - 1;
    if( search_start_line < 0) {
        search_start_line = hist_lines.size()-1;
    }
    int idx = search_start_line;
    bool found = false;
    do {
        if( hist_lines[idx].substr_pos(cur_search_substr) >= 0 ) {
            current_line = idx;
            found = true;
            continue;
        }

        idx--;
        if( idx < 0 ) {
            idx = hist_lines.size()-1;
        }
        if( idx == search_start_line ) {
            break;
        }
    } while(!found);

    if( !found ) {
        idx = hist_lines.size()-1;
    }

    last_search_line = idx;
    if( idx == 0 ) return 0;

    return &hist_lines[current_line];
}
//============================================================================
LineEditContext::LineEditContext(LineInputMode mode, int rows, int cols,
                                 LineHistory & hist, const UCS_string & prmt)
   : screen_rows(rows),
     screen_cols(cols),
     allocated_height(1),
     uidx(0),
     ins_mode(true),
     history(hist),
     history_entered(false)
{
   if (mode == LIM_Quote_Quad)
      {
        // the prompt was printed by ⍞ already. Make it the beginning of
        // user_line so that it can be edited.
        //
        user_line = prmt.no_pad();
        uidx = user_line.size();
      }
   else
      {
        prompt = prmt.no_pad();
      }

   refresh_all();
}
//----------------------------------------------------------------------------
LineEditContext::~LineEditContext()
{
   // restore block cursor
   //
   if (!ins_mode)   CIN << "\x1B[1 q" << flush;
}
//----------------------------------------------------------------------------
void
LineEditContext::adjust_allocated_height()
{
const int rows = 1 + get_total_length() / screen_cols;

   if (allocated_height >= rows)   return;

   // scroll some lines so that prior text is not overridden.
   //
   CIN.set_cursor(-1, 0);
   loop(a, rows - allocated_height)   CIN << endl;

   allocated_height = rows;

   // redraw screen from -allocated_height:0 onwards
   //
   if (CIN.can_clear_EOS())
      {
        CIN.set_cursor(-allocated_height, 0);
        CIN.clear_EOS();
      }
   else
      {
        loop(a, allocated_height)
           {
             CIN.set_cursor(a - allocated_height, 0);
             CIN.clear_EOL();
           }
      }

   refresh_all();
}
//----------------------------------------------------------------------------
void
LineEditContext::refresh_from_cursor()
{
const int saved_uidx = uidx;

   adjust_allocated_height();
   set_cursor();
   for (; uidx < user_line.size(); ++uidx)
       {
         refresh_wrapped_cursor();
         CIN << user_line[uidx];
       }

   // clear from end of user_line
   //
   move_idx(user_line.size());
   if (CIN.can_clear_EOS())
      {
        CIN.clear_EOS();
      }
   else
      {
        CIN.clear_EOL();

        // clear subsequent lines
        //
        for (int a = 1; a < allocated_height; ++a)
            {
              CIN.set_cursor(a - allocated_height, 0);
              CIN.clear_EOL();
            }
      }

   move_idx(saved_uidx);
}
//----------------------------------------------------------------------------
void
LineEditContext::refresh_all()
{
const int saved_uidx = uidx;
   uidx = 0;

   CIN.set_cursor(-allocated_height, 0);
   CIN << prompt << user_line;
   refresh_from_cursor();
   move_idx(saved_uidx);
}
//----------------------------------------------------------------------------
void
LineEditContext::delete_char()
{
   if (uidx == (user_line.size() - 1))   // cursor on last char
      {
        CIN << ' ' << UNI_BS;
        user_line.pop_back();
      }
   else
      {
        user_line.erase(uidx);
        refresh_from_cursor();
      }

   if (get_total_length() >= screen_cols)   set_cursor();
}
//----------------------------------------------------------------------------
void
LineEditContext::insert_char(Unicode uni)
{
   if (uidx >= user_line.size())   // append char
      {
        user_line.append(uni);
        adjust_allocated_height();
        refresh_wrapped_cursor();
        CIN << uni;
      }
   else if (ins_mode)              // insert char
      {
        user_line.insert(uidx, uni);
        adjust_allocated_height();
        refresh_wrapped_cursor();
        refresh_from_cursor();
      }
   else                            // replace char
      {
        user_line[uidx] = uni;
        adjust_allocated_height();
        refresh_wrapped_cursor();
        refresh_from_cursor();
      }

   move_idx(uidx + 1);
}
//----------------------------------------------------------------------------
void
LineEditContext::cut_to_EOL()
{
   if (uidx >= user_line.size())   return;   // nothing to cut

   cut_buffer = UCS_string(user_line, uidx, user_line.size() - uidx);
   user_line.resize(uidx);
   refresh_from_cursor();
}
//----------------------------------------------------------------------------
void
LineEditContext::paste()
{
   if (cut_buffer.size() == 0)   return;

   if (uidx >= user_line.size())   // append cut buffer
      {
        user_line.append(cut_buffer);
      }
   else                            // insert cut buffer
      {
        const UCS_string rest(user_line, uidx, user_line.size() - uidx);
        user_line.resize(uidx);
        user_line.append(cut_buffer);
        user_line.append(rest);
      }

   refresh_from_cursor();
   move_idx(uidx + cut_buffer.size());
}
//----------------------------------------------------------------------------
void
LineEditContext::toggle_ins_mode()
{
    ins_mode = ! ins_mode;

   // CSI [0 q       : blinking block
   // CSI [1 q       : blinking block
   // CSI [2 q       : steady   block
   // CSI [3 q       : blinking underline
   // CSI [4 q       : steady   underline
   // CSI [5 q       : blinking bar (doesn't work) 
   // CSI [6 q       : steady   bar (doesn't work) 

   if (ins_mode)   CIN << "\x1B[0 q" << flush;
   else            CIN << "\x1B[3 q" << flush;
}
//----------------------------------------------------------------------------
void
LineEditContext::tab_expansion(LineInputMode mode)
{
   if (mode != LIM_ImmediateExecution)   return;

UCS_string line = user_line;
TabExpansion tab_exp(line);
const ExpandResult expand_result = tab_exp.expand_tab(line);

   switch(expand_result)
      {
        case ER_IGNORE: return;

        case ER_AGAIN:
             // expand_tab has shown a list of options.
             // Reset the input window and redisplay.
             allocated_height = 1;
             adjust_allocated_height();
             refresh_all();
             return;

        case ER_REPLACE:
             user_line.clear();
             user_line.append(line);
             uidx = 0;
             refresh_from_cursor();
             move_idx(user_line.size());
             return;

        default: FIXME;
      }
}
//----------------------------------------------------------------------------
void
LineEditContext::cursor_CLEAR_SEARCH()
{
   Log(LOG_get_line)   history.info(CERR << "cursor_CLEAR_SEARCH()") << endl;
   history.clear_search();
}
//----------------------------------------------------------------------------
void
LineEditContext::cursor_UP()
{
   Log(LOG_get_line)   history.info(CERR << "cursor_UP()") << endl;

const UCS_string * ucs = history.up();
   if (ucs == 0)   // no line above
      {
        Log(LOG_get_line)   CERR << "hit top of history()" << endl;
        Log(LOG_get_line)   history.info(CERR << "cursor_UP() done" << endl);
        return;
      }

   if (!history_entered)   // not yet in history: remember user_line
      {
        user_line_before_history = user_line;
        history_entered = true;
      }

   user_line = *ucs;
   adjust_allocated_height();

   uidx = 0;
   refresh_from_cursor();
   move_idx(user_line.size());
   Log(LOG_get_line)   history.info(CERR << "cursor_UP() done" << endl);
}
//----------------------------------------------------------------------------
void
LineEditContext::cursor_DOWN()
{
   Log(LOG_get_line)   history.info(CERR << "cursor_DOWN()" << endl);

const UCS_string * ucs = history.down();
   if (ucs == 0)   // no line below
      {
        Log(LOG_get_line)   CERR << "hit bottom of history()" << endl;
        // if inside history: restore user_line
        //
        if (history_entered)   user_line = user_line_before_history;
        history_entered = false;
        goto refresh;
      }

   if (!history_entered)   // not yet in history: remember user_line
      {
        user_line_before_history = user_line;
        history_entered = true;
      }

   user_line = *ucs;
   adjust_allocated_height();

refresh:
   uidx = 0;
   refresh_from_cursor();
   move_idx(user_line.size());
   Log(LOG_get_line)   history.info(CERR << "cursor_DOWN() done" << endl);
}
//----------------------------------------------------------------------------
void
LineEditContext::update_SEARCH(void)
{
    history.update_search(user_line);
}
//----------------------------------------------------------------------------
void
LineEditContext::cursor_SEARCH()
{
    user_line_before_history = user_line;
    history_entered = true;

    const UCS_string * ucs = history.search(user_line_before_history);
    if (ucs == 0)   // no line above
    {
        Log(LOG_get_line)
           {
             CERR << "hit top of history()" << endl;
             history.info(CERR << "cursor_SEARCH() done" << endl);
           }
        return;
    }

    adjust_allocated_height();

    uidx = 0;
    user_line = *ucs;
    refresh_from_cursor();
    move_idx(user_line.size());
    Log(LOG_get_line)   history.info(CERR << "cursor_SEARCH() done" << endl);
}
//============================================================================
LineInput::LineInput(bool do_read_history)
   : history(UserPreferences::uprefs.line_history_len),
     write_history(false)
{
   initial_termios_errno = 0;

   if (tcgetattr(STDIN_FILENO, &initial_termios))
      initial_termios_errno = errno;

   if (do_read_history)
      {
        history.read_history(UserPreferences::uprefs.line_history_path.c_str());
        write_history = true;
      }

   current_termios = initial_termios;

   // set current_termios to raw mode
   //
   current_termios.c_iflag &= ~( ISTRIP | // don't strip off bit 8
                                 INLCR  | // don't NL → CR
                                 IGNCR);  // don't ignore CR
   current_termios.c_iflag |=    IGNBRK | // ignore break
                                 IGNPAR |
                                 ICRNL  ; // CR → NL

   current_termios.c_lflag &= ~(ECHO | ECHONL | ICANON | IEXTEN);
   current_termios.c_lflag |= ISIG;

#ifndef apl_TARGET_LIBAPL
# ifndef apl_TARGET_PYTHON
   tcsetattr(STDIN_FILENO, TCSANOW, &current_termios);
# endif // apl_TARGET_PYTHON
#endif // apl_TARGET_LIBAPL
}
//----------------------------------------------------------------------------
void
LineInput::restore_termios()
{
   if (initial_termios_errno == 0)
      {
#ifndef apl_TARGET_LIBAPL
# ifndef apl_TARGET_PYTHON
        tcsetattr(STDIN_FILENO, TCSANOW, &initial_termios);
# endif // not apl_TARGET_PYTHON
#endif // not apl_TARGET_LIBAPL
      }
   initial_termios_errno = 1;   // prevent multiple calls
}
//----------------------------------------------------------------------------
LineInput::~LineInput()
{
   restore_termios();
   if (write_history)
      history.save_history(UserPreferences::uprefs.line_history_path.c_str());
}
//----------------------------------------------------------------------------
void
LineInput::init(bool do_read_history)
{
   the_line_input = new LineInput(do_read_history);
}
//============================================================================
void
InputMux::get_line(LineInputMode mode, const UCS_string & prompt,
                   UCS_string & line, bool & eof, LineHistory & hist)
{
   if (get_line_callback)
      {
        get_line_callback(mode, prompt, line, eof, hist);
        return;
      }

   if (InputFile::is_validating())   Quad_QUOTE::done(true, LOC);

   InputFile::increment_current_line_no();

   // check if we have input from a file. We do NOT use the file if the input
   // is for ⍞ unless we are in a .tc testcase file.
   //
bool interactive = (mode == LIM_Quote_Quad) || (mode == LIM_Quad_Quad);
   if (InputFile::is_validating())   interactive = false;
   if (InputFile::pushed_file())     interactive = true;

   if (!interactive)
      {
        UTF8_string file_line;
        bool file_eof = false;
        IO_Files::get_file_line(file_line, file_eof);

        if (!file_eof)
           {
             line = UCS_string(file_line);

             switch(mode)
                {
                  case LIM_ImmediateExecution:
                  case LIM_Quad_Quad:
                  case LIM_Quad_INP:
                       CIN << prompt << line << endl;
                       break;

                  case LIM_Quote_Quad:
                       line = prompt;

                       // for each leading backspace in line: discard last
                       // prompt character. This is for testing the user
                       // backspacing over the ⍞ prompt
                       //
                       while (line.size()      &&
                              file_line.size() &&
                              file_line[0] == UNI_BS)
                             {
                               file_line.erase(0);
                               line.pop_back();
                             }
                       line.append_UTF8(file_line.c_str());
                       break;

                  case LIM_Nabla:
                       break;

                  default: FIXME;
             }

          return;
        }
   }

   // no (more) input from files: get line from terminal
   //
   if (UserPreferences::uprefs.raw_cin)
      {
        Quad_QUOTE::done(mode != LIM_Quote_Quad, LOC);
        CIN << '\r' << prompt;
        char buffer[4000];
        const APL_time_us from = now();
         const char * s = fgets(buffer, sizeof(buffer) - 1, stdin);
        Workspace::add_wait(now() - from);

        if (s == 0)
           {
             eof = true;
             return;
           }
        buffer[sizeof(buffer) - 1] = 0;

        int slen = strlen(buffer);
        if (slen && buffer[slen - 1] == '\n')   buffer[--slen] = 0;
        if (slen && buffer[slen - 1] == '\r')   buffer[--slen] = 0;

        UTF8_string line_utf(buffer);
        line = UCS_string(line_utf);
        return;
      }

   Quad_QUOTE::done(mode != LIM_Quote_Quad, LOC);

const APL_time_us from = now();
   if (start_input)   (*start_input)();

#if PARALLEL_ENABLED
   CPU_pool::lock_pool(false);
#endif

   for (int control_D_count = 0;;)
       {
         bool _eof = false;
         LineInput::get_terminal_line(mode, prompt, line, _eof, hist);
         if (!_eof)   break;

         ++control_D_count;

         // ^D or end of file
         if (UserPreferences::uprefs.control_Ds_to_exit)   // ^D limit desired
            {
              if (control_D_count >= UserPreferences::uprefs.control_Ds_to_exit)
                 {
                   CIN << endl;
#if PARALLEL_ENABLED
                   Thread_context::kill_pool();
#endif // PARALLEL_ENABLED
                   UserPreferences::uprefs.silent = true;   // exit silently
                   Command::cmd_OFF(4);    // exit()s
                   return;  // not reached
                 }
            }
         else if (control_D_count < 5)
            {
              CIN << endl;
              COUT << "      ^D or end-of-input detected ("
                   << control_D_count << "). Use )OFF to leave APL!"
                   << endl;
           }

         eof = true;

         if (control_D_count > 10 && (now() - from)/control_D_count < 10000)
            {
              // we got 10 or more times EOF (or possibly ^D) at a rate
              // of 10 ms or faster. That looks like end-of-input rather
              // than ^D typed by the user. Abort the interpreter.
              //
              CIN << endl;
              COUT << "      *** end of input" << endl;
#if PARALLEL_ENABLED
              Thread_context::kill_pool();
#endif
              Command::cmd_OFF(2);   // exit()s
              return;  // not reached
            }
      }
#if PARALLEL_ENABLED
   CPU_pool::unlock_pool(false);
#endif

   Log(LOG_get_line)   CERR << " '" << line << "'" << endl;

   Workspace::add_wait(now() - from);
   if (end_input)   (*end_input)();

   if (UserPreferences::uprefs.echo_CIN)   COUT << prompt << line << endl;
}
//============================================================================
void
LineInput::get_terminal_line(LineInputMode mode, const UCS_string & prompt,
                             UCS_string & line, bool & eof,
                             LineHistory & hist)
{
   // no file input: get line interactively
   //
   switch(mode)
      {
        case LIM_ImmediateExecution:
        case LIM_Quote_Quad:
        case LIM_Quad_Quad:
        case LIM_Quad_INP:
             Output::set_color_mode(Output::COLM_INPUT);

             /* fall through */

        case LIM_Nabla:
             edit_line(mode, prompt, line, eof, hist);
             return;

        default: FIXME;
      }

   Assert(0 && "Bad LineInputMode");
}
//----------------------------------------------------------------------------
void
LineInput::edit_line(LineInputMode mode, const UCS_string & prompt,
                     UCS_string & user_line, bool & eof, LineHistory & hist)
{
   the_line_input->current_termios.c_lflag &= ~ISIG;   // disable ^C
#ifndef apl_TARGET_LIBAPL
   tcsetattr(STDIN_FILENO, TCSANOW, &the_line_input->current_termios);
#endif // apl_TARGET_LIBAPL

   user_line.clear();

LineEditContext lec(mode, 24, Workspace::get_PW(), hist, prompt);

   for (;;)
       {
         const Unicode uni = get_uni();
         switch(uni)
            {
              case UNI_InsertMode:
                   lec.toggle_ins_mode();
                   continue;

              case UNI_CursorHome:
                   lec.cursor_HOME();
                   continue;

              case UNI_CursorEnd:
                   lec.cursor_END();
                   continue;

              case UNI_CursorLeft:
                   lec.cursor_LEFT();
                   continue;

              case UNI_CursorRight:
                   lec.cursor_RIGHT();
                   continue;

              case UNI_CursorDown:
                   lec.cursor_DOWN();
                   continue;

              case UNI_CursorUp:
                   lec.cursor_UP();
                   continue;

              case UNI_DC2:  // ^R - search line history
                   lec.cursor_SEARCH();
                   continue;

              case UNI_EOF:  // end of file
                   eof = user_line.size() == 0;
                   break;

              case UNI_ETX:   // ^C
                   lec.clear();
                   control_C(SIGINT);
                   break;


#ifdef cfg_WANT_CTRLD_DEL
              case UNI_SUB:   // ^Z
                   CERR << "^Z";
                   eof = true;
                   break;

              case UNI_EOT:   // ^D
                   lec.delete_char();
                   lec.update_SEARCH();
                   continue;
#else
              case UNI_EOT:   // ^D
                   CERR << "^D";
                   eof = true;
                   break;
#endif

              case UNI_BS:    // ^H (backspace)
                   lec.backspc();
                   lec.update_SEARCH();
                   continue;

              case UNI_HT:    // ^I (tab)
                   lec.tab_expansion(mode);
                   lec.update_SEARCH();
                   continue;

              case UNI_VT:    // ^K
                   lec.cut_to_EOL();
                   lec.update_SEARCH();
                   continue;

              case UNI_DELETE:
                   lec.delete_char();
                   lec.update_SEARCH();
                   continue;

              case UNI_CR:   // '\r' : ignore
                   continue;

              case UNI_LF:   // '\n': done
                   lec.cursor_CLEAR_SEARCH();
                   break;

              case UNI_EM:    // ^Y
                   lec.paste();
                   lec.update_SEARCH();
                   continue;

              case Invalid_Unicode:
                   continue;

              default:  // regular APL character
                   lec.insert_char(uni);
                   lec.update_SEARCH();
                   continue;
            }

         break;
       }

   the_line_input->current_termios.c_lflag |= ISIG;   // enable ^C
#ifndef apl_TARGET_LIBAPL
   tcsetattr(STDIN_FILENO, TCSANOW, &the_line_input->current_termios);
#endif // apl_TARGET_LIBAPL

   user_line = lec.get_user_line();

   // maybe add history line
   //
bool add_hist = false;
   switch(mode)
      {
        case LIM_ImmediateExecution:
             add_hist = !InputFile::is_validating() && user_line.has_black();
             break;

        case LIM_Quote_Quad:
        case LIM_Quad_Quad:
             add_hist = !InputFile::is_validating();
             break;

        case LIM_Quad_INP:
             add_hist = false;
             break;

        case LIM_Nabla:
             // ∇-history is handled in a special way in Nabla.cc
             // so we don't do it here.
             //
             add_hist = false;
             break;
      }
 
   if (add_hist)   hist.add_line(user_line);

   CIN << endl;
}
//----------------------------------------------------------------------------
int
LineInput::safe_fgetc()
{
   for (;;)
       {
          const int ret = fgetc(stdin);
          if (ret != EOF)       return ret;
          if (errno == EINTR)   continue;

          if (got_WINCH)
             {
               got_WINCH = false;
               continue;
             }

         return EOF;
       }
}
//----------------------------------------------------------------------------
Unicode
LineInput::get_uni()
{
again:

const int b0 = safe_fgetc();
   if (b0 == EOF)   return UNI_EOF;

   if (b0 & 0x80)   // non-ASCII unicode
      {
        int len;
        uint32_t bx = b0;   // the "significant" bits in b0
        if ((b0 & 0xE0) == 0xC0)        { len = 2;   bx &= 0x1F; }
        else if ((b0 & 0xF0) == 0xE0)   { len = 3;   bx &= 0x0F; }
        else if ((b0 & 0xF8) == 0xF0)   { len = 4;   bx &= 0x0E; }
        else if ((b0 & 0xFC) == 0xF8)   { len = 5;   bx &= 0x0E; }
        else if ((b0 & 0xFE) == 0xFC)   { len = 6;   bx &= 0x0E; }
        else
           {
             CERR << "Bad UTF8 sequence start at " << LOC << endl;
             return Invalid_Unicode;
           }

        uint32_t uni = 0;
        loop(l, len - 1)
            {
              const UTF8 subc = safe_fgetc();
              if ((subc & 0xC0) != 0x80)
                 {
                   CERR << "Bad UTF8 sequence: " << HEX(b0)
                        << "... at " LOC << endl;
                   return Invalid_Unicode;
                 }

              bx  <<= 6;
              uni <<= 6;
              uni |= subc & 0x3F;
            }

        return Unicode(bx | uni);
      }

   if (b0 == UNI_ESC)
      {
        char seq[Output::MAX_ESC_LEN];   seq[0] = UNI_ESC;
        for (int s = 1; s < Output::MAX_ESC_LEN; ++s)
            {
              const int bs = safe_fgetc();
              if (bs == EOF)   return UNI_EOF;
              seq[s] = bs;

              // check for exact match
              //
              loop(e, ESCmap_entry_count)
                  {
                  if (ESCmap::the_ESCmap[e].is_equal(seq, s + 1))
                     return ESCmap::the_ESCmap[e].uni;
                  }

              // check for prefix match
              //
              if (ESCmap::need_more(seq, s))   continue;

//            CERR << endl << "Unknown ESC sequence: ESC";
//            loop(ss, s)   CERR << " " << HEX2(seq[ss + 1]);
//            CERR << endl;

              return Invalid_Unicode;
            }
      }
   else if (b0 < UNI_SPACE)   // ^something (except ESC)
      {
        switch(b0)
           {
             case UNI_SOH: return UNI_CursorHome;    // ^A
             case UNI_STX: return UNI_CursorLeft;    // ^B
             case UNI_ETX: return UNI_ETX;           // ^C
             case UNI_EOT: return UNI_EOT;           // ^D
             case UNI_ENQ: return UNI_CursorEnd;     // ^E
             case UNI_ACK: return UNI_CursorRight;   // ^F
             case UNI_BS:  return UNI_BS;            // ^H
             case UNI_HT:  return UNI_HT;            // ^I
             case UNI_LF:  return UNI_LF;            // ^J
             case UNI_VT:  return UNI_VT;            // ^K
             case UNI_SO:  return UNI_CursorDown;    // ^N
             case UNI_DLE: return UNI_CursorUp;      // ^P
             case UNI_DC2: return UNI_DC2;           // ^R
             case UNI_EM:  return UNI_EM;            // ^Y

#ifdef cfg_WANT_CTRLD_DEL
             // the user prefers to delete with ^D
             // and to mark the end a file with ^Z.
             case UNI_SUB: return UNI_SUB;           // ^Z
#endif

             default: goto again;
           }
      }
   else if (b0 == UNI_DELETE)   return UNI_BS;

   return Unicode(b0);
}
//----------------------------------------------------------------------------
