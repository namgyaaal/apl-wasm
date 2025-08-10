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

#include "Common.hh"
#include "Command.hh"
#include "DiffOut.hh"
#include "InputFile.hh"
#include "LineInput.hh"
#include "Output.hh"
#include "Performance.hh"
#include "PrintOperator.hh"
#include "Svar_DB.hh"
#include "UserPreferences.hh"   // for preferences and command line options

bool Output::colors_enabled = false;
bool Output::colors_changed = false;

int Output::color_CIN_foreground  = 0;
int Output::color_CIN_background  = 7;
int Output::color_COUT_foreground = 0;
int Output::color_COUT_background = 8;
int Output::color_CERR_foreground = 5;
int Output::color_CERR_background = 8;
int Output::color_UERR_foreground = 5;
int Output::color_UERR_background = 8;

/// a filebuf for CERR
ErrOut CERR_filebuf;

DiffOut DOUT_filebuf(false);
DiffOut UERR_filebuf(true);

// Android is supposed to define its own CIN, COUT, CERR, and UERR ostreams
#ifndef apl_TARGET_ANDROID

CinOut CIN_filebuf;
CIN_ostream CIN;

ostream COUT(&DOUT_filebuf);
ostream CERR(CERR_filebuf.use());
ostream UERR(&UERR_filebuf);

#endif

extern ostream & get_CERR();
ostream & get_CERR()
{
   if (UserPreferences::uprefs.output_to_cout)
      return ErrOut::used ? CERR : cout;
   else
      return ErrOut::used ? CERR : cerr;
};

Output::ColorMode Output::color_mode = COLM_UNDEF;

/// CSI sequence for ANSI/VT100 escape sequences (ESC [)
#define CSI "\x1B["

/// VT100 escape sequence to change to cin color
char Output::color_CIN[MAX_ESC_LEN] = CSI "0;30;47m";

/// VT100 escape sequence to change to cout color
char Output::color_COUT[MAX_ESC_LEN] = CSI "0;39;49m";

/// VT100 escape sequence to change to cerr color
char Output::color_CERR[MAX_ESC_LEN] = CSI "0;35;49m";

/// VT100 escape sequence to change to uerr color
char Output::color_UERR[MAX_ESC_LEN] = CSI "0;35;49m";

/// VT100 escape sequence to reset colors to their default
char Output::color_RESET[MAX_ESC_LEN] = CSI "0;39;49m";

/// VT100 escape sequence to clear to end of line
char Output::clear_EOL[MAX_ESC_LEN] = CSI "K";

/// VT100 escape sequence to clear to end of screen
char Output::clear_EOS[MAX_ESC_LEN] = CSI "J";

/// the ESC sequences sent by the cursor keys...
char Output::ESC_CursorUp   [MAX_ESC_LEN]   = CSI "A";    ///< Key ↑
char Output::ESC_CursorDown [MAX_ESC_LEN]   = CSI "B";    ///< Key ↓
char Output::ESC_CursorRight[MAX_ESC_LEN]   = CSI "C";    ///< Key →
char Output::ESC_CursorLeft [MAX_ESC_LEN]   = CSI "D";    ///< Key ←
char Output::ESC_CursorEnd  [MAX_ESC_LEN]   = CSI "F";    ///< Key End
char Output::ESC_CursorHome [MAX_ESC_LEN]   = CSI "H";    ///< Key Home
char Output::ESC_InsertMode [MAX_ESC_LEN]   = CSI "2~";   ///< Key Ins
char Output::ESC_Delete     [MAX_ESC_LEN]   = CSI "3~";   ///< Key Del

/// the ESC sequences sent by the cursor keys with SHIFT and/or CTRL...
/// the "\0" in the middle is a wildcard match for 0x32/0x35/0x36
char Output::ESC_CursorUp_1   [MAX_ESC_LEN] = CSI "1;" "\0" "A";   ///< Key ↑
char Output::ESC_CursorDown_1 [MAX_ESC_LEN] = CSI "1;" "\0" "B";   ///< Key ↓
char Output::ESC_CursorRight_1[MAX_ESC_LEN] = CSI "1;" "\0" "C";   ///< Key →
char Output::ESC_CursorLeft_1 [MAX_ESC_LEN] = CSI "1;" "\0" "D";   ///< Key ←
char Output::ESC_CursorEnd_1  [MAX_ESC_LEN] = CSI "1;" "\0" "F";   ///< Key End
char Output::ESC_CursorHome_1 [MAX_ESC_LEN] = CSI "1;" "\0" "H";   ///< Key Home
char Output::ESC_InsertMode_1 [MAX_ESC_LEN] = CSI "2;" "\0" "~";   ///< Key Ins
char Output::ESC_Delete_1     [MAX_ESC_LEN] = CSI "3;" "\0" "~";   ///< Key Del

//----------------------------------------------------------------------------
int
CinOut::overflow(int c)
{
PERFORMANCE_START(cerr_perf)
   if (!InputFile::echo_current_file())   return 0;

   Output::set_color_mode(Output::COLM_INPUT);
   cerr << char(c);
PERFORMANCE_END(fs_CERR_B, cerr_perf, 1)

   return 0;
}
//----------------------------------------------------------------------------
int
ErrOut::overflow(int c)
{
PERFORMANCE_START(cerr_perf)

   Output::set_color_mode(Output::COLM_ERROR);
   if (UserPreferences::uprefs.output_to_cout)   cout << char(c);
   else                                          cerr << char(c);
PERFORMANCE_END(fs_CERR_B, cerr_perf, 1)

   return 0;
}
//----------------------------------------------------------------------------
void
Output::init(bool logit)
{
   if (!isatty(fileno(stdout)))
      {
        cout.flush();
        cout.setf(ios::unitbuf);
      }

   if (logit)
      {
        CERR << "using ANSI terminal output ESC sequences (or those "
                "configured in your preferences file(s))" << endl;


        CERR << "using ANSI terminal input ESC sequences(or those "
                "configured in your preferences file(s))" << endl;
      }
}
//----------------------------------------------------------------------------
void
Output::reset_dout()
{
   DOUT_filebuf.reset();
}
//----------------------------------------------------------------------------
void
Output::reset_colors()
{
   if (!colors_changed)   return;

   cout << color_RESET << clear_EOL;
   cerr << color_RESET << clear_EOL;
}
//----------------------------------------------------------------------------
void
Output::set_color_mode(Output::ColorMode mode)
{
   if (!colors_enabled)      return;   // colors disabled
   if (color_mode == mode)   return;   // no change in color mode

   // mode changed
   //
   color_mode = mode;
   colors_changed = true;   // to reset them in reset_colors()

   switch(color_mode)
      {
        case COLM_INPUT:  cerr << color_CIN  << clear_EOL;   break;

        case COLM_OUTPUT: cout << color_COUT << clear_EOL;   break;

        case COLM_ERROR:  cerr << color_CERR << clear_EOL;   break;

        case COLM_UERROR: cout << color_UERR << clear_EOL;   break;

        default: break;
      }
}
//----------------------------------------------------------------------------
void 
Output::toggle_color(const UCS_string & arg)
{
   if (arg.starts_iwith("ON"))         colors_enabled = true;
   else if (arg.starts_iwith("OFF"))   colors_enabled = false;
   else                                colors_enabled = !colors_enabled;
}
//----------------------------------------------------------------------------
bool
Output::color_enabled()
{
   return Output::colors_enabled;
}
//============================================================================
void
CIN_ostream::set_cursor(int y, int x)
{
   if (UserPreferences::uprefs.raw_cin)   return;

   if (y < 0)
      {
        // y < 0 means from bottom upwards
        //
        *this << CSI << "30;" << (1 + x) << 'H'
              << CSI << "99B" << std::flush;
        if (y < -1)   *this << CSI << (-(y + 1)) << "A";
      }
   else
      {
        // y ≥ 0 is from top downwards. This is currently not used.
        //
        *this << CSI << (1 + y) << ";" << (1 + x) << 'H' << std::flush;
      }
}
//----------------------------------------------------------------------------
