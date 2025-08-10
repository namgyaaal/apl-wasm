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


#ifndef __TAB_EXPANSION_HH_DEFINED__

#include "Common.hh"
#include "UCS_string.hh"

/// result of a tab expansion
enum ExpandResult
{
   /// no extension found: ignore the TAB entered by the user.
   ER_IGNORE  = 0,

   /// something has matched: replace the user string. Returned if there
   /// is a unique expansion of the string entered by the user.
   ER_REPLACE = 1,

   /// some epansions have been displayed, but the user string was not
   /// updated because the was no unique expansion.
   /// because the
   ER_AGAIN = 2,
};

/// tab completion hints
enum ExpandHint
{
   // o below indicates optional items
   //
   EH_NO_PARAM,       ///< no parameter

   EH_oWSNAME,        ///< optional workspace name
   EH_oLIB_WSNAME,    ///< library ref, workspace
   EH_oLIB_oPATH,     ///< directory path
   EH_FILENAME,       ///< filename
   EH_DIR_OR_LIB,     ///< directory path or library reference number
   EH_oPATH,           ///< optional directory path
   EH_WSNAME,         ///< workspace name

   EH_oFROM_oTO,      ///< optional from-to (character range)
   EH_oON_OFF,        ///< optional ON or OFF
   EH_SYMNAME,        ///< symbol name
   EH_ON_OFF,         ///< ON or OFF
   EH_LOG_NUM,        ///< log facility number
   EH_SYMBOLS,        ///< symbol names...
   EH_oCLEAR,         ///< optional CLEAR
   EH_oCLEAR_SAVE,    ///< optional CLEAR or SAVE
   EH_HOSTCMD,        ///< host command
   EH_UCOMMAND,       ///< user-defined command
   EH_COUNT,          ///< count
   EH_BOXING,         ///< boxing parameter
   EH_PRIMITIVE,      ///< apl primitive
   EH_CONFIG,         ///< config.h item (capability)
   EH_oAUTO,          ///< optional AUTO
};
//----------------------------------------------------------------------------
/// a class for doing interactivw Tab-expansion on input lines
class TabExpansion
{
public:
   /// constructor
   TabExpansion(UCS_string & line);

   /// perform tab expansion of the user input \b line. ExpandResult says how
   /// \b line was excapnded and line MAY have become longer.
   ExpandResult expand_tab(UCS_string & line);

protected:
   /// tab-expand a user-defined name (variable or defined function).
   ExpandResult expand_user_name(UCS_string & user_input);

   /// tab-expand an APL command
   ExpandResult expand_APL_command(UCS_string & user_input);

   /// tab-expand a system-defined name (⎕xxx)
   ExpandResult expand_distinguished_name(UCS_string & user_input);

   /// show the names of all APL capabilities (in config.h)
   ExpandResult expand_capability(UCS_string & user);

   /// show the names of all APL primitives and user defined functions
   ExpandResult expand_help_topics();

   /// tab-expand or show help for user defined functions
   ExpandResult expand_help_topics(UCS_string & user_input);

   /// perform tab expansion for command arguments
   ExpandResult expand_command_arg(UCS_string & user_input,
                                   ExpandHint ehint,
                                   const char * shint,
                                   const UCS_string cmd,
                                   const UCS_string arg);

   /// perform tab expansion for a filename
   ExpandResult expand_filename(UCS_string & user_input,
                                ExpandHint ehint, const char * shint,
                                const UCS_string cmd, UCS_string arg);

   /// perform tab expansion for a workspace name
   ExpandResult expand_wsname(UCS_string & user_input, const UCS_string cmd,
                              LibRef lib, const UCS_string filename);

   /// compute the lenght of the common part in all matches
   int compute_common_length(int len, const UCS_string_vector & matches);

   /// read filenames in \b dir and append matching filenames to \b matches
   void read_matching_filenames(DIR * dir, UTF8_string dirname,
                                UTF8_string prefix, ExpandHint ehint,
                                UCS_string_vector & matches);

   /// show the different alternatives for tab expansions
   ExpandResult show_alternatives(UCS_string & user, int prefix_len,
                                  UCS_string_vector & matches);

   /// true if the original line had a trailing blank
   const bool have_trailing_blank;
};
#define __TAB_EXPANSION_HH_DEFINED__
#endif // __TAB_EXPANSION_HH_DEFINED__


