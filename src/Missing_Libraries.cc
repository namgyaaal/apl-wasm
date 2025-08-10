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
#include "Error_macros.hh"
#include "Token.hh"

extern Token missing_files(const char * qfun,  const char ** libs,
                           const char ** hdrs, const char ** pkgs);

//============================================================================
/// a generic function for all ⎕XXX errors caused by missing libraries.
/// Declared extern (rather than /// static) to avoid -Wunused-function
/// warnings.
Token
missing_files(const char * qfun,   // the function, e.g. "⎕RE"
              const char ** libs,  // the required libraries
              const char ** hdrs,  // the required header files
              const char ** pkgs)  // the proposed packages
{
UCS_string & more = MORE_ERROR() <<
"Bad luck. The system function " << qfun <<
" has raised a SYNTAX ERROR even though the\n"
"syntax used was correct. The real reason for the SYNTAX ERROR was that a\n"
"library or header file on which " << qfun << " depends:\n"
"\n"
" ⋆ could not be found by ./configure, and/or\n"
" ⋆ was explitly disabled by a ./configure argument\n"
"\n"
"just before GNU APL was compiled.\n";

   if (libs && libs[0])
      {
        const char * numerus1 = libs[1] ? "ies "   : "y ";
        const char * numerus2 = libs[1] ? " were:" : " was:";
        more << "\nThe possibly missing (or disabled) librar" << numerus1
             << "needed by " << qfun << numerus2;
        for (int j = 0; libs[j]; ++j)   more << " " << libs[j];
        more << "\nTo locate installed versions of it, run e.g.:\n\n";
        for (int j = 0; libs[j]; ++j)
            more << "      )HOST find /usr -name '" << libs[j]
                 << "*' 2>/dev/null\n";
      }

   if (hdrs && hdrs[0])
      {
        const char * numerus = hdrs[1] ? "s were" : " was";
        more << "\nThe possibly missing header file" << numerus << ":";
        for (int j = 0; hdrs[j]; ++j)   more << " " << hdrs[j];
        more << "\nTo locate it, run e.g.:\n\n";
        for (int j = 0; hdrs[j]; ++j)
            more << "      )HOST find /usr -name '" << hdrs[j]
                 << "' 2>/dev/null\n";
      }

   more <<
"\n"
"This GNU APL interpreter instance was probably configured like this:\n"
"\n"
"      " cfg_CONFIGURE_ARGS "\n\n";

   if (pkgs && pkgs[0])
      {
        more <<
"If the problem was caused by missing libraries or header files, then (on a\n"
" standard GNU/Linux/Debian system) they can usually be installed with the\n"
"following command:\n"
"\n"
"      apt install";
        for (int j = 0; pkgs[j]; ++j)   more << " " << pkgs[j];
        more <<
"\n"
"\n"
"and, after that, reconfigure, recompile, and reinstall GNU APL:\n\n"
"      ";

   if (strstr(cfg_CONFIGURE_ARGS, "without") ||
       strstr(cfg_CONFIGURE_ARGS, "=no"))
      {
        // the user may have explicitly disabled something. We therefore
        // do not want to propose her current ./configure options.
        more << "./configure <configure-args ...>\n";
      }
   else
      {
        more << cfg_CONFIGURE_ARGS "\n";
      }

   more <<
"      make\n"
"      sudo make install\n"
"\n"
"in the top-level directory of the GNU APL package. On GNU/Linux distros\n"
"other than Debian, the package manager may not be apt, and the name of the\n"
"may also differ slightly. Always use the -dev variant of the package (which\n"
"also contains the required header files).\n";
      }

   SYNTAX_ERROR;
   return Token();
}
//============================================================================
