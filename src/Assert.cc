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

#include "Assert.hh"
#include "Backtrace.hh"
#include "Common.hh"
#include "Output.hh"
#include "IO_Files.hh"
#include "Workspace.hh"

#include "Workspace.icc"

/// prevent recursive do_Assert() calls.
static bool asserting = false;

//----------------------------------------------------------------------------
void
do_Assert(const char * cond, const char * fun, const char * file, int line)
{
char loc[FILENAME_MAX + 20];

   Log(LOG_delete)
      get_CERR() << "new    " << voidP(loc) << " at " LOC << endl;

   SPRINTF(loc, "%s:%d", file, line);

   get_CERR() << endl
        << "======================================="
           "=======================================" << endl;


   if (cond)       // normal assert()
      {
        get_CERR() << "Assertion failed: " << cond << endl
             << "in Function:      " << fun  << endl
             << "in file:          " << loc  << endl << endl;
      }
   else if (fun)   // segfault etc.
      {
        get_CERR() << "\n\n================ " << fun <<  " ================\n";
      }

   get_CERR() << "C/C++ call stack:" << endl;

   if (asserting)
      {
        get_CERR() << "*** do_Assert() called recursively ***" << endl;
      }
   else
      {
        asserting = true;

        BACKTRACE

        get_CERR() << endl << "SI stack:" << endl << endl;
        Workspace::list_SI(get_CERR(), SIM_SIS_dbg);
      }
   get_CERR() << "======================================="
           "=======================================" << endl;

   // count errors
   IO_Files::assert_error();

   if (Error * err = Workspace::get_error())
      new (err) Error(E_ASSERTION_FAILED, loc);

   asserting = false;

   throw E_ASSERTION_FAILED;
}
//----------------------------------------------------------------------------

