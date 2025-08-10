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

/*
   #define macros Assert(cond) and Assert1(cond)

   All macros may (depending on cfg_ASSERT_LEVEL_WANTED) evaluate the
   condition-expression cond. If cond is true (the expected case),
   then no more actions are performed.

   If cond is false (the unexpected case) then the macro will print a complaint
   show a stack-trace (both visible in APL). Interpretation of APL code will
   continue for the moment, but sooner or later the interpreter will crash.

   cfg_ASSERT_LEVEL_WANTED = 0: neither Assert() nor Assert1() will complain
   cfg_ASSERT_LEVEL_WANTED = 1: Assert() will complain. Assert1() will not
   cfg_ASSERT_LEVEL_WANTED = 2: both Assert() and Assert1() will complain

   That is, higher cfg_ASSERT_LEVEL_WANTED will raise more complaints.

   The default ./configure (intended for normal users) will have
   cfg_ASSERT_LEVEL_WANTED = 0.

   The ./configure DEVELOP_WANTED=yes (aka. make develop) will have
   cfg_ASSERT_LEVEL_WANTED = 2.
 */

#ifndef __ASSERT_HH_DEFINED__
#define __ASSERT_HH_DEFINED__

#include "Common.hh"

/// the complaint function
extern void do_Assert(const char * cond, const char * fun,
                      const char * file, int line)
#ifdef __GNUC__
    __attribute__ ((noreturn))
#endif
;

#ifndef cfg_ASSERT_LEVEL_WANTED

#error "ASSERT_LEVEL_WANTED not defined"

#elif cfg_ASSERT_LEVEL_WANTED == 0

// use 'if (x) {}' below to suppress 'uninitialized' compiler warnings

#define Assert1(x) if (x) {}
#define Assert(x)  if (x) {}

#elif cfg_ASSERT_LEVEL_WANTED == 1

#define Assert1(x) if (x) {}
#define Assert(x)  if (!(x))   do_Assert(#x, __FUNCTION__, __FILE__, __LINE__)

#elif cfg_ASSERT_LEVEL_WANTED == 2

#define Assert1(x) if (!(x))   do_Assert(#x, __FUNCTION__, __FILE__, __LINE__)
#define Assert(x)  if (!(x))   do_Assert(#x, __FUNCTION__, __FILE__, __LINE__)

#else

#error "Bad or #undef'ed cfg_ASSERT_LEVEL_WANTED"

#endif

#define NeverReach(X) do_Assert(X, __FUNCTION__, __FILE__, __LINE__)

/// normal assertion

/// assertion being fatal if wrong
#define Assert_fatal(x) if (!(x)) {\
   cerr << endl << endl << "FATAL error at " << __FILE__ << ":" << __LINE__ \
        << endl;   exit(2); }

#endif // __ASSERT_HH_DEFINED__

