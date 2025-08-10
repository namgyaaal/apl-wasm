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

#ifndef __SECURITY_HH_DEFINED__
#define __SECURITY_HH_DEFINED__

#include "Common.hh"
#include "UserPreferences.hh"

extern void not_allowed(const char * what);

#ifndef cfg_SECURITY_LEVEL_WANTED

#error "cfg_SECURITY_LEVEL_WANTED not defined"

#elif cfg_SECURITY_LEVEL_WANTED == 0

#define CHECK_SECURITY(X)

#elif cfg_SECURITY_LEVEL_WANTED == 1

#define CHECK_SECURITY(X)  if (uprefs.X)   not_allowed(#X);

#elif cfg_SECURITY_LEVEL_WANTED == 2

#define CHECK_SECURITY(X)  not_allowed(#X);

#else 

#error "Bad SECURITY_LEVEL_WANTED"

#endif

#define NeverReach(X) do_Assert(X, __FUNCTION__, __FILE__, __LINE__)

/// trivial assertion

/// normal assertion

/// assertion being fatal if wrong
#define Assert_fatal(x) if (!(x)) {\
   cerr << endl << endl << "FATAL error at " << __FILE__ << ":" << __LINE__ \
        << endl;   exit(2); }

#endif // __SECURITY_HH_DEFINED__

