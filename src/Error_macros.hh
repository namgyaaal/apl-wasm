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

#ifndef __ERROR_MACROS_HH_DEFINED__
#define __ERROR_MACROS_HH_DEFINED__

#include "Backtrace.hh"
#include "ErrorCode.hh"

/// throw an Error with \b code only (typically a standard APL error)
void throw_apl_error(ErrorCode code, const char * loc)
#ifdef __GNUC__
    __attribute__ ((noreturn))
#endif
;

#define ATTENTION         { throw_apl_error(E_ATTENTION,          LOC); }
#define AXIS_ERROR          throw_apl_error(E_AXIS_ERROR,           LOC)
#define DEFN_ERROR          throw_apl_error(E_DEFN_ERROR,           LOC)
#define DOMAIN_ERROR        throw_apl_error(E_DOMAIN_ERROR,         LOC)
#define INDEX_ERROR         throw_apl_error(E_INDEX_ERROR,          LOC)
#define INTERNAL_ERROR      throw_apl_error(E_INTERNAL_ERROR,       LOC)
#define INTERRUPT           { \
                              throw_apl_error(E_INTERRUPT,          LOC); }
#define LENGTH_ERROR        throw_apl_error(E_LENGTH_ERROR,         LOC)
#define LIMIT_ERROR_RANK    throw_apl_error(E_SYS_LIMIT_RANK,       LOC)
#define LIMIT_ERROR_SVAR    throw_apl_error(E_SYS_LIMIT_SVAR,       LOC)
#define LIMIT_ERROR_FUNOPER throw_apl_error(E_SYS_LIMIT_FUNOPER,    LOC)
#define LIMIT_ERROR_PREFIX  throw_apl_error(E_SYS_LIMIT_PREFIX,     LOC)
#define RANK_ERROR          throw_apl_error(E_RANK_ERROR,           LOC)
#define SYNTAX_ERROR        throw_apl_error(E_SYNTAX_ERROR,         LOC)
#define LEFT_SYNTAX_ERROR   throw_apl_error(E_LEFT_SYNTAX_ERROR,    LOC)
#define SYSTEM_ERROR        throw_apl_error(E_SYSTEM_ERROR,         LOC)
#define TODO                throw_apl_error(E_NOT_YET_IMPLEMENTED,  LOC)
#define FIXME               {  BACKTRACE; cleanup(false); exit(0);        \
                               throw_apl_error(E_THIS_IS_A_BUG,     LOC); }
#define VALUE_ERROR         throw_apl_error(E_VALUE_ERROR,          LOC)
#define VALENCE_ERROR       throw_apl_error(E_VALENCE_ERROR,        LOC)
#define WS_FULL           { throw_apl_error(E_WS_FULL,              LOC); }

#endif  // __ERROR_MACROS_HH_DEFINED__
