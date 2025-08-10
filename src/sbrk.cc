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

// this file provides a wrapper around sbrk() that allows sbrk(0) to be called
// without #including unistd.h. The reason is to avoid a weird warning on
// Apple machines telling that 'sbrk' is deprecated

#include <stdint.h>

/// the standard sbrk() function (known by everybody except Apple OS X)
extern "C" void * sbrk(int increment);
extern uint64_t top_of_memory();

/// our sbrk() so that it compiles under Apple OS X
uint64_t
top_of_memory()
{
   if (sizeof(const void *) == 4)
      return 0xFFFFFFFFULL & uint64_t(sbrk(0));
   else
      return uint64_t(sbrk(0));
}

