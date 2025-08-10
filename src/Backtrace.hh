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

#ifndef __BACKTRACE_HH_DEFINED__
#define __BACKTRACE_HH_DEFINED__

#include <stdint.h>

#include "Common.hh"
#include "PrintOperator.hh"

/// (maybe) init a dwarf object for the apl binary
extern void init_DWARF(const char * bin_dir, const char * bin_file);

/// show the current function call stack.
class Backtrace
{
public:
   /// show the current function call stack.
   static void show(const char * file, int line);

   /// show the current caller (only valid while being called)
   static const char * caller(int offset);

protected:
   /// demangle a line returned by backtrace_symbols()
   static int demangle_line(char * result, size_t result_max, const char * buf);

   /// find the source for PC \b pc
   static const char * find_src(int64_t pc);

   /// read the file apl.lines (if present) and set main_offset from the
   /// address of main() in apl.lines.
   static void read_apl_lines_file();

   /// print one item in the backtrace to cerr. NOTE: modifies s.
   static void show_item(int idx, char * s);

   /// print the dwarf info of one item in the backtrace to cerr.
   static void show_dwarf(int idx, const char * s);

   /// a mapping from PCs to source lines.
   struct PC_src
      {
        int64_t pc;             ///< the pc
        const char * src_loc;   ///< the source locstion
      };

   /// a mapping from PCs to source lines.
   static std::vector<PC_src> pc_2_src;

   /// compare PCs (helper for binary search)
   static int pc_cmp(const int64_t & key,
                     const Backtrace::PC_src & pc2, const void *);
};

#define BACKTRACE Backtrace::show(__FILE__, __LINE__);

#endif // __BACKTRACE_HH_DEFINED__
