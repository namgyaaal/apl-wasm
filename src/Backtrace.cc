/*
    This file is part of GNU APL, a free implementation of the
    ISO/IEC Standard 13751, "Programming Language APL, Extended"

    Copyright © 2008-2024  Dr. Jürgen Sauermann,
    Copyright ©      2024  Paul Rockwell (Apple)

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

#include <assert.h>
#include <fcntl.h>
#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "Common.hh"   // for HAVE_EXECINFO_H et al.
#ifdef HAVE_EXECINFO_H
# include <execinfo.h>
# include <cxxabi.h>
#define EXEC(x) x
#else
#define EXEC(x)
#endif

#if defined(HAVE_LIBDWARF) && defined(HAVE_LIBDWARF_LIBDWARF_H)
# define HAVE_DWARF 0   /* ongoing work, set to 1 if it works */ 
#endif

#include "Backtrace.hh"

#if HAVE_DWARF
# include <libelf.h>
# include <libdwarf/libdwarf.h>
# include <libdwarf/dwarf.h>

// global Dwarf_Error and Dwarf_Debug variables
Dwarf_Error error = 0;
Dwarf_Debug dbg = 0;
void
init_DWARF(const char * bin_dir, const char * bin_file)
{
string filename(bin_dir);
   filename += '/';
   filename += bin_file;

   get_CERR() << " Initializing DWARF for file " << filename << "..." << endl;

   if (const int rc = dwarf_init_path(
       filename.c_str(),
       0,            // char *            true_path_out_buffer
       0,            // unsigned int      true_path_bufferlen
       0,            // Dwarf_Unsigned    access
       0,            // unsigned int      groupnumber
       0,            // Dwarf_Handler     errhand
       0,            // Dwarf_Ptr         errarg
       &dbg,         // Dwarf_Debug*      dbg
       0, 0, 0,      // const char *      reserved1..3
       &error))      // Dwarf_Error *     error
      {
         get_CERR() << "init_DWARF(): rc=" << rc << ", error-" << error << endl
              << dwarf_errmsg(error) << endl;
      }
   else get_CERR() << "OK." << endl;
}

#else   // no dwarf

void
init_DWARF(const char * bin_dir, const char * bin_file)
{
}

#endif

#define NO_PC (-1LL)

using namespace std;

std::vector<Backtrace::PC_src> Backtrace::pc_2_src;

/// the difference between function main() in the result of backtrace_symbol()
/// and the address of main() in the loaded program. Only valid after the
/// line <main>: was processes in read_apl_lines_file().
//
static int64_t main_offset_0 = 0;

//----------------------------------------------------------------------------
/** compute \b main_offset_0, which is the difference between:

    a.  the 0-based address of main() in file apl.lines, and
    b.  the address of main() in memory after loading the apl binary.

   Only valid for apl.lines!
 */

/*
    main_line comes from apl.line an looks like this:

000000000014cedd <main>:

 */
static void
set_main_offset_0(const char * main_line)
{
const int64_t backtrace_main = strtoll(main_line, 0, 16);
const int64_t loaded_main = get_main();
   main_offset_0 = loaded_main - backtrace_main;

   cerr << hex << setfill(' ')
        << "      main() in apl.lines: " << setw(16) << backtrace_main << endl
        << "    + main_offset_0:       " << setw(16) << main_offset_0  << endl
        << "    ───────────────────────────────────────"               << endl
        << "    = loaded main():       " << setw(16) << loaded_main    << endl
        << dec;
}
//----------------------------------------------------------------------------
int
Backtrace::pc_cmp(const int64_t & key, const PC_src & pc_src, const void *)
{
   return key - pc_src.pc;
}
//----------------------------------------------------------------------------
const char *
Backtrace::find_src(int64_t pc)
{
   if (pc == NO_PC)   return 0;   // no pc

   if (pc_2_src.size())   // database was properly set up.
      {
        typedef Heapsort<Backtrace::PC_src> HS;
        if (const PC_src * posp = HS::search<const int64_t &>
                                            (pc,                // key
                                             pc_2_src.data(),   // array
                                             pc_2_src.size(),   // array size
                                             &pc_cmp,           // compare fun
                                             0))                // compare arg
        return posp->src_loc;   // found
      }

   (0) && cerr << "PC=" << hex << pc << dec << " not found in apl.lines" << endl;
   return 0;   // not found
}
//----------------------------------------------------------------------------
void
Backtrace::read_apl_lines_file()
{
  /// the status of file apl.lines
static enum APL_lines_status
      {
        LINES_not_checked,   ///< status is unknown
        LINES_outdated,      ///< file is present, but outdated
        LINES_unusable,      ///< file is not usable for some reason
        LINES_valid          ///< file is OK and up-to-date
      } lines_status = LINES_not_checked;

   if (lines_status != LINES_not_checked)   return;   // already called

   // line numbers work only if ./configure was called with CXXFLAGS set
   // to (at least) -rdynamic -gdwarf-2. Give up if this was not the case.
   //
   if (0 == strstr(cfg_CONFIGURE_ARGS, "-rdynamic") ||
       0 == strstr(cfg_CONFIGURE_ARGS, "-gdwarf-2"))
      {
        cerr << "*** useless apl.lines "
                "(no CXXFLAGS=-rdynamic -gdwarf-2)" << endl;
         lines_status = LINES_unusable;
         return;
      }

   // we are here for the first time.
   // Assume that apl.lines is outdated until proven otherwise.
   //
   lines_status = LINES_outdated;

struct stat st;
   if (stat("apl", &st))   return;   // stat apl failed

const time_t apl_time = st.st_mtime;

const int fd = open("apl.lines", O_RDONLY);
   if (fd == -1)   return;

   if (fstat(fd, &st))   // stat apl.lines failed
      {
        close(fd);
        return;
      }

const time_t apl_lines_time = st.st_mtime;

   if (apl_lines_time < apl_time)
      {
        close(fd);
        get_CERR() <<
"\nNOTE: Cannot show source file line numbers, because the file 'apl.lines'\n"
"      is older than the file 'apl' (= the GNU APL binary).\n"
"      To fix this: run 'make apl.lines' in directory:\n"
"      " << apl_DIR__src << endl;
        return;
      }

   pc_2_src.reserve(100000);
char buffer[1000];
FILE * file = fdopen(fd, "r");
size_t file_lines = 0;
size_t asm_lines = 0;

   assert(file);

const char * src_line = 0;
bool new_line = false;
int64_t prev_pc = NO_PC;

   for (;;)
       {
         const char * s = fgets(buffer, sizeof(buffer) - 1, file);
         if (s == 0)   break;   // end of file.
         ++file_lines;

         // the end of the line depends on the platform. On GNU/Linux it
         // is '\n' but on other platforms it might be '\r' \'n'. Or missing
         // completely if the file does not end with EOF.
         //
         buffer[sizeof(buffer) - 1] = '\0';   // just in case
         int slen = strlen(buffer);
         if (slen && buffer[slen - 1] == '\n')   buffer[--slen] = '\0';
         if (slen && buffer[slen - 1] == '\r')   buffer[--slen] = '\0';
         if (slen == 0)   continue;   // empty line

         /* the file supposedly contains only 3 types of non-empty lines:

            case 1: the (single) <main>: line,
            case 2: absolute source file path lines, and
            case 3: code lines.

            For example:

000000000014c7a9 <main>:                                   [<main> line]
main():
/home/eedjsa/apl-1.9/src/main.cc:611                       [source file path]
  14c7a9:»······55                   »··push   %rbp        [code line]
  14c7aa:»······48 89 e5             »··mov    %rsp,%rbp   [code line]
  ...

            NOTE: case 2 only works with:

            CXXFLAGS='-rdynamic -gdwarf-2' ./configure ...

            since newer g++ compiler versions strip location information
            by default.
          */

         if (strstr(s, "<main>:"))   // case 1: <main line>
            {
              set_main_offset_0(s);
            }

         if (s[0] == '/')            // case 2: source file path
            {
              src_line = strdup(strrchr(s, '/') + 1);
              new_line = true;
              continue;
            }

         if (!new_line)   continue;

         if (s[8] == ':')   // case 3: code line
            {
              ++asm_lines;
              errno = 0;
              long long pc = strtoll(s, 0, 16);   // opcode address
              if (errno)   // strtoll() failed
                 {
                   pc = NO_PC;
                 }
              else         // strtoll() succeeded
                 {
                   if (pc < prev_pc)
                      {
                         cerr << endl
                              << "in file apl.lines:" << file_lines << endl
                              << lhex << "prev_pc was: " << prev_pc << endl
                              << "pc is:       " << pc << nohex     << endl
                              << "line is:     '" << s << "'"       << endl
                              << endl;
                         assert(0 && "file apl.lines is not ordered by PC");
                         break;
                      }

                   PC_src pcs = { pc, src_line };
                   (0) && cerr << "adding " << uhex << pcs.pc << dec
                             << " aka. " << pcs.src_loc << endl;
                   pc_2_src.push_back(pcs);
                   prev_pc = pc;

                   // new_line = false;   causes some lines to not be found
                 }
              continue;
            }
       }

   fclose(file);   // also closes fd

   cerr << "\n      assembler lines in apl.lines: " << asm_lines
        << "\n    + source line numbers found:    " << (file_lines - asm_lines)
        << "\n    ──────────────────────────────────────"
        << "\n    = total_lines in apl.lines:     " << file_lines
        << endl;
   lines_status = LINES_valid;
}
//----------------------------------------------------------------------------
void
Backtrace::show_item(int idx, char * s)
{
#ifdef HAVE_EXECINFO_H
    
// Change this to #define DISPLAY_ASM_OFFSET if you would like the asm_offset
// displayed in the backtrace the default is not to display
//
#undef  DISPLAY_ASM_OFFSET
    
int64_t abs_addr = NO_PC;
char * fun = nullptr;
long long asm_offset = 0;
   (void)asm_offset;   // avoid warning if not used
    
#ifdef __APPLE__
/*
    on macOS/Darwin, string s looks like this:

    0x00000001000a93f0 _ZN9Workspace19immediate_executionEb + 68
    ││││││││││││││││││ ││││││││││││││││││││││││││││││││││││   ││
    ││││││││││││││││││ ││││││││││││││││││││││││││││││││││││   └┴─── asm_offset
    ││││││││││││││││││ └┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴──────── fun
    └┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴───────────────────────────────────────────── abs_addr
 */

   // Find the abs_addr by skipping to the first ' 0x'

   if (char * a_a = strstr(s, " 0x") )   // the space before abs_addr
      {
          // extract abs_addr now that we found it
          *a_a = '\0';
          a_a++;
          if (char * e = strchr(a_a,' ')) {
              *e = '\0';
              fun = e + 1;
              abs_addr = strtoll(a_a, nullptr, 16);
              abs_addr -= main_offset_0;
          }
      }

    if (fun)
    {
        if (char * e = strchr(fun , ' '))
        {
            *e = '\0';
            e++ ;                                 // skip and look for "+ " ' '
            if (char * a_o = strstr(e,"+ "))
            {
                a_o += 2;
                asm_offset = strtoll(a_o,nullptr,10);
            }
        }
    }
    
#else /* __APPLE__ not defined,  a.k.a. other Linux, etc. platforms */

/*
    string s looks like this:
    
     ./apl(_ZN10APL_parser9nextTokenEv+0x1dc) [0x80778dc]
           │││││││││││││││││││││││││││ │││││   │││││││││
           │││││││││││││││││││││││││││ │││││   └┴┴┴┴┴┴┴┴───── abs_addr
           │││││││││││││││││││││││││││ └┴┴┴┴───────────────── asm_offset
           └┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴─────────────────────── fun
   */

   // split off abs_addr from s.
   //

   if (char * space = strchr(s, ' '))   // the space before [abs_addr]
      {
        *space = '\0';
        space += 2;
        if (char * e = strchr(space, ']'))   *e = 0;
        abs_addr = strtoll(space, 0, 16);
        abs_addr -= main_offset_0;
      }

   // split off function from s.
   //
   {
    char * opar = strchr(s, '(');
    if (opar)
       {
         *opar = '\0';
         fun = opar + 1;
         char * e = strchr(opar + 1, ')');
         if (e)   *e = '\0';
       }
   }

   // split off +asm_offset from fun.
   //
   if (fun)
      {
       if (char * plus = strchr(fun, '+'))
          {
            *plus++ = '\0';
            asm_offset = strtoll(plus, nullptr, 16);
          }
      }

# endif /* __APPLE__ */

const char * src_loc = find_src(abs_addr);

char obuf[200] = "@@@@";
   if (fun)
      {
       strncpy(obuf, fun, sizeof(obuf) - 1);
       obuf[sizeof(obuf) - 1] = '\0';

       size_t obuflen = sizeof(obuf) - 1;
       int status = 0;
//     cerr << "mangled fun is: " << fun << endl;
       __cxxabiv1::__cxa_demangle(fun, obuf, &obuflen, &status);
       switch(status)
          {
            case 0: // demangling succeeded
                 break;

            case -2: // not a valid name under the C++ ABI mangling rules.
                 break;

            default:
                 cerr << "__cxa_demangle() returned " << status << endl;
                 break;
          }
      }

// cerr << setw(2) << idx << ": ";

   // we normally prefer uppercase hex, but 'objcopy' and friends produce
   // lowercase hex and we follow suit as to simplify searching in their files.
   //
   cerr << "0x" << lhex << abs_addr << nohex;

// cerr << left << setw(20) << s << right << " ";

   // indent.
   //
   for (int i = -1; i < idx; ++i)   cerr << " ";

   cerr << obuf;

# ifdef DISPLAY_ASM_OFFSET
   if (asm_offset > 0)   cerr << " + " << asm_offset;
# endif

   if (src_loc)
      {
        char cc[200];
        SPRINTF(cc, "%s", src_loc);
        if (char * disc = strstr(cc, "discriminator"))   disc[-1] = '\0';
        cerr << " at " << cc;
      }
   cerr << endl;
#endif   // _APPLE_
}

#if HAVE_DWARF
//----------------------------------------------------------------------------
/// the context of a search in a dwarf file
struct Dwarf_context
{
   Dwarf_context(int64_t _PC, ostream * _out)
   : PC(_PC),
     address_of_main(0),
     out(_out),
     found(false),
     attr_name(0),
     attr_PC_low(0),
     found_PC_range(false),
     res(DW_DLV_OK),
     indent("    ")
   {}

   /// the PC to look for
   const int64_t PC;

   /// the dwarf address of main()
   int64_t address_of_main;
   /// output stream, non-zero if debug printouts are requested
   ostream * out;

   /// found the information that was searched
   bool found;

   /// the value of the name attribute of a die (at various levels).
   const char * attr_name;

   /// the low PC of a die (
   int64_t attr_PC_low;

   /// the address of main()
   int64_t main_low;

   /// the end of main()
   int64_t main_high;

   /// found the PC looked for
   bool found_PC_range;

   /// the last dwarf_xxx() function call result
   int res;

   /// one step of indentation (blanks)
   const char * indent;
};
//===========================================================================
class Dwarf
{
public:
   /// find PC in the entire GNU APL binary
   static int find_PC_in_file(Dwarf_context & ctx, int64_t PC);

   /// store all (top-level) CU pointers 
   static int init_CU_dies(Dwarf_context & ctx);

   /// return attribute \b which_attribute of the die
   static const Dwarf_Attribute get_die_attribute(Dwarf_context & ctx,
                                                  Dwarf_Die die,
                                                  Dwarf_Half which_attribute);

   /// return the name of the die (DW_AT_name)
   static const char * get_die_name(Dwarf_context & ctx, Dwarf_Die die);

protected:
   /// scan the attribute list of die for attibutes of interest
   /// the attributes found are stored in ctx, the caller shall deallocate
   /// decode \b die (a Dwarf Information Element)
   static void decode_CU(Dwarf_context & ctx, const Dwarf_Die die,
                         Dwarf_Addr PC);

   /// decode a subprogram die
   static void decode_subprogram(Dwarf_context & ctx, const Dwarf_Die func);

   static Dwarf_Half get_DW_TAG(const Dwarf_Die die, const char * loc);
#define GET_TAG(die)   Dwarf::get_DW_TAG(die, LOC)

   /// return filename:line with directory stripped off
   static string get_file_and_line(Dwarf_context & ctx,
                                   const Dwarf_Line & line);

   /// decode attribute and maybe print it \b to \b out
   static ostream & decode_attribute(Dwarf_context & ctx,
                                     const Dwarf_Attribute attribute);
   /// print the tag of \b die to \b out
   static ostream & print_tag(Dwarf_context & ctx, const Dwarf_Die die);

   /// return the absolute value in attr. The value in attr is either an
   /// absolute PC or an offset from lowpc. In both cases the absolute
   /// PC is computed and returned.
   static Dwarf_Addr get_highpc(Dwarf_context & ctx, Dwarf_Attribute attr,
                                Dwarf_Addr lowpc);

   /// process a range list (and maybe print it to \b out).
   /// Return \b true if \b PC was found in one of the ranges
   static void decode_ranges(Dwarf_context & ctx, const Dwarf_Attribute attr);

   static vector<Dwarf_Die> CU_dies;
};
//-----------------------------------------------------------------------------
vector<Dwarf_Die> Dwarf::CU_dies;

Dwarf_Half
Dwarf::get_DW_TAG(Dwarf_Die die, const char * loc)
{
Dwarf_Half tag = 0;
   if (const int res = dwarf_tag(die, &tag, &error))
      {
        cerr << __FUNCTION__ << "() failed at" << loc <<  ": "
             << dwarf_errmsg(error) << endl;
        Assert(0);
      }
   return tag;
}
//-----------------------------------------------------------------------------
ostream &
Dwarf::print_tag(Dwarf_context & ctx, const Dwarf_Die die)
{
   if (!ctx.out)   return *ctx.out;   // no output desired
      
const Dwarf_Half tag = GET_TAG(die);

const char * tagname = 0;
   ctx.res = dwarf_get_TAG_name(tag, &tagname);
   Assert(ctx.res == DW_DLV_OK && tagname);

   return *ctx.out << HEX2(tag) << " - " << tagname;
}
//----------------------------------------------------------------------------
Dwarf_Addr
Dwarf::get_highpc(Dwarf_context & ctx, Dwarf_Attribute attr, Dwarf_Addr lowpc)
{
Dwarf_Half what = 0;
   ctx.res = dwarf_whatattr(attr, &what, &error);
   Assert(ctx.res == DW_DLV_OK);

   if (what == DW_FORM_addr)
      {
        Dwarf_Addr PC_high = 0;
        ctx.res = dwarf_formaddr(attr, &PC_high, &error);
        Assert(ctx.res == DW_DLV_OK);
        return PC_high;
      }

   if (what == DW_FORM_ref2)   // todo
      return lowpc;

   // supposedly offset from lowpc
   //
Dwarf_Unsigned offset = 0;
   ctx.res = dwarf_formudata(attr, &offset, &error);
   Assert(ctx.res == DW_DLV_OK);
   return lowpc + offset;
}
//-----------------------------------------------------------------------------
void
Dwarf::decode_ranges(Dwarf_context & ctx, const Dwarf_Attribute attribute)
{
Dwarf_Rnglists_Head rl_head = 0;
Dwarf_Unsigned count = 0;
Dwarf_Unsigned global_offset_of_rle_set = 0;

Dwarf_Unsigned original_off = 0;
   ctx.res = dwarf_global_formref(attribute, &original_off, &error);
   Assert(ctx.res == DW_DLV_OK);

   ctx.res = dwarf_rnglists_get_rle_head(attribute,
                                         DW_FORM_sec_offset,
                                         original_off,
                                         &rl_head, &count,
                                         &global_offset_of_rle_set,
                                         &error);
   if (ctx.res != DW_DLV_OK)
      {
        ctx.out && *ctx.out << "??? dwarf_rnglists_get_rle_head() ???" << endl;
        return;
      }

   // figure the max length of sn index
int idx_len = 1;
   for (size_t cnt = count; cnt > 9; cnt /= 10)   ++idx_len;

   ctx.out && *ctx.out << "ranges[" << count << "]:"<< endl;
   for (size_t r = 0; r < count; ++r)
       {
         ctx.out && *ctx.out << "        [" << right << setw(idx_len) << r
                    << left << "] ";
         unsigned elen = 0, eout = 0;
         Dwarf_Unsigned raw1 = 0, raw2 = 0, cooked1 = 0, cooked2 = 0;
         Dwarf_Bool unavaliable = false;
         ctx.res = dwarf_get_rnglists_entry_fields_a(rl_head, r,
                                                      &elen, &eout,
                                                      &raw1, &raw2,
                                                      &unavaliable,
                                                      &cooked1, &cooked2,
                                                      &error);
         Assert(ctx.res == DW_DLV_OK);
         if (unavaliable)
            {
              ctx.out && *ctx.out << "    not available"     << endl;
            }
         else if (raw1 == 0 && raw2 == 0)
            {
              ctx.out && *ctx.out << "    end of list"       << endl;
            }
         else
            {
              const int64_t end = raw1 + raw2;     // excluding
              if (int64_t(raw1) <= ctx.PC && ctx.PC < end)
                 {
                   ctx.found_PC_range = true;
                 }
              ctx.out && *ctx.out << "    " << HEX8(raw1)
                                  << " - " HEX8(end) << endl;
            }
       }

   dwarf_dealloc_rnglists_head(rl_head);
   ctx.out && *ctx.out << endl;
}
//-----------------------------------------------------------------------------
string
Dwarf::get_file_and_line(Dwarf_context & ctx, const Dwarf_Line & line)
{
char * path = 0;   // directory and name
   ctx.res = dwarf_linesrc(line, &path, &error);
   Assert(ctx.res == DW_DLV_OK);

const char * slash = strrchr(path, '/');
const char * filename = slash ? slash + 1 : path;

Dwarf_Unsigned line_number = 0;
   ctx.res = dwarf_lineno(line, &line_number, &error);
   Assert(ctx.res == DW_DLV_OK);

string ret(filename);
   ret += ':';
   ret += to_string(line_number);

   return ret;
}
//-----------------------------------------------------------------------------
ostream &
Dwarf::decode_attribute(Dwarf_context & ctx, const Dwarf_Attribute attribute)
{
Dwarf_Half attr_num = 0;
   ctx.res = dwarf_whatattr(attribute, &attr_num, &error);
   Assert(ctx.res == DW_DLV_OK);

   // attributes of special interest: name and PC_low
   //
   if (attr_num == DW_AT_name)
      {
        char * name;
        ctx.res = dwarf_formstring(attribute, &name, &error);
        Assert(ctx.res == DW_DLV_OK);
        ctx.attr_name = name;
      }

   if (attr_num == DW_AT_low_pc)
      {
        Dwarf_Addr addr = 0;
        ctx.res = dwarf_formaddr(attribute, &addr, &error);
        Assert(ctx.res == DW_DLV_OK);
        ctx.attr_PC_low = addr;
      }

   // attribute name
   //
const char * attr_name = 0;
   ctx.res = dwarf_get_AT_name(attr_num, &attr_name);
   Assert(ctx.res == DW_DLV_OK);
   ctx.out && *ctx.out << attr_name;

   // attribute form (data type)
   //
Dwarf_Half form = 0;
   ctx.res = dwarf_whatform(attribute, &form, &error);

Dwarf_Bool hasform = false;
   ctx.res = dwarf_hasform(attribute, form, &hasform, &error);
   Assert(ctx.res == DW_DLV_OK);

   if (hasform)
      {
          ctx.out && *ctx.out << " = ";
          switch(form)
             {
               case DW_FORM_addr:         // 0x01
                    {
                      Dwarf_Addr addr = 0;
                      ctx.res = dwarf_formaddr(attribute, &addr, &error);
                      Assert(ctx.res == DW_DLV_OK);
                      ctx.out && *ctx.out << HEX(addr);
                    }
                    break;

               case DW_FORM_data1:        // 0x0B
               case DW_FORM_data2:        // 0x0B
               case DW_FORM_data4:        // 0x0B
               case DW_FORM_data8:        // 0x0B
                    {
                      Dwarf_Unsigned data = 0;
                      ctx.res = dwarf_formudata(attribute, &data, &error);
                      Assert(ctx.res == DW_DLV_OK);
                      ctx.out && *ctx.out << data;
                    }
                    break;

               case DW_FORM_strp:       // 0x0E
               case DW_FORM_line_strp:  // 0x1F
                    {
                      char * string = 0;
                      ctx.res = dwarf_formstring(attribute, &string, &error);
                      Assert(ctx.res == DW_DLV_OK);
                      ctx.out && *ctx.out << string;
                    }
                    break;

               case DW_FORM_sec_offset:   // 0x17
                    decode_ranges(ctx, attribute);
                    break;

               default: cerr << "\n\n════ TODO: form " << HEX2(form) << endl;
                        Assert(0 && "TODO");
             }
      }

   return *ctx.out;
}
//-----------------------------------------------------------------------------
void
Dwarf::decode_subprogram(Dwarf_context & ctx, const Dwarf_Die child)
{

Dwarf_Attribute * attr_list = 0;
Dwarf_Signed attr_cnt = 0;
ctx.res = dwarf_attrlist(child, &attr_list, &attr_cnt, &error);
Assert(ctx.res == DW_DLV_OK);

bool name_is_main  = false;
bool got_PC_low    = false;
Dwarf_Addr PC_low  = 0;
Dwarf_Addr PC_high = 0;
const char * sub_name = "NO-NAME";

   for (Dwarf_Signed a = 0; a < attr_cnt; ++a)
       {
         const Dwarf_Attribute & attr = attr_list[a];
         Dwarf_Half attr_num = 0;
         ctx.res = dwarf_whatattr(attr, &attr_num, &error);
         Assert(ctx.res == DW_DLV_OK);

         if (attr_num == DW_AT_name)
            {
              char * name = 0;
              ctx.res = dwarf_formstring(attr, &name, &error);
              Assert(ctx.res == DW_DLV_OK);
              sub_name = name;
              name_is_main = !strcmp(name, "main");
            }
         else if (attr_num == DW_AT_low_pc)
            {
              got_PC_low = true;
              ctx.res = dwarf_formaddr(attr, &PC_low, &error);
              Assert(ctx.res == DW_DLV_OK);
              if (name_is_main &&   // function is main(), and
                  ctx.PC == 0)      // we are searching for it
                 {
                   ctx.out && *ctx.out << "    MAIN: PC-low is: "
                                       << HEX(PC_low) << endl;
                   ctx.found = true;
                   ctx.address_of_main = PC_low;
                   return;
                 }
            }
         else if (attr_num == DW_AT_high_pc)
            {
              if (!got_PC_low)
                 {
                   cerr << "\n\n***SUBPROGRAM with "
                          "high PC but no low PC" << endl;
                   return;
                 }
              PC_high = Dwarf::get_highpc(ctx, attr, PC_low);
              if (PC_high == 0)   continue;   // DW_FORM_ref2

              // got a range...
              //
              if (ctx.PC                    &&   // search a range
                  ctx.PC >= int64_t(PC_low) &&
                  ctx.PC < int64_t(PC_high))
                 {
                   cerr << "found PC in DW_TAG_subprogram die "
                        << sub_name << endl;
                   ctx.found = true;
                   ctx.attr_PC_low = PC_low;
                   return;
                 }
            }
       }
    //          print_tag(ctx, child) << endl;
}
//-----------------------------------------------------------------------------
void
Dwarf::decode_CU(Dwarf_context & ctx, const Dwarf_Die CU_die, Dwarf_Addr PC)
{
Dwarf_Half tag;
   ctx.res = dwarf_tag(CU_die, &tag, &error);
   Assert(ctx.res == DW_DLV_OK);
   Assert(tag == DW_TAG_compile_unit);

   // figure the name of the CU
   //
Dwarf_Attribute * attr_list = 0;
Dwarf_Signed attr_count = 0;
   ctx.res = dwarf_attrlist(CU_die, &attr_list, &attr_count, &error);
   Assert(ctx.res == DW_DLV_OK);

const char * CU_name = Dwarf::get_die_name(ctx, CU_die);
   cerr << "search PC=" << HEX(PC) << " in " << CU_name << endl;

   if (PC)
      {
        // read the line numbers table in CU
        //
        Dwarf_Line * line_table = 0;     // start of table
        Dwarf_Signed line_count;   // number of entries

        ctx.res = dwarf_srclines(CU_die, &line_table, &line_count, &error);
//      ctx.out && *ctx.out << "The CU '" << CU_name << "' has " << line_count
//           << " source line records." << endl;

        Dwarf_Unsigned previous_line = -1;
        Dwarf_Unsigned previous_fileno = -1;
        Dwarf_Addr start_addr    = -1;
        Dwarf_Addr min_start = Dwarf_Addr(-1);
        Dwarf_Addr max_end   = Dwarf_Addr(0);
        for (Dwarf_Signed l = 0; l < line_count; ++l)
            {
              const Dwarf_Line & line_entry = line_table[l];
              Dwarf_Addr addr = 0;
              ctx.res = dwarf_lineaddr(line_entry, &addr, &error);
              Assert(ctx.res == DW_DLV_OK);

              Dwarf_Unsigned line_number = 0;
              ctx.res = dwarf_lineno(line_entry, &line_number, &error);
              Assert(ctx.res == DW_DLV_OK);

              Dwarf_Unsigned fileno = 0;
              ctx.res = dwarf_line_srcfileno(line_entry, &fileno, &error);
              Assert(ctx.res == DW_DLV_OK);

              if (line_number != previous_line ||   // new source line
                  fileno      != previous_fileno)   // new source file
                 {
                   start_addr = addr; 
                   if (min_start > start_addr)   min_start = start_addr;
                 }
              previous_line = line_number;
              previous_fileno = fileno;

              Dwarf_Bool is_end   = false;
              ctx.res = dwarf_lineendsequence(line_entry, &is_end, &error);
              Assert(ctx.res == DW_DLV_OK);
              if (is_end)
                 {
if (!strcmp(CU_name, "main.cc"))
{
   cerr << "  [" << HEX(start_addr) << "-" << HEX(addr)
        << "] l=" << (addr - start_addr) << " PC=" << HEX(PC) << "  "
        << get_file_and_line(ctx, line_entry) << endl;
}

                   if (max_end < addr)   max_end = addr;
                   if (start_addr <= PC && PC <= addr)
                      {
                        char * src_file = 0;
                        ctx.res = dwarf_linesrc(line_entry, &src_file, &error);
                        Assert(ctx.res == DW_DLV_OK);

                        const char * filename = strrchr(src_file, '/');
                        const char * last = filename ? filename + 1 : src_file;

                        cerr << "    found PC " << PC << " in range "
                             << HEX(start_addr) << "-" << HEX(addr)
                             << "  " << last <<  ":" << line_number << endl;
                        return;
                      }
                 }
            }

        // not found
        //
#if 0
        cerr << "    PC " << HEX(PC);
        if      (PC < min_start)   cerr << " is below " << HEX(min_start) << endl;
        else if (PC >= max_end)    cerr << " is above " << HEX(max_end) << endl;
        else                       cerr << " may be in " << CU_name << endl;
#endif // 0
        return;
      }   // if PC

   if (ctx.out)
      {
        *ctx.out << ctx.indent << "CU: ";
        print_tag(ctx, CU_die);
        *ctx.out << endl;
      }

   ctx.attr_name = 0;
   ctx.attr_PC_low = -1;
   loop(a, attr_count)
       {
         if (ctx.out)
            {
              *ctx.out << ctx.indent << ctx.indent;
              decode_attribute(ctx, attr_list[a]) << endl;
            }
       }

   if (ctx.out && ctx.attr_name && (ctx.attr_PC_low != -1))   // name and pc_low
      {
        // all CU dies have a low PC of 0.
        //
        *ctx.out << "\n════════ CU " << ctx.attr_name << "..." << endl;
      }

   // decode children
   //
Dwarf_Die child = 0;
   dwarf_child(CU_die, &child, &error);
   while (child)
         {
           if (GET_TAG(child) == DW_TAG_subprogram)
              {
                decode_subprogram(ctx, child);
              }

           Dwarf_Die sibling = 0;
           ctx.res = dwarf_siblingof(dbg, child, &sibling, &error);
           child = ctx.res ? 0 : sibling;
         }

   ctx.out && *ctx.out << endl;
}
//----------------------------------------------------------------------------
const char *
Dwarf::get_die_name(Dwarf_context & ctx, Dwarf_Die die)
{
const Dwarf_Attribute attr = get_die_attribute(ctx, die, DW_AT_name);
   Assert(attr);

char * name = 0;
   ctx.res = dwarf_formstring(attr, &name, &error);
   Assert(ctx.res == DW_DLV_OK);
   return name;
}
//----------------------------------------------------------------------------
const Dwarf_Attribute
Dwarf::get_die_attribute(Dwarf_context & ctx, Dwarf_Die die,
                         Dwarf_Half which_attribute)
{
Dwarf_Attribute * attr_list = 0;
Dwarf_Signed attr_cnt = 0;
int res = 0;

   res = dwarf_attrlist(die, &attr_list, &attr_cnt, &error);
   Assert(res == DW_DLV_OK);

   loop(a, attr_cnt)
      {
        const Dwarf_Attribute & attr = attr_list[a];
        Dwarf_Half attr_num = 0;
        res = dwarf_whatattr(attr, &attr_num, &error);
        Assert(res == DW_DLV_OK);
        if (attr_num == which_attribute)   return attr;
      }

   cerr << "could not find attribute " << HEX2(which_attribute) << endl;
   return 0;
}
//----------------------------------------------------------------------------
#if 0
static Dwarf_Die
find_PC_in_subprogram(Dwarf_Die subprogram_die, int64_t PC)
{
int res = DW_DLV_OK;   // = 0. Otherwise DW_DLV_NO_ENTRY=-1 or DW_DLV_ERROR=1

   // double-check tag
   {
     Dwarf_Half tag = 0;
        res = dwarf_tag(subprogram_die, &tag, &error);
        Assert(res == DW_DLV_OK);
        Assert(tag == DW_TAG_subprogram);
   }
   
char * function_name = 0;
Dwarf_Addr PC_low = 0;
Dwarf_Addr PC_high = 0;

   if (Dwarf_Attribute attr = Dwarf::get_die_attribute(subprogram_die,
                                                       DW_AT_name))
      {
        res = dwarf_formstring(attr, &function_name, &error);
        Assert(res == DW_DLV_OK);
      }

   if (Dwarf_Attribute attr = Dwarf::get_die_attribute(subprogram_die,
                                                       DW_AT_low_pc))
      {
        res = dwarf_formaddr(attr, &PC_low, &error);
        Assert(res == DW_DLV_OK);
      }
   if (Dwarf_Attribute attr = Dwarf::get_die_attribute(subprogram_die,
                                                       DW_AT_high_pc))
      {
(void)attr;
//      PC_high = get_highpc(attr, PC_low);
      }
   if (PC_high && PC_low && function_name)
      {
        strncmp(function_name, "_GLOBAL__sub_I_", 15) || (function_name += 15);
        char obuf[200] = "@@@@";
        size_t obuflen = sizeof(obuf) - 1;
        int status = 0;
        __cxxabiv1::__cxa_demangle(function_name, obuf, &obuflen, &status);
        if (status)   // __cxa_demangle failed
           {
             cerr << "    See " << function_name;
           }
         else         // __cxa_demangle was OK
           {
             if (char * cp = strchr(obuf, '('))
                {
                  cp[1] = ')';
                  cp[2] = 0;
                }
            cerr << "    See " << obuf;
           }
         cerr << " is " << HEX(PC_low) << " ["
              << HEX(PC_high - PC_low) << "]" << endl;
      }
   return 0;
}
#endif // 0
//----------------------------------------------------------------------------
int
Dwarf::init_CU_dies(Dwarf_context & ctx)
{
Dwarf_Unsigned abbrev_offset  = 0;
Dwarf_Half     address_size   = 0;
Dwarf_Half     version_stamp  = 0;
Dwarf_Half     offset_size    = 0;
Dwarf_Half     extension_size = 0;
Dwarf_Sig8     signature;
Dwarf_Unsigned typeoffset     = 0;
Dwarf_Unsigned next_cu_header = 0;
Dwarf_Half     header_cu_type = 0;
Dwarf_Bool     is_info        = true;
int            res            = DW_DLV_OK;
 
   for (;;)
       {
           Dwarf_Die no_die = 0;
           Dwarf_Die CU_die = 0;
           Dwarf_Unsigned cu_header_length = 0;
       
           memset(&signature,0, sizeof(signature));
           res = dwarf_next_cu_header_d(dbg,is_info,&cu_header_length,
               &version_stamp, &abbrev_offset,
               &address_size, &offset_size,
               &extension_size,&signature,
               &typeoffset, &next_cu_header,
               &header_cu_type, &error);
           if (res == DW_DLV_ERROR)
              {
                cerr << "dwarf_next_cu_header_d(): res=" << res << ", error-"
                     << error << endl << dwarf_errmsg(error) << endl;
                return res;
              }

           if (res == DW_DLV_NO_ENTRY)
              {
                if (is_info == true)   // first DW_DLV_NO_ENTRY
                   {
                     //  Done with .debug_info, now check for .debug_types.
                    is_info = false;
                     continue;
                   }

                return res;  // all CUs processed
              }

           /*  The CU will have a single sibling, a CU_die. 
               It is essential to call this right after
               a call to dwarf_next_cu_header_d() because
               there is no explicit connection provided to
               dwarf_siblingof_b(), which returns a DIE
               from whatever CU was last accessed by
               dwarf_next_cu_header_d()! 
               The lack of explicit connection was a
               design mistake in the API (made in 1992).
            */

           res = dwarf_siblingof_b(dbg, no_die, is_info, &CU_die, &error);
           if (res == DW_DLV_ERROR)   return res;

           Assert(res != DW_DLV_NO_ENTRY);   // supposedly impossible

           CU_dies.push_back(CU_die);
       }
}
//----------------------------------------------------------------------------
/// find the PC in the entire binary file
int
Dwarf::find_PC_in_file(Dwarf_context & ctx, int64_t PC)
{
   if (CU_dies.size() == 0)   Dwarf::init_CU_dies(ctx);

   ctx.found = false;
   for (size_t cu = 0; cu < CU_dies.size(); ++cu)
       {
          const Dwarf_Die & CU_die = CU_dies[cu];
          Dwarf::decode_CU(ctx, CU_die, PC);
 break;
 //       dwarf_dealloc_die(CU_die);
          if (ctx.found)   break;
  //////   find_PC_in_CU(CU_die, PC);
       
       }

   if (ctx.found)
      {
        if (PC)   // search for range
           {
              ctx.out && *ctx.out << endl << "found range for PC=" << HEX(PC)
                                  << ": " << ctx.found_PC_range << endl;
           }
        else      // search for main()
           {
             cerr << "found main() at dwarf address: "
                  << HEX(ctx.address_of_main) << endl;
           }
   return DW_DLV_OK;
      }
   else
      {
        return DW_DLV_NO_ENTRY;
      }
}
//----------------------------------------------------------------------------
void
Backtrace::show_dwarf(int idx, const char * s)
{
static int call_count = 0;
   if (call_count++ > 2)   return;

int64_t PC = 0;
#ifdef __APPLE__
/*
    on macOS/Darwin, string s looks like this:

    0x00000001000a93f0 _ZN9Workspace19immediate_executionEb + 68
    ││││││││││││││││││ ││││││││││││││││││││││││││││││││││││   ││
    ││││││││││││││││││ ││││││││││││││││││││││││││││││││││││   └┴─── asm_offset
    ││││││││││││││││││ └┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴──────── fun
    └┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴───────────────────────────────────────────── abs_addr
 */

   PC = strtoll(s);

#else   // not APPLE
/*
    string s looks like this:
    
     ./apl(_ZN10APL_parser9nextTokenEv+0x1dc) [0x80778dc]
           │││││││││││││││││││││││││││ │││││   │││││││││
           │││││││││││││││││││││││││││ │││││   └┴┴┴┴┴┴┴┴───── abs_addr
           │││││││││││││││││││││││││││ └┴┴┴┴───────────────── asm_offset
           └┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴─────────────────────── fun
   */
   if (const char * bracket = strrchr(s, '['))   PC = strtoll(bracket + 3, 0, 16);

#endif

Dwarf_context ctx0(0,  &cerr);
Dwarf_context ctx1(PC, &cerr);

   // On the first time through we need to find the dwarf address of
   // function main() (i.e. of the only function that is guaranteed to exist).
static int64_t main_in_dwarf = -1;
   if (main_in_dwarf == -1)   // so search for main() in the dwarf file
      {
         Dwarf::find_PC_in_file(ctx0, 0);   // called once to find main()
         if (ctx0.found)
            {
              main_in_dwarf = ctx0.address_of_main;
              cerr << "Found the address of main() in dwarf: "
                   << HEX(main_in_dwarf) << endl;
            }
         else
            {
              cerr << "\n*** Function main() not found in dwarf" << endl;
            }
      }

const int64_t main_in_program = get_main();
const int64_t adjust = main_in_program - main_in_dwarf;
const int64_t dwarf_PC = PC - adjust;

   /* the first two lines of the backtrace have an address that is
      far outside the program addresses, for example:

      0x7f6e1cd3be40 __libc_start_main           ← outside
      0x7f6e1cd3bd90                             ← outside
      0x55e6fba0d02b   main                      ← OK.

      We skip them.
    */ 
   if (dwarf_PC > 0x1000000)   return;

   cerr << "\n\nNow searching for PC = " << HEX(PC)
        << " (dwarf PC = " << HEX(dwarf_PC) << ")...";

   cerr << right
        << "\n      main() in program: " << HEX16s(main_in_program)
        << "\n    - main() in dwarf:   " << HEX16s(main_in_dwarf)
        << "\n    ─────────────────────────────────────"
        << "\n    = ∆                  " << HEX16s(adjust)
        << "\n"
        << "\n      PC in program:     " << HEX16s(PC)
        << "\n    - ∆:                 " << HEX16s(adjust)
        << "\n    ─────────────────────────────────────"
        << "\n    = PC in dwarf:       " << HEX16s(dwarf_PC)
        << left << endl << endl;
         
                 

   // now search for the PC. Before that we need to translate the PC in
   // the locaded program to an address in dwarf.
   Dwarf::find_PC_in_file(ctx1, dwarf_PC);   // now search the PC
   if (ctx1.found)
      {
        cerr << "Found the PC in dwarf" << endl;
      }
   else
      {
        cerr << "\n*** dwarf PC=" << HEX(dwarf_PC)
             << " not found in dwarf" << endl;
      }
}
//----------------------------------------------------------------------------
#else   // not HAVE_DWARF
void
Backtrace::show_dwarf(int idx, const char * s)
{
}
#endif   // HAVE_DWARF
//----------------------------------------------------------------------------
void
Backtrace::show(const char * file, int line)
{
   // CYGWIN, for example, has no execinfo.h and the functions declared there
   //
#ifndef HAVE_EXECINFO_H
   cerr << "Cannot show function call stack since execinfo.h seems not"
           " to exist on this OS (WINDOWs ?)." << endl;
   return;

#else

   main_offset_0 = 0;
   read_apl_lines_file();   // also calls set_main_offset_0()

void * buffer[200];
const int size = backtrace(buffer, sizeof(buffer)/sizeof(*buffer));

char ** strings = backtrace_symbols(buffer, size);

   cerr << endl
        << "----------------------------------------"  << endl
        << "-- Stack trace at " << file << ":" << line << endl
        << "----------------------------------------"  << endl;

   if (strings == nullptr)
      {
        cerr << "backtrace_symbols() failed. Using backtrace_symbols_fd()"
                " instead..." << endl << endl;
        // backtrace_symbols_fd(buffer, size, STDERR_FILENO);
        for (int b = size - 1; b > 0; --b)
            {
              for (int s = b + 1; s < size; ++s)   cerr << " ";
                  backtrace_symbols_fd(buffer + b, 1, STDERR_FILENO);
            }
        cerr << "========================================" << endl;
        return;
      }

   for (int i = 1; i < size - 1; ++i)   // loop over stacktrace lines
       {
         // make a copy si of strings[i] that show_item() can mess up.
         // variable i is the number passed to show_item() and counts
         // from oldest to latest, while 'strings' runs from latest to
         // oldest. We want the lines to be displayed from oldest to latest.
         //
         const char * const_si = strings[size - i - 1];
         char si[strlen(const_si) + 1];
         strcpy(si, const_si);
         si[sizeof(si) - 1] = 0;   // 0-terminate (just in case)
         show_item(i - 1, si);
       }

   cerr << "========================================" << endl;

   // then repeat with dwarf (experimental)
   //
   for (int i = 1; i < size - 1; ++i)   // loop over stacktrace lines
       {
         // make a copy si of strings[i] that show_item() can mess up
         //
         const char * si = strings[size - i - 1];
         show_dwarf(i - 1, si);
       }

   cerr << "========================================" << endl;

   // crashes at times
   free(strings);   // but not strings[x] !
   
#endif
}
//----------------------------------------------------------------------------
#ifdef HAVE_EXECINFO_H
int
Backtrace::demangle_line(char * result, size_t result_max, const char * buf)
{
std::string tmp;
   tmp.reserve(result_max + 1);
   for (const char * b = buf; *b &&  b < (buf + result_max); ++b)
       tmp += *b;
   tmp.append(0);

char * e = 0;
int status = 3;
char * p = strchr(&tmp[0], '(');
   if (p == 0)   goto error;
   else          ++p;

   e = strchr(p, '+');
   if (e == 0)   goto error;
   else *e = 0;

// cerr << "mangled fun is: " << p << endl;
   __cxxabiv1::__cxa_demangle(p, result, &result_max, &status);
   if (status)   goto error;
   return 0;

error:
   strncpy(result, buf, result_max);
   result[result_max - 1] = 0;
   return status;
}
//----------------------------------------------------------------------------
const char *
Backtrace::caller(int offset)
{
void * buffer[200];
const int size = backtrace(buffer, sizeof(buffer)/sizeof(*buffer));
char ** strings = backtrace_symbols(buffer, size);

char * demangled = static_cast<char *>(malloc(200));
   if (demangled)
       {
         *demangled = 0;
         demangle_line(demangled, 200, strings[offset]);
         return demangled;
       }
   return strings[offset];
}
//----------------------------------------------------------------------------
#endif // HAVE_EXECINFO_H
