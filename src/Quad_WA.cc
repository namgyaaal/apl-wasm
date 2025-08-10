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

#include <sys/resource.h>
#include "UserPreferences.hh"
#include "Quad_WA.hh"

extern uint64_t top_of_memory();

rlim_t Quad_WA::initial_rlimit = RLIM_INFINITY;
uint64_t Quad_WA::total_memory = 0x40000000;   // a little more than 1 Gig
int64_t  Quad_WA::WA_margin = 0;  // 100000000;
int      Quad_WA::WA_scale = 90;   // percent
unsigned long long Quad_WA::initial_sbrk = 0;

Quad_WA::_mem_info Quad_WA::meminfo;

//============================================================================
Quad_WA::Quad_WA()
   : RO_SystemVariable(ID_Quad_WA)
{
   Symbol::assign(IntScalar(0, LOC), false, LOC);
}
//----------------------------------------------------------------------------
Value_P
Quad_WA::get_apl_value() const
{
   return IntScalar(total_memory -
                    (Value::total_ravel_count * sizeof(Cell)
                    + Value::value_count * sizeof(Value)), LOC);
}
//----------------------------------------------------------------------------
void
Quad_WA::init(bool log_startup)
{
   initial_sbrk = top_of_memory();

rlimit rl;

#ifndef RLIMIT_AS // BSD does not define RLIMIT_AS
# define RLIMIT_AS RLIMIT_DATA
#endif

   rl.rlim_cur = 0;   // suppress warning if getrlimit() is faked
   getrlimit(RLIMIT_AS, &rl);
   initial_rlimit = rl.rlim_cur;

   if (log_startup)
      {
        CERR << "initial RLIMIT_AS (aka. virtual memory) is: ";
        if (initial_rlimit == ~rlim_t(0))   CERR << "'unlimited'" << endl;
        else CERR << initial_rlimit << endl;
      }

   // if the user has set a memory rlimit (and hopefully knowing what she is
   // doing) then leave it as is; otherwise set the limit to 80 % of the
   // avaiable memory
   //
   total_memory = get_free_memory();
   if (log_startup)
      CERR << "estimated available memory: " << total_memory
           << " bytes (" << (total_memory/1000000) << " MB)" << endl;

   if (rl.rlim_cur == RLIM_INFINITY)
      {
        // the user has not set a memory limit, so we may do that. However,
        // limits > 2GB are treated as RLIM_INFINITY, so setting larger
        // limits may have undesirable effects
        //
        if (total_memory < 0x8000000)
           {
             rl.rlim_cur = total_memory;
             setrlimit(rl.rlim_cur, &rl);
             getrlimit(RLIMIT_AS, &rl);
             if (log_startup)
                CERR << "decreasing RLIMIT_AS to: " << rl.rlim_cur
                     << " bytes (" << int64_t(rl.rlim_cur/1000000)
                     << " MB)" << endl;
           }
      }
   else if (uint64_t(rl.rlim_cur) > total_memory)
      {
        // the user has set a memory limit, but it is above the available
        // memory
        //
        CERR <<
"*** Warning: the process memory limit (RLIMIT_AS) of " << rl.rlim_cur << endl
<< " is more than the estimated total_memory of " << total_memory << "." << endl
<< " This could cause improper WS FULL handling." << endl;
      }
}
//----------------------------------------------------------------------------
int
Quad_WA::read_meminfo()
{
int ret = 0;

   meminfo.Available = 0;
   meminfo.Cached    = 0;
   meminfo.MemFree   = 0;

FILE * pm = fopen("/proc/meminfo", "r");
   if (pm == 0)   return 0;   // error

const struct _tag
   {
     const char * name;
     uint64_t   & value;
   } tags[] = { { "MemAvailable:", meminfo.Available },
                { "Cached:"      , meminfo.Cached    },
                { "MemFree:"     , meminfo.MemFree   }
              };

   for (;;)   // read all lines of /proc.meminfo
       {
         char buffer[2000];
         if (fgets(buffer, sizeof(buffer) - 1, pm) == 0)   break;
         buffer[sizeof(buffer) - 1] = 0;   // to allow strncmp()

         for (size_t t = 0; t < sizeof(tags) / sizeof(*tags); ++t)
             {
               const _tag & tag = tags[t];
               const size_t name_len = strlen(tag.name);
               if (!strncmp(buffer, tag.name, name_len))
                  {
                    // proc/meminfo valuesa re in kbytes, so we * 1024
                    tag.value =  1024 * strtoll(buffer + name_len, 0, 10);
                    ++ret;
                  }
             }
       }
   fclose(pm);

   return ret;   // success
}
//----------------------------------------------------------------------------
int64_t
Quad_WA::read_procfile(const char * filename)
{
   if (FILE * pm = fopen(filename, "r"))
      {
        long long ret = -1;
        const int count = fscanf(pm, "%lld", &ret);
        fclose(pm);
        if (count == 1) return ret;
      }

   return -1;   // something went wrong
}
//----------------------------------------------------------------------------
uint64_t
Quad_WA::get_free_memory()
{
uint64_t result = 0xC0000000;   // assume ~3 GB on error

   if (read_meminfo() == 3)
      {
        result = meminfo.Available ?  meminfo.Available
                            : meminfo.Cached + meminfo.MemFree;
      }

   // limit size on 32-bit machines to 2.95 < 3 GB
   if (sizeof(const void *) == 4 && result > 0xB0000000)  result = 0xB0000000;

   return result;
}
//----------------------------------------------------------------------------
void
Quad_WA::parse_mem(bool log_startup)
{
const char * mem_arg = UserPreferences::uprefs.mem_arg;

   if (mem_arg == 0)   // no --mem option given
      {
        if (log_startup)
           CERR << "--mem option not used" << endl;
        return;
     }

   if (*mem_arg == 0)   // -mem option without rgument
      {
        if (log_startup)
           CERR << "using --mem with default value: 50%" << endl;

        mem_arg = "50%";
      }
   else
      {
        if (log_startup)
           CERR << "using --mem with user's value " << mem_arg << endl;
      }

   if (3 != read_meminfo())
      {
        CERR << "problem with the --mem option: "
                "no readable /proc/meminfo: " << strerror(errno) << endl;
        exit(3);
      }

char mem_unit = mem_arg[strlen(mem_arg) -1];
   if (mem_unit == 'b' || mem_unit == 'B')   // optional trailing B
      mem_unit = mem_arg[strlen(mem_arg) -2];

uint64_t mem_number = strtoll(mem_arg, 0, 0);
uint64_t mem_val = 0;
   if (mem_unit == '%')
      {
        if (mem_number < 5)
           {
             CERR << "*** FATAL ERROR: the --mem value '" << mem_arg
                  << "' is too small (minimum is: 5%)" << endl;
             exit(3);
           }

        if (mem_number > 95)
           {
             CERR << "*** FATAL ERROR: the --mem value '" << mem_arg
                  << "' is too large (maximum is 95%)" << endl;
             exit(3);
           }

        mem_val = (meminfo.MemFree / 100) * mem_number;
      }
   else if (mem_unit == 'k')
      {
        mem_val = mem_number*1000;
        if (mem_val < 100000000)
           {
             CERR << "*** FATAL ERROR: the --mem value '" << mem_arg
                  << "' is too small (minimum is: 100,000k)" << endl;
             exit(3);
           }

        if (mem_number*1000 > meminfo.MemFree)   // more than we have
           {
             CERR << "*** FATAL ERROR: the --mem value '" << mem_arg
                  << "' is too large (max. is MemFree = "
                  << meminfo.MemFree/1000 << " kB)" << endl;
             exit(3);
           }

      }
   else if (mem_unit == 'M')
      {
        mem_val = mem_number*1000000;
        if (mem_val < 100000000)
           {
             CERR << "*** FATAL ERROR: the --mem value '" << mem_arg
                  << "' is too small (minimum is: 100M)" << endl;
             exit(3);
           }

        if (mem_number*1000*1000 > meminfo.MemFree)   // more than we have
           {
             CERR << "*** FATAL ERROR: the --mem value '" << mem_arg
                  << "' is too large (max. is MemFree = "
                  << meminfo.MemFree/1000/1000 << " MB)" << endl;
             exit(3);
           }
      }
   else if (mem_unit == 'G')
      {
        mem_val = mem_number*1000000000;
        if (mem_val < 100000000)
           {
             CERR << "*** FATAL ERROR: the --mem value '" << mem_arg
                  << "' is too small (minimum is: 100M)" << endl;
             exit(3);
           }

        if (mem_number*1000*1000*1000 > meminfo.MemFree)   // more than we have
           {
             CERR << "*** FATAL ERROR: the --mem value '" << mem_arg
                  << "' is too large (max. is MemFree = "
                  << meminfo.MemFree/1000/1000/1000 << " GB)" << endl;
             exit(3);
           }
      }
   else
      {
        CERR << "*** FATAL ERROR: invalid unit in --mem value '" << mem_arg
             << "' (not k, M, G, kB, MB, or GB)" << endl;
        exit(3);
      }

   // first check if --mem can be used at all
   //
const int64_t overcommit = read_procfile("/proc/sys/vm/overcommit_memory");
   if (overcommit == -1)
      {
        CERR << "*** FATAL ERROR: --mem option used, "
                "but /proc/sys/vm/overcommit_memory is not readable" << endl;
         exit(3);
      }

   if (overcommit != 2)
      {
        CERR << "*** FATAL ERROR: the --mem option used with "
                "memory overcommit enabled." << endl
             <<   "    I.e. /proc/sys/vm/overcommit_memory says '" << overcommit
             <<"' while --mem requires '2'" << endl;
         exit(3);
      }

   CERR << "!!! the estimated total_memory of " << total_memory/1000 << "kB"
           " = " << total_memory/1000000 << "MB"
           " = " << total_memory/1000000000 << "GB" << endl
        << "!!!  was overridden with " << mem_val/1000 << "kB"
           " = " << mem_val/1000000 << "MB"
           " = " << mem_val/1000000000 << "GB"
               " by the --mem option !!!" << endl;

   total_memory = mem_val;
}
//============================================================================

