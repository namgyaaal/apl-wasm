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

#ifndef __COMMON_HH_DEFINED__
#define __COMMON_HH_DEFINED__

// NOTE: the path to config.h is set as -I $(pwd) in ./configure, so we do not
// need ../config.h
//
#include "config.h"   // for xxx_WANTED and other macros from ./configure

/// stringify x
#define STR(x) #x

// #include some notoriously needed include files, but only if they
// exist on the platform

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_DIRENT_H
# include <dirent.h>   // typedefs DIR as struct __dirstream
#else
  typedef struct __dirstream DIR;
#endif

#include <string.h>

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>   // for gettimeofday()

#ifndef RLIM_INFINITY   // Raspberry
#define RLIM_INFINITY (~rlim_t(0))
#endif

#ifndef cfg_MAX_RANK_WANTED
# define cfg_MAX_RANK_WANTED 8
#endif

enum { MAX_RANK = cfg_MAX_RANK_WANTED };

// if someone (like curses on Solaris) has #defined erase() then
// #undef it because class vector<> would complain about it
#ifdef erase
#undef erase
#endif

#include <complex>
#include <iostream>
#include <iomanip>
#include <fstream>

#include "APL_types.hh"
#include "Assert.hh"
#include "Logging.hh"
#include "SystemLimits.hh"

using namespace std;

/// true when a WINCH (window size changed) signal was received
extern bool got_WINCH;

/// libapl_version > 0, or 0 if not using libapl
extern const int libapl_version;

/// return the address of main(int argc, char * argv[]) if preset (apl binary)
/// or 0 (for shared libraries) as to avoid that the main symbol is undefined.
extern int64_t get_main();

/// interpreter capabilities (for ]NEXTFILE)
#define  apl_CAPABILITIES  "⎕FFT", "GTK",  "GUI", "⎕PNG", "POSTGRES", "⎕PLOT", \
 "⎕RE", "⎕SQL", "SQLITE3", "X11",  "XCB",

/// true when gtk_init() was called (from ⎕PLOT or from ⎕PNG)
extern bool gtk_init_done;

/// initialize
extern void init_1(const char * argv0, bool log_startup);
extern void init_2(bool log_startup);

/// clean up
extern void cleanup(bool soft);

/// true if Control-C was hit (once)
extern bool attention_is_raised();

/// set the attention_raised flag
extern void set_attention_raised(const char * loc);

/// clear the attention_raised flag
extern void clear_attention_raised(const char * loc);

/// true if Control-C was hit twice within 500 ms
extern bool interrupt_is_raised();

/// set the interrupt_raised flag
extern void set_interrupt_raised(const char * loc);

/// clear the interrupt_raised flag
extern void clear_interrupt_raised(const char * loc);

/// the number of Control-C signals received
extern uint64_t interrupt_count;

/// time when ^C was hit last
extern APL_time_us interrupt_when;

/// signal handler for ^C
extern void control_C(int);

/// normal APL output (to stdout)
extern ostream COUT;

/// debug output (to stderrear_interrupt_raised
extern ostream CERR;

/// debug output (to stderr)
extern ostream UERR;

class UCS_string;
extern UCS_string & MORE_ERROR();   // in Workspace.cc; clears MORE info.

/// loop from (including) 0 up to (excluding) e
#define loop(v, e)       for (ShapeItem v = 0, __end__ = e; v < __end__; ++v)

/// loop from (excluding) e down to (including) 0
#define rev_loop(v, e)   for (ShapeItem v = e; --v >= 0;)

// #define TROUBLESHOOT_NEW_DELETE

void * common_new(size_t size);
void common_delete(void * p);

/// current time as microseconds since epoch
APL_time_us now();

/// CPU cycle counter (if present)
#if HAVE_RDTSC
inline uint64_t cycle_counter()
{
uint32_t lo, hi;
   __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
   return uint64_t(hi) << 32 | lo;
}

#elif HAVE_GETTIMEOFDAY

inline uint64_t cycle_counter()
{
timeval tv;
   gettimeofday(&tv, 0);
   return tv.tv_sec * 1000000ULL + tv.tv_usec;
}
#else // neither HAVE_RDTSC nor HAVE_GETTIMEOFDAY
#define cycle_counter() 0
#endif // HAVE_RDTSC

//----------------------------------------------------------------------------
#if HAVE_SEM_INIT

# define __sem_destroy(sem) sem_destroy(sem)
# define __sem_init(sem, pshared, value) sem_init(sem, pshared, value)

#else // not HAVE_SEM_INIT

# define __sem_destroy(sem) { if (sem) sem_close(sem); }
# define __sem_init(sem, _pshared, value)                   \
{                                                          \
char sname[100];                                           \
   { /* create a unique name */                            \
     timeval tv;   gettimeofday(&tv, 0);                   \
     unsigned int secs = tv.tv_sec, usecs = tv.tv_usec;    \
     snprintf(sname, sizeof(sname), "/gnu_apl_%s_%u_%u",   \
              #sem, secs, usecs);                          \
   }                                                       \
   enum { mode = S_IRWXU | S_IRWXG | S_IRWXO };            \
   sem = sem_open(sname, O_CREAT, mode, value);            \
}
#endif // HAVE_SEM_INIT
//----------------------------------------------------------------------------
/**
  Software probes. A probe is a measurement of CPU cycles executed between two
  points P1 and P2 in the source code.
 */
/// CPU cycle counter for performance measurements
class Probe
{
public:
   /// some Proble related parameters
   enum { PROBE_COUNT = 100,   ///< the number of probes
          PROBE_LEN   = 20     ///< the number of measurements in each probe
        };

   /// constructor
   Probe()
      { init(); }

   /// initialize this probe
   void init()
      {
        idx = 0;
        start_p = &dummy;
        stop_p  = &dummy;
      }

   /// init the p'th probe
   static int init(int p)
      { if (p >= PROBE_COUNT)   return -3;
        probes[p].init();
        return 0;
      }

   /// initialize all probes
   static void init_all()
      { loop(p, PROBE_COUNT)   init(p); }

   /// start time measurement
   void start()
      {
         if (idx < PROBE_LEN)
            {
               measurement & m = measurements[idx];
               start_p = &m.cycles_from;
               stop_p =  &m.cycles_to;
            }
         else
            {
               start_p = &dummy;
               stop_p  = &dummy;
            }

         // write to *start_p and *stop_p so that they are loaded into the cache
         //
         *stop_p = cycle_counter();
         *start_p = cycle_counter();

         // now the real start of the measurment
         //
         *start_p = cycle_counter();
      }

   /// stop time measurement
   void stop()
      {
        // the real end of the measurment
        //
        *stop_p = cycle_counter();
        ++idx;
      }

   /// get the m'th time (from P1 to P2) of this probe
   int64_t get_time(int m) const
      { if (m >= idx)   return -1;
        const int64_t diff = measurements[m].cycles_to
                           - measurements[m].cycles_from;
        if (diff < 0)   return -2;
        return diff;
      }

   /// get the m'th time of the p'th probe
   static int get_time(int p, int m)
      { if (p >= PROBE_COUNT)   return -3;
        return probes[p].get_time(m);
      }

   /// get the m'th start time of this probe
   int64_t get_start(int m) const
      { if (m >= idx)   return -1;
        return measurements[m].cycles_from;
      }

   /// get the m'th start time of the p'th probe
   static int get_start(int p, int m)
      { if (p >= PROBE_COUNT)   return -3;
        return probes[p].get_start(m);
      }

   /// get the m'th stop time of this probe
   int64_t get_stop(int m) const
      { if (m >= idx)   return -1;
        return measurements[m].cycles_to;
      }

   /// get the m'th stop time of the p'th probe
   static int get_stop(int p, int m)
      { if (p >= PROBE_COUNT)   return -3;
        return probes[p].get_stop(m);
      }

   /// get the number of times int the p'th probe
   static int get_length(int p)
      { if (p >= PROBE_COUNT)   return -3;
        return probes[p].idx;
      }

   static Probe & P0;    ///< start of vector probes
   static Probe & P_1;   ///< individual probe 1
   static Probe & P_2;   ///< individual probe 2
   static Probe & P_3;   ///< individual probe 3
   static Probe & P_4;   ///< individual probe 4
   static Probe & P_5;   ///< individual probe 5

protected:
   /// one time measurement, measuring cCPU cycles spent between
   /// ttw points in the source code
   struct measurement
      {
        int64_t cycles_from;   ///< the cycle counter at point 1
        int64_t cycles_to;     ///< the cycle counter at point 2
      };

   /// all measurements
   measurement measurements[PROBE_LEN];

   /// index into \b probes
   int idx;

   /// start time
   int64_t * start_p;

   /// end time
   int64_t * stop_p;

   /// a dummy used when \b idx exceeds \b probes
   static int64_t dummy;

   /// all probes
   static Probe probes[];
};
//----------------------------------------------------------------------------
/// Year, Month, Day, hour, minute, second, μsecond
struct YMDhmsu
{
   /// construct YMDhmsu from usec since Jan. 1. 1970 00:00:00
   YMDhmsu(APL_time_us t);

   /// return usec since Jan. 1. 1970 00:00:00
   APL_time_us get() const;

   int year;     ///< year, e.g. 2013
   int month;    ///< month 1-12
   int day;      ///< day 1-31
   int hour;     ///< hour 0-23
   int minute;   ///< minute 0-59
   int second;   ///< second 0-59
   int micro;    ///< microseconds 0-999999
};
//----------------------------------------------------------------------------
/// whether ⎕LX shall be executed at the end of the file
enum LX_mode
{
   no_LX = 0,     ///< no
   do_LX = 1      ///< yes
};
//----------------------------------------------------------------------------

#ifdef TROUBLESHOOT_NEW_DELETE
inline void * operator new(size_t size)   { return common_new(size); }
inline void   operator delete(void * p)   { common_delete(p); }
#endif

using namespace std;

//----------------------------------------------------------------------------

/// return true iff \b uni is a padding character (used internally).
inline bool is_iPAD_char(Unicode uni)
{
   return ((uni >= UNI_iPAD_U2) && (uni <= UNI_iPAD_U1))    // ² ³ ¹
       || ((uni >= UNI_iPAD_U0) && (uni <= UNI_iPAD_L9));   // ⁰ ⁴..⁹ ₀..₉
}
//----------------------------------------------------------------------------

extern std::ostream & get_CERR();

/// The current location in the source file.
#define LOC Loc(__FILE__, __LINE__)
/// The location line l in file f.
#define Loc(f, l) f ":" STR(l)

/// print x and its source code location
#define Q(x) get_CERR() << std::left << setw(20) << #x ":" \
                        << " '" << x << "' at " LOC << endl;

/// same as Q1 (for printouts guarded by Log macros). Unlike Q () which MUST
/// NOT REMAIN IN THE CODE, Q1() SHOULD remain in the code.
#define Q1(x) get_CERR() << std::left << setw(20) << #x ":" \
                         << " '" << x << "' at " LOC << endl;

//----------------------------------------------------------------------------

#ifdef cfg_VALUE_HISTORY_WANTED

   enum { VALUEHISTORY_SIZE = 100000 };
   extern void add_event(const Value * val, VH_event ev, int ia,
                        const char * loc);
#  define ADD_EVENT(val, ev, ia, loc)   add_event(val, ev, ia, loc);

#else

   enum { VALUEHISTORY_SIZE = 0 };
#  define ADD_EVENT(_val, _ev, _ia, _loc)

#endif

//============================================================================
/// Function_Line ++ (post increment)
inline Function_Line operator ++(Function_Line & fl, int)
{
const Function_Line before_increment = fl;
   fl = Function_Line(fl + 1);
   return before_increment;
}
//============================================================================
inline void skip_spaces(const char * & p)
{
   while (*p && *p <= ' ')   ++p;
}
//============================================================================
inline Function_PC
operator +(Function_PC pc, int offset)
{
   return Function_PC(int(pc) + offset);
}
//----------------------------------------------------------------------------
inline Function_PC
operator -(Function_PC pc, int offset)
{
   return Function_PC(int(pc) - offset);
}
//----------------------------------------------------------------------------
/// Function_PC ++ (post increment)
inline Function_PC
operator ++(Function_PC & pc, int)
{
const Function_PC before_increment = pc; 
   pc = pc + 1;
   return before_increment;
}
//----------------------------------------------------------------------------
/// Function_PC ++ (pre increment)
inline Function_PC &
operator ++(Function_PC & pc)
{
   pc = pc + 1;
   return pc;
}
//----------------------------------------------------------------------------
/// Function_PC -- (pre decrement)
inline Function_PC &
operator --(Function_PC & pc)
{
   pc = pc - 1;
   return pc;
}
//----------------------------------------------------------------------------
/// frequently used cast to const char *
inline const char *
charP(const void * vp)
{
  return reinterpret_cast<const char *>(vp);
}
//----------------------------------------------------------------------------

#define uhex  std::hex << uppercase << setfill('0')
#define uhexs  std::hex << uppercase
#define lhex  std::hex << nouppercase << setfill('0')
#define nohex std::dec << nouppercase << setfill(' ')

/// formatting for hex (and similar) values
#define HEX(x)     "0x" << uhex <<             int64_t(x) << nohex
#define HEX2(x)    "0x" << uhex << std::right << \
                           setw(2) << int(x) << std::left << nohex
#define HEX4(x)    "0x" << uhex << std::right << \
                           setw(4) << int(x) << std::left << nohex
#define HEX8(x)    "0x" << uhex << std::right << \
                           setw(8) << int32_t(x) << std::left << nohex
#define HEX16(x)   "0x" << uhex << std::right << \
                           setw(16) << int64_t(x) << std::left << nohex
#define HEX16s(x)          uhexs << std::right << \
                           setw(16) << int64_t(x) << std::left << nohex
#define UNI(x)     "U+" << uhex <<      setw(4) << int(x) << nohex

/// cast to a const void *
inline const void * voidP(const void * addr) { return addr; }

/// set the last byte in buffer to 0 (so that string functions won't fail
/// even if the  buffer was not 0-terminated for some reason.
#define NULL_TERMINATE(buffer)   { buffer[sizeof(buffer) - 1] = 0; }

#define SPRINTF(buffer, ...) \
{ snprintf(buffer, sizeof(buffer), __VA_ARGS__); NULL_TERMINATE(buffer) }

#endif // __COMMON_HH_DEFINED__
