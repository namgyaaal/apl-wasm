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

#ifndef __PARALLEL_HH_DEFINED__
#define __PARALLEL_HH_DEFINED__

#include <pthread.h>
#include "Common.hh"

// set PARALLEL_ENABLED if wanted and its prerequisites are satisfied
//
#if cfg_CORE_COUNT_WANTED == 0
   //
   // parallel not wanted
   //
# undef PARALLEL_ENABLED

#elif HAVE_AFFINITY_NP
   //
   // parallel wanted and pthread_setaffinity_np() supported
   //
# define PARALLEL_ENABLED 1

#else
   //
   // parallel wanted, but pthread_setaffinity_np() and friends are missing
   //
#warning "CORE_COUNT_WANTED configured, but pthread_setaffinity_np() missing"
# undef PARALLEL_ENABLED

#endif

#ifdef PARALLEL_ENABLED

# define PRINT_LOCKED(x) \
   { sem_wait(Parallel::print_sema); x; sem_post(Parallel::print_sema); }

#else

# define PRINT_LOCKED(x) x;

#endif // PARALLEL_ENABLED

// define some atomic functions (even if the platform does not support them)
//
#ifndef PARALLEL_ENABLED
   //
   // parallel execution disabled, no need for atomicity
   //
typedef int _Atomic_word;

inline int atomic_fetch_add(volatile _Atomic_word & counter, int increment)
   { const int ret = counter;   counter += increment;   return ret; }

/// read \b counter
inline int atomic_read(volatile _Atomic_word & counter)
   { return counter; }

/// add to \b counter
inline void atomic_add(volatile _Atomic_word & counter, int increment)
   { counter += increment; }

#elif HAVE_EXT_ATOMICITY_H
#include <ext/atomicity.h>

/// atomic \b counter += \b increment, return old value
inline int atomic_fetch_add(volatile _Atomic_word & counter, int increment)
   { _GLIBCXX_READ_MEM_BARRIER;
     const int ret = __gnu_cxx::__exchange_and_add_dispatch(
                         const_cast<_Atomic_word *>(&counter), increment);
     _GLIBCXX_WRITE_MEM_BARRIER;
     return ret; }

/// atomic read \b counter
inline int atomic_read(volatile _Atomic_word & counter)
   { _GLIBCXX_READ_MEM_BARRIER;
     return __gnu_cxx::__exchange_and_add_dispatch(
                         const_cast<_Atomic_word *>(&counter), 0); }

/// atomic \b counter += \b increment
inline void atomic_add(volatile _Atomic_word & counter, int increment)
   { __gnu_cxx::__atomic_add_dispatch(
                         const_cast<_Atomic_word *>(&counter), increment);
     _GLIBCXX_WRITE_MEM_BARRIER;
   }

#elif HAVE_OSX_ATOMIC

#include <libkern/OSAtomic.h>

/// a counter for atomic operations
typedef int32_t _Atomic_word;

/// atomic \b counter += \b increment, return old value
inline int atomic_fetch_add(volatile _Atomic_word & counter, int increment)
   { return OSAtomicAdd32Barrier(increment, &counter) - increment; }

/// atomic read \b counter
inline int atomic_read(volatile _Atomic_word & counter)
   { return OSAtomicAdd32Barrier(0, &counter); }

/// atomic \b counter += \b increment
inline void atomic_add(volatile _Atomic_word & counter, int increment)
   { OSAtomicAdd32Barrier(increment, &counter); }

#elif HAVE_SOLARIS_ATOMIC

#include <atomic.h>

/// a counter for atomic operations
typedef uint32_t _Atomic_word;

/// atomic \b counter += \b increment, return old value
inline int atomic_fetch_add(volatile _Atomic_word & counter, int increment)
   { return atomic_add_32_nv(&counter, increment) - increment; }

/// atomic read \b counter
inline int atomic_read(volatile _Atomic_word & counter)
   { return atomic_add_32_nv(&counter, 0); }

/// atomic \b counter += \b increment
inline void atomic_add(volatile _Atomic_word & counter, int increment)
   { atomic_add_32(&counter, increment); }

#else

/// a counter for atomic operations
typedef int _Atomic_word;

/// atomic \b counter += \b increment, return old value
inline int atomic_fetch_add(volatile _Atomic_word & counter, int increment)
   { CERR << "\n*** something is VERY WRONG if this function is called" << endl;
      counter += increment; return counter - increment; }

/// atomic read \b counter
inline int atomic_read(volatile _Atomic_word & counter)
   { CERR << "\n*** something is VERY WRONG if this function is called" << endl;
     return counter; }

/// atomic \b counter += \b increment
inline void atomic_add(volatile _Atomic_word & counter, int increment)
   { CERR << "\n*** something is VERY WRONG if this function is called" << endl;
     counter += increment; }

#endif

#include <ostream>
#include <semaphore.h>

#include "Cell.hh"

using namespace std;

//============================================================================
/**
  Multi-core GNU APL uses a pool of threads numbered 0, 1, ... core_count()-1

  (master-) thread 0 is the interpreter (which also performs parallel work),
  while (worker-) threads 1 ... are only activated when some parallel work
  is available.

  The worker threads 1... are either working, or busy-waiting for work, or
  blocked on a semaphore:

         init
          ↓
       blocked ←→ busy-waiting ←→ working

  The transitions

      blocked ←→ busy-waiting

  occur before and after the master thread waits for terminal input or when
  the number of cores is being changed (with ⎕SYL[26;2]). The workers are
  blocked while the master thread waits for terminal input. The reason for this
  is that a busy-waiting worker is consuming power for no reason (to the extent
  that the CPU fan turns on even though there is no useful work in sight).

  The transitions

        busy-waiting ←→ working

  occurs when the execution of APL primitives, primarily scalar functions,
  suggests to execute in parallel (i.e. if the vectors involved are
  sufficiently long).

 **/
//============================================================================
/// The set of CPUs (= hyper-threads) over which the computational load is
/// being distrinuted.
class CPU_pool
{
public:
   /// constructor
   CPU_pool();

   /// initialize the pool
   static void init(bool logit);

   /// add a CPU to the pool
   static void add_CPU(CPU_Number cpu)
      { the_CPUs.push_back(cpu); }

   /// get the idx;th CPU
   static CPU_Number get_CPU(size_t idx)
      { return the_CPUs[idx]; }

   /// get the number of (active) CPUs
   static CoreCount get_count()
      { return CoreCount(the_CPUs.size()); }

   /// resize the pool
   static void resize(CoreCount new_size)
      { the_CPUs.resize(new_size); }

   /// set new active core count, return true on error
   static bool change_core_count(CoreCount new_count, bool logit);

   /// make all pool members lock on their pool_sema
   static void lock_pool(bool logit);

   /// unlock all pool members from their pool_sema
   static void unlock_pool(bool logit);

protected:
   /// the CPU numbers that can be used
   static std::basic_string<CPU_Number> the_CPUs;
};
//============================================================================
/**
  _Atomic_word class coordinating the different cores working in parallel on the
  same ravel(s)
**/
/// Parallel APL execution
class Parallel
{
public:

   // we have two approaches to locking. One is based on atomic_fetch_add(),
   // the other is based on pthread_rwlock_wrlock().
   //
#if 1   /* use atomic_fetch_add() */

   /// a lock coordinating parallel access to shared objects
   typedef _Atomic_word parallel_lock_t;
#define LOCK_INITIALIZER 0

   /// acquire \b lock
   static inline void acquire_lock(volatile parallel_lock_t & lock)
      {
        for (;;)
            {
              if (atomic_fetch_add(lock, 1) == 0)   return;   // got the lock
              atomic_add(lock, -1);   // undo the atomic_fetch_add()
            }
      }

   /// release \b lock
   static inline void release_lock(volatile parallel_lock_t & lock)
      {
        atomic_add(lock, -1);
      }

#else   /* use pthread_rwlock_wrlock() */

   /// a lock coordinating parallel access to shared objects
   typedef pthread_rwlock_t parallel_lock_t;
#define LOCK_INITIALIZER PTHREAD_RWLOCK_INITIALIZER

   static inline void acquire_lock(parallel_lock_t & lock)
     { pthread_rwlock_wrlock(&lock); }

   static inline void release_lock(parallel_lock_t & lock)
     { pthread_rwlock_unlock(&lock); }

#endif

   /// true if parallel execution is enabled
   static bool run_parallel;

   /// initialize
   static void init(bool logit);

   /// a semaphore to protect printing from different threads
   static sem_t * print_sema;

   /// a semaphore to tell when a thread has started
   static sem_t * pthread_create_sema;

   /// return the core number for \b idx
protected:
   /// the main() function of the worker threads
   static void * worker_main(void *);

   /// initialize \b all_CPUs (which then determines the max. core count)
   static void init_all_CPUs(bool logit);

   /// true after init() has been called
   static bool init_done;
};
//============================================================================

#endif // __PARALLEL_HH_DEFINED__
