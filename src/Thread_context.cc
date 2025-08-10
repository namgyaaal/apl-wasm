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

#include <sched.h>
#include <signal.h>

#include "Common.hh"
#include "Parallel.hh"
#include "SystemVariable.hh"
#include "Thread_context.hh"
#include "UserPreferences.hh"

CoreCount Thread_context::active_core_count = CCNT_1;   // the master

Thread_context * Thread_context::thread_contexts = 0;
CoreCount Thread_context::thread_contexts_count = CCNT_0;

Thread_context::PoolFunction * Thread_context::do_work =
    &Thread_context::PF_no_work;
volatile _Atomic_word Thread_context::busy_worker_count = 0;

//============================================================================
Thread_context::Thread_context()
   : N(CNUM_INVALID),
     thread(0),
     job_number(0),
     job_name("no-job-name"),
     blocked(false)
{
}
//----------------------------------------------------------------------------
Thread_context::~Thread_context()
{
   // PRINT_LOCKED(CERR << "\n*** DELETING Thread_context #" << N << endl)

   // if (thread && N)   pthread_kill(thread, SIGKILL);   prints "Killed"

   if (thread && N)   pthread_cancel(thread);
   thread = 0;
}
//----------------------------------------------------------------------------
void
Thread_context::init_sequential(bool logit)
{
   thread_contexts_count = CCNT_1;
   thread_contexts = new Thread_context[thread_contexts_count];
   thread_contexts[0].N = CNUM_MASTER;
}
//----------------------------------------------------------------------------
void
Thread_context::cleanup()
{
   // CERR << "Thread_context::cleanup()" << endl;
   thread_contexts_count = CCNT_0;

    delete [] thread_contexts;
   thread_contexts = 0;
}
//----------------------------------------------------------------------------
void
Thread_context::print_all(ostream & out)
{
   if (thread_contexts_count)
      {
        PRINT_LOCKED(
           out << "thread_contexts_count: " << thread_contexts_count << endl
               << "busy_worker_count:     " << busy_worker_count     << endl
               << "active_core_count:     " << active_core_count     << endl;

           loop(e, thread_contexts_count)   thread_contexts[e].print(out);
           out << endl)
      }
}
//----------------------------------------------------------------------------
void
Thread_context::print(ostream & out) const
{
const void * vpth = reinterpret_cast<const void *>(thread);

   out << "thread #"     << setw(2) << N << ":" << setw(16) << vpth
       << (blocked ? " BLKD" : " RUN ")
       << " job:"        << setw(5) << int(job_number)
       << " " << job_name << endl;
}
//----------------------------------------------------------------------------
void
Thread_context::PF_no_work(Thread_context & tctx)
{
   PRINT_LOCKED(CERR << "*** function no_work() called by thread #"
                     << tctx.get_N() << endl)
}
//----------------------------------------------------------------------------
void
Thread_context::M_lock_pool()
{
   do_work = PF_lock_unlock_pool;
   M_fork("PF_lock_unlock_pool");
}
//----------------------------------------------------------------------------
void
Thread_context::PF_lock_unlock_pool(Thread_context & tctx)
{
   Log(LOG_Parallel)
      {
        PRINT_LOCKED(CERR << "worker #" << tctx.get_N()
                          << " will now block itself on its pool_sema" << endl)
      }

   tctx.do_join = false;
   tctx.PF_join();

   tctx.blocked = true;
   sem_wait(&tctx.pool_sema);
   tctx.blocked = false;

   Log(LOG_Parallel)
      {
        PRINT_LOCKED(CERR << "thread #" << tctx.get_N()
                          << " was unblocked from pool_sema" << endl)
      }
}

//----------------------------------------------------------------------------
void Thread_context::set_active_core_count(CoreCount new_count)
{
   Log(LOG_Parallel)
      get_CERR() << "Thread_context::set_active_core_count("
                 << new_count << ")" << endl;

   active_core_count = new_count;
}
//----------------------------------------------------------------------------
// functions and variables  that are only needed #if PARALLEL_ENABLED...

#if PARALLEL_ENABLED

//============================================================================
void
Thread_context::init_entry(CoreNumber n)
{
   N = n;
   __sem_init(&pool_sema, 0, 0);
}
//----------------------------------------------------------------------------
void
Thread_context::init_parallel(CoreCount count, bool logit)
{
   delete thread_contexts;

   thread_contexts_count = count;
   thread_contexts = new Thread_context[thread_contexts_count];
   loop(c, thread_contexts_count)
       thread_contexts[c].init_entry(CoreNumber(c));
}
//----------------------------------------------------------------------------
void
Thread_context::bind_to_cpu(CPU_Number core, bool logit)
{
#if ! HAVE_AFFINITY_NP

   CPU = CPU_0;

#else

   CPU = core;

   Log(LOG_Parallel || logit)
      {
        PRINT_LOCKED(CERR << "Binding thread #" << N
                          << " to core " << core << endl;);
      }

cpu_set_t cpus;
   CPU_ZERO(&cpus);

   if (active_core_count == CCNT_1)
      {
        // there is only one core in total (which is the master).
        // restore its affinity to all cores.
        //
        loop(a, CPU_pool::get_count())
            CPU_SET(CPU_pool::get_CPU(a), &cpus);
      }
   else
      {
        CPU_SET(CPU, &cpus);
      }

const int err = pthread_setaffinity_np(thread, sizeof(cpu_set_t), &cpus);
   if (err)
      {
        cerr << "pthread_setaffinity_np() failed with error "
             << err << endl;
      }
#endif // HAVE_AFFINITY_NP
}
//----------------------------------------------------------------------------
void
Thread_context::kill_pool()
{
   loop(c, thread_contexts_count)
      {
        if (c)   pthread_kill(thread_contexts[c].thread, SIGKILL);
      }
}
//============================================================================
#endif // PARALLEL_ENABLED
