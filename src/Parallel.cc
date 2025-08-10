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

#if !PARALLEL_ENABLED
# define sem_init(x, y, z)   /* NO-OP */
#endif // PARALLEL_ENABLED

const char * Parallel_job_list_base::started_loc = 0;

#if cfg_CORE_COUNT_WANTED == 0
bool Parallel::run_parallel = false;
#else   // cfg_CORE_COUNT_WANTED != 0
bool Parallel::run_parallel = true;
#endif   // cfg_CORE_COUNT_WANTED == 0

bool Parallel::init_done = false;

//============================================================================
void
Parallel::init(bool logit)
{
   // supposedly called only once...
   //
   Assert(!init_done);
   init_done = true;

#if PARALLEL_ENABLED

   // init global semaphores
   //
   __sem_init(print_sema,          /* shared */ 0, /* value */ 1);
   __sem_init(pthread_create_sema, /* shared */ 0, /* value */ 0);

   // compute number of cores available and set total_CPU_count accordingly
   //
   CPU_pool::init(logit);

   // initialize thread contexts
   //
   Thread_context::init_parallel(CPU_pool::get_count(), logit);

   // create threads...
   //
   Thread_context::get_context(CNUM_MASTER)->thread = pthread_self();
   for (int w = CNUM_WORKER1; w < CPU_pool::get_count(); ++w)
       {
         Thread_context * tctx = Thread_context::get_context(CoreNumber(w));
         const int result = pthread_create(&(tctx->thread), /* attr */ 0,
                                             worker_main, tctx);
         if (result)
            {
              CERR << "pthread_create() failed at " << LOC
                   << " : " << strerror(result) << endl;
              Thread_context::set_active_core_count(CCNT_1);
              return;
            }

         // pthread_setname_np() fails for names ≥ 16 chars (including \0).
         char worker_name[40];   // max 16 chars!
         SPRINTF(worker_name, "apl/pool-%d", w);
         pthread_setname_np(tctx->thread, worker_name);
         // wait until new thread has reached its work loop
         sem_wait(pthread_create_sema);
       }

   // bind threads to cores
   //
   loop(c, CPU_pool::get_count())
       {
         const CPU_Number cpu = CPU_pool::get_CPU(c);
         Thread_context::get_context(CoreNumber(c))->bind_to_cpu(cpu, logit);
       }

   if (logit)   Thread_context::print_all(CERR);

   // the threads above start in state locked. Wake them up...
   //
# if cfg_CORE_COUNT_WANTED == -3
   CPU_pool::change_core_count(CCNT_1, logit);
# else
   CPU_pool::change_core_count(CPU_pool::get_count(), logit);
# endif

#else // not PARALLEL_ENABLED

   Thread_context::init_sequential(logit);
   CPU_pool::add_CPU(CPU_0);

#endif // PARALLEL_ENABLED
}

sem_t __print_sema;
sem_t __pthread_create_sema;
sem_t * Parallel::print_sema          = &__print_sema;
sem_t * Parallel::pthread_create_sema = &__pthread_create_sema;

//============================================================================
bool
CPU_pool::change_core_count(CoreCount new_count, bool logit)
{
   // this function is called from ⎕SYL[26]←new_countm but also from
   // other places. It returns true on error.
   //
   Log(LOG_Parallel || logit)
      get_CERR() << "change_core_count(" << new_count << ")" << endl;

   if (new_count < CCNT_0)                  return true;   // error
   if (new_count > CPU_pool::get_count())   return true;   // error

   Parallel::run_parallel = new_count != CCNT_0;
   if (new_count == CCNT_0)   new_count = CCNT_1;

const CoreCount current_count = Thread_context::get_active_core_count();
   if (new_count == current_count)
      {
        Log(LOG_Parallel || logit)
           {
             CERR <<
                "Parallel::change_core_count(): keeping current core count of "
                  << Thread_context::get_active_core_count() << endl;
             Thread_context::print_all(CERR);
           }
      }
   else if (new_count > current_count)
      {
        Log(LOG_Parallel || logit)
           CERR << "Parallel::change_core_count(): increasing core count from "
                << Thread_context::get_active_core_count()
                << " to " << new_count << endl;

        lock_pool(logit);
        Thread_context::set_active_core_count(new_count);
        for (int c = 1; c < new_count; ++c)
            Thread_context::get_context(CoreNumber(c))->job_number =
                            Thread_context::get_master().job_number;
        unlock_pool(logit);

        Log(LOG_Parallel || logit)   Thread_context::print_all(CERR);
      }
   else   // new_count < current_count
      {
        Log(LOG_Parallel || logit)
           CERR << "Parallel::change_core_count(): decreasing core count from "
                << Thread_context::get_active_core_count()
                << " to " << new_count << endl;
        lock_pool(logit);
        Thread_context::set_active_core_count(new_count);
        unlock_pool(logit);

        Log(LOG_Parallel || logit)   Thread_context::print_all(CERR);
      }

   return false;   // no error
}
//----------------------------------------------------------------------------
#if PARALLEL_ENABLED
void
CPU_pool::init(bool logit)
{
   // determine the max number of cores. We first handle the ./configure cases
   // that can live without pthread_getaffinity_np() and friends
   //
CoreCount count = CoreCount(cfg_CORE_COUNT_WANTED);

#if cfg_CORE_COUNT_WANTED >= 1      // static parallel

   Parallel::run_parallel = true;
   loop(c, cfg_CORE_COUNT_WANTED)   add_CPU(CPU_Number(c));
   return;

#elif cfg_CORE_COUNT_WANTED == 0    // sequential

   Parallel::run_parallel = false;
   add_CPU(CPU_0);
   return;

#elif cfg_CORE_COUNT_WANTED == -1   // handled below
# if ! HAVE_AFFINITY_NP
#  error "CORE_COUNT_WANTED == -1 on platform without pthread_getaffinity_np"
# endif

#elif cfg_CORE_COUNT_WANTED == -2   // --cc N

   Parallel::run_parallel = UserPreferences::uprefs.requested_cc > 0;
   loop(c, UserPreferences::uprefs.requested_cc)   add_CPU(CPU_Number(c));
   return;

#elif cfg_CORE_COUNT_WANTED == -3   // handled below
# if ! HAVE_AFFINITY_NP
#  error "CORE_COUNT_WANTED == -3 on platform without pthread_getaffinity_np"
# endif

#else  // cfg_CORE_COUNT_WANTED == xxx

# error "Bad CORE_COUNT_WANTED value in ./configure"

#endif // cfg_CORE_COUNT_WANTED == xxx

   // at this point cfg_CORE_COUNT_WANTED is -1 (all cores) or -3 (⎕SYL)
   // Figure how many cores we have.
   //
#if ! HAVE_AFFINITY_NP

   Log(LOG_Parallel || logit)
      {
        CERR <<
        "The number of cores could not be detected because function\n"
        "pthread_getaffinity_np() is not provided by your platform.\n"
        "Assuming a maximum of 64 cores." << endl;
      }

   Parallel::run_parallel = true;
   loop(c, 64)   add_CPU(CPU_Number(c));
   return;

#endif

cpu_set_t CPUs;
   CPU_ZERO(&CPUs);

const int err = pthread_getaffinity_np(pthread_self(), sizeof(CPUs), &CPUs);
   if (err)
      {
        CERR << "pthread_getaffinity_np() failed with error "
             << err << endl;
        add_CPU(CPU_0);
        return;
      }

   {
     const CoreCount CPU_count = CoreCount(CPU_COUNT(&CPUs));
     // start with even CPUs followed by odd CPUs. This is to avoid that
     // a secondart CPU thread is used before all primary threads have been
     // exhausted.
     //
     for (size_t c = 0; c < 8*sizeof(cpu_set_t); c += 2)   //even
         {
           if (CPU_ISSET(c, &CPUs))
              {
                add_CPU(CPU_Number(c));
                if (get_count() == CPU_count)   break;   // all CPUs found
              }
         }

     for (size_t c = 1; c < 8*sizeof(cpu_set_t); c += 2)   //odd
         {
           if (CPU_ISSET(c, &CPUs))
              {
                add_CPU(CPU_Number(c));
                if (get_count() == CPU_count)   break;   // all CPUs found
              }
         }
   }

   if (get_count() == 0)
      {
        CERR << "*** no cores detected, assuming at least one! "
             << err << endl;
        add_CPU(CPU_0);
        return;
      }

   // at this point we know the max. number of CPUs and can map cases
   // -1 and -3 to the max. number of CPUs.
   //
   if (count < 0)   count = get_count();

   // if there are more CPUs than requested then limit all_CPUs accordingly
   if (get_count() > count)   resize(count);

   Log(LOG_Parallel || logit)
      {
        CERR << "detected " << get_count() << " cores:";
        loop(cc, get_count())   CERR << " #" << get_CPU(cc);
        CERR << endl;
      }
}

#else   // not PARALLEL_ENABLED
void
CPU_pool::init(bool logit)
{
   Parallel::run_parallel = false;
   add_CPU(CPU_0);
}
#endif  // not PARALLEL_ENABLED

//----------------------------------------------------------------------------
void
CPU_pool::lock_pool(bool logit)
{
   if (Thread_context::get_active_core_count() <= 1)   return;

   Thread_context::get_master().M_lock_pool();
   Thread_context::M_join();

   Log(LOG_Parallel || logit)
      {
        PRINT_LOCKED(
            CERR << "Parallel::lock_pool() : pool state is now :" << endl; )
        Thread_context::print_all(CERR);
      }
}
//----------------------------------------------------------------------------
void
CPU_pool::unlock_pool(bool logit)
{
   if (Thread_context::get_active_core_count() <= 1)   return;

   Log(LOG_Parallel || logit)
     {
        for (int a = 1; a < Thread_context::get_active_core_count(); ++a)
            {
              PRINT_LOCKED(CERR << "Parallel::unlock_pool() : " << endl; )
              Thread_context * tc = Thread_context::get_context(CoreNumber(a));
              sem_post(&tc->pool_sema);
              PRINT_LOCKED(
              CERR << "    pool_sema of thread #" << a << " incremented."
                   << endl;)
            }
     }
   else
     {
        for (int a = 1; a < Thread_context::get_active_core_count(); ++a)
            sem_post(&Thread_context::get_context(CoreNumber(a))->pool_sema);
       }
}
//----------------------------------------------------------------------------
void *
Parallel::worker_main(void * arg)
{
Thread_context & tctx = *reinterpret_cast<Thread_context *>(arg);

   Log(LOG_Parallel)
      {
        PRINT_LOCKED(CERR << "worker #" << tctx.get_N() << " started" << endl)
      }

   // tell the creator that we have started
   //
   sem_post(pthread_create_sema);

   // start in state locked
   //
   tctx.blocked = true;
   sem_wait(&tctx.pool_sema);
   tctx.blocked = false;

   Log(LOG_Parallel)
      PRINT_LOCKED(CERR << "thread #" << tctx.get_N()
                        << " was unblocked (initially) from pool_sema" << endl)

   for (;;)
       {
         tctx.PF_fork();
         tctx.job_name = Thread_context::get_master().job_name;
         tctx.do_join = true;
         Thread_context::do_work(tctx);
         if (tctx.do_join)   tctx.PF_join();
       }

   /* not reached */
   return 0;
}
//============================================================================
