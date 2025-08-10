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

#ifndef PJOB_HH_DEFINED
#define PJOB_HH_DEFINED

class PrimitiveFunction;

#include "Assert.hh"
#include "ConstCell_P.hh"
#include "PrimitiveFunction.hh"

/**
 A PJob is a computation that can be carried out in parallel on different
 cores. The PJob_XXX structs defined in this file contain the essential
 information needed to do the job.

 A Job is typically implemented by iterating some Cell function along the
 ravels of the result Z, the right argument B, and possibly (for dyadic Cell
 functions) the left argument A.
 **/
//----------------------------------------------------------------------------
/// one monadic scalar job
class PJob_scalar_B
{
public:
   /// default constructor
   PJob_scalar_B()
   : len_Z(0),
     error(E_NO_ERROR),
     fun(0),
     fun1(0)
   {}

   /// constructor
   PJob_scalar_B(Value_P Z, Value_P B)
   : value_B(B, LOC),
     value_Z(Z, LOC),
     len_Z(Z->nz_element_count()),
     error(E_NO_ERROR),
     fun(0),
     fun1(0)
   {}

   /// destructor
   ~PJob_scalar_B()
      {
        value_Z.clear(LOC);
        value_B.clear(LOC);
      }

   /// the left argument for the value being computed
   Value_P value_B;

   /// the value being computed
   Value_P value_Z;

   /// the length of the result
   ShapeItem len_Z;

   /// an error detected during computation of, eg. fun1 or fun2
   ErrorCode error;

   /// the APL (not Cell !) function being computed
   const PrimitiveFunction * fun;   // not initialized by constructor!

   /// the monadic cell function to be computed
   prim_f1 fun1;   // not initialized by constructor!

   /// return Bbz]
   const Cell & B_at(ShapeItem b) const
      { return value_B->get_cravel(b); }

   /// return Z[z]
   Cell & Z_at(ShapeItem z)
      { return value_Z->get_wravel(z); }
};
//----------------------------------------------------------------------------
/// one dyadic scalar job
class PJob_scalar_AB
{
public:
   /// default constructor
   PJob_scalar_AB()
   : len_Z(0),
     inc_A(0),
     inc_B(0),
     error(E_NO_ERROR),
     fun(0),
     fun2(0)
   {}

   /// constructor
   PJob_scalar_AB(Value_P Z, Value_P A, Value_P B)
   : value_A(A, LOC),
     value_B(B, LOC),
     value_Z(Z, LOC),
     len_Z(Z->nz_element_count()),
     inc_A(A->get_increment()),
     inc_B(B->get_increment()),
     error(E_NO_ERROR),
     fun(0),
     fun2(0)
   {}

   /// destructor
   ~PJob_scalar_AB()
      {
        value_Z.clear(LOC);
        value_B.clear(LOC);
        value_A.clear(LOC);
      }

   /// the left argument for the value being computed
   Value_P value_A;

   /// the right argument for the value being computed
   Value_P value_B;

   /// the value being computed
   Value_P value_Z;

   /// the length of the result
   ShapeItem len_Z;

   /// 0 (for scalar A) or 1
   int inc_A;

   /// 0 (for scalar B) or 1
   int inc_B;

   /// an error detected during computation of, eg. fun1 or fun2
   ErrorCode error;

   /// the APL (not Cell !) function being computed
   const PrimitiveFunction * fun;   // not initialized by constructor!

   /// the dyadic cell function to be computed
   prim_f2 fun2;   // not initialized by constructor!

   /// return A[z]
   const Cell & A_at(ShapeItem a) const
      { return value_A->get_cravel(a * inc_A); }

   /// return B[z]
   const Cell & B_at(ShapeItem b) const
      { return value_B->get_cravel(b * inc_B); }

   /// return Z[z]
   Cell & Z_at(ShapeItem z)
      { return value_Z->get_wravel(z); }
};
// ============================================================================
/// a number of jobs, where each job can be executed in parallel
class Parallel_job_list_base
{
public:
   /// from where the joblist was started
   static const char * started_loc;
};
// ----------------------------------------------------------------------------
/** a list of parallel jobs. It is used to control the parallel computation
    of nested results. Initially the Parallel_job_list is created with one
    job. If nested values are encountered they are not computed immediately
    but added to the joblist for later execution.
 **/
/// A linked list of parallel jobs
template <class T, bool has_destructor>
class Parallel_job_list : public Parallel_job_list_base
{
public:
   /// constructor: empty job list
   Parallel_job_list()
   {} 

   /// start execution of \b jobs
   void start(const T & first_job, const char * loc)
      {
#if 0
         if (started_loc)
            {
              PRINT_LOCKED(
              CERR << endl << "*** Attempt to start a new joblist at " << loc
                   << " while joblist from " << started_loc
                   << " is not finished" << endl;
              BACKTRACE
            }
#endif

         started_loc = loc;
         jobs.clear();
         jobs.push_back(first_job);
      }

   /// cancel the entire job list
   void cancel_jobs()
      {
        jobs.clear();
        started_loc = 0;
      }

   /// set current job. CAUTION: jobs may be modified by the
   /// execution of current_job. Therefore we copy current_job from
   /// jobs (and can then safely use current_job *).
   T * next_job()
      {
        if (jobs.size() == 0)   // joblist done
           {
             started_loc = 0;
             return 0;
           }

        current_job = jobs.back();
        jobs.pop_back();
        return &current_job;
      }

   /// return the current job
   T & get_current_job()
      { return current_job; }

   /// return the current size
   size_t get_size()
      { return jobs.size(); }

   /// add \b job to \b jobs
   void add_job(const T & job)
      { jobs.push_back(job); }

   /// return true if not all jobs are done yet
   bool busy()
      { return started_loc != 0; }

   /// where joblist was started (also an indicator if all jobs were done)
   const char * get_started_loc()
      { return started_loc; }

protected:
   /// jobs to be performed
   std::vector<T> jobs;

   /// the currently executed worklist item
   T current_job;
};
//============================================================================
#endif // PJOB_HH_DEFINED
