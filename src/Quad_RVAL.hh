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

#ifndef __Quad_RVAL_DEFINED__
#define __Quad_RVAL_DEFINED__

using namespace std;

#include "QuadFunction.hh"

/// a random APL value
class Quad_RVAL : public QuadFunction
{
public:
   /// Constructor.
   Quad_RVAL();

   static Quad_RVAL  fun;          ///< Built-in function.

protected:
   /// overloaded Function::eval_AB()
   virtual Token eval_AB(Value_P A, Value_P B) const;

   /// overloaded Function::eval_B()
   virtual Token eval_B(Value_P B) const
      { return Token(TOK_APL_VALUE1, do_eval_B(*B, 0)); }

   /// do eval_AB(A, B);
   static Value_P do_eval_AB(int A, const Value & B);

   /// do eval_B(B);
   static Value_P do_eval_B(const Value & B, int depth);

   /// set or return the state of the random generator
   static Value_P generator_state(const Value & B);

   /// set or return the desired rank of random numbers
   static Value_P result_rank(const Value & B);

   /// set or return the desired ranks of random numbers
   static Value_P result_shape(const Value & B);

   /// set or return the desired types of random numbers
   static Value_P result_type(const Value & B);

   /// set or return the desired max. depth of random numbers
   static Value_P result_maxdepth(const Value & B);

   /// choose an integer value at random according to distribution \b dist
   static int choose_integer(const vector<int> & dist);

   /// initialize \b cell with a random character
   static void random_character(Value & Z);

   /// initialize \b cell with a random integer
   static void random_integer(Value & Z);

   /// initialize \b cell with a random float
   static void random_float(Value & Z);

   /// initialize \b cell with a random complex number
   static void random_complex(Value & Z);

   /// initialize \b cell with a random nested value
   static void random_nested(Value & Z, const Value & B, int depth);

   /// return a 17-bit random number
   // of random_r()
   static uint64_t rand17();

   /// the number of bytes in the state of the random number generator
   static size_t N;

   /// the desired rank of random values
   static vector<int> desired_ranks;

   /// the desired rank of random values
   static Shape desired_shape;

   /// the desired types (or a distribution of types) of random values
   static vector<int> desired_types;

   /// the desiredlimit on the depths of the random values
   static int desired_maxdepth;

   /// the state buffer of the random number generator
   static char state[256];

#if HAVE_LIBC
   /// the state of the random number generator
   static struct random_data rdata;
#endif
};

#endif // __Quad_RVAL_DEFINED__

