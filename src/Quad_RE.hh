/*
    This file is part of GNU APL, a free implementation of the
    ISO/IEC Standard 13751, "Programming Language APL, Extended"

    Copyright © 2008-2016  Elias Mårtenson

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

#ifndef __Quad_RE_DEFINED__
#define __Quad_RE_DEFINED__

#include "QuadFunction.hh"
#include "Value.hh"

class Regexp;

/// The class implementing ⎕RE
class Quad_RE : public QuadFunction
{
public:
   /// Constructor.
   Quad_RE() : QuadFunction(TOK_Quad_RE)
   {}

   static Quad_RE  fun;          ///< Built-in function.

protected:
   /// the type of result
   enum Result_type
      {
        RT_string    = 0,   ///< matched string
        RT_partition = 1,   ///< dito.
        RT_pos_len   = 2,   ///< position and length
        RT_reduce    = 3,   ///< dito.
      } result_type;    ///< the type of result

   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_AXB(A, Str0(LOC), B); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const;

   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const
      { VALENCE_ERROR; }

   /// overloaded Function::eval_XB().
   virtual Token eval_XB(Value_P X, Value_P B) const
      { VALENCE_ERROR; }

#ifdef HAVE_LIBPCRE2_32

   /// libpcre flags
   class Flags
      {
        public:
           /// constructor
           Flags(const UCS_string & flags_in);

           /// return the flags
           int get_compflags() const
              { return flags; }

           /// return true if no match shall give an error
           bool get_error_on_no_match() const
              { return error_on_no_match; }

           /// return global (return all matches and not only first match)
           bool get_global() const
              { return global; }

           /// return the result type
           Result_type get_result_type() const
              { return result_type; }

        protected:
           /// the flags (see pcre.h)
           int flags;

           /// make no match an error
           bool error_on_no_match;

           /// dito.
           bool global;

           /// dito.
           Result_type result_type;
      };

   /// return a result whose format is given by flags
   static Value_P regex_results(const Regexp & A, const Flags & X,
                                const UCS_string & B);

   /// return a result that can be used directly by ⊂ or /
   static Value_P partition_result(const Regexp & A, const Flags & X,
                                   const UCS_string & B);

   /// return a result that contains the matched strings
   static Value_P string_result(const Regexp & A, const Flags & X,
                                 const UCS_string & B, ShapeItem & B_offset);

   /// return a result that can be used by [] (position and length)
   static Value_P index_result(const Regexp & regex, const Flags & X,
                                const UCS_string & B, ShapeItem & B_offset);

# endif
};

#endif
