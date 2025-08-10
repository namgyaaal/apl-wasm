/*
    This file is part of GNU APL, a free implementation of the
    ISO/IEC Standard 13751, "Programming Language APL, Extended"

    Copyright © 2017 Elias Mårtenson

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

#ifndef __Regexp_HH__DEFINED__
#define __Regexp_HH__DEFINED__

#include "UCS_string.hh"

#define PCRE2_CODE_UNIT_WIDTH 32
#include <pcre2.h>

/// A Regular expression match
class RegexpMatch
{
public:
   /// constructor
   RegexpMatch(pcre2_code * code, const UCS_string & B, PCRE2_SIZE start);

   /// destructor
   virtual ~RegexpMatch();

   /// return the number of offset pairs in a match
   uint32_t get_ovector_count() const
       { return ovector_count; }

   /// return the offset pairs in a match
    const PCRE2_SIZE * get_ovector() const
       { return ovector; }

   /// return \b true if there was a match
    bool is_match() const;

   /// return the number of matches
    int num_matches() const;

   /// return the matched string
   UCS_string matched_string() const;

protected:
   /// the offset pairs
   PCRE2_SIZE * ovector;

   /// the number of offset pairs
   uint32_t ovector_count;

   /// the data related to a match
   pcre2_match_data * match_data;

   /// the result of a match
   int match_result;

   /// the right argument B of ⎕RE
   const UCS_string & matched_B;
};

/// Helper class for ⎕RE
class Regexp
{
public:
    /// constructor
    Regexp(const UCS_string & pattern, int flags);

    /// destructor
    virtual ~Regexp();

   /// return a new match
    const RegexpMatch * match(const UCS_string & match, PCRE2_SIZE size) const;

   /// return the number of matches
    int expression_count() const;

   /// returned the compiled regular expression
    pcre2_code * get_code() const
       { return code; }

   /// return a description for error code \b eec
   static UCS_string pcre_error(int ec);

protected:
   /// compiled regular expression
   pcre2_code * code;
};

#endif // __Regexp_HH__DEFINED__
