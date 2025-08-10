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

#include "UCS_string.hh"

#ifndef __UCS_STRING_VECTOR_HH_DEFINED__
#define __UCS_STRING_VECTOR_HH_DEFINED__

//----------------------------------------------------------------------------
/// a vector of UCS_strings.
class UCS_string_vector : public std::vector<UCS_string>
{
public:
   /// constructor: empty string vector
   UCS_string_vector()   {}

   /// constructor: from APL character matrix (removes trailing blanks)
   UCS_string_vector(const Value & val, bool surrogate);

   /// overload vector<UCS_string>::size() so that it returns a signed length
   ShapeItem size() const
      { return ShapeItem(std::vector<UCS_string>::size()); }

   /// return true iff one of the strings is equal to \b ucs
   bool contains(const UCS_string & ucs) const
      {
        loop(s, size())   if (ucs == at(s))   return true;
        return false;
      }

   /// sort the strings in this vector alphabetically
   void sort()
      {
        if (size() < 2)   return;
        Heapsort<UCS_string>::sort(&front(), size(), 0,
                                   UCS_string::compare_names);
      }

   /// compute columns width so that items align nicely (for )VARS, )FNS, etc.)
   void compute_column_width(int tab_size, std::vector<int> & result);

   /// replacement for erase(std::vector::iterator position)
   void erase(size_t pos)
      { std::vector<UCS_string>::erase(begin() + pos); }

   /// replacement for insert(std::vector::iterator position, value)
   void insert(size_t pos, const UCS_string & value)
      { std::vector<UCS_string>::insert(begin() + pos, value); }

   /// print items of \b this vector in a table with \b column_count columns
   std::ostream & print_table(std::ostream & out, size_t column_count) const;

   /// return the size of the longest UCS_string in \b this vector.
   ShapeItem max_width(size_t col, size_t column_count) const;

private:
   /// prevent the inadvertent use of iterator nonsense
   std::vector<UCS_string>::iterator erase(
        std::vector<UCS_string>::iterator position);

   /// prevent the inadvertent use of iterator nonsense
   std::vector<UCS_string>::iterator insert(
        std::vector<UCS_string>::iterator position,
        const UCS_string & value);
};
//----------------------------------------------------------------------------

#endif // __UCS_STRING_VECTOR_HH_DEFINED__
