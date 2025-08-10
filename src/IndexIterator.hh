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

#include "Shape.hh"
#include "Value.hh"

//----------------------------------------------------------------------------
/**
   An iterator for one dimension of indices. When the iterator reached its
   end, it wraps around and increments its upper IndexIterator (if any).

   This way, multi-dimensional iterators can be created.
 */
/// Base class for ElidedIndexIterator and TrueIndexIterator
class IndexIterator
{
public:
   /// constructor: IndexIterator with weight w
   IndexIterator(ShapeItem w, ShapeItem cnt)
   : weight(w),
     count(cnt),
     upper(0),
     pos(0)
   {}

    virtual ~IndexIterator() {}

   /// go the the next index
   void operator ++();

   /// return true if more indices are coming
   bool has_more() const   { return count && pos < count; }

   /// return the current index
   virtual ShapeItem get_ivalue() const = 0;

   /// return the current index
   virtual ShapeItem get_pos(ShapeItem p) const = 0;

   /// print this iterator (for debugging purposes).
   ostream & print(ostream & out) const;

   /// return the next higher IndexIterator
   IndexIterator * get_upper() const   { return upper; };

   /// set the next higher IndexIterator
   void set_upper(IndexIterator * up)   { Assert(upper == 0);   upper = up; };

   /// get the number of indices.
   ShapeItem get_index_count() const   { return count; }

protected:
   /// the weight of this IndexIterator
   const ShapeItem weight;

   /// number of index elements
   const ShapeItem count;

   /// The next higher iterator.
   IndexIterator * upper;

   /// The current value.
   ShapeItem pos;
};
//----------------------------------------------------------------------------
/// Iterator for an elided index (= ⍳(⍴B)[N] for some N)
class ElidedIndexIterator : public IndexIterator
{
public:
   /// constructor
   ElidedIndexIterator(ShapeItem w, ShapeItem sh)
   : IndexIterator(w, sh)
   {}

   /// get the current index.
   virtual ShapeItem get_ivalue() const { return pos * weight; }

   /// get the index i.
   virtual ShapeItem get_pos(ShapeItem i) const
      { Assert(i < count);   return i; }
};
//----------------------------------------------------------------------------
/// an IndexIterator for a true (i.e. non-elided) index array
class TrueIndexIterator : public IndexIterator
{
public:
   /// constructor
   TrueIndexIterator(ShapeItem w, Value_P value,
                     uint32_t qio, ShapeItem max_idx);

   /// destructor
   ~TrueIndexIterator()
      { delete [] indices; }

   /// return the current index
   virtual ShapeItem get_ivalue() const
      { Assert(pos < count);   return indices[pos]; }

   /// return the i'th index
   virtual ShapeItem get_pos(ShapeItem i) const
      { Assert(i < count);   return indices[i]; }

protected:
   /// the indices
   ShapeItem * indices;
};
//----------------------------------------------------------------------------
/**
    a multi-dimensional index iterator, consisting of several
   one-dimensional iterators.
 **/
/// A multi-dimensional index iterator
class MultiIndexIterator
{
public:
   /// constructor from IndexExpr for value with rank/shape
   MultiIndexIterator(const Shape & shape, const IndexExpr & IDX);

   /// destructor
   ~MultiIndexIterator();

   /// get the current index (offset into a ravel) and increment iterators
   ShapeItem operator ++(int);

   /// return true if more indices are coming
   bool has_more() const
      { return highest_it && highest_it->has_more(); }

protected:
   /// the iterator for the highest dimension
   IndexIterator * highest_it;

   /// the iterator for the lowest dimension
   IndexIterator * lowest_it;

   /// true if one of the iterators has length 0
   bool empty;
};
//----------------------------------------------------------------------------

