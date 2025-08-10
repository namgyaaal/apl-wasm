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

#ifndef __ARRAY_ITERATOR_HH_DEFINED__
#define __ARRAY_ITERATOR_HH_DEFINED__

#include "Common.hh"
#include "Shape.hh"
#include "SystemLimits.hh"

//----------------------------------------------------------------------------
/// An iterator counting 0, 1, 2, ... N (excluding) along an axis of length N
class AxisIterator
{
public:
   /// default constructor (for arrays).
   /// Will be re-initialized with placement new.
   AxisIterator()
   : axis_length(0),
     axis_weight(0),
     wrap(0)
   {}

   /// constructor for placement new. The weight is the step size in
   /// the associated ravel ehrn the iterator is incremented.
   /// A weight of 0 indicates a scalar
   AxisIterator(ShapeItem len, ShapeItem weight, bool _wrap)
   : axis_length(len),
     axis_weight(weight),
     shape_offset(0),
     ravel_offset(0),
     wrap(_wrap)
   {}

   /** increment the offset(s) and return:

       true  (= "carry")       if the end of the axis was reached, or else
       false (= no more items) if the end was NOT reached
    **/
   bool next()
      {
        ravel_offset += axis_weight;

        // NOTE: increment of scalar shape_offset needed for more() below.
        if (++shape_offset < axis_length || is_scalar_iterator()) return false;

        // carry has occurred.
        if (wrap)   shape_offset = ravel_offset = 0;
        return true;   // carry: increment next higher (if any) iterator
      }

   /// true if more items coming
   bool has_more() const
      { return is_scalar_iterator() ? shape_offset == 0   // before next()
                                    : shape_offset < axis_length; }

   /// return the current shape offset (not weighted)
   ShapeItem get_shape_offset() const
      { return shape_offset; }

   /// return the current ravel offset (weighted)
   ShapeItem get_ravel_offset() const
      { return ravel_offset; }

   /// return the weight (ravel increment for one next() call of this iterator
   ShapeItem get_weight() const
      { return axis_weight; }

   /// return the total weight (ravel increment for \b axis_length next() calls
   ShapeItem get_total_weight() const
      { return axis_weight * axis_length; }

   /// true if \b this is an iterator for a scalar value.
   bool is_scalar_iterator() const
      { return axis_weight == 0; }

protected:
   /// the length of the axis; the interator counts 0, 1, ... axis_length
   /// (excluding).
   const ShapeItem axis_length;

   /// the weight (ravel increment) of this itertor (product of lower iterator
   /// weights). IMPORTANT: Scalars are have an axis_weight of 0 here.
   const ShapeItem axis_weight;

   /// the current offset, 0, 1, ... axis_len (excluding)
   ShapeItem shape_offset;

   /// the current offset, 0, weight, ... weight*axis_len (excluding)
   ShapeItem ravel_offset;

   /// true iff \b this iterator shall wrap around at the end. In order for
   /// more() of the entire array to be fast, we wrap all iterators except
   /// the first at the end so that more() of the first iterator becomes false
   /// when iterating over entire array is done.
   const bool wrap;
};
//----------------------------------------------------------------------------
/// An iterator counting along all axes of \b shape
class ArrayIterator
{
public:
   /// constructor from Shape
   ArrayIterator(const Shape & shape)
   : rank(shape.get_rank()),
     total_ravel_offset(0)
      {
        if (rank == 0)   // scalar
           {
             new (&get_iterator(0)) AxisIterator(1, 0, false);
             return;
           }

        ShapeItem weight = 1;
        for (sAxis a = rank - 1; a >= 0; --a)
            {
              // axis_iterators[0] is the one with the highest weight, and
              // axis_iterators[rank - 1 the one with the lowest weight (1).
              // 
              const ShapeItem len = shape.get_shape_item(a);
              new (&get_iterator(a)) AxisIterator(len, weight, a > 0);
              weight *= len;
            }
      }

   /// constructor from Shape and axis permutation
   ArrayIterator(const Shape & shape,  const Shape & perm)
   : rank(shape.get_rank()),
     total_ravel_offset(0)
      {
        if (rank == 0)   // scalar
           {
             new (&get_iterator(0)) AxisIterator(1, 0, false);
             return;
           }

        ShapeItem weight = 1;
        for (sAxis a = rank - 1; a >= 0; --a)
            {
              const sAxis perm_a = perm.get_shape_item(a);

              // axis_iterators[0] is the one with the highest weight, and
              // axis_iterators[rank - 1 the one with the lowest weight (1).
              // 
              const ShapeItem len = shape.get_shape_item(perm_a);
              new (&get_iterator(perm_a)) AxisIterator(len, weight, perm_a);
              weight *= len;
            }
      }

   // Note: The axis_iterators[a] for a > 0 wrap at the end and therefore
   // axis_iterators[a].more() is always true for a > 0.
   // As a consequence we only need to check axis_iterators[0].more() to see
   // if the entire iteration is done.
   //
   /// return true iff this iterator has more items to come.
   bool has_more() const
      {
        return get_iterator(0).has_more();
      }

   /// the work-horse of this iterator.
   void operator++()
      {
        if (rank == 0)   // scalar
           {
             get_iterator(0).next();
             return;
           }

        for (sAxis a = rank - 1; a >= 0; --a)
            {
              AxisIterator & iterator = get_iterator(a);
              total_ravel_offset += iterator.get_weight();
              if (!iterator.next())   break;   // if no carry
              total_ravel_offset -= iterator.get_total_weight();
            }
      }

   /// Get the current offset for axis r
   ShapeItem get_shape_offset(sAxis a) const
      { return get_iterator(a).get_shape_offset(); }

   /// Get the current total offset
   ShapeItem get_ravel_offset() const
      { return total_ravel_offset; }

   /// return the offsets of the axis_iterators as a shape
   Shape get_shape_offsets()
      {
        Shape ret;
        loop(r, rank)
            ret.add_shape_item(get_iterator(r).get_shape_offset());
        return ret;
      }

protected:
   /// return the iterator for axis a
   const AxisIterator & get_iterator(sAxis a) const
      { return axis_iterators[a]; }

   /// return the iterator for axis a
   AxisIterator & get_iterator(sAxis a)
      { return axis_iterators[a]; }

   /// the number of valid axes (= iterators).
   const uRank rank;

   /// the sum of the ravel offsets of all iterators
   ShapeItem total_ravel_offset;

   /** axis iterators.                   Shape:  ⊏sh0:sh1:..:shN⊐
                                                    │   │      │
       axis_iterators[0] ←→ get_shape_item(0), ─────┘   │      └── weight 1
       axis_iterators[1] ←→ get_shape_item(1), ─────────┘ ...
    **/
   AxisIterator axis_iterators[MAX_RANK];
};
//============================================================================

#endif // __ARRAY_ITERATOR_HH_DEFINED__
