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

#ifndef __INDEXEXPR_HH_DEFINED__
#define __INDEXEXPR_HH_DEFINED__

#include "Cell.hh"
#include "DynamicObject.hh"
#include "Value.hh"

//----------------------------------------------------------------------------
/**
     An array of index values.
 */
/// The interal representation of some APL index [A1;A2;...;An]
class IndexExpr : public DynamicObject
{
public:
   /// constructor: empty (0-dimensional) IndexExpr
   IndexExpr(Assign_state ass_state, const char * loc);

   /// destructor
   ~IndexExpr();

   /// The quad-io for this index.
   uint32_t quad_io;

   /// The values (0 = elided index as in [] or [;].
   Value_P values[MAX_RANK];

   /// append a value.
   void add_index(Value_P val)
      {
       if (rank >= MAX_RANK)    RANK_ERROR;
       values[rank++] = val;
       if (+val)   ++value_count;
      }

   /// return the number of values (= number of semicolons + 1),
   /// including elided indices
   uRank get_rank() const   { return rank; }

   /// return true iff the number of dimensions is 1 (i.e. no ; and non-empty)
   bool is_axis() const   { return rank == 1; }

   /// Return an axis (from an IndexExpr of rank 1.
   sAxis get_axis(sRank max_axis) const;

   /// return true iff this index is part of indexed assignment ( A[]← )
   Assign_state get_assign_state() const
      { return assign_state; }

   /// return the single axis value and clear it in \b this IndexExpr.
   Value_P extract_axis();

   /// check that all indices indices of \b this IndexExpr are valid indices
   /// for \b shape, raise INDEX_ERROR if not.
   void check_index_range(const Shape & shape) const;

   /// return axis rk (rk in shape order as opposed to index order)
   const Value * get_axis_value(uAxis ax) const
      { return values[rank - ax - 1].get(); }

   /// print stale IndexExprs, and return the number of stale IndexExprs.
   static int print_stale(ostream & out);

   /// erase stale IndexExprs
   static void erase_stale(const char * loc);

protected:
   /// the number of dimensions (including elided)
   uRank rank;

   /// the number of values (excluding elided)
   uRank value_count;

   /// true iff this index is part of indexed assignment ( A[]← )
   const Assign_state assign_state;
};
//----------------------------------------------------------------------------

#endif // __INDEXEXPR_HH_DEFINED__
