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

#include "Error.hh"
#include "IndexExpr.hh"
#include "IndexIterator.hh"
#include "Output.hh"
#include "PrintOperator.hh"

//----------------------------------------------------------------------------
void
IndexIterator::operator ++()
{
   ++pos;
   if (pos >= get_index_count())
      {
         if (upper)
            {
              pos = 0;
              ++*upper;
            }
         else
            {
              pos = get_index_count();   // so that has_more() works
            }
      }

   // print(CERR);
}
//----------------------------------------------------------------------------
ostream &
IndexIterator::print(ostream & out) const
{
   out << "Iterator "     << voidP(this) << ":"  << endl
       << "   upper:       " << voidP(upper)     << endl
       << "   value:       " << get_ivalue()     << endl
       << "   indices[" << get_index_count()     << "] :";

   loop(i, get_index_count())   out << " " << get_pos(i);

   return out << endl;
}
//============================================================================
TrueIndexIterator::TrueIndexIterator(ShapeItem w, Value_P value,
                                     uint32_t qio, ShapeItem max_idx)
   : IndexIterator(w, value->element_count())
{
// CERR << "TrueIndexIterator(w=" << w << ", value=" << *value
//      << ", max=" << max_idx << ")" << endl;

   indices = new ShapeItem[count];
   loop(v, count)
      {
        const ShapeItem idx = value->get_cravel(v).get_near_int() - qio;

        // instead of testing signed < 0 and >= max, we test unsigned >= max.
        //
        if (idx >= max_idx)   INDEX_ERROR;

        indices[v] = idx * w;
      }
}
//============================================================================
MultiIndexIterator::MultiIndexIterator(const Shape & shape,
                                       const IndexExpr & IDX)
   : highest_it(0),
     lowest_it(0),
     empty(false)
{
   // IDX is parsed from right to left:    Value[I2;I1;I0]  --> I0 I1 I2
   // the shapes of Value and IDX are then related as follows:
   //
   // Value    IDX
   // ---------------
   // 0        rank-1   (rank = IDX->value_count())
   // 1        rank-2
   // ...      ...
   // rank-2   1
   // rank-1   0
   // ---------------
   //

   if (shape.get_rank() != IDX.get_rank())
      {
        Log(LOG_error_throw)
           {
             Q1(shape.get_rank())
             Q1(shape)
             Q1(IDX)
             Q1(IDX.get_rank())
           }
        INDEX_ERROR;
      }

ShapeItem weight = 1;
   loop(idx_r, shape.get_rank())
       {
         const sAxis ax_r = shape.get_rank() - idx_r - 1;  // see comment above.
         const ShapeItem  sh_r = shape.get_shape_item(ax_r);
         Value_P I = IDX.values[idx_r];
         IndexIterator * new_it;
         if (!I)   // elided index
            {
              new_it = new ElidedIndexIterator(weight, sh_r);
              if (sh_r == 0)   empty = true;
            }
         else
            {
              new_it = new TrueIndexIterator(weight, I, IDX.quad_io, sh_r);
              if (I->element_count() == 0)   empty = true;
            }

         Log(LOG_delete)
            CERR << "new    " << voidP(new_it) << " at " LOC << endl;

         if (highest_it)   highest_it->set_upper(new_it);
         else              lowest_it = new_it;

         weight *= sh_r;
         highest_it = new_it;
       }
}
//----------------------------------------------------------------------------
MultiIndexIterator::~MultiIndexIterator()
{
   for (IndexIterator * it = lowest_it; it;)
       {
         IndexIterator * del = it;
         it = it->get_upper();
         Log(LOG_delete)
            CERR << "delete " HEX(uint64_t(del))
                 << " at " LOC << endl;
         delete del;
       }
}
//----------------------------------------------------------------------------
ShapeItem
MultiIndexIterator::operator ++(int)
{
   Assert(!lowest_it || has_more());

ShapeItem ret = 0;
   for (IndexIterator * it = lowest_it; it; it = it->get_upper())
            ret += it->get_ivalue();

   if (lowest_it)   ++*lowest_it;
   return ret;
}
//============================================================================

