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

#include "IndexExpr.hh"
#include "PrintBuffer.hh"
#include "PrintOperator.hh"
#include "Value.hh"
#include "Workspace.hh"

//----------------------------------------------------------------------------
IndexExpr::IndexExpr(Assign_state astate, const char * loc)
   : DynamicObject(loc),
     quad_io(Workspace::get_IO()),
     rank(0),
     value_count(0),
     assign_state(astate)
{
}
//----------------------------------------------------------------------------
IndexExpr::~IndexExpr()
{
   if (value_count)
      {
        loop(r, rank)   if (+values[r])   values[r].reset();
      }
}
//----------------------------------------------------------------------------
Value_P
IndexExpr::extract_axis()
{
   if (rank == 0 || !values[0])    return Value_P();   // no index or [ ]

   Assert1(rank == 1);   // must only be called for [ axis ]
   --value_count;
Value_P ret = values[0];
   values[0].reset();   // decrement owner count
   return ret;
}
//----------------------------------------------------------------------------
void
IndexExpr::check_index_range(const Shape & shape) const
{
   loop(r, rank)
      {
        if (const Value * ival = get_axis_value(r))   // unless elided index
           {
             const ShapeItem max_idx = shape.get_shape_item(r) + quad_io;
             loop(i, ival->element_count())
                {
                  const APL_Integer idx = ival->get_cravel(i).get_near_int();
                  if (idx < quad_io || idx >= max_idx)   // invalid index
                     {
                       UCS_string & ucs = MORE_ERROR();
                       ucs << "Offending index: " << idx
                           << " (with ⎕IO: " << quad_io << ")\n"
                           << "offending axis:  " << (r + quad_io)
                           << " of some";
                       loop(s, shape.get_rank())
                           ucs << " " << shape.get_shape_item(s);
                       ucs << "⍴...";
                       if (idx >= max_idx)
                          ucs << "\nthe max index for axis "
                              << (r + quad_io)<< " is: "
                              << (max_idx - quad_io);
                       INDEX_ERROR;
                     }
                }
           }
      }
}
//----------------------------------------------------------------------------
sRank
IndexExpr::get_axis(sRank max_axis) const
{
   if (rank != 1)   INDEX_ERROR;

const APL_Integer qio = Workspace::get_IO();

Value_P I = values[0];
   if (!I->is_scalar_or_len1_vector())     INDEX_ERROR;

   if (!I->get_cfirst().is_near_int())   INDEX_ERROR;

   // if axis becomes (signed) negative then it will be (unsigned) too big.
   // Therefore we need not test for < 0.
   //
sRank axis = I->get_cfirst().get_near_int() - qio;
   if (axis >= max_axis)   INDEX_ERROR;

   return axis;
}
//----------------------------------------------------------------------------
int
IndexExpr::print_stale(ostream & out)
{
int count = 0;

   for (const DynamicObject * dob = all_index_exprs.get_next();
        dob != &all_index_exprs; dob = dob->get_next())
       {
         const IndexExpr * idx = dob->pIndexExpr();

         out << dob->where_allocated();

         try           { out << *idx; }
         catch (...)   { out << " *** corrupt ***"; }

         out << endl;

         ++count;
       }

   return count;
}
//----------------------------------------------------------------------------
void
IndexExpr::erase_stale(const char * loc)
{
   Log(LOG_Value__erase_stale)
      CERR << endl << endl << "erase_stale() called from " << loc << endl;

   for (DynamicObject * dob = all_index_exprs.get_next();
        dob != &all_index_exprs; dob = dob->get_next())
       {
         IndexExpr * idx = dob->pIndexExpr();

         Log(LOG_Value__erase_stale)
            {
              CERR << "Erasing stale IndexExpr:" << endl
                   << "  Allocated by " << idx->alloc_loc << endl;
            }

         dob = dob->get_prev();
         idx->unlink();
         delete idx;
       }
}
//----------------------------------------------------------------------------
ostream &
operator <<(ostream & out, const IndexExpr & idx)
{
   out << "[";
   loop(i, idx.get_rank())
      {
        if (i)   out << ";";
        if (const Value * ival = idx.get_axis_value(i))
           {
             // value::print() may print a trailing LF that we don't want here.
             // We therefore print the index values ourselves.
             const PrintContext pctx = Workspace::get_PrintContext(PR_APL_MIN);
             PrintBuffer pb(*ival, pctx, 0);

             UCS_string ucs(pb, ival->get_rank(), Workspace::get_PW());
             out << ucs;
           }
      }
   return out << "]";
}
//----------------------------------------------------------------------------
