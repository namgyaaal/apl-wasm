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

#ifndef __POINTERCELL_HH_DEFINED__
#define __POINTERCELL_HH_DEFINED__

#include "Cell.hh"

//----------------------------------------------------------------------------
/*!
    A cell pointing to another APL value. This is used to create nested
    arrays. This class essentially overloads certain functions in class
    Cell with nested array specific implementations.
 */
/// A cell pointing to a (nested) APL value
class PointerCell : public Cell
{
public:
   /// constructor: a cell containing nested sub-array \b val.
   PointerCell(Value * val, Value & cell_owner);

   /// constructor: a cell containing nested sub-array \b val where val
   /// is allowed to be a simple scalar (used only in ScalarFunction.cc)
   PointerCell(Value * val, Value & cell_owner, uint32_t magic);

   /// overloaded Cell::init_other
   virtual void init_other(void * other, Value & cell_owner,
                           const char * loc) const;

   /// overloaded Cell::is_pointer_cell()
   virtual bool is_pointer_cell() const   { return true; }

   /// overloaded Cell::get_pointer_value()
   virtual Value_P get_pointer_value()  const;

   /// overloaded Cell::_is_member_anchor()
   virtual bool is_member_anchor() const;

   /// overloaded Cell::greater()
   virtual bool greater(const Cell & other) const;

   /// overloaded Cell::equal()
   virtual bool equal(const Cell & other, double qct) const;

   /// compare \b this with other, throw DOMAIN ERROR on illegal comparisons
   virtual Comp_result compare(const Cell & other) const;

   /// overloaded Cell::release()
   virtual void release(const char * loc);

   /// the Quad_CR representation of this cell
   virtual PrintBuffer character_representation(const PrintContext &pcx) const;

   /// return the owner of this PointerCell
   Value * get_cell_owner() const
      { return value.pval.owner; }

   /// isolate value.pval.valp (make \b value.pval the sole owner)
   void isolate(const char * loc)
      { if (+value.pval.valp)   value.pval.valp.isolate(LOC); }

protected:
   ///  overloaded Cell::get_cell_type()
   virtual CellType get_cell_type() const
      { return CT_POINTER; }

   ///  overloaded Cell::deep_cell_types()
   CellType deep_cell_types() const;

   ///  overloaded Cell::deep_cell_types()
   CellType deep_cell_subtypes() const;

   /// overloaded Cell::get_classname()
   virtual const char * get_classname() const   { return "PointerCell"; }

   /// overloaded Cell::CDR_size() should not be called for pointer cells
   virtual int CDR_size() const { NeverReach("PointerCell::CDR_size() called");}
};
//============================================================================

#endif // __POINTERCELL_HH_DEFINED__
