/*
    This file is part of GNU APL, a free implementation of the
    ISO/IEC Standard 13751, "Programming Language APL, Extended"

    Copyright © 2021-2022  Dr. Jürgen Sauermann

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

#ifndef __CONSTCELL_P_DEFINED__
#define __CONSTCELL_P_DEFINED__

#include <cstdint>
#include "Assert.hh"
#include "Value_P.hh"

typedef uint64_t Cell_offset;

//----------------------------------------------------------------------------
/// a "smart" Cell *, allowing only a subset of what a normal Cell *
/// is capable of. MUST NOT BE USED FOR RAVELs OF PACKED VALUES
class ConstCell_P
{
public:
   /// default constructor
   ConstCell_P()
   : base(0),
     end(0),
     offset(0),
     increment(false)
   {}

   /// copy constructor
   ConstCell_P(const ConstCell_P & other)
   : base(other.base),
     end(other.end),
     offset(0),
     increment(other.increment)
   {}

   /// constructor from the first Cell of a ravel
   ConstCell_P(const Value & owner, bool _inc)
   : base(&owner.get_cfirst()),
     end(owner.element_count()),
     offset(0),
     increment(_inc)
   { Assert(!owner.is_packed()); }

   /// constructor from pointer to the owner of the Cell
   ConstCell_P(Value_P owner, bool _inc)
   : base(&owner->get_cfirst()),
     end(owner->element_count()),
     offset(0),
     increment(_inc)
   { Assert(!owner->is_packed()); }

   /// constructor: from a single Cell (of a scalar)
   ConstCell_P(const Cell & cell)
   : base(&cell),
     end(1),
     offset(0),
     increment(0)
   {}

   /// return true iff offset is valid (and then *() != 0). For this to work,
   /// increment needs to be true
   bool operator +() const
      { Assert1(increment);   return offset < end || offset == 0; }

   /// return the ravel offset of the current Cell
   Cell_offset operator ()() const
      { return offset; }

   /// return a reference to the Cell at off
   const Cell & operator [](Cell_offset off) const
      { return base[off]; }

   /// return a reference to the current Cell
   const Cell & operator *() const
      { return base[offset]; }

   /// return a pointer to the current Cell (or 0 at the end)
   const Cell * operator ->() const
      { return operator +() ? base + offset : 0; }

   /// move to the next Cell
   void operator ++()
      { if (increment)   ++offset; }

   /// return the lengths of the Cells
   const Cell_offset get_length() const
      { return end; }

protected:
   /// the first Cell of the ravel (aka. cfirst() in class Value)
   const Cell * base;

   /// the first Cell after  the ravel
   const Cell_offset end;

   /// the current offset
   Cell_offset offset;

   /// whether operator ++() shall increment \b offset
   const bool increment;
};
//----------------------------------------------------------------------------
/// a "smart" Cell *, remembering its owner and allowing only a subset
/// of what a normal Cell * is capable of.
class ConstRavel_P
{
public:
   /// constructor from the owner of the Cell
   ConstRavel_P(const Value & _owner, bool _inc)
   : owner(_owner),
     end(_owner.element_count()),
     offset(0),
     increment(_inc)
   {}

   /// constructor from pointer to the owner of the Cell
   ConstRavel_P(Value_P _owner, bool _inc)
   : owner(*_owner),
     end(_owner->element_count()),
     offset(0),
     increment(_inc)
   {}

   /// return true iff offset is valid (and then *() != 0). For this to work,
   /// increment needs to be true
   bool operator +() const
      { Assert1(increment);   return offset < end || offset == 0; }

   /// return the ravel offset of the current Cell
   Cell_offset operator ()() const
      { return offset; }

   /// return a reference to the current Cell
   const Cell & operator *() const
      { return owner.get_cravel(offset); }

   /// return a pointer to the current Cell (or 0 at the end)
   const Cell * operator ->() const
      { return operator +() ? &owner.get_cravel(offset) : 0; }

   /// move to the next Cell
   void operator ++()
      { if (increment)   ++offset; }

   /// return the lengths of the Cells
   const Cell_offset get_length() const
      { return end; }

   /// return the owner of the Cells
   const Value & get_owner() const
      { return owner; }

protected:
   /// the owner of the ravel
   const Value & owner;

   /// position of the first Cell after the ravel
   const Cell_offset end;

   /// the current offset
   Cell_offset offset;

   /// whether operator ++() shall increment \b offset
   const bool increment;   // ++ shall/shall not increment offset
};
//----------------------------------------------------------------------------
#endif // __CONSTCELL_P_DEFINED__
