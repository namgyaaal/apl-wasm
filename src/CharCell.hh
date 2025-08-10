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

#ifndef __CHARCELL_HH_DEFINED__
#define __CHARCELL_HH_DEFINED__

#include "Cell.hh"

//----------------------------------------------------------------------------
/*!
    A cell containing a single Unicode (most likely an APL character)
 */
/// A Cell containing a single Unicode character
class CharCell : public Cell
{
   friend class Value;       // for zU()
   class APL_value;
   friend void set_char(int, APL_value, uint64_t);   // for zU() in libapl.cc

public:
   /// Construct a character cell containing \b av
   CharCell(Unicode av)
      { value.aval = av; }

   /// overloaded Cell::init_other
   virtual void init_other(void * other, Value & cell_owner, const char * loc)
       const { new (other)   CharCell(value.aval); }

   /// overloaded Cell::is_character_cell()
   virtual bool is_character_cell() const
      { return true; }

   /// overloaded Cell::greater()
   virtual bool greater(const Cell & other) const;

   /// overloaded Cell::equal()
   virtual bool equal(const Cell & other, double qct) const;

   /// overloaded Cell::is_example_field()
   virtual bool is_example_field() const;

   /// overloaded Cell::compare()
   virtual Comp_result compare(const Cell & other) const;

   /// the Quad_CR representation of this cell
   virtual PrintBuffer character_representation(const PrintContext &pctx) const;

#ifndef __LIBAPL__
 protected:   // public: in libapl.cc
#endif // __LIBAPL__

   /// initialize the (un-initialized) Cell *Z to Unicode uni
  static ErrorCode zU(Cell * Z, Unicode uni)
      { new (Z) CharCell(uni);   return E_NO_ERROR; }

protected:
   /// overloaded Cell::get_cell_type()
   virtual CellType get_cell_type() const
      { return CT_CHAR; }

   /// overloaded Cell::get_cell_subtype()
   virtual CellType get_cell_subtype() const;

   /// overloaded Cell::get_char_value()
   virtual Unicode get_char_value() const   { return value.aval; }

   /// overloaded Cell::get_byte_value()
   virtual int get_byte_value() const;

   /// overloaded Cell::get_classname()
   virtual const char * get_classname()  const   { return "CharCell"; }

   /// overloaded Cell::CDR_size()
   virtual int CDR_size() const;

   /// overloaded Cell::bif_not_bitwise()
   virtual ErrorCode bif_not_bitwise(Cell * Z) const;

   /// overloaded Cell::bif_and_bitwise()
   virtual ErrorCode bif_and_bitwise(Cell * Z, const Cell * A) const;

   /// overloaded Cell::bif_and_bitwise()
   virtual ErrorCode bif_or_bitwise(Cell * Z, const Cell * A) const;

   /// overloaded Cell::bif_equal_bitwise()
   virtual ErrorCode bif_equal_bitwise(Cell * Z, const Cell * A) const;

   /// overloaded Cell::bif_not_equal_bitwise()
   virtual ErrorCode bif_not_equal_bitwise(Cell * Z, const Cell * A) const;
};
//----------------------------------------------------------------------------

#endif // __CHARCELL_HH_DEFINED__
