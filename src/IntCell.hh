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

#ifndef __INTCELL_HH_DEFINED__
#define __INTCELL_HH_DEFINED__

#include "Common.hh"   // for cfg_RATIONAL_NUMBERS_WANTED
#include "RealCell.hh"

//----------------------------------------------------------------------------
/*!
    A cell containing a single APL integer.  This class essentially
    overloads certain functions in class Cell with integer specific
    implementations.
 */
/// A cell containing a single Integer value
class IntCell : public RealCell
{
   friend class Cell;          // for zI() and friends
   friend class ComplexCell;   // for zI() and friends
   friend class FloatCell;     // for zI() and friends
   friend class NumericCell;   // for zI() and friends
   friend class PointerCell;   // for zI() and friends
   friend class RealCell;      // for zI() and friends
   friend class Quad_EX;       // for zI() and friends
   friend class Value;         // for zI() and friends

public:
   /// Construct an integer cell with value \b 0
   IntCell()
      { value.ival = 0;  }

   /// Construct an integer cell with value \b i
   IntCell(APL_Integer i)
      { value.ival = i;  }

   /// overloaded Cell::init_other
   virtual void init_other(void * other, Value & cell_owner, const char * loc)
      const { new (other)   IntCell(value.ival); }

   /// overloaded Cell::is_integer_cell()
   virtual bool is_integer_cell() const
      { return true; }

   /// overloaded Cell::bif_near_int64_t()
   virtual ErrorCode bif_near_int64_t(Cell * Z) const;

   /// overloaded Cell::bif_within_quad_CT()
   virtual ErrorCode bif_within_quad_CT(Cell * Z) const;

   /// overloaded Cell::greater()
   virtual bool greater(const Cell & other) const;

   /// overloaded Cell::equal()
   virtual bool equal(const Cell & other, double qct) const;

   /// overloaded Cell::bif_add()
   virtual ErrorCode bif_add(Cell * Z, const Cell * A) const;

   /// overloaded from the corresponding Cell:: function (see class Cell)
   virtual ErrorCode bif_ceiling(Cell * Z) const;

   /// overloaded from the corresponding Cell:: function (see class Cell)
   virtual ErrorCode bif_conjugate(Cell * Z) const;

   /// overloaded from the corresponding Cell:: function (see class Cell)
   virtual ErrorCode bif_direction(Cell * Z) const;

   /// overloaded Cell::bif_divide()
   virtual ErrorCode bif_divide(Cell * Z, const Cell * A) const;

   /// overloaded from the corresponding Cell:: function (see class Cell)
   virtual ErrorCode bif_exponential(Cell * Z) const;

   /// overloaded from the corresponding Cell:: function (see class Cell)
   virtual ErrorCode bif_factorial(Cell * Z) const;

   /// overloaded from the corresponding Cell:: function (see class Cell)
   virtual ErrorCode bif_floor(Cell * Z) const;

   /// overloaded from the corresponding Cell:: function (see class Cell)
   virtual ErrorCode bif_magnitude(Cell * Z) const;

   /// overloaded Cell::bif_multiply()
   virtual ErrorCode bif_multiply(Cell * Z, const Cell * A) const;

   /// overloaded Cell::bif_power()
   virtual ErrorCode bif_power(Cell * Z, const Cell * A) const;

   /// overloaded from the corresponding Cell:: function (see class Cell)
   virtual ErrorCode bif_nat_log(Cell * Z) const;

   /// overloaded from the corresponding Cell:: function (see class Cell)
   virtual ErrorCode bif_negative(Cell * Z) const;

   /// overloaded from the corresponding Cell:: function (see class Cell)
   virtual ErrorCode bif_pi_times(Cell * Z) const;

   /// overloaded from the corresponding Cell:: function (see class Cell)
   virtual ErrorCode bif_pi_times_inverse(Cell * Z) const;

   /// overloaded from the corresponding Cell:: function (see class Cell)
   virtual ErrorCode bif_reciprocal(Cell * Z) const;

   /// overloaded from the corresponding Cell:: function (see class Cell)
   virtual ErrorCode bif_roll(Cell * Z) const;

   /// overloaded Cell::bif_subtract()
   virtual ErrorCode bif_subtract(Cell * Z, const Cell * A) const;

   /// compare this with other, throw DOMAIN ERROR on illegal comparisons
   virtual Comp_result compare(const Cell & other) const;

   /// overloaded from the corresponding Cell:: function (see class Cell)
   virtual ErrorCode bif_maximum(Cell * Z, const Cell * A) const;

   /// overloaded from the corresponding Cell:: function (see class Cell)
   virtual ErrorCode bif_minimum(Cell * Z, const Cell * A) const;

   /// overloaded from the corresponding Cell:: function (see class Cell)
   virtual ErrorCode bif_residue(Cell * Z, const Cell * A) const;

   /// the Quad_CR representation of this cell
   virtual PrintBuffer character_representation(const PrintContext &pctx) const;

   /// return true iff this cell needs scaling (exponential format) in pctx
   /// According to lrm p. 13, integer cells ignore ⎕PP an never use scaling
   virtual bool need_scaling(const PrintContext &pctx) const
      { return false; }

#ifdef cfg_RATIONAL_NUMBERS_WANTED
   /// overloaded Cell::get_numerator()
   virtual APL_Integer get_numerator() const
      { return value.ival; }

   /// overloaded Cell::get_denominator()
   virtual APL_Integer get_denominator() const
      { return 1; }
#endif

   /// overloaded Cell::bif_add_inverse()
   virtual ErrorCode bif_add_inverse(Cell * Z, const Cell * A) const;

   /// overloaded Cell::bif_multiply_inverse()
   virtual ErrorCode bif_multiply_inverse(Cell * Z, const Cell * A) const;

   /// swap \b this Intcell and \b other (for Heapsort<IntCell> )
   void swap_ivals(IntCell & other)
      {
         const APL_Integer tmp = value.ival;
         value.ival = other.value.ival;
         other.value.ival = tmp;
      }

   /// overloaded Cell::get_int_value()
   virtual APL_Integer get_int_value()  const   { return value.ival; }

   /// set the integer value of this IntCell
   void set_int_value(APL_Integer val)
     { value.ival = val; }

   /// downcast to const IntCell
   virtual const IntCell & cIntlCell() const   { return *this; }

   /// downcast to IntCell
   virtual IntCell & wIntCell()   { return *this; }

   /// overloaded Cell::get_byte_value()
   virtual int get_byte_value() const;

   /// \b false (for packed Cells)
   static const IntCell boolean_FALSE;

   /// \b true (for packed Cells)
   static const IntCell boolean_TRUE;

#ifndef __LIBAPL__
 protected:   // public: in libapl.cc
#endif // __LIBAPL__

   /// initialize Z to APL_Integer v
   static ErrorCode zI(Cell * Z, APL_Integer aint)
      { new (Z) IntCell(aint);   return E_NO_ERROR; }

protected:
   /// initialize the (un-initialized) Cell *Z to integer 0
   static ErrorCode z0(Cell * Z)
      { new (Z) IntCell();   return E_NO_ERROR; }

   /// initialize the (un-initialized) Cell *Z to integer 1
   static ErrorCode z1(Cell * Z)
      { new (Z) IntCell(1);   return E_NO_ERROR; }

   /// initialize the (un-initialized) Cell *Z to integer ¯1
   static ErrorCode z_1(Cell * Z)
      { new (Z) IntCell(-1);   return E_NO_ERROR; }

   /// overloaded Cell::get_cell_type()
   virtual CellType get_cell_type() const
      { return CT_INT; }

   /// overloaded Cell::get_cell_subtype()
   virtual CellType get_cell_subtype() const;

   /// overloaded Cell::get_real_value()
   virtual APL_Float get_real_value() const
      { return APL_Float(value.ival);  }

   /// overloaded Cell::get_imag_value()
   virtual APL_Float get_imag_value() const
      { return 0.0;  }

   /// overloaded Cell::get_complex_value()
   virtual APL_Complex get_complex_value() const
      { return APL_Complex(value.ival, 0.0); }

   /// overloaded Cell::get_near_bool()
   virtual bool get_near_bool()  const
      { if (value.ival == 0)   return false;
        if (value.ival == 1)   return true;
        DOMAIN_ERROR; }

   /// overloaded Cell::get_near_int()
   virtual APL_Integer get_near_int()  const
      { return value.ival; }

   /// overloaded Cell::get_checked_near_int()
   virtual APL_Integer get_checked_near_int()  const
      { return value.ival; }

   /// overloaded Cell::is_near_zero()
   virtual bool is_near_zero() const
      { return value.ival == 0; }

   /// overloaded Cell::is_near_one()
   virtual bool is_near_one() const
      { return value.ival == 1; }

   /// overloaded Cell::is_near_int()
   virtual bool is_near_int() const
      { return true; }

   /// overloaded Cell::is_near_int64_t()
   virtual bool is_near_int64_t() const
      { return true; }

   /// overloaded Cell::is_near_real()
   virtual bool is_near_real() const
      { return true; }

   /// overloaded Cell::get_classname()
   virtual const char * get_classname() const   { return "IntCell"; }

   /// overloaded Cell::CDR_size()
   virtual int CDR_size() const;
};
//----------------------------------------------------------------------------
inline void
Hswap(IntCell & c1, IntCell & c2)
{
   c1.swap_ivals(c2);
}
//----------------------------------------------------------------------------

#endif // __INTCELL_HH_DEFINED__
