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

#ifndef __NUMERICCELL_HH_DEFINED__
#define __NUMERICCELL_HH_DEFINED__

#include "Cell.hh"

//----------------------------------------------------------------------------
/*! A cell that is either an integer cell, a floating point cell, or a
    complex number cell. This class contains all cell functions for which
    the detailed type makes no difference.
*/
/// Base class for RealCell and ComplexCell
class NumericCell : public Cell
{
public:
   /// initialize the (un-initialized) Cell *Z to (if possible) an IntCell
   /// or else to a FloatCell with value flt.
   static ErrorCode zV(Cell * Z, APL_Float flt);

   /// initialize the (un-initialized) Cell *Z to (if possible) an IntCell,
   /// a FloatCell, or a ComplexCell with value cpx.
   static ErrorCode zV(Cell * Z, APL_Complex cpx);

   /// initialize the (un-initialized) Cell *Z to (if possible) an IntCell,
   /// a FloatCell, or a ComplexCell with value cpx.
   static ErrorCode zV(Cell * Z, APL_Float real, APL_Float imag);

protected:
   /// overloaded Cell::is_numeric()
   virtual bool is_numeric() const
      { return true; }

   /// overloaded Cell::bif_not()
   virtual ErrorCode bif_not(Cell * Z) const;

   /// overloaded Cell::bif_not_bitwise()
   virtual ErrorCode bif_not_bitwise(Cell * Z) const;

   /// overloaded Cell::bif_and()
   virtual ErrorCode bif_and(Cell * Z, const Cell * A) const;

   /// overloaded Cell::bif_and_bitwise()
   virtual ErrorCode bif_and_bitwise(Cell * Z, const Cell * A) const;

   /// overloaded Cell::bif_equal_bitwise()
   virtual ErrorCode bif_equal_bitwise(Cell * Z, const Cell * A) const;

   /// overloaded Cell::bif_not_equal_bitwise()
   virtual ErrorCode bif_not_equal_bitwise(Cell * Z, const Cell * A) const;

   /// overloaded Cell::bif_binomial()
   virtual ErrorCode bif_binomial(Cell * Z, const Cell * A) const;

   /// compute binomial funtion for integers a and b
   static ErrorCode K33_binomial(Cell * Z, APL_Integer N,
                                 APL_Integer K, bool negate);

   /// overloaded Cell::bif_nand()
   virtual ErrorCode bif_nand(Cell * Z, const Cell * A) const;

   /// overloaded Cell::bif_nand_bitwise()
   virtual ErrorCode bif_nand_bitwise(Cell * Z, const Cell * A) const;

   /// overloaded Cell::bif_nor()
   virtual ErrorCode bif_nor(Cell * Z, const Cell * A) const;

   /// overloaded Cell::bif_nor_bitwise()
   virtual ErrorCode bif_nor_bitwise(Cell * Z, const Cell * A) const;

   /// overloaded Cell::bif_or()
   virtual ErrorCode bif_or(Cell * Z, const Cell * A) const;

   /// overloaded Cell::bif_or_bitwise()
   virtual ErrorCode bif_or_bitwise(Cell * Z, const Cell * A) const;

   /// overloaded Cell::get_classname()
   virtual const char * get_classname() const   { return "NumericCell"; }

   /// N over K for complex A and/or B
   ErrorCode complex_binomial(Cell * Z, const Cell * A) const;

   /// N over K for non-integer A and/or B
   ErrorCode real_binomial(Cell * Z, const Cell * A) const;

   /// return the greatest common divisor of integers a and b
   static ErrorCode int_gcd(APL_Integer & z, APL_Integer a, APL_Integer b);

   /// return the greatest common divisor of real a and b
   static ErrorCode flt_gcd(APL_Float & z, APL_Float a, APL_Float b,
                            double qct);

   /// N over K for positive integers N and K.
   static ErrorCode integer_binomial(Cell * Z, APL_Integer N, APL_Integer K,
                                     bool negate);

   /// return the greatest common divisor of complex a and b
   static ErrorCode cpx_gcd(APL_Complex & z, APL_Complex a, APL_Complex b,
                              double qct);

   /// multiply \b a by 1, -1, i, or -i so that a.real is maximal
   static APL_Complex cpx_max_real(APL_Complex a);
};
//----------------------------------------------------------------------------

#endif // __NUMERICCELL_HH_DEFINED__
