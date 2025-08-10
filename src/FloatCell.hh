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

#ifndef __FLOATCELL_HH_DEFINED__
#define __FLOATCELL_HH_DEFINED__

#include "Common.hh"   // for cfg_RATIONAL_NUMBERS_WANTED
#include "IntCell.hh"
#include "RealCell.hh"

//----------------------------------------------------------------------------
/**
 A cell containing a single APL floating point value.  This class overloads
 certain functions in class Cell with floating point number specific
 implementations.

 The actual APL floating point value is either rational (and then defined by
 value.numerator ÷ value.fval.denominator, or not (and then
 value.fval.denominator is 0 and value.fval.u1.flt contains the double value)
 */
/// A Cell containing a single floating point (or rational) value
class FloatCell : public RealCell
{
   friend class ComplexCell;   // for zF() and zR()
   friend class IntCell;       // for zF() and zR()
   friend class NumericCell;   // for zF() and zR()
   friend class RealCell;      // for zF() and zR()
   friend class Value;         // for zF() and zR()

public:
   /// Construct an floating point cell from a double \b r.
   FloatCell(APL_Float r)
      { value.fval.u1.flt = r;   value.fval.denominator = 0; }

#ifdef cfg_RATIONAL_NUMBERS_WANTED
   /// Construct an floating point cell from a quotient of integers. The caller
   /// must ensure that denom > 0 and that common divisors have been removed!
   FloatCell(APL_Integer numer, APL_Integer denom)
      {
        Assert1(denom > 0);
        value.fval.u1.num = numer;
        value.fval.denominator = denom;
      }

   /// overloaded Cell::init_other
   virtual void init_other(void * other, Value & cell_owner,
                           const char * loc) const
      { if (value.fval.denominator)
           new (other)   FloatCell(value.fval.u1.num,
                                   value.fval.denominator);
        else
           new (other)   FloatCell(value.fval.u1.flt);
      }

   /// return the numerator for rational numbers, undefined for others)
   virtual APL_Integer get_numerator() const
      { return value.fval.u1.num; }

   /// return the denominator (> 0 for rational numbers, 0 for others)
   virtual APL_Integer get_denominator() const
      { return value.fval.denominator; }

   /// return the value of \b this cell as a double (even if the value is
   /// rational)
   APL_Float dfval() const
      {
        if (const APL_Integer denom = get_denominator())
           return (1.0 * get_numerator() / denom);
        return value.fval.u1.flt;
      }

# if APL_Float_is_class
   /// overloaded Cell::release()
   virtual void release(const char * loc)
      {
        if (get_denominator() == 0)   // APL_Float class used
           release_APL_Float(value.fval.u1.flt.pAPL_Float());
      }
# endif

#else // not cfg_RATIONAL_NUMBERS_WANTED
   /// overloaded Cell::init_other
   virtual void init_other(void * other, Value & cell_owner, const char * loc)
      const { new (other)   FloatCell(dfval()); }

   /// return the value of \b this cell as a double (even if the value is
   /// rational)
   APL_Float dfval() const
      { return value.fval.u1.flt; }

# if APL_Float_is_class
   /// overloaded Cell::release()
   virtual void release(const char * loc)
      { release_APL_Float(value.fval.u1.flt.pAPL_Float()); }
# endif

#endif

   /// Overloaded Cell::is_float_cell().
   virtual bool is_float_cell()     const   { return true; }

   /// Overloaded Cell::is_finite().
   virtual bool is_finite() const
      { return isfinite(dfval()); }

   /// Overloaded Cell::greater().
   virtual bool greater(const Cell & other) const;

   /// Overloaded Cell::equal().
   virtual bool equal(const Cell & other, double qct) const;

   /// Overloaded Cell::bif_add().
   virtual ErrorCode bif_add(Cell * Z, const Cell * A) const;

  /// Overloaded from the corresponding Cell:: function (see class Cell).
   virtual ErrorCode bif_ceiling(Cell * Z) const;

  /// Overloaded from the corresponding Cell:: function (see class Cell).
   virtual ErrorCode bif_conjugate(Cell * Z) const;

  /// Overloaded from the corresponding Cell:: function (see class Cell).
   virtual ErrorCode bif_direction(Cell * Z) const;

   /// Overloaded Cell::bif_divide().
   virtual ErrorCode bif_divide(Cell * Z, const Cell * A) const;

  /// Overloaded from the corresponding Cell:: function (see class Cell).
   virtual ErrorCode bif_exponential(Cell * Z) const;

  /// Overloaded from the corresponding Cell:: function (see class Cell).
   virtual ErrorCode bif_factorial(Cell * Z) const;

  /// Overloaded from the corresponding Cell:: function (see class Cell).
   virtual ErrorCode bif_floor(Cell * Z) const;

  /// Overloaded from the corresponding Cell:: function (see class Cell).
   virtual ErrorCode bif_magnitude(Cell * Z) const;

   /// Overloaded Cell::bif_multiply().
   virtual ErrorCode bif_multiply(Cell * Z, const Cell * A) const;

   /// Overloaded Cell::bif_power().
   virtual ErrorCode bif_power(Cell * Z, const Cell * A) const;

  /// Overloaded from the corresponding Cell:: function (see class Cell).
   virtual ErrorCode bif_nat_log(Cell * Z) const;

  /// Overloaded from the corresponding Cell:: function (see class Cell).
   virtual ErrorCode bif_negative(Cell * Z) const;

  /// Overloaded from the corresponding Cell:: function (see class Cell).
   virtual ErrorCode bif_pi_times(Cell * Z) const;

  /// Overloaded from the corresponding Cell:: function (see class Cell).
   virtual ErrorCode bif_pi_times_inverse(Cell * Z) const;

  /// Overloaded from the corresponding Cell:: function (see class Cell).
   virtual ErrorCode bif_reciprocal(Cell * Z) const;

  /// Overloaded from the corresponding Cell:: function (see class Cell).
   virtual ErrorCode bif_roll(Cell * Z) const;

   /// compare this with other, throw DOMAIN ERROR on illegal comparisons
   virtual Comp_result compare(const Cell & other) const;

  /// Overloaded from the corresponding Cell:: function (see class Cell).
   virtual ErrorCode bif_maximum(Cell * Z, const Cell * A) const;

  /// Overloaded from the corresponding Cell:: function (see class Cell).
   virtual ErrorCode bif_minimum(Cell * Z, const Cell * A) const;

   /// Overloaded from the corresponding Cell:: function (see class Cell).
   virtual ErrorCode bif_residue(Cell * Z, const Cell * A) const;

   /// Overloaded Cell::bif_subtract().
   virtual ErrorCode bif_subtract(Cell * Z, const Cell * A) const;

   /// the Quad_CR representation of this cell.
   virtual PrintBuffer character_representation(const PrintContext &pctx) const;

   /// return true iff this cell needs scaling (exponential format) in pctx.
   virtual bool need_scaling(const PrintContext &pctx) const
      { return need_scaling(dfval(), pctx.get_PP()); }

   /// overloaded Cell::bif_add_inverse()
   virtual ErrorCode bif_add_inverse(Cell * Z, const Cell * A) const;

   /// overloaded Cell::bif_multiply_inverse()
   virtual ErrorCode bif_multiply_inverse(Cell * Z, const Cell * A) const;

   /// return true if the integer part of val is longer than ⎕PP
   static bool is_big(APL_Float val, int quad_pp);

   /// Return true iff the (fixed point) floating point number num shall
   /// be printed in scaled form (like 1.2E6).
   static bool need_scaling(APL_Float val, int quad_pp);

   /// replace normal chars by special chars specified in ⎕FC
   static void map_FC(UCS_string & ucs);

   /// greatest common divisor, Knuth Vol. 1 p. 14
   static APL_Integer gcd(APL_Integer m, APL_Integer n)
      {
        if (m == 0)   return n;
        if (n == 0)   return m;

        if (m < 0)   m = -m;
        if (n < 0)   n = -n;

        // E1: Initialize
        APL_Integer a_ = 1;
        APL_Integer b  = 1;
        APL_Integer a  = 0;
        APL_Integer b_ = 0;
        APL_Integer c  = m;
        APL_Integer d  = n;
        for (;;)
           {
             // E2: divide
             const APL_Integer r = c%d;

             // E3: remainder zero?
             if (r == 0)   return d;

             const APL_Integer q = c/d;   // from E2

             // E4: recycle
             const APL_Integer ta_ = a_;
             const APL_Integer tb_ = b_;
             c  = d;
             d  = r;
             a_ = a;
             a  = ta_ - q*a;
             b_ = b;
             b  = tb_ - q*b;
           }
      }

#ifndef __LIBAPL__
 protected:   // public: in libapl.cc
#endif

   /// initialize the (un-initialized) Cell *Z to APL_Float flt
   static ErrorCode zF(Cell * Z, APL_Float flt)
      { new (Z) FloatCell(flt);   return E_NO_ERROR; }

protected:
#ifdef cfg_RATIONAL_NUMBERS_WANTED
   /// initialize the (un-initialized) Cell *Z to APL_Float numer ÷ denom)
   static ErrorCode zR(Cell * Z, APL_Integer numer, APL_Integer denom)
      { new (Z) FloatCell(numer, denom);   return E_NO_ERROR; }
#endif

   ///  Overloaded Cell::get_cell_type().
   virtual CellType get_cell_type() const
      { return CT_FLOAT; }

   /// Overloaded Cell::get_real_value().
   virtual APL_Float get_real_value() const   { return dfval();  }

   /// Overloaded Cell::get_imag_value().
   virtual APL_Float get_imag_value() const   { return 0.0;  }

   /// Overloaded Cell::get_complex_value()
   virtual APL_Complex get_complex_value() const
      { return APL_Complex(dfval(), 0.0); }

   /// Overloaded Cell::get_near_bool().
   virtual bool get_near_bool()  const;

   /// Overloaded Cell::get_near_int().
   virtual APL_Integer get_near_int()  const
      { return near_int(dfval()); }

   /// Overloaded Cell::get_checked_near_int().
   virtual APL_Integer get_checked_near_int()  const
      { 
        if (dfval() < 0.0)   return APL_Integer(dfval() - 0.3);
        else                 return APL_Integer(dfval() + 0.3);
      }

   /// Overloaded Cell::is_near_int().
   virtual bool is_near_int() const
      { return Cell::is_near_int(dfval()); }

   /// Overloaded Cell::is_near_int64_t().
   virtual bool is_near_int64_t() const
      { return Cell::is_near_int64_t(dfval()); }

   /// overloaded Cell::bif_near_int()
   virtual ErrorCode bif_near_int64_t(Cell * Z) const;

   /// overloaded Cell::bif_within_quad_CT()
   virtual ErrorCode bif_within_quad_CT(Cell * Z) const;

   /// Overloaded Cell::is_near_zero().
   virtual bool is_near_zero() const
      { return dfval() >= -INTEGER_TOLERANCE
            && dfval() <   INTEGER_TOLERANCE; }

   /// Overloaded Cell::is_near_one().
   virtual bool is_near_one() const
      { return dfval() >= (1.0 - INTEGER_TOLERANCE)
            && dfval() <  (1.0 + INTEGER_TOLERANCE); }

   /// Overloaded Cell::is_near_real().
   virtual bool is_near_real() const
      { return true; }

   /// Overloaded Cell::get_classname().
   virtual const char * get_classname()   const   { return "FloatCell"; }

   /// Overloaded Cell::CDR_size()
   virtual int CDR_size() const { return 8; }

   /// downcast to const FloatCell
   virtual const FloatCell & cFloatCell() const   { return *this; }

   /// downcast to FloatCell
   virtual FloatCell & vFloatCell()   { return *this; }
};
//============================================================================

#endif // __FLOATCELL_HH_DEFINED__
