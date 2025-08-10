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

#ifndef __CELL_HH_DEFINED__
#define __CELL_HH_DEFINED__

#include <complex>

#include "Common.hh"
#include "ErrorCode.hh"
#include "PrintBuffer.hh"
#include "Value_P.hh"

class CharCell;
class ComplexCell;
class FloatCell;
class IntCell;
class LvalCell;
class PointerCell;

//----------------------------------------------------------------------------
/**
 **   Base class for one item of an APL ravel. The item is one of the following:
 **
 **  A character scalar
 **  An integer scalar
 **  A floating point scalar
 **  A complex number scalar
 **  An APL array
 **
 **   The kind of item is defined by its class, which is derived from Cell.
 **/
/// The base class for all Cells
class Cell
{
public:
   /// Construct an un-initialized Cell
   Cell() {}

   /// init \b this Cell from a (possibly deep) copy of \b other
   void init(const Cell & other, Value & this_owner, const char * loc)
      { other.init_other(this, this_owner, loc); }

   /// init (uninitialized) Cell \b other from \b this initialized cell
   virtual void init_other(void * other, Value & other_owner,
                           const char * loc) const
      { Assert(0 && "Cell::init_other() called on base class"); }

   /// init this Cell from value. If value is a scalar then its first element
   /// is used (and value is erased). Otherwise a PointerCell is created.
   void init_from_value(Value * value, Value & cell_owner, const char * loc);

   /// Return \b true if \b this cell is greater than \b other, with:
   /// 1. PointerCell > NumericCell > CharCell
   /// 2a. NumericCells sorted by get_real_value(). then by get_imag_value()
   /// 2b. CharCells sorted by get_char_value()
   /// 2c. PointerCells sorted by rank, then by shape, then by ravel
   virtual bool greater(const Cell & other) const;

   /// like greater() but static
   static bool A_greater_B(const Cell * const & A, const Cell * const & B,
                           const void * unused_comp_arg);

   /// return \b true if \b this cell is equal to \b other
   virtual bool equal(const Cell & other, double qct) const;

   /// return pointer value of a PointerCell or create a scalar with a
   /// copy of this cell.
   Value_P to_value(const char * loc) const;

   /// init \b this cell to be the type of \b other
   void init_type(const Cell & other, Value & cell_owner, const char * loc);

   /// ISO p.15: return \b true if A and B are on the same half-plane
   static bool same_half_plane(APL_Complex A, APL_Complex B);

   /// ISO p.19: return \b true if real A is tolerantly equal to real B within C
   static bool tolerantly_equal(APL_Float A, APL_Float B, double qct);

   /// ISO p. 19: return \b true if complex A and B are tolerantly equal
   /// within real qct
   static bool tolerantly_equal(APL_Complex A, APL_Complex B, double qct);

   /// ISO p. 19: A is integral (close to a Gaussian Integer) within qct
   static bool integral_within(APL_Float A, double qct);

   /// Return the character value of a cell
   virtual Unicode get_char_value() const;

   /// Return the byte value (-128..255 incl) of a cell
   virtual int get_byte_value() const   { DOMAIN_ERROR; }

   /// Return the integer value of a cell
   virtual APL_Integer get_int_value() const   { DOMAIN_ERROR; }

   /// Return the real part of a cell
   virtual APL_Float get_real_value() const   { DOMAIN_ERROR; }

   /// Return the imaginary part of a cell
   virtual APL_Float get_imag_value() const   { DOMAIN_ERROR; }

   /// Return the complex value of a cell
   virtual APL_Complex get_complex_value() const   { DOMAIN_ERROR; }

   /// Return the APL value of a cell (Asserts for non-pointer cells)
   virtual Value_P get_pointer_value()  const;

   /// Return the APL value of a cell (Asserts for non-lval cells)
   virtual Cell * get_lval_value() const   { LEFT_SYNTAX_ERROR; }

   /// the Quad_CR representation of this cell
   virtual PrintBuffer character_representation(const PrintContext &pctx) const
      { DOMAIN_ERROR; }

   /// return true if this cell needs scaling (exponential format) in pctx
   virtual bool need_scaling(const PrintContext &pctx) const
      { return false; }

   /// Return value if it is close to boolean, or else throw DOMAIN_ERROR
   virtual bool get_near_bool()  const
      { DOMAIN_ERROR; }

   /// Return value if it is close to int, or else throw DOMAIN_ERROR
   virtual APL_Integer get_near_int()  const
      { DOMAIN_ERROR; }

   /// Return value if it is (known to be) close to int, or else Assert()
   virtual APL_Integer get_checked_near_int()  const
      { NeverReach("Value is not an integer"); }

   /// return true iff value is numeric and close to 0
   virtual bool is_near_zero() const
      { return false; }

   /// return true iff value is numeric and close to 1
   virtual bool is_near_one() const
      { return false; }

   /// return true iff value is numeric and close to 0 or 1
   bool is_near_bool() const
      { return is_near_zero() || is_near_one(); }

   /// True iff value is numeric and close to an int
   virtual bool is_near_int() const
      { return false; }

   /// True iff value is numeric and close to an int
   virtual bool is_near_int64_t() const
      { return false; }

   /// return true iff value is numeric and close to a real number
   virtual bool is_near_real() const
      { return false; }

   /// return the minimum number of data bytes to store this cell in
   /// CDR format. The actual number of data bytes can be bigger if other
   /// cells need more bytes,
   virtual int CDR_size() const
      { NeverReach("CDR_size called on Cell base class"); }

   /// Release content pointed to (complex, APL value)
   virtual void release(const char * loc) {}

   /// Return \b true iff \b this cell is an integer cell
   virtual bool is_integer_cell() const
      { return false; }

   /// Return \b true iff \b this cell is a numeric cor character cell
   bool is_simple_cell() const
      { return (get_cell_type() & CT_SIMPLE) != 0; }

   /// Return \b true iff \b this cell is a floating point cell
   virtual bool is_float_cell() const
      { return false; }

   /// Return \b true iff \b this cell is a character cell
   virtual bool is_character_cell() const
      { return false; }

   /// Return \b true iff \b this cell is a pointer (to a sub-array) cell
   virtual bool is_pointer_cell() const
      { return false; }

   /// Return \b true iff \b this cell is a pointer to a cell
   virtual bool is_lval_cell() const
      { return false; }

   /// Return \b true iff \b this cell is a pointer to a cell from pick()
   virtual bool is_picked_lval_cell() const
      { return false; }

   /// Return \b true iff \b this cell is a complex number cell
   virtual bool is_complex_cell() const
      { return false; }

   /// Return \b true iff \b this cell is an integer, float, or complex cell
   virtual bool is_numeric() const
      { return false; }

   /// Return \b true iff \b this cell is an integer or float cell
   virtual bool is_real_cell() const   // int or flt
      { return false; }

   /// Return \b true unless \b this cell contains infinity or NaN
   virtual bool is_finite() const
      { return true; }

   /// Return \b true iff \b this cell is an example field character cell
   virtual bool is_example_field() const
      { return false; }

   /// Return true iff this is a PointerCell which pouints to a member value
   virtual bool is_member_anchor() const
      { return false; }

   /// A union containing all possible cell values for the different Cell types
   union SomeValue
      {
        Unicode        aval;      ///< for CharCell
        APL_Float_Base cval[2];   ///< for ComplexCell
        ErrorCode      eval;      ///< an error code
        APL_Integer    ival;      ///< for IntCell

        struct _fval              ///< for FloatCell
           {
             /// either a floating point value, or the denominator of a quotient
             union _flt_num
                {
                  APL_Float_Base flt;   ///< the non-rational value
                  APL_Integer    num;   ///< the numerator of the quotient
                } u1;                   ///< primary value, 27 ⎕CR

             /// 0 for non-rational valued, or the denominator of a quotient
             APL_Integer denominator;
           }           fval;        ///< a rational or floating point value
        Cell          *lval;        ///< for LvalCell (selective assignment)

        /// a pointer to, and the owner of, a nested APL (sub-) value
        struct _pval
           {
             Value_P_Base valp;     ///< smart pointer to a value
             Value       *owner;    ///< the value that contains valp
           } pval;                  ///< for PointerCell
      };

   /// return the type of \b this cell
   virtual CellType get_cell_type() const
      { return CT_BASE; }

   /// return the subtype of \b this cell. The subtype is the set of containers
   /// that can store the cell (only for Int and Char cells).
   virtual CellType get_cell_subtype() const
      { return get_cell_type(); }

   /// return the name of the class
   virtual const char * get_classname()  const   { return "Cell"; }

   /// round the value of this Cell up and store the result in Z
   virtual ErrorCode bif_ceiling(Cell * Z) const
      { return E_DOMAIN_ERROR; }

   /// conjugate the value of this Cell and store the result in Z
   virtual ErrorCode bif_conjugate(Cell * Z) const
      { return E_DOMAIN_ERROR; }

   /// store the direction (sign) of this Cell in Z
   virtual ErrorCode bif_direction(Cell * Z) const
      { return E_DOMAIN_ERROR; }

   /// store e to the power of the value of this Cell in Z
   virtual ErrorCode bif_exponential(Cell * Z) const
      { return E_DOMAIN_ERROR; }

   /// store the factorial (N!) of the value of this Cell in Z
   virtual ErrorCode bif_factorial(Cell * Z) const
      { return E_DOMAIN_ERROR; }

   /// Round the value of this Cell down and store the result in Z
   virtual ErrorCode bif_floor(Cell * Z) const
      { return E_DOMAIN_ERROR; }

   /// store the absolute value of this Cell in Z
   virtual ErrorCode bif_magnitude(Cell * Z) const
      { return E_DOMAIN_ERROR; }

   /// store the base e logarithm of the value of this Cell in Z
   virtual ErrorCode bif_nat_log(Cell * Z) const
      { return E_DOMAIN_ERROR; }

   /// store the negative of the value of this Cell in Z
   virtual ErrorCode bif_negative(Cell * Z) const
      { return E_DOMAIN_ERROR; }

   /// store the logical complement of the value of this Cell in Z
   virtual ErrorCode bif_not(Cell * Z) const
      { return E_DOMAIN_ERROR; }

   /// store the bitwise complement of the value of this Cell in Z
   virtual ErrorCode bif_not_bitwise(Cell * Z) const
      { return E_DOMAIN_ERROR; }

   /// store the nearby integer of the value of this Cell in Z
   virtual ErrorCode bif_near_int64_t(Cell * Z) const
      { return E_DOMAIN_ERROR; }

   /// store the nearby integer of the value of this Cell in Z
   virtual ErrorCode bif_within_quad_CT(Cell * Z) const
      { return E_DOMAIN_ERROR; }

   /// store pi times the value of this Cell in Z
   virtual ErrorCode bif_pi_times(Cell * Z) const
      { return E_DOMAIN_ERROR; }

   /// store pi times the value of this Cell in Z
   virtual ErrorCode bif_pi_times_inverse(Cell * Z) const
      { return E_DOMAIN_ERROR; }

   /// store the reciprocal of the value of this Cell in Z
   virtual ErrorCode bif_reciprocal(Cell * Z) const
      { return E_DOMAIN_ERROR; }

   /// store a random number between 1 and the value of this Cell in Z
   virtual ErrorCode bif_roll(Cell * Z) const
      { return E_DOMAIN_ERROR; }

   /// store the sum of A and \b this cell in Z
   virtual ErrorCode bif_add(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// store the logical and of A and \b this cell in Z
   virtual ErrorCode bif_and(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// store the bitwise and of A and \b this cell in Z
   virtual ErrorCode bif_and_bitwise(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// store the binomial (n over k) of A and \b this cell in Z
   virtual ErrorCode bif_binomial(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// store the difference between A and \b this cell in Z
   virtual ErrorCode bif_subtract(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// store the quotient between A and \b this cell in Z
   virtual ErrorCode bif_divide(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// compare this with other, throw DOMAIN ERROR on illegal comparisons
   virtual Comp_result compare(const Cell & other) const
      { DOMAIN_ERROR; }

   /// store 1 in Z if A == the value of \b this cell in Z, else 0
   ErrorCode bif_equal(Cell * Z, const Cell * A) const;

   /// bitwise comparison of *this and A
   virtual ErrorCode bif_equal_bitwise(Cell * Z, const Cell * A) const
      { DOMAIN_ERROR; }

   /// bitwise comparison of *this and A
   virtual ErrorCode bif_not_equal_bitwise(Cell * Z, const Cell * A) const
      { DOMAIN_ERROR; }

   /// store 1 in Z if A != the value of \b this cell in Z, else 0
   ErrorCode bif_not_equal(Cell * Z, const Cell * A) const;

   /// store 1 in Z if A > the value of \b this cell in Z, else 0
   ErrorCode bif_greater_than(Cell * Z, const Cell * A) const;

   /// store 1 in Z if A <= the value of \b this cell in Z, else 0
   ErrorCode bif_less_eq(Cell * Z, const Cell * A) const;

   /// store 1 in Z if A < the value of \b this cell in Z, else 0
   ErrorCode bif_less_than(Cell * Z, const Cell * A) const;

   /// store 1 in Z if A is >= the value of \b this cell in Z, else 0
   ErrorCode bif_greater_eq(Cell * Z, const Cell * A) const;

   /// store the base A logarithm of \b this cell in Z
   virtual ErrorCode bif_logarithm(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// store the larger of A and of \b this cell in Z
   virtual ErrorCode bif_maximum(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// store the smaller of A and of \b this cell in Z
   virtual ErrorCode bif_minimum(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// store the product of A and \b this cell in Z
   virtual ErrorCode bif_multiply(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// store the logical nand of A and \b this cell in Z
   virtual ErrorCode bif_nand(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// store the bitwise nand of A and \b this cell in Z
   virtual ErrorCode bif_nand_bitwise(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// store the logical NOR of A and \b this cell in Z
   virtual ErrorCode bif_nor(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// store the bitwise NOR of A and \b this cell in Z
   virtual ErrorCode bif_nor_bitwise(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// store the logical OR of A and \b this cell in Z
   virtual ErrorCode bif_or(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// store the bitwise or of A and \b this cell in Z
   virtual ErrorCode bif_or_bitwise(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// store A to the power of \b this cell in Z
   virtual ErrorCode bif_power(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// store A modulo \b this cell in Z
   virtual ErrorCode bif_residue(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// store a circle function (according to A) of \b this cell in Z
   virtual ErrorCode bif_circle_fun(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// store a circle function (according to A) of \b this cell in Z
   virtual ErrorCode bif_circle_fun_inverse(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

#ifdef cfg_RATIONAL_NUMBERS_WANTED
   /// return the numerator of a quotient
   virtual APL_Integer get_numerator() const   { FIXME }

   /// return the denominator of a quotient, or 0 for non-quotients
   virtual APL_Integer get_denominator() const { FIXME }
#endif

   /// the inverse of bif_add
   virtual ErrorCode bif_add_inverse(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// the inverse of bif_bif_multiply
   virtual ErrorCode bif_multiply_inverse(Cell * Z, const Cell * A) const
      { return E_DOMAIN_ERROR; }

   /// return \b true if z = a + b had an overflow.
   static bool sum_overflow(APL_Integer z, APL_Integer a, APL_Integer b)
      {
        if (z < 0)   return ((a | b) & 0x8000000000000000) == 0;
        else         return ((a & b) & 0x8000000000000000) != 0;
      }

   /// return \b true if z = a - b had an overflow.
   static bool diff_overflow(APL_Integer z, APL_Integer a, APL_Integer b)
      {
        if (z < 0)   return ((a | ~b) & 0x8000000000000000) == 0;
        else         return ((a & ~b) & 0x8000000000000000) != 0;
      }

   /// return \b true if multiplying a and b will (probably) overflow.
   /// For some huge a or b the result may incorrectly return true.
   /// a or b the result may incorrectly return true.
   static bool prod_overflow(int64_t a, int64_t b)
      {
        const int64_t prod = (a >> 4) * (b >> 4);
        return prod >=  0x0FFFFFFFFFFFFFFELL
            || prod <= -0x0FFFFFFFFFFFFFFELL;
      }

   /// placement new
   void * operator new(std::size_t, void *);

   /// copy (deep) count cells from src to dest)
   static void copy(Cell * & dst, const Cell * & src, ShapeItem count,
                    Value & cell_owner)
      { loop(c, count)   src++->init_other(dst++, cell_owner, LOC); }

   /// copy (deep) count cells from src to val (which is under construction))
   static void copy(Value & val, const Cell * & src, ShapeItem count);

   /// true iff value is close to an int (within +- qct)
   static bool is_near_int(APL_Float value);

   /// true iff value is close to an int64_t (i.e. fots into an int64_t
   static bool is_near_int64_t(APL_Float value);

   /// true iff value is close to 0 (within +- qct)
   static bool is_near_zero(APL_Float value)
      { return (value < INTEGER_TOLERANCE) && (value > -INTEGER_TOLERANCE); }

   /// return value if it is close to int, or else throw DOMAIN_ERROR
   static APL_Integer near_int(APL_Float value);

   /// compare cells[a] and cells[b] ascendingly
   static bool greater_cp(const ShapeItem & a, const ShapeItem & b,
                           const void * cells);

   /// compare cells[a] and cells[b] descendingly
   static bool smaller_cp(const ShapeItem & a, const ShapeItem & b,
                          const void * cells);

   /// raw pointer to the primary value (for 27 ⎕CR)
   const void * get_u0() const   { return &value.cval[0]; }

   /// raw pointer to the secondary value (for 28 ⎕CR)
   const void * get_u1() const   { return &value.cval[1]; }

   /// a stable compare function to be used with Heapsort. Equal cells
   /// are compared by their address. Return true iff A > B or A = B
   /// and &A > &B.
   static bool compare_stable(const Cell * const & A, const Cell * const & B,
                       const void * comp_arg);

   /// a stable compare function to be used with Heapsort. The cell contents
   /// are ignored and only the cell addresses are being compared. Return
   /// true iff &A > &B.
   static bool compare_ptr(const Cell * const & A, const Cell * const & B,
                          const void * comp_arg);

   /// return 0-based indices i1, i2, ... iN so that
   /// value[i1] < value[i2] < ... < value[iN].
   static ErrorCode sorted_indices(vector<ShapeItem> & indices,
                                     const Value & value, Sort_order order,
                                     ShapeItem comp_len);

   /// the name of \b ct
   static const char * get_cell_type_name(CellType ct);

protected:
   /// the primary value of \b this cell
   SomeValue value;

private:
   /** Cells that are allocated with new() shall always be contained in
       APL values (using placement new() on the ravel of the value.
       We prevent the accidental use of non-placement new() by defining
       it but not implementing it.
    **/
   void * operator new(std::size_t);

   /** Cells shalle never be overriden with other Cells since they may
       require their desctuctor to be called (e.g. PointerCell)
       We prevent the accidental use of non-placement new() by defining
       it but not implementing it.
    **/

   Cell & operator =(const Cell & other);
};
//----------------------------------------------------------------------------
inline void
Hswap(const Cell * & c1, const Cell * & c2)
{
const Cell * tmp = c1;   c1 = c2;   c2 = tmp;
}
//----------------------------------------------------------------------------
inline void
Hswap(Cell * & c1, Cell * & c2)
{
Cell * tmp = c1;   c1 = c2;   c2 = tmp;
}
//----------------------------------------------------------------------------

typedef ErrorCode (Cell::*prim_f1)(Cell *) const;
typedef ErrorCode (Cell::*prim_f2)(Cell *, const Cell *) const;

#endif // __CELL_HH_DEFINED__
