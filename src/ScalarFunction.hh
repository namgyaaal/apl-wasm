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

#ifndef __SCALAR_FUNCTION_HH_DEFINED__
#define __SCALAR_FUNCTION_HH_DEFINED__

#include "Id.hh"
#include "Parallel.hh"
#include "PrimitiveFunction.hh"
#include "Thread_context.hh"

#include "Value.hh"

//----------------------------------------------------------------------------
/// Base class for all scalar functions
class ScalarFunction : public PrimitiveFunction
{
   // ScalarFunction is only a helper class whose function shall not be
   // called directly but only via their virtual counterparts in base
   // class Function. We therefore delare all functions protected:
   //
protected:
   /// Construct a ScalarFunction with \b Id \b id
   ScalarFunction(TokenTag tag, CellFunctionStatistics * stat_AB,
                                CellFunctionStatistics * stat_B,
                                ShapeItem thresh_AB,
                                ShapeItem thresh_B)
   : PrimitiveFunction(tag, stat_AB, stat_B)
   {
     set_dyadic_threshold(thresh_AB);
     set_monadic_threshold(thresh_B);
   }

   /// Evaluate a scalar function monadically.
   Token eval_scalar_B(Value_P B, prim_f1 fun) const;

   /// Evaluate a scalar function dyadically.
   Token eval_scalar_AB(Value_P A, Value_P B, prim_f2 fun) const;

   /// overloaded Function::get_scalar_f2
   virtual prim_f2 get_scalar_f2() const = 0;

   /// overloaded Function::eval_fill_AB()
   virtual Token eval_fill_AB(Value_P A, Value_P B) const
      { return do_eval_fill_AB(A, B); }

   /// overloaded Function::eval_fill_B()
   virtual Token eval_fill_B(Value_P B) const
      { return do_eval_fill_B(B); }

   /// overloaded Function::has_result()
   virtual bool has_result() const   { return true; }

   /// return true if this function can be parallelized
   virtual bool may_parallel() const   { return true; }

   /// compute the monadic scalar function \b fun along one ravel
   Value_P do_scalar_B(ErrorCode & ec, Value_P B, prim_f1 fun) const;

   /// compute the dyadic scalar function \b fun along one ravel
   Value_P do_scalar_AB(ErrorCode & ec, Value_P A,
                                        Value_P B, prim_f2 fun) const;

   /// compute cell_A fun cell_B, scalar-extending PointerCells
   void expand_nested(Value * Z, const Cell * cell_A,
                      const Cell * cell_B, prim_f2 fun) const;

   /// Evaluate a scalar function dyadically with axis.
   Token eval_scalar_AXB(Value_P A, Value_P X, Value_P B, prim_f2 fun) const;

   /// A helper function for eval_scalar_AXB().
   Value_P eval_scalar_AXB(Value_P A, AxesBitmap axes_in_X,
                           Value_P B, prim_f2 fun, bool reversed) const;

   /// A helper function for eval_fill_AB().
   Token do_eval_fill_AB(const Value_P A, const Value_P B) const;

   /// A helper function for eval_fill_B().
   Token do_eval_fill_B(const Value_P B) const;

   /// Evaluate \b the identity function.
   static Token eval_scalar_identity_fun(Value_P B, sAxis axis,
                                         const Cell & FI0);

   /// parallel eval_scalar_AB
   static Thread_context::PoolFunction PF_scalar_AB;

   /// parallel eval_scalar_B
   static Thread_context::PoolFunction PF_scalar_B;

   /// 0 (for eval_scalar_identity_fun)
   static const IntCell integer_0;

   /// 1 (for eval_scalar_identity_fun)
   static const IntCell integer_1;

   /// -BIG_FLOAT (for eval_scalar_identity_fun)
   static const FloatCell float_min;

   /// BIG_FLOAT (for eval_scalar_identity_fun)
   static const FloatCell float_max;

   /// return the conforming shape of sh_A and sh_A, or set ec and return 0
   /// if shapes cannot be conformed.
   /// RANK ERROR or LENGTH ERROR if the dont.
   static const Shape * conforming_shape(ErrorCode & ec, const Shape & sh_A,
                                                         const Shape & sh_B);
};

#define PERF_A(x)   TOK_F2_ ## x,                           \
                  & Performance::cfs_F2_    ## x ## _AB, 0, \
                    Performance::thresh_F2_ ## x ## _AB, -1

#define PERF_AB(x)  TOK_F12_ ## x,                    \
                  & Performance::cfs_F12_ ## x ## _AB,    \
                  & Performance::cfs_F12_ ## x ## _B,     \
                    Performance::thresh_F12_ ## x ## _AB, \
                    Performance::thresh_F12_ ## x ## _B

#define PERF_B(x)   TOK_F12_ ## x,                          \
                  0, & Performance::cfs_F12_    ## x ## _B, \
                  -1,  Performance::thresh_F12_ ## x ## _B

//============================================================================
/** Scalar functions binomial and factorial.
 */
/// The class implementing !
class Bif_F12_BINOM : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F12_BINOM()
   : ScalarFunction(PERF_AB(BINOM))
   {}

   static Bif_F12_BINOM fun;      ///< Built-in function.

protected:
   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const
      { return eval_scalar_B(B, &Cell::bif_factorial); }

   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_binomial); }

   /// overloaded Function::get_scalar_f2
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_binomial; }

   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, integer_1); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_binomial); }
};
//----------------------------------------------------------------------------
/** Scalar function less than.
 */
/// The class implementing <
class Bif_F2_LESS : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F2_LESS()
   : ScalarFunction(PERF_A(LESS))
   {}

   static Bif_F2_LESS  fun;         ///< Built-in function.

protected:
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_less_than); }

   /// overloaded Function::get_scalar_f2
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_less_than; }

   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, integer_0); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_less_than); }
};
//----------------------------------------------------------------------------
/** Scalar function equal.
 */
/// The class implementing =
class Bif_F2_EQUAL : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F2_EQUAL()
   : ScalarFunction(PERF_A(EQUAL))
   {}

   static Bif_F2_EQUAL  fun;        ///< Built-in function.

protected:
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_equal); }

   /// overloaded Function::get_scalar_f2
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_equal; }

   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, integer_1); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_equal); }
};
//----------------------------------------------------------------------------
/** Scalar function EQ bitwise (i.e. bitwise not A xor B)
 */
/// The class implementing ⊤=
class Bif_F2_EQUAL_B : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F2_EQUAL_B()
   : ScalarFunction(PERF_A(EQUAL_B))
   {}

   static Bif_F2_EQUAL_B  fun;          ///< Built-in function.

protected:
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B,
                              &Cell::bif_equal_bitwise); }

   /// overloaded Function::get_scalar_f2
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_equal_bitwise; }

   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, integer_1); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_equal_bitwise); }
};
//----------------------------------------------------------------------------
/** Scalar function NE bitwise (i.e. bitwise A xor B)
 */
/// The class implementing ⊤≠
class Bif_F2_UNEQ_B : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F2_UNEQ_B()
   : ScalarFunction(PERF_A(UNEQ_B))
   {}

   static Bif_F2_UNEQ_B  fun;          ///< Built-in function.

protected:
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B,
                              &Cell::bif_not_equal_bitwise); }

   /// overloaded Function::get_scalar_f2
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_not_equal_bitwise; }

   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, integer_1); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_not_equal_bitwise); }
};
//----------------------------------------------------------------------------
/** Scalar function greater than.
 */
/// The class implementing >
class Bif_F2_GREATER : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F2_GREATER()
   : ScalarFunction(PERF_A(GREATER))
   {}

   static Bif_F2_GREATER  fun;      ///< Built-in function.

protected:
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_greater_than); }

   /// overloaded Function::get_scalar_f2
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_greater_than; }

   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, integer_0); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_greater_than); }
};
//----------------------------------------------------------------------------
/** Scalar function AND/LCM
 */
/// The class implementing ∧
class Bif_F2_AND : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F2_AND()
   : ScalarFunction(PERF_A(AND))
   {}

   static Bif_F2_AND  fun;          ///< Built-in function.

protected:
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_and); }

   /// overloaded Function::get_scalar_f2
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_and; }

   /// return the associative cell function of this function
   virtual assoc_f2 get_assoc() const { return &Cell::bif_and; }

   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, integer_1); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_and); }
};
//----------------------------------------------------------------------------
/** Scalar function AND bitwise
 */
/// The class implementing ⊤∧
class Bif_F2_AND_B : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F2_AND_B()
   : ScalarFunction(PERF_A(AND_B))
   {}

   static Bif_F2_AND_B  fun;          ///< Built-in function.

protected:
   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const
      { return eval_scalar_B(B, &Cell::bif_within_quad_CT); }

   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B,
                              &Cell::bif_and_bitwise); }

   /// overloaded Function::get_scalar_f2
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_and_bitwise; }

   /// return the associative cell function of this function
   virtual assoc_f2 get_assoc() const { return &Cell::bif_and_bitwise; }

   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, integer_1); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_and_bitwise); }
};
//----------------------------------------------------------------------------
/** Scalar function OR/GCD
 */
/// The class implementing ∨
class Bif_F2_OR : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F2_OR()
   : ScalarFunction(PERF_A(OR))
   {}

   static Bif_F2_OR  fun;           ///< Built-in function.

protected:
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_or); }

   /// overloaded Function::get_scalar_f2
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_or; }

   /// return the associative cell function of this function
   virtual assoc_f2 get_assoc() const { return &Cell::bif_or; }

   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, integer_0); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_or); }
};
//----------------------------------------------------------------------------
/** Scalar function OR bitwise
 */
/// The class implementing ⊤∨
class Bif_F2_OR_B : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F2_OR_B()
   : ScalarFunction(PERF_A(OR_B))
   {}

   static Bif_F2_OR_B  fun;           ///< Built-in function.

protected:
   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const
      { return eval_scalar_B(B, &Cell::bif_near_int64_t); }

   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_or_bitwise); }

   /// overloaded Function::get_scalar_f2
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_or_bitwise; }

   /// return the associative cell function of this function
   virtual assoc_f2 get_assoc() const { return &Cell::bif_or_bitwise; }

   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, integer_0); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_or_bitwise); }
};
//----------------------------------------------------------------------------
/** Scalar function less or equal.
 */
/// The class implementing ≤
class Bif_F2_LEQU : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F2_LEQU()
   : ScalarFunction(PERF_A(LEQU))
   {}

   static Bif_F2_LEQU  fun;          ///< Built-in function.

protected:
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_less_eq); }

   /// overloaded Function::get_scalar_f2()
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_less_eq; }

   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, integer_1); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_less_eq); }
};
//----------------------------------------------------------------------------
/** Scalar function greater or equal.
 */
/// The class implementing ≥
class Bif_F2_MEQU : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F2_MEQU()
   : ScalarFunction(PERF_A(MEQU))
   {}

   static Bif_F2_MEQU  fun;          ///< Built-in function.

protected:
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_greater_eq); }

   /// overloaded Function::get_scalar_f2()
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_greater_eq; }

   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, integer_1); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_greater_eq); }
};
//----------------------------------------------------------------------------
/** Scalar function not equal
 */
/// The class implementing ≠
class Bif_F2_UNEQU : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F2_UNEQU()
   : ScalarFunction(PERF_A(UNEQU))
   {}

   static Bif_F2_UNEQU  fun;         ///< Built-in function.

   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_not_equal); }

   /// overloaded Function::get_scalar_f2()
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_not_equal; }

protected:
   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, integer_0); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_not_equal); }
};
//----------------------------------------------------------------------------
/** Scalar function find.
 */
/// The class implementing ⋸
class Bif_F2_FIND : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F2_FIND()
   : ScalarFunction(PERF_A(FIND))
   {}

   static Bif_F2_FIND  fun;         ///< Built-in function.

   /// overloaded ScalarFunction::may_parallel()
   virtual bool may_parallel() const   { return false; }

protected:
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const;

   /// overloaded Function::get_scalar_f2()
   virtual prim_f2 get_scalar_f2() const
      { return 0; }

   /// Return true iff A is contained in B.
   static bool contained(const Shape & shape_A, const Cell * cA,
                         Value_P B, const Shape & idx_B, double qct);
};
//----------------------------------------------------------------------------
/** Scalar function NOR
 */
/// The class implementing ⍱
class Bif_F2_NOR : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F2_NOR()
   : ScalarFunction(PERF_A(NOR))
   {}

   static Bif_F2_NOR  fun;          ///< Built-in function.

protected:
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_nor); }

   /// overloaded Function::get_scalar_f2()
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_nor; }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_nor); }
};
//----------------------------------------------------------------------------
/** Scalar function NOR bitwise
 */
/// The class implementing ⊤⍱
class Bif_F2_NOR_B : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F2_NOR_B()
   : ScalarFunction(PERF_A(NOR_B))
   {}

   static Bif_F2_NOR_B  fun;          ///< Built-in function.

protected:
   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const
      { return eval_scalar_B(B, &Cell::bif_not_bitwise); }

   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B,
                              &Cell::bif_nor_bitwise); }

   /// overloaded Function::get_scalar_f2()
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_nor_bitwise; }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_nor_bitwise); }
};
//----------------------------------------------------------------------------
/** Scalar function nand.
 */
/// The class implementing ⍲
class Bif_F2_NAND : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F2_NAND()
   : ScalarFunction(PERF_A(NAND))
   {}

   static Bif_F2_NAND  fun;         ///< Built-in function.

protected:
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_nand); }

   /// overloaded Function::get_scalar_f2()
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_nand; }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_nand); }
};
//----------------------------------------------------------------------------
/** Scalar function NAND bitwise
 */
/// The class implementing ⊤⍲
class Bif_F2_NAND_B : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F2_NAND_B()
   : ScalarFunction(PERF_A(NAND_B))
   {}

   static Bif_F2_NAND_B  fun;         ///< Built-in function.

protected:
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_nand_bitwise); }

   /// overloaded Function::get_scalar_f2()
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_nand_bitwise; }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_nand_bitwise); }
};
//----------------------------------------------------------------------------
/** Scalar functions power and exponential.
 */
/// The class implementing ⋆
class Bif_F12_POWER : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F12_POWER()
   : ScalarFunction(PERF_AB(POWER))
   {}

   static Bif_F12_POWER  fun;       ///< Built-in function.

protected:
   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const
      { return eval_scalar_B(B, &Cell::bif_exponential); }

   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_power); }

   /// overloaded Function::get_scalar_f2()
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_power; }

   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, integer_1); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_power); }

   /// overloaded Function::get_monadic_inverse()
   virtual cFunction_P get_monadic_inverse() const;

   /// overloaded Function::get_dyadic_inverse()
   virtual cFunction_P get_dyadic_inverse() const;
};
//----------------------------------------------------------------------------
/** Scalar functions add and conjugate.
 */
/// The class implementing +
class Bif_F12_PLUS : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F12_PLUS(bool inv)
   : ScalarFunction(PERF_AB(PLUS)),
     inverse(inv)
   {}

   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const
      { return eval_scalar_B(B, &Cell::bif_conjugate); }

   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B,
               inverse ? &Cell::bif_add_inverse : &Cell::bif_add); }

   /// overloaded Function::get_scalar_f2()
   virtual prim_f2 get_scalar_f2() const
      { return inverse ? &Cell::bif_add_inverse : &Cell::bif_add; }

   /// return the associative cell function of this function
   virtual assoc_f2 get_assoc() const { return &Cell::bif_add; }

   static Bif_F12_PLUS  fun;           ///< Built-in function.
   static Bif_F12_PLUS  fun_inverse;   ///< Built-in function.

protected:
   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, integer_0); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_add); }

   /// overloaded Function::get_dyadic_inverse()
   virtual cFunction_P get_dyadic_inverse() const;

   /// true if the inverse shall be computed. This allows Bif_F12_CIRCLE to
   /// be instantiated twice: once for non-inverted operation and once for
   /// inverted operation, and both instances can share some code in this class
   const bool inverse;
};
//----------------------------------------------------------------------------
/** Scalar functions subtract and negative.
 */
/// The class implementing -
class Bif_F12_MINUS : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F12_MINUS()
   : ScalarFunction(PERF_AB(MINUS))
   {}

   static Bif_F12_MINUS  fun;       ///< Built-in function.

protected:
   /// overloaded Function::eval_AB()
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_subtract); }

   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const
      { return eval_scalar_B(B, &Cell::bif_negative); }

   /// overloaded Function::get_scalar_f2()
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_subtract; }

   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, integer_0); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_subtract); }

   /// overloaded Function::get_monadic_inverse()
   virtual cFunction_P get_monadic_inverse() const;

   /// overloaded Function::get_dyadic_inverse()
   virtual cFunction_P get_dyadic_inverse() const;
};
//----------------------------------------------------------------------------
/** Scalar function roll and non-scalar function dial.
 */
/// The class implementing ?
class Bif_F12_ROLL : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F12_ROLL()
   : ScalarFunction(PERF_B(ROLL))
   {}

   static Bif_F12_ROLL  fun;        ///< Built-in function.

protected:
   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const;

   /// dial A from B.
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const;

   /// overloaded Function::get_scalar_f2
   virtual prim_f2 get_scalar_f2() const
      { return 0; }

   /// overloaded ScalarFunction::may_parallel()
   virtual bool may_parallel() const   { return false; }

   /// recursively check that all ravel elements of B are integers ≥ 0 and
   /// return \b true iff not.
   static bool check_B(const Value & B, double qct);
};
//----------------------------------------------------------------------------
/** Scalar function not and non-scalar function without.
 */
/// The class implementing ∼
class Bif_F12_WITHOUT : public ScalarFunction // (almost)
{
public:
   /// Constructor.
   Bif_F12_WITHOUT()
   : ScalarFunction(PERF_B(WITHOUT))
   {}

   static Bif_F12_WITHOUT  fun;     ///< Built-in function.

   /// Compute A without B.
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const;

   /// Overloaded Function::eval_identity_fun. Dyadic ∼ is not scalar, therefore
   /// its eval_identity_fun() differs from ScalarFunction::eval_identity_fun().
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const;

   /// overloaded ScalarFunction::may_parallel()
   virtual bool may_parallel() const   { return false; }

protected:
   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const
      { return eval_scalar_B(B, &Cell::bif_not); }

   /// overloaded Function::get_scalar_f2
   virtual prim_f2 get_scalar_f2() const
      { return 0; }

   /// eval_AB for large A and/or B
   static Value_P large_eval_AB(const Value & A, const Value & B);
};
//----------------------------------------------------------------------------
/** Scalar functions times and direction.
 */
/// The class implementing ×
class Bif_F12_TIMES : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F12_TIMES(bool inv)
   : ScalarFunction(PERF_AB(TIMES)),
     inverse(inv)
   {}

   static Bif_F12_TIMES  fun;           ///< Built-in function.
   static Bif_F12_TIMES  fun_inverse;   ///< Built-in function.

   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B,
               inverse ? &Cell::bif_multiply_inverse : &Cell::bif_multiply); }

protected:
   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const
      { return eval_scalar_B(B, &Cell::bif_direction); }

   /// overloaded Function::get_scalar_f2()
   virtual prim_f2 get_scalar_f2() const
      { return inverse ? &Cell::bif_multiply_inverse : &Cell::bif_multiply; }

   /// return the associative cell function of this function
   virtual assoc_f2 get_assoc() const { return &Cell::bif_multiply; }

   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, integer_1); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_multiply); }

   /// overloaded Function::get_dyadic_inverse()
   virtual cFunction_P get_dyadic_inverse() const;

   /// true if the inverse shall be computed. This allows Bif_F12_CIRCLE to
   /// be instantiated twice: once for non-inverted operation and once for
   /// inverted operation, and both instances can share some code in this class
   const bool inverse;
};
//----------------------------------------------------------------------------
/** Scalar functions divide and reciprocal.
 */
/// The class implementing ÷
class Bif_F12_DIVIDE : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F12_DIVIDE()
   : ScalarFunction(PERF_AB(DIVIDE))
   {}

   static Bif_F12_DIVIDE  fun;      ///< Built-in function.

protected:
   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const
      { return eval_scalar_B(B, &Cell::bif_reciprocal); }

   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_divide); }

   /// overloaded Function::get_scalar_f2
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_divide; }

   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, integer_1); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_divide); }

   /// overloaded Function::get_monadic_inverse()
   virtual cFunction_P get_monadic_inverse() const;

   /// overloaded Function::get_dyadic_inverse()
   virtual cFunction_P get_dyadic_inverse() const;
};
//----------------------------------------------------------------------------
/** Scalar functions circle functions and pi times.
 */
/// The class implementing ○
class Bif_F12_CIRCLE : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F12_CIRCLE(bool inv)
   : ScalarFunction(PERF_AB(CIRCLE)),
     inverse(inv)
   {}

   static Bif_F12_CIRCLE  fun;              ///< Built-in function.
   static Bif_F12_CIRCLE  fun_inverse;      ///< Built-in function.

protected:
   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const
      { return eval_scalar_B(B,
               inverse ? &Cell::bif_pi_times_inverse : &Cell::bif_pi_times); }

   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, inverse ? &Cell::bif_circle_fun_inverse
                                            : &Cell::bif_circle_fun); }

   /// overloaded Function::get_scalar_f2
   virtual prim_f2 get_scalar_f2() const
      { return inverse ? &Cell::bif_circle_fun_inverse
                       : &Cell::bif_circle_fun; }

   /// overloaded Function::get_monadic_inverse()
   virtual cFunction_P get_monadic_inverse() const;

   /// overloaded Function::get_dyadic_inverse()
   virtual cFunction_P get_dyadic_inverse() const;

   /// true if the inverse shall be computed. This allows Bif_F12_CIRCLE to
   /// be instantiated twice: once for non-inverted operation and once for
   /// inverted operation, and both instances can share some code in this class
   const bool inverse;
};
//----------------------------------------------------------------------------
/** Scalar functions maximum and round up.
 */
/// The class implementing ⌈
class Bif_F12_RND_UP : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F12_RND_UP()
   : ScalarFunction(PERF_AB(RND_UP))
   {}

   static Bif_F12_RND_UP  fun;      ///< Built-in function.

protected:
   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const
      { return eval_scalar_B(B, &Cell::bif_ceiling); }

   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_maximum); }

   /// overloaded Function::get_scalar_f2()
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_maximum; }

   /// return the associative cell function of this function
   virtual assoc_f2 get_assoc() const { return &Cell::bif_maximum; }

   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, float_min); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_maximum); }
};
//----------------------------------------------------------------------------
/** Scalar functions minimum and round down.
 */
/// The class implementing ⌊
class Bif_F12_RND_DN : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F12_RND_DN()
   : ScalarFunction(PERF_AB(RND_DN))
   {}

   static Bif_F12_RND_DN  fun;      ///< Built-in function.

protected:
   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const
      { return eval_scalar_B(B, &Cell::bif_floor); }

   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_minimum); }

   /// overloaded Function::get_scalar_f2()
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_minimum; }

   /// return the associative cell function of this function
   virtual assoc_f2 get_assoc() const { return &Cell::bif_minimum; }

   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
      { return eval_scalar_identity_fun(B, axis, float_max); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_minimum); }
};
//----------------------------------------------------------------------------
/** Scalar functions residue and magnitude.
 */
/// The class implementing ∣
class Bif_F12_STILE : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F12_STILE()
   : ScalarFunction(PERF_AB(STILE))
   {}

   static Bif_F12_STILE  fun;       ///< Built-in function.

   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_residue); }

   /// overloaded Function::get_scalar_f2()
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_residue; }

protected:
   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const
      { return eval_scalar_B(B, &Cell::bif_magnitude); }

   /// overloaded Function::eval_identity_fun();
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const
   { return eval_scalar_identity_fun(B, axis, integer_0); }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_residue); }
};
//----------------------------------------------------------------------------
/** Scalar functions logarithms.
 */
/// The class implementing ⍟
class Bif_F12_LOGA : public ScalarFunction
{
public:
   /// Constructor.
   Bif_F12_LOGA()
   : ScalarFunction(PERF_AB(LOGA))
   {}

   static Bif_F12_LOGA  fun;        ///< Built-in function.

protected:
   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const
      { return eval_scalar_B(B, &Cell::bif_nat_log); }

   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return eval_scalar_AB(A, B, &Cell::bif_logarithm); }

   /// overloaded Function::get_scalar_f2()
   virtual prim_f2 get_scalar_f2() const
      { return &Cell::bif_logarithm; }

   /// overloaded Function::eval_AXB().
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { return eval_scalar_AXB(A, X, B, &Cell::bif_logarithm); }

   /// overloaded Function::get_monadic_inverse()
   virtual cFunction_P get_monadic_inverse() const;

   /// overloaded Function::get_dyadic_inverse()
   virtual cFunction_P get_dyadic_inverse() const;
};
//----------------------------------------------------------------------------

#endif // __SCALAR_FUNCTION_HH_DEFINED__
