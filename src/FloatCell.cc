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

#include <stdio.h>
#include <string.h>
#include <math.h>

#include <complex>

#include "Common.hh"
#include "ComplexCell.hh"
#include "Error.hh"
#include "FloatCell.hh"
#include "IntCell.hh"
#include "UTF8_string.hh"
#include "Workspace.hh"

#include "Cell.icc"
#include "Value.hh"

//----------------------------------------------------------------------------
bool
FloatCell::equal(const Cell & A, double qct) const
{
   if (!A.is_numeric())       return false;
   if (A.is_complex_cell())   return A.equal(*this, qct);
   return tolerantly_equal(A.get_real_value(), get_real_value(), qct);
}
//----------------------------------------------------------------------------
bool
FloatCell::greater(const Cell & other) const
{
const APL_Float this_val  = get_real_value();

   switch(other.get_cell_type())
      {
        case CT_INT:
             {
               const APL_Float other_val(other.get_int_value());
               if (this_val == other_val)   return this > &other;
               return this_val > other_val;
             }

        case CT_FLOAT:
             {
               const APL_Float other_val = other.get_real_value();
               if (this_val == other_val)   return this > &other;
               return this_val > other_val;
             }

        case CT_CHAR:    return true;
        case CT_COMPLEX: break;
        case CT_POINTER: return false;
        case CT_CELLREF: DOMAIN_ERROR;
        default:         Assert(0 && "Bad celltype");
      }

const Comp_result comp = compare(other);
   if (comp == COMP_EQ)   return this > &other;
   return (comp == COMP_GT);
}
//----------------------------------------------------------------------------
bool
FloatCell::get_near_bool()  const
{
   if (dfval() > INTEGER_TOLERANCE)   // maybe 1
      {
        if (dfval() > (1.0 + INTEGER_TOLERANCE))   DOMAIN_ERROR;
        if (dfval() < (1.0 - INTEGER_TOLERANCE))   DOMAIN_ERROR;
        return true;
      }

   // maybe 0. We already know that dfval() ≤ qct
   //
   if (dfval() < -INTEGER_TOLERANCE)   DOMAIN_ERROR;
   return false;
}
//----------------------------------------------------------------------------
Comp_result
FloatCell::compare(const Cell & other) const
{
   if (other.is_integer_cell())   // integer
      {
        const double qct = Workspace::get_CT();
        if (equal(other, qct))   return COMP_EQ;
        return (dfval() <= other.get_int_value())  ? COMP_LT : COMP_GT;
      }

   if (other.is_float_cell())
      {
        const double qct = Workspace::get_CT();
        if (equal(other, qct))   return COMP_EQ;
        return (dfval() <= other.get_real_value()) ? COMP_LT : COMP_GT;
      }

   if (other.is_complex_cell())   return Comp_result(-other.compare(*this));

   if (other.is_character_cell())   return COMP_GT;   // numeric > char
   if (other.is_pointer_cell())     return COMP_LT;   // numeric < nested
   DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
// monadic built-in functions...
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_near_int64_t(Cell * Z) const
{
   if (!is_near_int64_t())       return E_DOMAIN_ERROR;

   return IntCell::zI(Z, get_near_int());
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_within_quad_CT(Cell * Z) const
{
const double val = dfval();
   if (val > LARGE_INT)   return E_DOMAIN_ERROR;
   if (val < SMALL_INT)   return E_DOMAIN_ERROR;

const double max_diff = Workspace::get_CT() * val;   // scale ⎕CT

const APL_Float val_dn = floor(val);
   if (val < (val_dn + max_diff))   return FloatCell::zF(Z, val_dn);

const APL_Float val_up = ceil(val);
   if (val > (val_up - max_diff))   return FloatCell::zF(Z, val_up);

   return E_DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_factorial(Cell * Z) const
{
   // max N! that fits into double is about 170
   //
   if (dfval() > 170.0)   return E_DOMAIN_ERROR;

const APL_Float arg = dfval() + 1.0;
   return FloatCell::zF(Z, tgamma(arg));
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_conjugate(Cell * Z) const
{
   // convert quotients (if any) to double
   return FloatCell::zF(Z, dfval());
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_negative(Cell * Z) const
{
#ifdef cfg_RATIONAL_NUMBERS_WANTED
   if (const APL_Integer denom = get_denominator())
      return FloatCell::zR(Z, -get_numerator(), denom);
#endif

   return FloatCell::zF(Z, - dfval());
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_direction(Cell * Z) const
{
   // Note: bif_direction does NOT use ⎕CT
   //
#ifdef cfg_RATIONAL_NUMBERS_WANTED
   // denominator is either 0 (for Floats) or positive (for quotients)
   if (const APL_Integer denom = get_denominator())
      {
        if (get_numerator() > 0)   return IntCell::zI(Z,  1);
        if (get_numerator() < 0)   return IntCell::zI(Z, -1);
        return FloatCell::zF(Z, 0);
      }
#endif

   if (dfval() > 0.0)   return IntCell::z1(Z);
   if (dfval() < 0.0)   return IntCell::z_1(Z);
   return IntCell::z0(Z);
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_magnitude(Cell * Z) const
{
#ifdef cfg_RATIONAL_NUMBERS_WANTED
   if (const APL_Integer denom = get_denominator())
      {
        const APL_Integer numer = get_numerator();
        return FloatCell::zR(Z, numer < 0 ? -numer : numer, denom);
      }
#endif

   if (dfval() < 0.0)   return FloatCell::zF(Z, -dfval());
   else                 return FloatCell::zF(Z, dfval());
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_reciprocal(Cell * Z) const
{
#ifdef cfg_RATIONAL_NUMBERS_WANTED
   if (const APL_Integer denom = get_denominator())
      {
        if (uint64_t(denom) < 0x8000000000000000ULL)   // small enough for int32
           {
             const APL_Integer numer = get_numerator();
             // simply exchange numerator and denominator, but make sure that
             // the denominator is positive
             //
             if (numer == 1)    return IntCell::zI(Z,  denom);   // 1 ÷ X ÷ X
             if (numer == -1)   return IntCell::zI(Z, -denom);   // 1 ÷ -X → -X
             if (numer < 0)     return FloatCell::zR(Z, -denom, -numer);
             else               return FloatCell::zR(Z, denom, numer);
           }

        // at this point denom does not fit into numer. Fall through
      }
#endif

const APL_Float z = 1.0/dfval();
   if (!isfinite(z))   return E_DOMAIN_ERROR;

   return FloatCell::zF(Z, 1.0/dfval());
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_roll(Cell * Z) const
{
   if (!is_near_int())   return E_DOMAIN_ERROR;

const APL_Integer set_size = get_checked_near_int();
   if (set_size <= 0)   return E_DOMAIN_ERROR;

const uint64_t rnd = Workspace::get_RL(set_size);
   return IntCell::zI(Z, Workspace::get_IO() + (rnd % set_size));
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_pi_times(Cell * Z) const
{
   return FloatCell::zF(Z, dfval() * M_PI);
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_pi_times_inverse(Cell * Z) const
{
   return FloatCell::zF(Z, dfval() / M_PI);
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_ceiling(Cell * Z) const
{
#ifdef cfg_RATIONAL_NUMBERS_WANTED
   if (const APL_Integer denom = get_denominator())
      {
        // since the quotient is exact, we ignore ⎕CT
        //
        const APL_Integer numer = get_numerator();
        APL_Integer quotient = numer / denom;
        if (numer > (quotient * denom))   ++quotient;
        return IntCell::zI(Z, quotient);
      }
#endif

   // see comments for bif_floor below.

const APL_Float b = dfval();
   // if b is large then return it as is.
   //
   if (b >= LARGE_INT)   return FloatCell::zF(Z, b);
   if (b <= SMALL_INT)   return FloatCell::zF(Z, b);

APL_Integer bi = b;
   while (bi < b)         ++bi;
   while ((bi - 1) > b)   --bi;
   if (bi == b)   return IntCell::zI(Z, bi);   // b already equal to its floor

const APL_Float D = bi - b;

   if (D >= (1.0 - Workspace::get_CT()))   --bi;
   return IntCell::zI(Z, bi);
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_floor(Cell * Z) const
{
#ifdef cfg_RATIONAL_NUMBERS_WANTED
   if (const APL_Integer denom = get_denominator())
      {
        // since the quotient is exact, we ignore ⎕CT
        //
        const APL_Integer numer = get_numerator();
        APL_Integer quotient = numer / denom;
        if (numer < (quotient * denom))   --quotient;
        return IntCell::zI(Z, quotient);
      }
#endif

/* Informal description (iso p. 78):
   For real-numbers, Z is the greatest integer tolerantly less than
   or equal to B. Uses comparison-tolerance.

   Formal description:
   Return the tolerant-floor of B within comparison-tolerance.

   tolerant-floor (p.19) is defined for complex A:
   Let A be a member of the set of numbers in the unit-square at the
   complex-integer C, and let D be A minus C.
   If the sum of the real and imaginary parts of D is tolerantly-less-than
   one within B, then Z is C.
   Otherwise, if the imaginary-part of D is greater-than the real-part of D,
   then Z is C plus imaginary-one.
   Otherwise, Z is C plus one.

   Unfortunately tolerantly-less-than is not defined in the standard. We
   interpret it as meaning 'less than and not tolerrantly-equal'.

   Replacing B with ⎕CT, and A with B, and considering that the imaginary
   part of B is always 0 if B is real this becomes:

   tolerant-floor of (real) B within ⎕CT:
   Let B be a member of the set of numbers in the hals-open interval [C, C+1),
   and let D be B minus C.
   If D is tolerantly-less-than one within ⎕CT, then Z is C.
   Otherwise, Z is C plus one.

   In other word, Let RB be B rounded down. Then Z is RB if B < RB + 1 - ⎕CT
   and RB+1 otherwise.

   Note: if B cannot fit into int64_t then, due to the smaller precision
   of double, it is already equal to its floor and we return it unchanged.
*/

const APL_Float b = dfval();
   // if b is large then return it as is.
   //
   if (b >= LARGE_INT)   return FloatCell::zF(Z, b);
   if (b <= SMALL_INT)   return FloatCell::zF(Z, b);

APL_Integer bi = b;
   while (bi > b)         --bi;
   while ((bi + 1) < b)   ++bi;
   if (bi == b)   return IntCell::zI(Z, bi);   // b already equal to its floor

const APL_Float D = b - bi;

   if (D >= (1.0 - Workspace::get_CT()))   ++bi;
   return IntCell::zI(Z, bi);
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_exponential(Cell * Z) const
{
   // e to the B-th power
   //
   return FloatCell::zF(Z, exp(dfval()));
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_nat_log(Cell * Z) const
{
const APL_Float val = dfval();
   if (val == 0.0)     return E_DOMAIN_ERROR;

   if (val > 0.0)   // real result
      {
        return FloatCell::zF(Z, log(val));
      }

const APL_Complex bb(val, 0);
   return ComplexCell::zC(Z, log(bb));
}
//----------------------------------------------------------------------------
// dyadic built-in functions...
//
// where possible a function with non-real A is delegated to the corresponding
// member function of A. For numeric cells that is the ComplexCell function
// and otherwise the default function (that returns E_DOMAIN_ERROR.
//
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_add(Cell * Z, const Cell * A) const
{
   if (A->is_real_cell())
      {
#ifdef cfg_RATIONAL_NUMBERS_WANTED
   if (const APL_Integer denom_B = get_denominator())
   if (const APL_Integer denom_A = A->get_denominator())
      {
        // both A and B are rational
        //
        if (Cell::prod_overflow(denom_A, denom_B))   goto big;

        // compute common denominator...
        const APL_Integer gcd_AB   = gcd(denom_A, denom_B);
        const APL_Integer mult_A  = denom_A / gcd_AB;
        const APL_Integer mult_B  = denom_B / gcd_AB;
        const APL_Integer denom_AB = denom_A * mult_B;
        if (Cell::prod_overflow(denom_AB, mult_B))                goto big;

        // compute numerators...
        const APL_Integer numer_A = A->get_numerator();
        if (Cell::prod_overflow(numer_A, mult_B))                goto big;
        const APL_Integer numer_A1 = numer_A * mult_B;
        const APL_Integer numer_B = get_numerator();
        if (Cell::prod_overflow(numer_B, mult_A))                goto big;
        const APL_Integer numer_B1 = numer_B * mult_A;

        const APL_Integer sum_AB = numer_A1 + numer_B1;
        if (Cell::sum_overflow(sum_AB, numer_A1, numer_B1))      goto big;
        const APL_Integer sum_gcd = gcd(sum_AB, denom_AB);
        if (sum_gcd == denom_AB)   return IntCell::zI(Z, sum_AB / denom_AB);
        if (sum_gcd == 1)   return FloatCell::zR(Z, sum_AB, denom_AB);
        return FloatCell::zR(Z, sum_AB/sum_gcd, denom_AB/sum_gcd);
      }
      big:

#endif

        return FloatCell::zF(Z, A->get_real_value() + get_real_value());
      }

   // delegate to A
   //
   return A->bif_add(Z, this);
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_add_inverse(Cell * Z, const Cell * A) const
{
   return A->bif_subtract(Z, this);
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_subtract(Cell * Z, const Cell * A) const
{
   if (A->is_real_cell())   // real result
      {
#ifdef cfg_RATIONAL_NUMBERS_WANTED
   if (const APL_Integer denom_B = get_denominator())
   if (const APL_Integer denom_A = A->get_denominator())
      {
        // both A and B are rational
        //
        if (Cell::prod_overflow(denom_A, denom_B))   goto big;

        // compute common denominator...
        const APL_Integer gcd_AB   = gcd(denom_A, denom_B);
        const APL_Integer mult_A  = denom_A / gcd_AB;
        const APL_Integer mult_B  = denom_B / gcd_AB;
        const APL_Integer denom_AB = denom_A * mult_B;

        // compute numerators...
        const APL_Integer numer_A = A->get_numerator();
        if (Cell::prod_overflow(numer_A, mult_B))                goto big;
        const APL_Integer numer_A1 = numer_A * mult_B;
        const APL_Integer numer_B = get_numerator();
        if (Cell::prod_overflow(numer_B, mult_A))                goto big;
        const APL_Integer numer_B1 = numer_B * mult_A;

        const APL_Integer diff_AB = numer_A1 - numer_B1;
        if (Cell::diff_overflow(diff_AB, numer_A1, numer_B1))    goto big;
        const APL_Integer diff_gcd = gcd(diff_AB, denom_AB);
        if (diff_gcd == denom_AB)   return IntCell::zI(Z, diff_AB / denom_AB);
        if (diff_gcd == 1)   return FloatCell::zR(Z, diff_AB, denom_AB);
        return FloatCell::zR(Z, diff_AB/diff_gcd, denom_AB/diff_gcd);
      }
      big:

#endif

       return FloatCell::zF(Z, A->get_real_value() - get_real_value());
      }

   if (A->is_complex_cell())   // complex result
      {
       return ComplexCell::zC(Z, A->get_real_value() - get_real_value(),
                                 A->get_imag_value());
      }

   return E_DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_multiply(Cell * Z, const Cell * A) const
{
#ifdef cfg_RATIONAL_NUMBERS_WANTED
   if (APL_Integer denom_B = get_denominator())
   if (APL_Integer denom_A = A->get_denominator())
      {
        // both A and B are rational
        //
        APL_Integer numer_A = A->get_numerator();
        APL_Integer numer_B = get_numerator();
        const APL_Integer gcd_A_B = gcd(numer_A, denom_B);
         if (gcd_A_B > 1)   { numer_A /= gcd_A_B;   denom_B /= gcd_A_B; }

        const APL_Integer gcd_B_A = gcd(numer_B, denom_A);
         if (gcd_B_A > 1)   { numer_B /= gcd_B_A;   denom_A /= gcd_B_A; }

        if (Cell::prod_overflow(numer_A, numer_B))   goto big;
        if (Cell::prod_overflow(denom_A, denom_B))   goto big;

        const APL_Integer numer = numer_A * numer_B;
        const APL_Integer denom = denom_A * denom_B;
        const APL_Integer prod_gcd = gcd(numer, denom);
        if (prod_gcd == denom)   return IntCell::zI(Z, numer / denom);
        if (prod_gcd == 1)       return FloatCell::zR(Z, numer, denom);
        return FloatCell::zR(Z, numer/prod_gcd, denom/prod_gcd);
      }
      big:

#endif

   if (!A->is_numeric())   return E_DOMAIN_ERROR;

const APL_Float ar = A->get_real_value();
const APL_Float ai = A->get_imag_value();

   if (ai == 0.0)   // real result
      {
        const APL_Float z = ar * dfval();
        if (!isfinite(z))   return E_DOMAIN_ERROR;
        return FloatCell::zF(Z, z);
      } 

   // complex result
   //
const double zr = ar * dfval();
const double zi = ai * dfval();
   if (!isfinite(zr))   return E_DOMAIN_ERROR;
   if (!isfinite(zi))   return E_DOMAIN_ERROR;
   return ComplexCell::zC(Z, zr, zi);
} 
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_multiply_inverse(Cell * Z, const Cell * A) const
{
   return A->bif_divide(Z, this);
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_divide(Cell * Z, const Cell * A) const
{
#ifdef cfg_RATIONAL_NUMBERS_WANTED
   if (const APL_Integer B_denom = get_denominator())  // B is rational
      {
        const APL_Integer B_numer = get_numerator();
        if (B_numer == 0)   // A ÷ 0
           {
             if (A->is_near_zero())   return IntCell::z1(Z);   // 0÷0 is 1
             return E_DOMAIN_ERROR;
           }
        const FloatCell inv_B(B_denom, B_numer);
        return inv_B.bif_multiply(Z, A);
      }
#endif

   if (!A->is_numeric())   return E_DOMAIN_ERROR;

const APL_Float ar = A->get_real_value();
const APL_Float ai = A->get_imag_value();

   if (dfval() == 0.0)   // A ÷ 0
      {
         if (ar != 0.0)   return E_DOMAIN_ERROR;
         if (ai != 0.0)   return E_DOMAIN_ERROR;

         return IntCell::z1(Z);   // 0÷0 is 1 in APL
      }


   if (ai == 0.0)   // real result
      {
        const APL_Float real = ar / dfval() ;
        return isfinite(real) ? FloatCell::zF(Z, real) : E_DOMAIN_ERROR;
      }

   // complex result
   //
const double zar = ar / dfval();
const double zai = ai / dfval();
   if (!isfinite(zar))   return E_DOMAIN_ERROR;
   if (!isfinite(zai))   return E_DOMAIN_ERROR;
   return ComplexCell::zC(Z, zar, zai);
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_power(Cell * Z, const Cell * A) const
{
   // some A to the real B-th power
   //
   if (!A->is_numeric())   return E_DOMAIN_ERROR;

const APL_Float ar = A->get_real_value();
const APL_Float ai = A->get_imag_value();

   // 1. A == 0
   //
   if (ar == 0.0 && ai == 0.0)
       {
         if (dfval() == 0.0)   return IntCell::z1(Z);   // 0⋆0 is 1
         if (dfval()  > 0.0)   return IntCell::z0(Z);   // 0⋆N is 0
         return E_DOMAIN_ERROR;                        // 0⋆¯N = 1÷0
       }

   // 2. real A > 0   (real result)
   //
   if (ai == 0.0)   // real A
      {
        if (ar  == 1.0)   return IntCell::z1(Z);   // 1⋆b = 1

        if (ar >= 0)
           {
             const APL_Float z = pow(ar, dfval());
             return isfinite(z) ? FloatCell::zF(Z, z) : E_DOMAIN_ERROR;
           }

        // ar < 0: fall through
      }

   // 3. complex or negative A → complex result
   //
const APL_Complex a(ar, ai);
const APL_Complex z = complex_power(a, dfval());
   if (!isfinite(z.real()))   return E_DOMAIN_ERROR;
   if (!isfinite(z.imag()))   return E_DOMAIN_ERROR;

   return ComplexCell::zC(Z, z);
}
//----------------------------------------------------------------------------
inline double
p_modulo_q(double P, double Q)
{
  // return R ← P - (×P) ⌊ | Q × ⌊ | P ÷ Q as described in ISO p. 89
  //            │   │    │ │ │   │ │ │
  //            │   │    │ │ │   │ │ └──────── quotient
  //            │   │    │ │ │   │ └────────── abs_quotient
  //            │   │    │ │ │   └──────────── floor_quotient
  //            │   │    │ │ └──────────────── floor_quotient
  //            │   │    │ └────────────────── prod
  //            │   │    └──────────────────── abs_prod
  //            │   └───────────────────────── prod2
  //            └───────────────────────────── r
  //

const APL_Float quotient = P / Q;   // quotient←b÷a and check overflows
   if (!isfinite(quotient))   return 0.0;   // exponent overflow

   if (!isfinite(Q / P))   // exponent underflow
      return ((P < 0) == (Q < 0)) ? P : 0.0;

   {
     const double qct = Workspace::get_CT();
     if ((qct != 0) && Cell::integral_within(quotient, qct))   return 0.0;
   }

const APL_Float null(0.0);
const APL_Float abs_quotient = quotient < null ? -quotient : quotient;
   if (abs_quotient > 4.5E15)
      {
        // if "| P ÷ Q" is too large then 'abs_quotient' is not exact any more.
        // In this case, for every R with 0 ≤ R < Q there ie an A such that
        // A has the same floating point representation as 'abs_quotient' and
        // (P - R) is an integer multiple of Q.
        //
        // Normally we would raise a DOMAIN ERROR to inform the user about the
        // problem, but the ISO standard does not allow that. We therefore
        // return 0 which is a valid remainder (although not the only one).
        //
        return 0.0;
      }

   if (abs_quotient < 1.0)
      {
        // P is smaller in magnitude than Q. If P and Q have the same sign then
        // P mod Q is P, otherwise Q - P.
        //
        return (P < null) == (Q < null) ? P : Q + P;
      }

const APL_Float floor_quotient = floor(abs_quotient);
const APL_Float prod           = Q * floor_quotient;
const APL_Float abs_prod       = prod < null ? -prod : prod;
const APL_Float prod2          = P < 0 ? -abs_prod : abs_prod;
const APL_Float r              = P - prod2;

   return r;

/*
// Q1(P)
// Q1(Q)
// Q1(quotient)
// Q1(abs_quotient)
// Q1(floor_quotient)
// Q1(abs_prod)
// Q1(prod2)
// Q1(r)

Assert(isnormal(abs_quotient)   || abs_quotient   == 0.0);
Assert(isnormal(floor_quotient) || floor_quotient == 0.0);
Assert(isnormal(abs_prod)       || abs_prod       == 0.0);
Assert(isnormal(prod2)          || prod2          == 0.0);
Assert(isnormal(r)              || r              == 0.0);

   return r;
*/
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_residue(Cell * Z, const Cell * A) const
{
   if (!A->is_numeric())   return E_DOMAIN_ERROR;

   if (A->get_imag_value() != 0.0)   // complex A
      {
        ComplexCell B(get_real_value());
        return B.bif_residue(Z, A);
      }

const APL_Float a = A->get_real_value();
const APL_Float b = dfval();

   // if A is zero, return B
   //
   if (a == 0.0)   return FloatCell::zF(Z, b);

   // IBM: if B is zero , return 0
   //
   if (b == 0.0)   return IntCell::z0(Z);

   // if ⎕CT != 0 and B ÷ A is close to an integer within ⎕CT then return 0.
   //
   // Note: In that case, the integer to which A ÷ B is close is either
   // floor(A ÷ B) or ceil(A ÷ B).
   //
const APL_Float null(0.0);
const APL_Float z = p_modulo_q(b, a);
Assert(isnormal(z) || z == null);

APL_Float r2;
   if      (z < null && a < null)   r2 = z;     // (×R) = ×Q)
   else if (z > null && a > null)   r2 = z;     // (×R) = ×Q)
   else                       r2 = z + a;       // (×R) ≠ ×Q)
Assert(isnormal(r2) || r2 == null);

   if (r2 == null)   return IntCell::z0(Z);
   if (r2 == a)      return IntCell::z0(Z);
   else              return FloatCell::zF(Z, r2);
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_maximum(Cell * Z, const Cell * A) const
{
const APL_Float b = this->dfval();
   if (A->is_integer_cell())
      {
         const APL_Integer a = A->get_int_value();
         if (a >= b)   return IntCell::zI(Z, a);
         else          return FloatCell::zF(Z, b);
      }

   if (A->is_float_cell())
      {
         const APL_Float a = A->get_real_value();
         if (a >= b)   return FloatCell::zF(Z, a);
         else          return FloatCell::zF(Z, b);
      }

   // complex A and float B: delegate to A
   //
   return A->bif_maximum(Z, this);
}
//----------------------------------------------------------------------------
ErrorCode
FloatCell::bif_minimum(Cell * Z, const Cell * A) const
{
const APL_Float b = this->dfval();
   if (A->is_integer_cell())
      {
         const APL_Integer a = A->get_int_value();
         if (a <= dfval())   return IntCell::zI(Z, a);
         else                return FloatCell::zF(Z, b);
      }

   if (A->is_float_cell())
      {
         const APL_Float a = A->get_real_value();
         if (a <= b)   return FloatCell::zF(Z, a);
         else          return FloatCell::zF(Z, b);
      }

   // complex A and float B: delegate to A
   //
   return A->bif_minimum(Z, this);
}
//============================================================================
// throw/nothrow boundary. Functions above MUST NOT (directly or indirectly)
// throw while funcions below MAY throw.
//============================================================================
PrintBuffer
FloatCell::character_representation(const PrintContext & pctx) const
{
#ifdef cfg_RATIONAL_NUMBERS_WANTED
   if (const APL_Integer denom = get_denominator())
      {
        if (Workspace::get_v_Quad_PS().get_print_quotients())   // show A÷B
           {
             ColInfo info;
             info.flags |= CT_FLOAT;

             UCS_string ucs;
             APL_Integer numer = get_numerator();
             if (numer < 0)
                {
                  ucs.append(UNI_OVERBAR);
                  numer = -numer;
                }
             ucs.append(UCS_string::from_uint(numer));
             info.int_len = ucs.size();

             ucs.append(UNI_DIVIDE);

             ucs.append(UCS_string::from_uint(denom));
             info.denom_len = ucs.size() - info.int_len;
             info.real_len = ucs.size();
             return PrintBuffer(ucs, info);
           }
      }
#endif

bool scaled = pctx.get_scaled();   // may be changed by print function
UCS_string ucs = UCS_string(dfval(), scaled, pctx);

ColInfo info;
   info.flags |= CT_FLOAT;
   if (scaled)   info.flags |= real_has_E;

   // assume integer.
   //
int int_fract = ucs.size();
   info.real_len = ucs.size();
   info.int_len = ucs.size();
   loop(u, ucs.size())
      {
        if (ucs[u] == UNI_FULLSTOP)
           {
             info.int_len = u;
             if (!pctx.get_scaled())   break;
             continue;
           }

        if (ucs[u] == UNI_E)
           {
             if (info.int_len > u)   info.int_len = u;
             int_fract = u;
             break;
           }
      }

   info.fract_len = int_fract - info.int_len;

   return PrintBuffer(ucs, info);
}
//----------------------------------------------------------------------------
bool
FloatCell::is_big(APL_Float val, int quad_pp)
{
static const APL_Float big[MAX_Quad_PP + 1] =
{
                  1ULL, // not used since MIN_Quad_PP == 1
                 10ULL,
                100ULL,
               1000ULL,
              10000ULL,
             100000ULL,
            1000000ULL,
           10000000ULL,
          100000000ULL,
         1000000000ULL,
        10000000000ULL,
       100000000000ULL,
      1000000000000ULL,
     10000000000000ULL,
    100000000000000ULL,
   1000000000000000ULL,
  10000000000000000ULL,
 1000000000000000000ULL,
};

   return val >= big[quad_pp] || val <= -big[quad_pp];
}
//----------------------------------------------------------------------------
bool
FloatCell::need_scaling(APL_Float val, int quad_pp)
{
   // A number is printed in scaled format if (see lrm pp. 11-13) either:
   //
   // (1) its integer part is longer that quad-PP, or

   // (2a) is non-zero, and
   // (2b) its integer part is 0, and
   // (2c) its fractional part starts with at least 5 zeroes.
   //
   if (val < 0.0)   val = - val;   // simplify comparisons

   if (is_big(val, quad_pp))        return true;    // case 1.

   if (val == 0.0)                  return false;   // not 2a.

   if (val < 0.000001)              return true;    // case 2

   return false;
}
//----------------------------------------------------------------------------
void
FloatCell::map_FC(UCS_string & ucs)
{
   loop(u, ucs.size())
      {
        switch(ucs[u])
           {
             case UNI_FULLSTOP: ucs[u] = Workspace::get_FC(0);   break;
             case UNI_COMMA:    ucs[u] = Workspace::get_FC(1);   break;
             case UNI_OVERBAR:        ucs[u] = Workspace::get_FC(5);   break;
             default:                 break;
           }
      }
}
//----------------------------------------------------------------------------
