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

/*
 This file is an example of how to define a

 'class APL_Float'

 that can be used instead of the

 'typedef double APL_Float'

  in file APL_types.hh. It can be used as a starting point for creating your
  own APL_Float class, for example to use some bignum library instead of the
  built-in C++ double type.

  This file (but not the APL_Float class itself) is expected to (and does)
  provide the following (wrapper-) functions:

  APL_Float abs(x)
  APL_Float acos(x)
  APL_Float asin(x)
  APL_Float asinh(x)
  APL_Float atan(x)
  APL_Float ceil(x)
  APL_Float cos(x)
  APL_Float cosh(x)
  APL_Float acosh(x)
  APL_Float exp(x)
  APL_Float floor(x)
  APL_Float isfinite(x)
  APL_Float log(x)
  APL_Float round(x)
  APL_Float sqrt(x)
  APL_Float sin(x)
  APL_Float sinh(x)
  APL_Float tan(x)
  APL_Float tgamma(x)
  APL_Float tanh(x)

  APL_Float atan2(x,y)
  APL_Float fmod(x,y)
  APL_Float pow(x,y)

  APL_Float operator-(x)

  APL_Float operator+(x, y)
  APL_Float operator+(x, y)
  APL_Float operator+(x, y)
  APL_Float operator+(x, y)

  bool operator<(x, y)
  bool operator>(x, y)
  bool operator<=(x, y)
  bool operator>=(x, y)
  bool operator==(x, y)
  bool operator!=(x, y)
  bool is_normal(x)

  APL_Float_Base & operator+=(x)
  APL_Float_Base & operator-=(x)
  APL_Float_Base & operator*=(x)
  APL_Float_Base & operator/=(x)

  Each of these functions and operators must have multiple variants that all
  have the same return type, but different argument types for x and y. The
  possible argument types are:

  const APL_Float_Base, double, APL_Integer, int64, and uint64.

  Not all combinations are needed, though. Please note that the wrapper
  macros used in this example will reduce the result precision to double
  regardless of the precision of the operands.
 */

#ifndef __APL_FLOAT_AS_CLASS_HH_DEFINED__
#define __APL_FLOAT_AS_CLASS_HH_DEFINED__

#ifndef __APL_TYPES_HH_DEFINED__
# error This file shall not be #included directly, but by #including APL_types.hh
#endif

class APL_Float;

/// the essential data of an APL_Float, so that it can be used in a union
class APL_Float_Base
{
public:
   /// return the value as double
   double _get() const
      { return dval; }

   /// set the value from a double
   APL_Float_Base & _set(double d)
      { dval = d;    return *this; }

   /// cast to APL_Float (creates a copy)
   inline operator APL_Float() const;

   /// up-cast const APL_Float_Base * to const APL_Float *
   inline const APL_Float * pAPL_Float() const;

   /// up-cast APL_Float_Base * to APL_Float *
   inline APL_Float * pAPL_Float();

   /// cast to double
   inline operator double()
      { return dval; }

   /// cast to APL_Integer
   inline operator const APL_Integer() const
      {
        if (dval >= 0.0)  return APL_Integer(dval);
        const APL_Integer pa(-dval);
        return -pa;
      }

protected:
   /// the value
   double dval;
};
//----------------------------------------------------------------------------
/// a non-rational APL floating point value
class APL_Float : public APL_Float_Base
{
public:
   /// default constructor
   APL_Float()                      { dval = 0.0; }

   /// constructor from a \b double value \b d
   APL_Float(double d)              { dval = d; }

   /// assingment from another APL_Float
   APL_Float & operator =(const APL_Float & other)
      { dval = other.dval; return *this; }
};
/// isnormal() for APL_Float
inline bool isnormal(const APL_Float & val)
{
  return std::isnormal(val._get());
}
//----------------------------------------------------------------------------
/// cast from APL_Float_Base to APL_Float
APL_Float_Base::operator APL_Float() const
{
   return APL_Float(dval);
}
//----------------------------------------------------------------------------
/// up-cast from const APL_Float_Base * to const APL_Float *
const APL_Float *
APL_Float_Base::pAPL_Float() const
{
   return static_cast<const APL_Float *>(this);
}
//----------------------------------------------------------------------------
/// up-cast from APL_Float_Base * to APL_Float *
APL_Float *
APL_Float_Base::pAPL_Float()
{
   return static_cast<APL_Float *>(this);
}
//----------------------------------------------------------------------------

#define wrap1(type, fun)                                                \
inline type fun(const APL_Float_Base & d)                               \
   { return type(fun(double(d._get()))); }

/// double fun(double, double) ->
/// APL_Float fun(const APL_Float_Base &, const APL_Float_Base &)
#define wrap2(type, fun)                                                \
inline type fun(const APL_Float_Base & d, const APL_Float_Base & e)     \
   { return type(fun(double(d._get()),                                  \
                     double(e._get()))); }                              \
inline type fun(const APL_Float_Base & d, double e)                     \
   { return type(fun(double(d._get()), e)); }                           \
inline type fun(double d, const APL_Float_Base & e)                     \
   { return type(fun(d, double(e._get()))); }

/// double operator(double, double) ->
/// APL_Float operator (const APL_Float_Base &, const APL_Float_Base &)
#define op_wrap2(type, oper)                                            \
inline type operator oper(const APL_Float_Base & d,                     \
                          const APL_Float_Base & e)                     \
   { return type(double(d._get()) oper                                  \
                 double(e._get())); }                                   \
inline type operator oper(const APL_Float_Base & d, double e)           \
   { return type(double(d._get()) oper e); }                            \
inline type operator oper(const APL_Float_Base & d, APL_Integer e)      \
   { return type(double(d._get()) oper e); }                            \
inline type operator oper(const APL_Float_Base & d, uint64_t e)         \
   { return type(double(d._get()) oper e); }                            \
inline type operator oper(double d, const APL_Float_Base & e)           \
   { return type(d oper double(e._get())); }                            \
inline type operator oper(APL_Integer d, const APL_Float_Base & e)      \
   { return type(d oper double(e._get())); }                            \
inline type operator oper(int d, const APL_Float_Base & e)              \
   { return type(d oper double(e._get())); }

/// double operator(double, double) ->
/// APL_Float_Base & operator (const APL_Float_Base &, const APL_Float_Base &)
#define assop_wrap1(op)                                                \
inline APL_Float_Base & operator op ## =(APL_Float_Base & d,           \
                                         const APL_Float_Base & e)     \
   { return d._set(d._get() op e._get()); }                            \
inline APL_Float_Base & operator op ## =(APL_Float_Base & d, double e) \
   { return d._set(d._get() op e); }

// wrappers for libc functions with a single (double) argument
wrap1(double, abs)
wrap1(double, acos)
wrap1(double, asin)
wrap1(double, asinh)
wrap1(double, atan)
wrap1(double, ceil)
wrap1(double, cos)
wrap1(double, cosh)
wrap1(double, acosh)
wrap1(double, exp)
wrap1(double, floor)
wrap1(double, isfinite)
wrap1(double, log)
wrap1(double, round)
wrap1(double, sqrt)
wrap1(double, sin)
wrap1(double, sinh)
wrap1(double, tan)
wrap1(double, tgamma)
wrap1(double, tanh)

// wrappers for libc functions with two (double) arguments
wrap2(double, atan2)
wrap2(double, fmod)
wrap2(double, pow)

// wrapper for monadic -
inline APL_Float operator-(const APL_Float_Base & d)
   { return APL_Float(-d._get()); }

// wrappers for dyadic + - * and /
op_wrap2(double, +)
op_wrap2(double, -)
op_wrap2(double, *)
op_wrap2(double, /)

// wrappers for < <= > >= == and !=
op_wrap2(bool, <)
op_wrap2(bool, >)
op_wrap2(bool, <=)
op_wrap2(bool, >=)
op_wrap2(bool, ==)
op_wrap2(bool, !=)

// wrappers for += -= *= and /=
assop_wrap1(+)
assop_wrap1(-)
assop_wrap1(*)
assop_wrap1(/)

//----------------------------------------------------------------------------
inline complex<APL_Float>
complex_sqrt(complex<APL_Float> x)
{
const complex<double> cx(x.real()._get(), x.imag()._get());
const complex<double> cz = sqrt(cx);
   return complex<APL_Float>(cz.real(), cz.imag());
}
//----------------------------------------------------------------------------
inline complex<APL_Float>
complex_exponent(complex<APL_Float> x)
{
const complex<double> cx(x.real()._get(), x.imag()._get());
const complex<double> cz = exp(cx);
   return complex<APL_Float>(cz.real(), cz.imag());
}
//----------------------------------------------------------------------------
inline complex<APL_Float>
complex_power(complex<APL_Float> x, complex<APL_Float> y)
{
const complex<double> cx(x.real()._get(), x.imag()._get());
const complex<double> cy(y.real()._get(), y.imag()._get());
const complex<double> cz = pow(cx, cy);
   return complex<APL_Float>(cz.real(), cz.imag());
}
//----------------------------------------------------------------------------
#endif // __APL_FLOAT_AS_CLASS_HH_DEFINED__

