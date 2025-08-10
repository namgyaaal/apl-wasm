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

#ifndef __BIF_F12_TAKE_DROP_HH_DEFINED__
#define __BIF_F12_TAKE_DROP_HH_DEFINED__

#include "Common.hh"
#include "PrimitiveFunction.hh"

//============================================================================
/** primitive functions Take and First */
/// The class implementing ↑
class Bif_F12_TAKE : public NonscalarFunction_default_identity
{
public:
   /// Constructor
   Bif_F12_TAKE()
   : NonscalarFunction_default_identity(TOK_F12_TAKE)
   {}

   /// overloaded Function::eval_B()
   virtual Token eval_B(Value_P B) const
      { return Token(TOK_APL_VALUE1, first(*B));}

   /// overloaded Function::eval_AB()
   virtual Token eval_AB(Value_P A, Value_P B) const;

   /// overloaded Function::eval_AXB()
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const;

   /// overloaded Function::eval_XB()
   virtual Token eval_XB(Value_P X, Value_P B) const;

   /// Take from B according to ravel_A
   static Value_P do_take(const Shape & shape_Zi, const Value & B,
                          AxesBitmap axes);

   /// Fill Z with B, pad as necessary
   static void fill(const Shape & shape_Zi, Value & Z_owner,
                    const Value & B, AxesBitmap axes);

   static Bif_F12_TAKE  fun;   ///< Built-in function

   /// ↑B
   static Value_P first(const Value & B);

protected:
   /// Take A from B
   Token take(Value_P A, Value_P B);
};
//============================================================================
/** primitive function drop */
/// The class implementing ↓
class Bif_F12_DROP : public NonscalarFunction
{
public:
   /// Constructor
   Bif_F12_DROP()
   : NonscalarFunction(TOK_F12_DROP)
   {}

   /// overloaded Function::eval_AB()
   virtual Token eval_AB(Value_P A, Value_P B) const;

   /// overloaded Function::eval_AXB()
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const;

   static Bif_F12_DROP  fun;   ///< Built-in function

protected:

};
//============================================================================
/// A helper class for Bif_F12_TAKE and Bif_F12_DROP. It implements an iterator
/// that iterates over the indices (as dictated by left argument A) of the
/// right argument B of A↑B or A↓B,
class TakeDropIterator
{
public:
   /// constructor
   TakeDropIterator(bool take, const Shape & sh_A, const Shape & sh_B)
   : ref_B(sh_B),
     current_offset(0),
     outside_B(0),
     has_overtake(false),
     done(false)
      {
        // increase ⍴⍴B if necessary
        //
        ref_B.expand_rank(sh_A.get_rank());

        ShapeItem _weight = 1;
        loop(r, sh_A.get_rank())
            {
              const ShapeItem sA = sh_A.get_rank() == 0
                                 ? 1 : sh_A.get_transposed_shape_item(r);
              const ShapeItem sB = (sh_B.get_rank() == 0 ||
                                    r >= sh_B.get_rank())
                                 ? 1 : sh_B.get_transposed_shape_item(r);

              ShapeItem _from, _to;
              if (take)   // sh_A ↑ B
                 {
                   if (sA < 0)   // take from end
                      {
                        _to   = sB;
                        _from = sB + sA;   // + since sA < 0
                      }
                   else          // take from start
                      {
                        _from = 0;
                        _to   = sA;
                      }

                   if (_from < 0 || _from >= sB)   outside_B |= 1ULL << r;
                   if (_from < 0 || _to >= sB)     has_overtake = true;
                 }
              else        // sh_A ↓ B
                 {
                   if (sA < 0)   // drop from end
                      {
                        _from = 0;
                        _to   = sB + sA;   // + since sA < 0
                      }
                   else          // drop from start
                      {
                        _from = sA;
                        _to   = sB;
                      }

                   if (_from >= _to)   // over-drop
                      {
                        _from = 0;
                        _to   = 0;
                      }
                 }

              Assert(_from <= _to);

              _ftwc & ftwc_r = ftwc[r];
              ftwc_r.from    = _from;
              ftwc_r.to      = _to;
              ftwc_r.weight  = _weight;
              ftwc_r.current = _from;
              current_offset += _from * _weight;

              if (_from == _to)   done = true;   // empty array

              _weight *= sB;
            }
      }

   /// the work-horse of the iterator
   void operator++()
      {
        loop(r, ref_B.get_rank())
           {
             _ftwc & ftwc_r = ftwc[r];
             ++ftwc_r.current;
              current_offset += ftwc_r.weight;
              if (has_overtake)
                 {
                   if (ftwc_r.current < 0 ||
                       ftwc_r.current >= ref_B.get_transposed_shape_item(r))
                      outside_B |= 1ULL << r;
                   else
                      outside_B &= ~(1ULL << r);
                 }

              if (ftwc_r.current < ftwc_r.to)   return;

              // end of dimension reached: reset this dimension
              // and increment next dimension
              //
              current_offset -= ftwc_r.weight * (ftwc_r.current - ftwc_r.from);
              ftwc_r.current = ftwc_r.from;
              if (has_overtake)
                 {
                   if (ftwc_r.current < 0 ||
                       ftwc_r.current >= ref_B.get_transposed_shape_item(r))
                      outside_B |= 1ULL << r;
                   else
                      outside_B &= ~(1ULL << r);
                 }
           }
        done = true;
      }

   /// return the prototype for an axis
   ShapeItem axis_proto(AxesBitmap axes) const;

   /// return true iff this inerator has more items to come.
   bool has_more() const   { return !done; }

   /// return the current offset (if inside B) or else -1.
   ShapeItem operator()() const
      { return outside_B ? -1 : current_offset; }

protected:
   /// shape of the source array
   Shape ref_B;

   /// from / to / weight / current
   struct _ftwc ftwc[MAX_RANK];

   /// the current offset from ↑B
   ShapeItem current_offset;

   /// a bitmask of dimensions with overtake
   ShapeItem outside_B;

   /// true iff over-Take of at least one dimension
   bool has_overtake;

   /// true iff this interator has reached its final item
   bool done;
};
//============================================================================
#endif // __BIF_F12_TAKE_DROP_HH_DEFINED__
