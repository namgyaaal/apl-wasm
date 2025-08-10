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

#ifndef __COLLATING_CACHE_HH_DEFINED__
#define __COLLATING_CACHE_HH_DEFINED__

#include "Common.hh"
#include "PrimitiveFunction.hh"
#include "Token.hh"
#include "Shape.hh"

class Cell;

//----------------------------------------------------------------------------
/** one item of a CollatingCache. Every character in A (of A⍋B or A⍒B gets
   one CollatingCacheEntry; if the same character occurs multiple times in A,
   then only the first one (in row-major order) creates the cache entry
   and the remaining copies of the character re-use the same
   CollatingCacheEntry.

   Later on, if two Unicodes c1 and c2 shall be compared, then the ce_shapes
   (which are actually indices into A) of the CollatingCacheEntry for c1 and
   c2 are used for the comparison and the lowest dimansion wins.
**/
/// One item in a CollatingCache
class CollatingCacheEntry
{
   friend class CollatingCache;

public:
   /// return the Unicode of this cache entry.
   Unicode get_ce_char() const
      { return ce_char; }

protected:
   /// constructor: an invalid entry (for allocatingi
   /// vector<CollatingCacheEntry> items)
   CollatingCacheEntry()
   : ce_char(Invalid_Unicode),
     ce_shape()
   {}

   /// constructor: an entry with character \b c and shape \b shape
   CollatingCacheEntry(Unicode uni, const Shape & shape_A)
   : ce_char(uni),
     ce_shape(shape_A)   // shape_A means not found
   {}

   /// the character
   const Unicode ce_char;

   /// the shape
   Shape ce_shape;

   /// assignment (to allow const ce_char)
   void operator =(const CollatingCacheEntry & other)
      { new (this)   CollatingCacheEntry(other); }

   /// compare this entry with \b other at \b axis
   int compare_axis(const CollatingCacheEntry & other, sAxis axis) const
      {
        return ce_shape.get_shape_item(axis)
             - other.ce_shape.get_shape_item(axis);
      }

   /// compare \b key with \b entry (for Heapsort::search())
   static int compare_chars(const Unicode & key,
                            const CollatingCacheEntry & entry,
                            const void * unused_ctx)
      { return key - entry.ce_char; }
};
//----------------------------------------------------------------------------
inline ostream &
operator << (ostream & out, const CollatingCacheEntry & entry)
{
   return out << "CC-entry(" << entry.get_ce_char() << ")";
}
//----------------------------------------------------------------------------
/** A collating cache which is the internal representation of the left
    argument A of dydic A⍋B or A⍒B
 */
/// A cache for speeding up dyadic ⍋ and ⍒
class CollatingCache : public vector<CollatingCacheEntry>
{
public:
   /// constructor: cache of rank r and comparison length clen
   CollatingCache(const Value & A, const vector<ShapeItem> & base,
                  ShapeItem clen);

   /// return the number of dimensions of the collating sequence
   sRank get_rank() const { return rank; }

   /// return the number of items to compare
   ShapeItem get_comp_len() const { return comp_len; }

   /// find the significance (= the index of) the entry for \b uni
   ShapeItem get_significance(Unicode uni) const;

   /// compare cache items \b ia and \b ib ascendingly.
   /// \b comp_arg is a CollatingCache pointer
   static bool greater_vec(const ShapeItem & ia, const ShapeItem & ib,
                           const void * comp_arg);

   /// compare cache items \b ia and \b ib descendingly.
   /// \b comp_arg is a CollatingCache pointer
   static bool smaller_vec(const ShapeItem & ia, const ShapeItem & ib,
                           const void * comp_arg);

protected:
   /// the rank of the collating sequence
   const sRank rank;

   
   /// the significances (according to A) of the items in B
   const vector<ShapeItem> & significances_B;

   /// the number of items to compare
   const ShapeItem comp_len;
};
//============================================================================
/** primitive functions grade up and grade down
 */
/// Base class for ⍋ and ⍒
class Bif_F12_SORT : public NonscalarFunction
{
public:
   /// Constructor
   Bif_F12_SORT(TokenTag tag)
   : NonscalarFunction(tag)
   {}

   /// sort integer vector B 
   static Token sort(Value_P B, Sort_order order);

protected:
   /// a helper structure for sorting: a char and a shape
   struct char_shape
      {
         APL_Char achar;    ///< a char
         Shape    ashape;   ///< a shape
      };

   /// sort char vector B according to collationg sequence A
   static Token sort_collating(Value_P A, Value_P B, Sort_order order);

   /// find or create the collating cache entry for \b uni
   static ShapeItem find_collating_cache_entry(Unicode uni,
                                               CollatingCache & cache);
};
//----------------------------------------------------------------------------
/** System function grade up ⍋
 */
/// The class implementing ⍋
class Bif_F12_SORT_ASC : public Bif_F12_SORT
{
public:
   /// Constructor
   Bif_F12_SORT_ASC()
   : Bif_F12_SORT(TOK_F12_SORT_ASC)
   {}

   /// overloaded Function::eval_B()
   virtual Token eval_B(Value_P B) const
      { Token ret = sort(B, SORT_ASCENDING);
        if (ret.get_Class() == TC_VALUE)   return ret;
        DOMAIN_ERROR;   // complex value(s)
      }

   /// overloaded Function::eval_AB()
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return sort_collating(A, B, SORT_ASCENDING); }

   static Bif_F12_SORT_ASC  fun;   ///< Built-in function
protected:
};
//----------------------------------------------------------------------------
/** System function grade down ⍒
 */
/// The class implementing ⍒
class Bif_F12_SORT_DES : public Bif_F12_SORT
{
public:
   /// Constructor
   Bif_F12_SORT_DES()
   : Bif_F12_SORT(TOK_F12_SORT_DES)
   {}

   /// overloaded Function::eval_B()
   virtual Token eval_B(Value_P B) const
      { Token ret = sort(B, SORT_DESCENDING);
        if (ret.get_Class() == TC_VALUE)   return ret;
        DOMAIN_ERROR;   // complex value(s)
      }

   /// overloaded Function::eval_AB()
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return sort_collating(A, B, SORT_DESCENDING); }

   static Bif_F12_SORT_DES  fun;   ///< Built-in function
protected:
};
//----------------------------------------------------------------------------


#endif // __COLLATING_CACHE_HH_DEFINED__
