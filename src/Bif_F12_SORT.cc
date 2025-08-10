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

#include "Assert.hh"
#include "Bif_F12_SORT.hh"
#include "Cell.hh"
#include "Heapsort.hh"
#include "Macro.hh"
#include "Value.hh"
#include "Workspace.hh"

Bif_F12_SORT_ASC  Bif_F12_SORT_ASC::fun;     // ⍋
Bif_F12_SORT_DES  Bif_F12_SORT_DES::fun;     // ⍒

//----------------------------------------------------------------------------
/** class CollatingCache is a helper that efficiently maps characters in
    vector B to an integer vector B1 (the significances of B) according to
    the collating sequence A.

    Sorting with collation has to first search every item B[j] in A where
    the first match in A defines the significance of B[j]. For that reason
    subsequent copies A[a+k] of some A[a] will never match and are therefore
    not stored in the CollatingCache.
 **/
CollatingCache::CollatingCache(const Value & A,
                               const vector<ShapeItem> & signif,
                               ShapeItem clen)
   : rank(A.get_rank()),
     significances_B(signif),
     comp_len(clen)
{
const ShapeItem ec_A = A.element_count();
UCS_string UA;
   UA.reserve(ec_A);
   loop(a, ec_A)   UA.append(A.get_cravel(a).get_char_value());

UCS_string UA1 = UA.unique();

   reserve(UA1.size());

   // create CollatingCacheEntry for every char in UA1. At this point, all
   // entries are located at the end of A.
   //
   loop(a, UA1.size())
       {
         const Unicode uni = UA1[a];
         const CollatingCacheEntry entry(uni, A.get_shape());
         push_back(entry);
       }

   // move entries back
   //
   loop(a, ec_A)
      {
        const Unicode uni = A.get_cravel(a).get_char_value();
        CollatingCacheEntry & entry = at(get_significance(uni));

        ShapeItem aq = a;
        loop(r, A.get_rank())
           {
             const sAxis axis = entry.ce_shape.get_rank() - r - 1;
             const ShapeItem ar = aq % A.get_shape_item(axis);
             Assert(ar <= A.get_shape_item(axis));
             if (entry.ce_shape.get_shape_item(axis) > ar)
                entry.ce_shape.set_shape_item(axis, ar);
             aq /= A.get_shape_item(axis);
           }
      }

   // add one entry for all characters in B that are not in A
   //
CollatingCacheEntry others(Invalid_Unicode, A.get_shape());
   push_back(others);
}
//----------------------------------------------------------------------------
bool
CollatingCache::greater_vec(const ShapeItem & Za, const ShapeItem & Zb,
                            const void * comp_arg)
{
const CollatingCache & cache =
                      *reinterpret_cast<const CollatingCache *>(comp_arg);
const ShapeItem * significance_a = &cache.significances_B[cache.comp_len * Za];
const ShapeItem * significance_b = &cache.significances_B[cache.comp_len * Zb];

const sRank rank = cache.get_rank();

   rev_loop(r, rank)
   loop(c, cache.get_comp_len())
       {
         const CollatingCacheEntry & a = cache[significance_a[c]];
         const CollatingCacheEntry & b = cache[significance_b[c]];
         if (const int diff = a.compare_axis(b, r))   return diff > 0;
       }

   return significance_a > significance_b;
}
//----------------------------------------------------------------------------
bool
CollatingCache::smaller_vec(const ShapeItem & Za, const ShapeItem & Zb,
                            const void * comp_arg)
{
const CollatingCache & cache =
                      *reinterpret_cast<const CollatingCache *>(comp_arg);
const ShapeItem * significance_a = &cache.significances_B[cache.comp_len * Za];
const ShapeItem * significance_b = &cache.significances_B[cache.comp_len * Zb];

const sRank rank = cache.get_rank();

   rev_loop(r, rank)
   loop(c, cache.get_comp_len())
       {
         const CollatingCacheEntry & a = cache[significance_a[c]];
         const CollatingCacheEntry & b = cache[significance_b[c]];
         if (const int diff = a.compare_axis(b, r))   return diff < 0;
       }

   return significance_a > significance_b;
}
//----------------------------------------------------------------------------
ShapeItem
CollatingCache::get_significance(Unicode uni) const
{
const CollatingCacheEntry * entries = &at(0);
const CollatingCacheEntry * entry =

   Heapsort<CollatingCacheEntry>::search<Unicode>(uni, entries, size(),
                                         CollatingCacheEntry::compare_chars, 0);

   if (entry)   return entry - entries;
   return size() - 1;   // the entry for characters not in A
}
//============================================================================
Token
Bif_F12_SORT::sort(Value_P B, Sort_order order)
{
   if (B->is_scalar())          return Token(TOK_ERROR, E_RANK_ERROR);
   if (!B->can_be_compared())   return Token(TOK_ERROR, E_DOMAIN_ERROR);

const ShapeItem len_BZ = B->get_shape_item(0);
   if (len_BZ == 0)   return Token(TOK_APL_VALUE1, Idx0(LOC));
const ShapeItem comp_len = B->element_count()/len_BZ;

vector<ShapeItem> ordered_indices_B;
   Cell::sorted_indices(ordered_indices_B, *B, order, comp_len);

Value_P Z(len_BZ, LOC);
const int qio = Workspace::get_IO();

   loop(b, len_BZ)   Z->next_ravel_Int(ordered_indices_B[b] + qio);

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Bif_F12_SORT::sort_collating(Value_P A, Value_P B, Sort_order order)
{
const APL_Integer qio = Workspace::get_IO();
   if (A->is_scalar())   RANK_ERROR;
   if (A->NOTCHAR())     DOMAIN_ERROR;
   if (B->NOTCHAR())     DOMAIN_ERROR;
   if (B->is_scalar())   return Token(TOK_APL_VALUE1, IntScalar(qio, LOC));

const ShapeItem len_BZ = B->get_shape_item(0);
   if (len_BZ == 0)   return Token(TOK_APL_VALUE1, Idx0(LOC));   // return ⍬
   if (len_BZ == 1)
      {
        Value_P Z(1, LOC);
        Z->next_ravel_Int(qio);
        Z->check_value(LOC);
        return Token(TOK_APL_VALUE1, Z);
      }

const ShapeItem ec_B = B->element_count();
const ShapeItem comp_len = ec_B/len_BZ;

   // create integer vector B1 which is a 1:1 mapping of the characters
   // in the ravel of B to their integer significance.
   //
vector<ShapeItem> B1;
   B1.reserve(ec_B);
CollatingCache cache(*A, B1, comp_len);
   loop(b, ec_B)
      {
        const Unicode uni = B->get_cravel(b).get_char_value();
        const ShapeItem b1 = cache.get_significance(uni);
        B1.push_back(b1);
      }

vector<ShapeItem> vZ;
   vZ.reserve(len_BZ);
   loop(z, len_BZ)   vZ.push_back(z);

   if (order == SORT_ASCENDING)
      Heapsort<ShapeItem>::sort(vZ.data(), len_BZ, &cache,
                                &CollatingCache::greater_vec);
   else
      Heapsort<ShapeItem>::sort(vZ.data(), len_BZ, &cache,
                                &CollatingCache::smaller_vec);
Value_P Z(len_BZ, LOC);
   loop(z, len_BZ)   Z->next_ravel_Int(vZ[z] + qio);
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------

