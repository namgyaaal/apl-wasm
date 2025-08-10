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

#include "Quad_MAP.hh"
#include "Workspace.hh"

Quad_MAP  Quad_MAP::fun;

//----------------------------------------------------------------------------
/*
    8 ⎕CR A←5 2⍴'eEwWaAzZ92' ◊ 8 ⎕CR B←'Halloween' ◊ A ⎕MAP B
 */
Token
Quad_MAP::eval_AB(Value_P A, Value_P B) const
{
bool recursive = false;

   /* a nested scalar A indicates recursive ⎕MAP. For example

      Map←6 2⍴'aAbBcCdDeEfF'   ⍝ lowercase to uppercase for a-f
      Map ⎕MAP B      ⍝ flat (top-level only) mapping of B
      (⊂Map) ⎕Map B   ⍝ recursive mapping of B

      if A is a nested  scalar, then setrecursive and proceed with ⊃A.
    */
   if (A->get_rank() == 0 && A->get_cfirst().is_pointer_cell())
      {
         recursive = true;
         A = A->get_cfirst().get_pointer_value();
      }

ShapeItem map_len = A->get_rows();               // the number of mappings
   if      (A->get_rank() == 0)   RANK_ERROR;
   else if (A->get_rank() == 1)
      {
        // A is a vector, accepted to avoid the need for (N 2⍴A)
        // A[1 3 5...] are the keys while A[2 4 6...] are the mapped values
        //
        map_len = A->element_count();
        if (map_len & 1)
           {
             MORE_ERROR() << "Odd length of A in A ⎕MAP B";
             LENGTH_ERROR;
           }
        map_len = map_len >> 1;
      }
   else if (A->get_rank() > 2)   RANK_ERROR;

   if (map_len == 0)
           {
             MORE_ERROR() << "A with length 0 in A ⎕MAP B";
             LENGTH_ERROR;
           }

ShapeItem * indices = new ShapeItem[map_len];
   if (indices == 0)   WS_FULL;

   loop(m, map_len)   indices[m] = m;

const ravel_comp_len ctx = { &A->get_cfirst(), 1};
   Heapsort<ShapeItem>::sort(indices, map_len, &ctx, &Quad_MAP::greater_map);

   // complain about duplicated keys
   //
const double qct = Workspace::get_CT();
   for (ShapeItem m = 1; m < map_len; ++m)
       {
          const Cell & cm1 = A->get_cravel(2*indices[m - 1]);
          const Cell & cm  = A->get_cravel(2*indices[m    ]);
          if (cm1.equal(cm, qct))
             {
               const int qio = Workspace::get_IO();
               MORE_ERROR() << "Duplicate keys (e.g. A["
                            << (qio + indices[m - 1]) << "] and A["
                            << (qio + indices[m]) << "]) in 'A ⎕MAP B'";
               delete[] indices;
               DOMAIN_ERROR;
             }
       }

Value_P Z = do_map(*A, indices, B.get(), recursive);
   delete[] indices;
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
bool
Quad_MAP::greater_map(const ShapeItem & a, const ShapeItem & b,
                      const void * ctx)
{
const ravel_comp_len * rcl = reinterpret_cast<const ravel_comp_len *>(ctx);
const Cell * cells = rcl->ravel;

   if (const Comp_result cr = cells[2*a].compare(cells[2*b]))   // A[1] ≠ B[1]
      return cr == COMP_GT;

   return a > b;
}
//----------------------------------------------------------------------------
int
compare_MAP(const Cell & key, const ShapeItem & item, const void * ctx)
{
const ravel_comp_len * rcl = reinterpret_cast<const ravel_comp_len *>(ctx);
const Cell * cells_A = rcl->ravel;

   return key.compare(cells_A[2*item]);
}
//----------------------------------------------------------------------------

Value_P
Quad_MAP::do_map(const Value & A, const ShapeItem * ordered_indices_A,
                 const Value * B, bool recursive)
{
Value_P Z(B->get_shape(), LOC);         // the result, ⍴Z ←→ ⍴B

const ravel_comp_len ctx = { &A.get_cfirst(),   // start of the ravel
                             1                  // number of chars to compare
                           };

const ShapeItem len_B = B->element_count();
const ShapeItem map_len = (A.get_rank() == 1) ? A.element_count() >> 1
                                              : A.get_rows();
   if (len_B == 0)   // empty value
      {
         const Cell & cell_B = B->get_cfirst();
         if (const ShapeItem * map =
                   Heapsort<ShapeItem>::search<const Cell &>
                                              (cell_B,
                                               ordered_indices_A,
                                               map_len,
                                               compare_MAP,
                                               &ctx))
            {
              const Cell & cell_A = A.get_cravel(*map*2 + 1);
              if (cell_A.is_pointer_cell())
                 {
                   Cell & cell_Z0 = Z->get_wproto();
                   cell_Z0.init(cell_A, *Z, LOC);
                   cell_Z0.get_pointer_value()->to_type(false);
                 }
              else if (cell_A.is_character_cell())
                 Z->set_proto_Spc();
              else if (cell_A.is_numeric())
                 Z->set_proto_Int();
              else
                 DOMAIN_ERROR;
            }
         else   // not mapped, simple
            {
              Z->set_default(cell_B, LOC);
            }
      }

   loop(b, len_B)
       {
         const Cell & cell_B = B->get_cravel(b);
         if (const ShapeItem * map = Heapsort<ShapeItem>::search<const Cell &>
                   (cell_B, ordered_indices_A, map_len, compare_MAP, &ctx))
            {
             Z->next_ravel_Cell(A.get_cravel(*map*2 + 1));
            }
         else   // cell_B shall not be mapped
            {
              if (recursive && cell_B.is_pointer_cell())   // nested: recursive
                 {
                   Value_P sub_B = cell_B.get_pointer_value();
                   Value_P sub_Z = do_map(A, ordered_indices_A,
                                          sub_B.get(), true);
                   Z->next_ravel_Pointer(sub_Z.get());
                 }
              else   // not mapped, simple
                 {
                  Z->next_ravel_Cell(cell_B);
                 }
            }
       }

   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------
