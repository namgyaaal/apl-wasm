/*
    This file is part of GNU APL, a free implementation of the
    ISO/IEC Standard 13751, "Programming Language APL, Extended"

    Copyright © 2017 Elias Mårtenson

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

#include "PointerCell.hh"
#include "Quad_RE.hh"
#include "Workspace.hh"

Quad_RE Quad_RE::fun;

#if HAVE_LIBPCRE2_32

# include "Regexp.hh"

//----------------------------------------------------------------------------
Quad_RE::Flags::Flags(const UCS_string & flags_string)
   : flags(0),
     error_on_no_match(false),
     global(false),
     result_type(RT_string)
{
int ofcnt = 0;
   loop (f, flags_string.size())
      {
        const Unicode uni = flags_string[f];
        switch(uni)
           {
             case UNI_SUBSET:     result_type = RT_partition;  ++ofcnt;  break;
             case UNI_DOWN_ARROW: result_type = RT_pos_len;    ++ofcnt;  break;
             case UNI_SLASH:      result_type = RT_reduce;     ++ofcnt;  break;
             case UNI_g:          global = true;                         break;
             case UNI_E:          error_on_no_match = true;              break;
             case UNI_i:          flags |= PCRE2_CASELESS;               break;
             case UNI_m:          flags |= PCRE2_MULTILINE;              break;
             case UNI_s:          flags |= PCRE2_DOTALL;                 break;
             case UNI_x:          flags |= PCRE2_EXTENDED;               break;
             default:
                  MORE_ERROR() << "Unknown ⎕RE flag: '" << UCS_string(1, uni)
                               << "'. Valid flags are: Eimsx⊂↓/";
                DOMAIN_ERROR;
           }
     }

   if (ofcnt > 1)
      {
        MORE_ERROR() << "Multiple ⎕RE output flags: '" << flags_string
                     << "'. The ⎕RE output flags are: ⊂↓/";
        DOMAIN_ERROR;
      }
}
//----------------------------------------------------------------------------
static Value_P
deep_value(int idx, const PCRE2_SIZE * ovector, int count, const int * parents,
           const int * child_count, const UCS_string * B)
{
   if (child_count[idx] == 0)   // simple RE (no sub-REs)
      {
        const PCRE2_SIZE start = ovector[2*idx];
        const PCRE2_SIZE end   = ovector[2*idx + 1];
        if (B)   // string
           {
             const UCS_string item(*B, start, end - start);
             Value_P Z(item, LOC);
             Z->check_value(LOC);
             return Z;
           }
        else     // pos+len
           {
             Value_P Z(2, LOC);
             Z->next_ravel_Int(start);
             Z->next_ravel_Int(end - start);
             Z->check_value(LOC);
             return Z;
           }
      }

ShapeItem ini = B ? 1 : 2;
Value_P Z(ini + child_count[idx], LOC);
   if (B)
      {
        const PCRE2_SIZE start = ovector[2*idx];
        const PCRE2_SIZE end = ovector[2*idx + 1];
        const UCS_string item(*B, start, end - start);
        Value_P sub_value(item, LOC);
        sub_value->check_value(LOC);
        Z->next_ravel_Pointer(sub_value.get());
      }
   else
      {
        Z->next_ravel_Int(ovector[2*idx]);
        Z->next_ravel_Int(ovector[2*idx + 1] - ovector[2*idx]);
      }

   loop(ch, count)
       {
         if (parents[ch] != idx)   continue;   // ch is not a child of idx
         Value_P CH = deep_value(ch, ovector, count, parents, child_count, B);
         Z->next_ravel_Pointer(CH.get());
       }

   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
Quad_RE::regex_results(const Regexp & A, const Flags & X, const UCS_string & B)
{
ShapeItem B_offset = 0;   // updated by XXX_result() functions

   switch(X.get_result_type())
      {
        case RT_reduce:
        case RT_partition: return partition_result(A, X, B);

        case RT_string:    if (X.get_global())   break;   // continue below
                                return string_result(A, X, B, B_offset);

        case RT_pos_len:   if (X.get_global())   break;   // continue below
                                return index_result (A, X, B, B_offset);

        default:           FIXME;
      }

   /* At this point the result type is RT_string or RT_pos_len, and the global
      flag is set (which means that all matches shall be returned).

      We have not yet called the corresponding string_result() or index_result()
      function (which creates the result), and therefore do not know how long
      Z will become.

      We handle this by:

      1. starting with a ⍴Z of of cfg_SHORT_VALUE_LENGTH_WANTED, and
      2. doubling ⍴Z whenever needed, and finally
      3. shrinking ⍴Z to the true number of matches.
    */

Value_P Z(cfg_SHORT_VALUE_LENGTH_WANTED, LOC);

   for (;;)
       {
         Value_P ZZ;
         switch(X.get_result_type())
            {
              case RT_string:
                   ZZ = string_result(A, X, B, B_offset);
                   break;

              case RT_pos_len:
                   ZZ = index_result(A, X, B, B_offset);
                   break;

              default:           FIXME;
            }

         if (B_offset == -1)   break;   // no more matches

         if (!Z->more())   // Z is full
            {
              const ShapeItem ec_Z = Z->element_count();
              Value_P Z2(2*ec_Z, LOC);
              loop(z, ec_Z)
                  {
                    const Cell & cell = Z->get_cravel(z);
                    Z2->next_ravel_Pointer(cell.get_pointer_value().get());
                    Z->release(z, LOC);
                  }
              Z = Z2;
            }

         Z->next_ravel_Pointer(ZZ.get());
       }

   // most likely, Z is over-allocated at this point and we should init
   // the not-yet-used Cells before shrinking Z.
   //
const Shape sh_Z(Z->get_valid_item_count());
   while (Z->more())   Z->next_ravel_0();  // init the remaining cells
   Z->check_value(LOC);

   Z->set_shape(sh_Z);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
Quad_RE::partition_result(const Regexp & A, const Flags & X,
                          const UCS_string & B)
{
const PCRE2_SIZE len = B.size();
Value_P Z(len, LOC);

PCRE2_SIZE B_offset = 0;
ShapeItem match_id_partition = 1;
ShapeItem match_id_compress  = 1;

ShapeItem & match_id = X.get_result_type() == RT_partition
                     ? match_id_partition : match_id_compress;

   // compress needs a numeric vector like  1 1 1 0 0 0 0 0 1 1 1...
   // partition needs a numeric vector like 1 1 1 2 2 2 2 2 3 3 3...
   // where the ranges of equal numbers belong to the same partition.
   //
   // We call RegexpMatch() multiple times and create a partition (-range)
   // for every match.
   //
PCRE2_SIZE last_end = 0;
   for (; B_offset < len; ++match_id_partition)
       {
         RegexpMatch rem(A.get_code(), B, B_offset);
         if (!rem.is_match())   break;

         const PCRE2_SIZE * ovector = rem.get_ovector();
         const PCRE2_SIZE start = ovector[0];
         const PCRE2_SIZE end   = ovector[1];

         // zeros (if any) between the previous and the current partition
         loop(z, start - last_end)   Z->next_ravel_0();
         last_end = end;

         loop (b, end - start)  Z->next_ravel_Int(match_id);

         if (!X.get_global())   break;
         B_offset = end;
       }

   while (Z->more())   Z->next_ravel_0();
   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
Quad_RE::string_result(const Regexp & A, const Flags & X,
                       const UCS_string & B, ShapeItem & B_offset)
{
RegexpMatch rem(A.get_code(), B, B_offset);
   if (!rem.is_match())
      {
        B_offset = -1;   // indicates an error
        if (X.get_global())               return Value_P();
        if (!X.get_error_on_no_match())   return Idx0(LOC);
        MORE_ERROR() << "No match";
        DOMAIN_ERROR;
      }

   // rem is a match
   //
const PCRE2_SIZE * ovector = rem.get_ovector();
   B_offset = ovector[1];
   if (0 && rem.num_matches() == 1)   // simple match
      {
        // single string
        //
        Value_P Z(2, LOC);
        const PCRE2_SIZE start = ovector[0];
        const PCRE2_SIZE end   = ovector[1];
        Z->next_ravel_Int(start);
        Z->next_ravel_Int(end - start);
        Z->check_value(LOC);
        return Z;
      }

const uint32_t ovector_count = rem.get_ovector_count();
   B_offset = ovector[1];
int parents[ovector_count];
int ccount[ovector_count];

   loop(o, ovector_count)
       {
         parents[o] = -1;
         ccount[o]  = 0;
       }

   for (int o = ovector_count - 1; o >= 0; --o)
       {
         const PCRE2_SIZE ostart = ovector[2*o];
         for (int p = o - 1; p >= 0; --p)
             {
               if (ovector[2*p] <= ostart && ostart < ovector[2*p + 1])
                  {
                    parents[o] = p;
                    ++ccount[p];
                    break;
                  }
             }
       }

   return deep_value(0, ovector, ovector_count, parents, ccount, &B);
}
//----------------------------------------------------------------------------
Value_P
Quad_RE::index_result(const Regexp & A, const Flags & X,
                      const UCS_string & B, ShapeItem & B_offset)
{
RegexpMatch rem(A.get_code(), B, B_offset);
   if (!rem.is_match())
      {
        B_offset = -1;
        if (X.get_global())               return Value_P();
        if (!X.get_error_on_no_match())   return Idx0(LOC);
        MORE_ERROR() << "No match";
        DOMAIN_ERROR;
      }

   // rem is a match
   //
const PCRE2_SIZE * ovector = rem.get_ovector();
   B_offset = ovector[1];
   if (rem.num_matches() == 1)   // simple match
      {
        // single string
        //
        Value_P Z(2, LOC);
        const PCRE2_SIZE start = ovector[0];
        const PCRE2_SIZE end   = ovector[1];
        Z->next_ravel_Int(start);
        Z->next_ravel_Int(end - start);
        Z->check_value(LOC);
        return Z;
      }

const uint32_t ovector_count = rem.get_ovector_count();
int parents[ovector_count];
   B_offset = ovector[1];
int ccount[ovector_count];

   loop(o, ovector_count)
       {
         parents[o] = -1;
         ccount[o]  = 0;
       }

   for (int o = ovector_count - 1; o >= 0; --o)
       {
         const PCRE2_SIZE ostart = ovector[2*o];
         for (int p = o - 1; p >= 0; --p)
             {
               if (ovector[2*p] <= ostart && ostart < ovector[2*p + 1])
                  {
                    parents[o] = p;
                    ++ccount[p];
                    break;
                  }
             }
       }

   return deep_value(0, ovector, ovector_count, parents, ccount, 0);
}
//----------------------------------------------------------------------------
Token
Quad_RE::eval_AXB(Value_P A, Value_P X, Value_P B) const
{
   if (A->get_rank() > 1)   RANK_ERROR;
   if (X->get_rank() > 1)   RANK_ERROR;

   if (!A->is_char_string())
      {
        MORE_ERROR() << "left ⎕RE arguments must be a string value";
        DOMAIN_ERROR;
      }

Flags flags(X->get_UCS_ravel());
Regexp regexp(A->get_UCS_ravel(), flags.get_compflags());

const Shape & shape = B->get_shape();
    if (shape.get_rank() == 0)
        return Token(TOK_APL_VALUE1, Idx0(LOC));

    if (B->is_char_string())
       {
         Value_P Z = regex_results(regexp, flags, B->get_UCS_ravel());
         Z->check_value(LOC);
         return Token(TOK_APL_VALUE1, Z);
       }

Value_P Z(shape, LOC);
   for (ShapeItem i = 0 ; i < shape.get_volume() ; i++)
       {
         const Cell & cell = B->get_cravel(i);
         Value_P B_sub = cell.to_value(LOC);
         if (!B_sub->is_char_string())
            {
              MORE_ERROR() << "Cell does not contain a string";
              DOMAIN_ERROR;
            }

         Value_P Z_sub = regex_results(regexp, flags, B_sub->get_UCS_ravel());
         Z_sub->check_value(LOC);
         Z->next_ravel_Pointer(Z_sub.get());
       }

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}

#else // ! HAVE_LIBPCRE2_32

extern Token missing_files(const char * qfun,  const char ** libs,
                           const char ** hdrs, const char ** pkgs);

Token
Quad_RE::eval_AXB(Value_P A, Value_P X, Value_P B) const
{
const char * libs[] = { "libpcre.so",   0 };
const char * hdrs[] = { "pcre2.h",      0 };
const char * pkgs[] = { "libpcre3-dev", 0 };

   return missing_files("⎕RE", libs, hdrs, pkgs);
}

#endif   // HAVE_LIBPCRE2_32
