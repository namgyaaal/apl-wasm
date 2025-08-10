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

#include "CDR.hh"
#include "CharCell.hh"
#include "ComplexCell.hh"
#include "FloatCell.hh"
#include "IntCell.hh"
#include "PointerCell.hh"
#include "Value.hh"
#include "Workspace.hh"

//----------------------------------------------------------------------------
void
CDR::to_CDR(CDR_string & result, const Value * value)
{
const CDR_type type = value->get_CDR_type();
const int len = value->total_CDR_size_brutto(type);
   result.reserve(len + 1);
   result.clear();

   fill(result, type, len, *value);
}
//----------------------------------------------------------------------------
void
CDR::fill(CDR_string & result, int type, int len, const Value & val)
{
   Assert((len & 0x0F) == 0);

/*
    struct tf3_header
    {
       uint32_t ptr;      // byte reversed
       uint32_t nb;       // byte reversed
       uint32_t nelm;     // byte reversed
       uint8_t  type;
       uint8_t  rank;
       uint8_t  fill[2];
       uint32_t dim[1];   // byte reversed
    };
*/

   // ptr
   //
const uint32_t ptr = 0x00002020;
   result.push_back(Unicode(ptr >> 24 & 0xFF));
   result.push_back(Unicode(ptr >> 16 & 0xFF));
   result.push_back(Unicode(ptr >>  8 & 0xFF));
   result.push_back(Unicode(ptr       & 0xFF));

   // nb (header + data + sub-values + padding)
   //
   result.push_back(Unicode(len >> 24 & 0xFF));
   result.push_back(Unicode(len >> 16 & 0xFF));
   result.push_back(Unicode(len >>  8 & 0xFF));
   result.push_back(Unicode(len       & 0xFF));

   // nelm
   //
const uint32_t nelm = val.element_count();
   result.push_back(Unicode(nelm >> 24 & 0xFF));
   result.push_back(Unicode(nelm >> 16 & 0xFF));
   result.push_back(Unicode(nelm >>  8 & 0xFF));
   result.push_back(Unicode(nelm       & 0xFF));

   // type, rank, 0, 0
   //
   result.push_back(Unicode(type));
   result.push_back(Unicode(val.get_rank()));
   result.push_back(Unicode_0);
   result.push_back(Unicode_0);

   // shape
   //
   loop(r, val.get_rank())
      {
        const uint32_t sh = val.get_shape_item(r);
        result.push_back(Unicode(sh >> 24 & 0xFF));
        result.push_back(Unicode(sh >> 16 & 0xFF));
        result.push_back(Unicode(sh >>  8 & 0xFF));
        result.push_back(Unicode(sh       & 0xFF));
      }

   // body
   //
   if (type == 0)        // packed bit vector
      {
        int accu = 0;

        loop(e, nelm)
           {
             const int bit = e%8;
             const APL_Integer i = val.get_cravel(e).get_int_value();
             Assert(i == 0 || i == 1);
             if (i)   accu |= 0x80 >> bit;
             if (bit == 7)
                {
                  result.push_back(Unicode(accu));
                  accu = 0;
                }
           }

        if (nelm % 8)   // partly filled
           {
             result.push_back(Unicode(accu));
           }

      }
   else if (type == 1)   // 4 byte ints vector
      {
        loop(e, nelm)
           {
             uint64_t i = val.get_cravel(e).get_int_value();
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             Assert(i == 0 || i == 0xFFFFFFFF);
           }
      }
   else if (type == 2)   // 8 byte float vector
      {
        loop(e, nelm)
           {
             const APL_Float v = val.get_cravel(e).get_real_value();
             const APL_Float * dv = &v;
             uint64_t i = *reinterpret_cast<const uint64_t *>(dv);
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
           }
      }
   else if (type == 3)   // 16 byte complex vector
      {
        loop(e, nelm)
           {
             APL_Float v = val.get_cravel(e).get_real_value();
             const APL_Float * dv = &v;
             uint64_t i = *reinterpret_cast<const uint64_t *>(dv);
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;

             v = val.get_cravel(e).get_imag_value();
             dv = &v;
             i = *reinterpret_cast<const uint64_t *>(dv);
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
           }
      }
   else if (type == 4)   // 1 byte char vector
      {
        loop(e, nelm)
           {
             const Unicode uni = val.get_cravel(e).get_char_value();
             Assert(uni >= 0);
             Assert(uni < 256);
             result.push_back(uni);
           }
      }
   else if (type == 5)   // 4 byte UNICODE vector
      {
        loop(e, nelm)
           {
             uint32_t i = val.get_cravel(e).get_char_value();
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
             result.push_back(Unicode(i & 0xFF));   i >>= 8;
           }
      }
   else if (type == 7)   // packed vector with 4 bytes offsets.
      {
        // the first offset is after header + offset-vector and then increases
        // by the size of each sub-value. I.e.
        //
        //   +------------------+
        //   | TOP-LEVEL-HEADER |                        16 bytes + shape
        //   +------------------+
        //   | OFFSET-1         | --------+               4 bytes
        //   +------------------+         |
        //   | OFFSET-2         | --------|--+            4 bytes
        //   +------------------+         |  |
        //   | ...              |         |  |              ...
        //   +------------------+         |  |
        //   | OFFSET-nelm      | --------|--|--+         4 bytes
        //   +------------------+         |  |  |
        //   | SUB-HEADER-1     | <-------+  |  |        16 bytes + shape
        //   | VALUE-1          |            |  |
        //   +------------------+            |  |
        //   | SUB-HEADER-2     | <----------+  |        16 bytes + shape
        //   | VALUE-2          |               |
        //   +------------------+               |
        //   | ...              |               |           ...
        //   +------------------+               |
        //   | SUB-HEADER-nelm  | <-------------+        16 bytes + shape
        //   | VALUE-nelm       |
        //   +------------------+
        //

        uint32_t offset = 16 + 4*val.get_rank() + 4*nelm;
        while (offset & 0x0F)   ++offset;
        loop(e, nelm)
           {
             result.push_back(Unicode(offset       & 0xFF));
             result.push_back(Unicode(offset >>  8 & 0xFF));
             result.push_back(Unicode(offset >> 16 & 0xFF));
             result.push_back(Unicode(offset >> 24 & 0xFF));

             const Cell & cell = val.get_cravel(e);
             if (cell.is_simple_cell())
                {
                  // a non-pointer sub value: 16 byte header,
                  // and 1-16 data bytes, padded up to 16 bytes,
                  //
                  offset += 32;
                }
             else if (cell.is_pointer_cell())
                {
                  const Value & sub_val = *cell.get_pointer_value();
                  const CDR_type sub_type = sub_val.get_CDR_type();
                  offset += sub_val.total_CDR_size_brutto(sub_type);
                }
              else
                DOMAIN_ERROR;
           }

        // pad top level to 16 bytes. end is aligned to 16 byte so we
        // case use it as reference.
        //
        while (result.size() & 0x0F)   result.push_back(Unicode_0);

        // append sub-values.
        //
        loop(e, nelm)
           {
             const Cell & cell = val.get_cravel(e);
             if (cell.is_simple_cell())
                {
                  Value_P sub_val(LOC);
                  sub_val->set_ravel_Cell(0, cell);

                  const CDR_type sub_type = sub_val->get_CDR_type();
                  const int sub_len = sub_val->total_CDR_size_brutto(sub_type);
                  fill(result, sub_type, sub_len, *sub_val);
                }
              else
                {
                  Value_P sub_val = cell.get_pointer_value();

                  const CDR_type sub_type = sub_val->get_CDR_type();
                  const int sub_len = sub_val->total_CDR_size_brutto(sub_type);
                  fill(result, sub_type, sub_len, *sub_val);
                }
           }

        // since we padded the top level to 16 and sub-values have a padded
        // length as well, we should be properly padded here.
        //
      }
   else Assert(0 && "Bad/unsupported CDR type");

   // final padding
   //
   while (result.size() & 15)   result.push_back(Unicode_0);
}
//----------------------------------------------------------------------------
Value_P
CDR::from_CDR(const CDR_string & cdr, const char * loc)
{
   // read header...
   //
/*
struct tf3_header
    {
       uint32_t ptr;      //  0: byte reversed
       uint32_t nb;       //  4: byte reversed
       uint32_t nelm;     //  8: byte reversed
       uint8_t  type;     // 12:
       uint8_t  rank;     // 13:
       uint8_t  fill[2];  // 14:
       uint32_t dim[1];   // 16: byte reversed
    }
*/

   if (cdr.size() < 20)
      {
        MORE_ERROR() << "CDR strings must have a header of at least 20 bytes";
        LENGTH_ERROR;
      }

const uint8_t * data = cdr.get_items();

const uint32_t nelm = get_4_be(data + 8);
const CDR_type vtype = CDR_type(data[12]);
const sRank rank = data[13];
Shape shape;
   loop(r, rank)
      {
        const ShapeItem sh = get_4_be(data + 16 + 4*r);
        shape.add_shape_item(sh);
      }

Value_P Z(shape, loc);

const uint8_t * ravel = data + 16 + 4*rank;

   if (vtype == CDR_BOOL1)        // packed bit vector
      {
        loop(n, nelm)
           {
             const int bit = ravel[n >> 3] & (0x80 >> (n & 7));
             Z->next_ravel_Int(bit ? 1 : 0);
           }
      }
   else if (vtype == CDR_INT32)   // 4 byte ints vector
      {
        loop(n, nelm)
           {
             APL_Integer d = *reinterpret_cast<const uint32_t *>(ravel + 4*n);
             if (d & 0x80000000)   d |= 0xFFFFFFFF00000000ULL;
             Z->next_ravel_Int(d);
           }
      }
   else if (vtype == CDR_FLT64)   // 8 byte float vector
      {
        loop(n, nelm)
           {
             const APL_Float v =
                  *reinterpret_cast<const APL_Float *>(ravel + 8*n);
             Z->next_ravel_Float(v);
           }
      }
  else if (vtype == CDR_CPLX128)   // 16 byte complex vector
      {
        loop(n, nelm)
           {
             const APL_Float vr =
                  *reinterpret_cast<const APL_Float *>(ravel + 16*n);
             const APL_Float vi =
                  *reinterpret_cast<const APL_Float *>(ravel + 16*n + 8);
             Z->next_ravel_Complex(vr, vi);
           }
      }
   else if (vtype == CDR_CHAR8)   // 1 byte char vector
      {
        loop(n, nelm)
           {
             uint32_t d = ravel[n];
             if (d & 0x80)   d |= 0xFFFFFFFFFFFFFF00ULL;
             Z->next_ravel_Char(Unicode(d));
           }
      }
   else if (vtype == CDR_CHAR32)   // 4 byte UNICODE vector
      {
        loop(n, nelm)
           {
             const uint32_t d = *reinterpret_cast<const uint32_t *>(ravel+4*n);
             Z->next_ravel_Char(Unicode(d));
           }
      }
   else if (vtype == CDR_NEST32)   // packed vector with 4 bytes offsets.
      {
        loop(n, nelm)
            {
              APL_Integer offset =
                         *reinterpret_cast<const uint32_t *>(ravel + 4*n);
              const uint8_t * sub_data = data + offset;
              const uint32_t sub_vtype = sub_data[12];
              const sRank sub_rank = sub_data[13];
              const uint8_t * sub_ravel = sub_data + 16 + 4*sub_rank;

              // if the sub-item is a non-nested scalar then we append it to
              // ret directly; otherwise we append a pointer to it,
              //
              if (sub_rank == 0)   // scalar
                 {
                   if (sub_vtype == 0)        // bit
                      {
                        Z->next_ravel_Int((*sub_ravel & 0x80) ? 1 : 0);
                        continue;   // next n
                      }

                   if (sub_vtype == 1)        // 4 byte int
                      {
                        APL_Integer d = *reinterpret_cast<const uint32_t *>
                                                         (sub_ravel);
                        if (d & 0x80000000)   d |= 0xFFFFFFFF00000000ULL;
                        Z->next_ravel_Int(d);
                        continue;   // next n
                      }

                   if (sub_vtype == 2)        // 8 byte real
                      {
                        Z->next_ravel_Float(
                                        *reinterpret_cast<const APL_Float *>
                                                                (sub_ravel));
                        continue;   // next n
                      }

                   if (sub_vtype == 3)        // 16 byte complex
                      {
                        Z->next_ravel_Complex(
                                        *reinterpret_cast<const APL_Float *>
                                                         (sub_ravel),
                                        *reinterpret_cast<const APL_Float *>
                                                          (sub_ravel + 8));
                        continue;   // next n
                      }

                   if (sub_vtype == 4)        // 1 byte ASCII char
                      {
                        Z->next_ravel_Char(Unicode(*sub_ravel));
                        continue;   // next n
                      }

                   if (sub_vtype == 5)        // 4 byte Unicode
                      {
                        Z->next_ravel_Char(Unicode(get_4_be(sub_ravel)));
                        continue;   // next n
                      }

                   // sub_vtype 6 (progression vector) can't be scalar
                 }

              // at this point the sub-item is not a scalar
              //
              if (sub_vtype == 6)        // arithmetic progression vector
                 {
                   // arithmetic progression vectors (APVs) are not well
                   // described. Normally a progression vector is a triple
                   //
                   // { initial-value, element-count, increment }
                   //
                   // But: we have only 8 bytes = 2 integers. We therefore
                   // assume that element-count == nelm so that the two
                   // integers are initial-value and increment.
                   //
                   Shape sh;
                   loop(r, sub_rank)
                      sh.add_shape_item(get_4_be(sub_data + 16));

                   Value_P sub_val(sh, LOC);
                   const APL_Integer qio = Workspace::get_IO();
                   loop(v, sh.get_volume())
                       {
                         sub_val->next_ravel_Int(v + qio);
                       }

                   continue;   // next n
                 }

              const uint32_t sub_cdr_len = get_4_be(sub_data + 4);
              CDR_string sub_cdr(sub_data, sub_cdr_len);
              Value_P sub_val = from_CDR(sub_cdr, LOC);
              Assert(+sub_val);
              Z->next_ravel_Pointer(sub_val.get());
            }
      }
   else
      {
        CERR << "Unsupprted CDR type " << vtype << endl;
        Assert(0 && "Bad/unsupported CDR type");
      }

   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------

