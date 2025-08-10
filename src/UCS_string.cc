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

#include <math.h>
#include <string.h>

#include "Backtrace.hh"
#include "Common.hh"
#include "FloatCell.hh"
#include "Heapsort.hh"
#include "Output.hh"
#include "PrintBuffer.hh"
#include "PrintOperator.hh"
#include "Symbol.hh"
#include "UCS_string.hh"
#include "UTF8_string.hh"
#include "Value.hh"

ShapeItem UCS_string::total_count = 0;
ShapeItem UCS_string::total_id = 0;

//----------------------------------------------------------------------------
UCS_string::UCS_string()
{
   create(LOC);
}
//----------------------------------------------------------------------------
UCS_string::UCS_string(Unicode uni)
   : basic_string<Unicode>(1, uni)
{
  create(LOC);
}
//----------------------------------------------------------------------------
UCS_string::UCS_string(const Unicode * data, size_t len)
   : basic_string<Unicode>(data, len)
{
   create(LOC);
}
//----------------------------------------------------------------------------
UCS_string::UCS_string(size_t len, Unicode uni)
   : basic_string<Unicode>(len, uni)
{
   create(LOC);
}
//----------------------------------------------------------------------------
UCS_string::UCS_string(const UCS_string & ucs)
   : basic_string<Unicode>(ucs)
{
   create(LOC);
}
//----------------------------------------------------------------------------
UCS_string::UCS_string(const UCS_string & ucs, size_t pos, size_t len)
   : basic_string<Unicode>(ucs, pos, len)
{
   create(LOC);
}
//----------------------------------------------------------------------------
UCS_string::UCS_string(const UTF8_string & utf)
{
   create(LOC);

   Log(LOG_char_conversion)
      CERR << "UCS_string::UCS_string(): utf = " << utf << endl;

size_t from = 0;

   for (size_t i = 0; i < utf.size();)
      {
start_of_sequence:

        const uint32_t b0 = utf[i++];
        uint32_t bx = b0;
        uint32_t more;
        if      ((b0 & 0x80) == 0x00)   { more = 0;             }
        else if ((b0 & 0xE0) == 0xC0)   { more = 1; bx &= 0x1F; }
        else if ((b0 & 0xF0) == 0xE0)   { more = 2; bx &= 0x0F; }
        else if ((b0 & 0xF8) == 0xF0)   { more = 3; bx &= 0x0E; }
        else if ((b0 & 0xFC) == 0xF8)   { more = 4; bx &= 0x07; }
        else if ((b0 & 0xFE) == 0xFC)   { more = 5; bx &= 0x03; }
        else   // invalid UTF start byte
           {
             Log(LOG_char_conversion)
                {
                 utf.dump_hex(CERR << "Bad UTF8 string: ", 40)
                                   << " at " << LOC <<  endl;
                 BACKTRACE
                }

             // map this string to the "supplementary private use area B" so
             // that its hex code becomes (sort of) visible
             //
             append(Unicode(utf[from] | 0x100000));
             i = ++from;   // retry, starting at next char
             goto start_of_sequence;
           }

        uint32_t uni = 0;
        for (; more; --more)
            {
              if (i >= utf.size())
                 {
                   Log(LOG_char_conversion)
                      {
                        utf.dump_hex(CERR << "Truncated UTF8 string: ", 40)
                                          << " len " << utf.size()
                                          << " at " << LOC << endl;
                        if (utf.size() >= 40)
                           {
                             const UTF8_string end(&utf[utf.size() - 10], 10);
                             end.dump_hex(CERR << endl << "(ending with : ", 20)
                                               << ")" << endl;
                           }
                      }

                   append(Unicode(utf[from] | 0x100000));
                   i = ++from;   // retry, starting at next char
                   goto start_of_sequence;
                 }

              const UTF8 subc = utf[i++];
              if ((subc & 0xC0) != 0x80)   // invalid UTF continuation byte
                 {
                   Log(LOG_char_conversion)
                      {
                        utf.dump_hex(CERR << "Bad UTF8 string: ", 40)
                                          << " len " << utf.size()
                                          << " at " << LOC <<  endl;
                        if (utf.size() >= 40)
                           {
                             const UTF8_string end(&utf[utf.size() - 10], 10);
                             end.dump_hex(CERR << endl << "(ending with : ", 20)
                                               << ")" << endl;
                           }
                        BACKTRACE
                      }

                   append(Unicode(utf[from] | 0x100000));
                   i = ++from;   // retry, starting at next char
                   goto start_of_sequence;
                 }

              bx  <<= 6;
              uni <<= 6;
              uni |= subc & 0x3F;
            }

         append(Unicode(bx | uni));
         from = i;
      }

   Log(LOG_char_conversion)
      CERR << "UCS_string::UCS_string(): ucs = " << *this << endl;
}
//----------------------------------------------------------------------------
UCS_string::UCS_string(APL_Float value, bool & scaled,
                       const PrintContext & pctx)
{
   create(LOC);

int quad_pp = pctx.get_PP();
   if (quad_pp > MAX_Quad_PP)   quad_pp = MAX_Quad_PP;
   if (quad_pp < MIN_Quad_PP)   quad_pp = MIN_Quad_PP;

const bool negative = (value < 0.0);
   if (negative)   value = -value;

int expo = 0;

   if (value >= 10.0)   // large number, positive exponent
      {
        if (value > BIG_FLOAT || !isnormal(value))   // something odd
           {
            if (isnormal(value) || isinf(value))   // rather large
               {
                 if (negative)   append_UTF8("¯∞");
                 else            append_UTF8("∞");
                 FloatCell::map_FC(*this);
               }
           else
               {
                 append_UTF8("-nan-");
               }
             return;
           }

       while (value >= 1e16)   { value *= 1e-16;   expo += 16; }
       while (value >= 1e4)    { value *= 1e-4;    expo +=  4; }
       while (value >= 1e1)    { value *= 1e-1;    ++expo;     }
      }
   else if (value < 1.0)   // small number, negative exponent
      {
       if (value < 1e-305)   // very small number: make it 0
          {
            append(UNI_0);
            return;
          }

       while (value < 1e-16)   { value *= 1e16;   expo -= 16; }
       while (value < 1e-4)    { value *= 1e4;    expo -=  4; }
       while (value < 1.0)     { value *= 10.0;   --expo;     }
      }

   // In theory, at this point, 1.0 ≤ value < 10.0. In reality value can
   // be outside, though, due to rounding errors.

   // create a string with quad_pp + 1 significant digits.
   // The last digit is used for rounding and then discarded.
   //
UCS_string digits;
   loop(d, (quad_pp + 2))
      {
        if (value >= 10.0)
           {
             // 10.0 or more is a rounding error from 9,999...
             digits.append(Unicode(10 + '0'));
             while (digits.size() < (quad_pp + 2))   digits.append(UNI_0);
             break;
           }
        else if (value < 0.0)
           {
             // less than 0.0 is a rounding error from 0.000...
             while (digits.size() < (quad_pp + 2))   digits.append(UNI_0);
             break;
             digits.append(UNI_0);
             value = 0.0;
           }
        else
           {
             const int dig = int(value);
             value -= dig;
             value *= 10.0;
             digits.append(Unicode(dig + '0'));
           }
      }

   if (digits[0] != '0')   digits.pop_back();

   // round last digit
   //
const Unicode last = digits.back();
   digits.pop_back();

   if (last >= '5')   digits.back() = Unicode(digits.back() + 1);
 
   // adjust carries of 2nd to last digit
   //
   for (int d = digits.size() - 1; d > 0; --d)   // all but first
       {
        if (digits[d] > '9')
           {
             digits[d] =     Unicode(digits[d]     - 10);
             digits[d - 1] = Unicode(digits[d - 1] +  1);
           }
       }

   // adjust carry of 1st digit
   //
   if (digits[0] > '9')
      {
        digits[0] = Unicode(digits[0] - 10);
        digits.insert(0, UNI_1);
        ++expo;
        digits.pop_back();
      }

   // remove trailing zeros
   //
   while (digits.size() > 1 && digits.back() == UNI_0)  digits.pop_back();

   // force scaled format if:
   //
   // value < .00001     ( value has ≥ 5 leading zeroes)
   // value > 10⋆quad_pp ( integer larger than ⎕PP)
   //
   if ((expo < -6) || (expo > quad_pp))   scaled = true;

   if (negative)   append(UNI_OVERBAR);

   if (scaled)
      {
        append(digits[0]);       // integer part
        if (digits.size() > 1)   // fractional part
           {
             append(UNI_FULLSTOP);
             loop(d, (digits.size() - 1))   append(digits[d + 1]);
           }
        if (expo < 0)
           {
             append(UNI_E);
             append(UNI_OVERBAR);
             append_number(-expo);
           }
        else if (expo > 0)
           {
             append(UNI_E);
             append_number(expo);
           }
        else if (!(pctx.get_style() & PST_NO_EXPO_0)) // expo == 0
           {
             append(UNI_E);
             append(UNI_0);
           }
      }
   else
      {
        if (expo < 0)   // 0.000...
           {
             append(UNI_0);
             append(UNI_FULLSTOP);
             loop(e, (-(expo + 1)))   append(UNI_0);
             append(digits);
           }
        else   // expo >= 0
           {
             loop(e, expo + 1)
                {
                  if (e < digits.size())   append(digits[e]);
                  else                     append(UNI_0);
                }

             if ((expo + 1) < digits.size())   // there are fractional digits
                {
                  append(UNI_FULLSTOP);
                  for (int e = expo + 1; e < digits.size(); ++e)
                     {
                       if (e < digits.size())   append(digits[e]);
                       else                     break;
                     }
                }
           }
      }

   FloatCell::map_FC(*this);
}
//----------------------------------------------------------------------------
UCS_string::UCS_string(const PrintBuffer & pb, sRank rank, int quad_PW)
{
   create(LOC);

   if (pb.get_row_count() == 0)   return;      // empty PrintBuffer

const int total_width = pb.get_column_count();

   // initialize chunk_lengths, based on the first row of the PrintBuffer.
   // All subsequent rows are aligned to the first row, therefore the first
   // row can be taken as a prototype for all rows.
size_t chunk_len = 0;
std::basic_string<int> chunk_lengths;
   chunk_lengths.reserve(2*total_width/quad_PW);
   for (int col = 0; col < total_width; col += chunk_len)
       {
         chunk_len = pb.get_line(0).compute_chunk_length(quad_PW,col);
         chunk_lengths.push_back(chunk_len);
       }

   // print rows, breaking at chunk_lengths
   //
   loop(row, pb.get_row_count())
       {
         if (row)   append(UNI_LF);   // end previous row
         int brk_idx = 0;

         for (int col = 0; col < total_width; col += chunk_len)
               {
                 chunk_len = chunk_lengths[brk_idx++];

                 if (col)   append_UTF8("\n      ");
                 UCS_string trow(pb.get_line(row), col, chunk_len);
                 trow.remove_trailing_padchars();
                 append(trow);
               }
       }

   // replace pad chars with blanks.
   //
   loop(u, size())
       {
         if (is_iPAD_char(at(u)))   at(u) = UNI_SPACE;
       }
}
//----------------------------------------------------------------------------
/// constructor
UCS_string::UCS_string(const Value & value)
{
   create(LOC);

   if (value.get_rank() > 1) RANK_ERROR;

const ShapeItem ec = value.element_count();
   reserve(ec);

   loop(e, ec)   append(value.get_cravel(e).get_char_value());
}
//----------------------------------------------------------------------------
/// constructor
UCS_string::UCS_string(const Cell & cell)
{
   create(LOC);

   if (cell.is_character_cell())
      {
        append(cell.get_char_value());
        return;
      }

   Assert(cell.is_pointer_cell());
const Value & value = *cell.get_pointer_value().get();

   if (value.get_rank() > 1)
      {
        MORE_ERROR() << "Bad rank " << value.get_rank()
                     << " when expecting a character string";
        RANK_ERROR;
      }

const ShapeItem ec = value.element_count();
   reserve(ec);

   loop(e, ec)   append(value.get_cravel(e).get_char_value());
}
//----------------------------------------------------------------------------
UCS_string::UCS_string(istream & in)
{
   create(LOC);

   for (;;)
      {
        const Unicode uni = UTF8_string::getc(in);
        if (uni == Invalid_Unicode)   return;
        if (uni == UNI_LF)      return;
        append(uni);
      }
}
//----------------------------------------------------------------------------
int
UCS_string::compute_chunk_length(int quad_PW, int col) const
{
   // the space available is ⎕PW for the first line or ⎕PW less the usual
   // 6 space indentation for subsequent lines.
   //
const int chunk_len = col ? quad_PW - 6 : quad_PW;

   // if the rest of the line (from col to the end of the line) is small,
   //  then return the length of the rest.
   //
int pos = col + chunk_len;
   if (pos >= size())   return size() - col;

   // otherwise the rest does not fit into ⎕PW. Find a whitespace
   // at which the line should be broken.
   //
   while (--pos > col)
      {
         const Unicode uni = at(pos);
         if (uni == UNI_SPACE || uni == UNI_iPAD_U2 || uni == UNI_iPAD_U3)
            {
               return pos - col + 1;
            }
      }

   // no whitespace found (the entire string may not have any).
   // Break at chunk_len.
   //
   return chunk_len;
}
//----------------------------------------------------------------------------
void
UCS_string::remove_comment()
{
   loop(j, size())
      {
        if (at(j) == UNI_COMMENT || at(j) == UNI_NUMBER_SIGN)
           {
             resize(j);
             remove_trailing_whitespaces();
             break;
           }
      }
}
//----------------------------------------------------------------------------
void
UCS_string::remove_trailing_padchars()
{
   // remove trailing pad chars from align() and append_string(),
   // but leave other pad chars intact.
   // But only if the line has no frame (vert).
   //
again:
   if (size() > 2 && contains(UNI_iPAD_L0))
      {
        Unicode first = Unicode_0;
        if (back() == UNI_iPAD_U9)        first = UNI_iPAD_U8;
        else if (back() == UNI_iPAD_U5)   first = UNI_iPAD_U4;

        if (first && at(size() - 2) == UNI_iPAD_L0)
           {
        // last column is a nested dimension separator row
        //
        ShapeItem len = size() - 2;
        while (len && (at(len - 1) == UNI_iPAD_L0 ||
                       at(len - 1) == UNI_iPAD_U0 ||
                       at(len - 1) == UNI_iPAD_U2 ||
                       at(len - 1) == UNI_iPAD_U6 ||
                       at(len - 1) == UNI_iPAD_U7))   --len;
             if (len && at(len - 1) == first)
                {
                  resize(len - 1);
                  goto again;
                }
           }
      }

   // If the line contains UNI_iPAD_L0 (higher dimension separator)
   // then discard all chars (unless the line is framed)..
   //
   while (size())
      {
        const Unicode last = back();
        if (last == UNI_iPAD_L0 ||   // ₀: higher dimension separator
            last == UNI_iPAD_L1 ||   // ₁: pad line to PrintBuffer width
            last == UNI_iPAD_L2 ||   // ₂: pad PrintBuffer width to line
            last == UNI_iPAD_L3 ||   // ₃: pad integer part to the left
            last == UNI_iPAD_L4 ||   // ₄: pad fractional part to the right
            last == UNI_iPAD_U2 ||   // ²  column separator (after notchar)
            last == UNI_iPAD_U2 ||   // ²  column separator (after notchar)
            last == UNI_iPAD_U7)     // ⁷  pad final to the right
            pop_back();
        else
            break;
      }
}
//----------------------------------------------------------------------------
void
UCS_string::remove_trailing_whitespaces()
{
   while (size() && back() <= UNI_SPACE)   pop_back();
}
//----------------------------------------------------------------------------
void
UCS_string::remove_leading_whitespaces()
{
int count = 0;
   loop(s, size())
      {
        if (at(s) <= UNI_SPACE)   ++count;
        else                            break;
      }

   if (count == 0)        return;      // no leading whitspaces
   if (count == size())   clear();     // only whitespaces
   else                   basic_string::erase(0, count);
}
//----------------------------------------------------------------------------
void
UCS_string::split_ws(UCS_string & rest)
{
   remove_leading_and_trailing_whitespaces();

   loop(clen, size())
       {
         if (Avec::is_white(at(clen)))   // first whilespace: end of command
            {
              ShapeItem arg = clen;
              while (arg < size() && Avec::is_white(at(arg)))   ++arg;
              while (arg < size())   rest.append(at(arg++));
              resize(clen);
              return;
            }
       }
}
//----------------------------------------------------------------------------
size_t
UCS_string::copy_black(UCS_string & dest, int idx) const
{
   while (idx < size() && at(idx) <= ' ')   ++idx;
   while (idx < size() && at(idx) >  ' ')   dest.append(at(idx++));
   while (idx < size() && at(idx) <= ' ')   ++idx;
   return idx;
}
//----------------------------------------------------------------------------
ShapeItem
UCS_string::LF_count() const
{
ShapeItem count = 0;
   loop(u, size())   if (at(u) == UNI_LF)   ++count;
   return count;
}
//----------------------------------------------------------------------------
ShapeItem
UCS_string::substr_pos(const UCS_string & sub) const
{
const ShapeItem start_positions = 1 + size() - sub.size();
   loop(start, start_positions)
      {
        bool mismatch = false;
        loop(u, sub.size())
           {
             if (at(start + u) != sub[u])
                {
                  mismatch = true;
                  break;
                }
           }

        if (!mismatch)   return start;   // found sub at start
      }

   return -1;   // not found
}
//----------------------------------------------------------------------------
ShapeItem
UCS_string::multi_pos(bool expect_end) const
{
const ShapeItem end = size() - 2;   // excluding, 3 valid chars
   loop(pos, end)   //
       {
         switch(at(pos))
            {
            case UNI_DOUBLE_QUOTE:   // """ or "..."
                 if (at(pos + 1) == UNI_DOUBLE_QUOTE &&
                     at(pos + 2) == UNI_DOUBLE_QUOTE)   return pos;   // """
                 for (++pos; pos < end && at(pos) != UNI_DOUBLE_QUOTE; ++pos)
                     {
                       if (at(pos) == UNI_BACKSLASH)   ++pos;   // \X
                     }
                 break;

            case UNI_SINGLE_QUOTE:   // '...'
                 for (++pos; pos < end && at(pos) != UNI_SINGLE_QUOTE; ++pos) ;
                 break;

            case UNI_NUMBER_SIGN:   // #
            case UNI_COMMENT:       // ⍝
                 return -1;

            case UNI_LEFT_DAQ:   // ««« : expliciy start of multiline
                 if (at(pos + 1) == UNI_LEFT_DAQ &&
                     at(pos + 2) == UNI_LEFT_DAQ)
                    {
                      if (expect_end)
                         CERR << "*** WARNING: see (second) ««« "
                                 "when expecting the closing »»»" << endl;
                      return pos;   // """
                    }

            case UNI_RIGHT_DAQ:   // »»» : expliciy end of multiline
                 if (at(pos + 1) == UNI_RIGHT_DAQ &&
                     at(pos + 2) == UNI_RIGHT_DAQ)
                    {
                      if (!expect_end)
                         CERR << "*** WARNING: see (closing) »»» "
                                 "without prior «««" << endl;
                      return pos;   // """
                    }

              default:   ;
            }
       }

   return -1;   // not found
}
//----------------------------------------------------------------------------
bool
UCS_string::has_black() const
{
   loop(s, size())   if (!Avec::is_white(at(s)))   return true;
   return false;
}
//----------------------------------------------------------------------------
bool 
UCS_string::starts_with(const char * prefix) const
{
   loop(s, size())
      {
        const char pc = *prefix++;
        if (pc == 0)   return true;   // prefix matches this string.

        const Unicode uni = at(s);
        if (uni != Unicode(pc))   return false;
      }

   // strings match, but prefix is longer
   //
   return false;
}
//----------------------------------------------------------------------------
bool 
UCS_string::ends_with(const char * suffix) const
{
const ShapeItem s_len = strlen(suffix);
   if (size() < s_len)   return false;

   suffix += s_len;    // goto end of suffix
   loop(s, s_len)   if (at(size() - s - 1) != *--suffix)   return false;
   return true;
}
//----------------------------------------------------------------------------
bool 
UCS_string::starts_with(const UCS_string & prefix) const
{
   if (prefix.size() > size())   return false;

   loop(p, prefix.size())   if (at(p) != prefix[p])   return false;

   return true;
}
//----------------------------------------------------------------------------
bool 
UCS_string::starts_iwith(const char * prefix) const
{
   loop(s, size())
      {
        char pc = *prefix++;
        if (pc == 0)   return true;   // prefix matches this string.
        if (pc >= 'a' && pc <= 'z')   pc -= 'a' - 'A';

        int uni = at(s);
        if (uni >= 'a' && uni <= 'z')   uni -= 'a' - 'A';

        if (uni != Unicode(pc))   return false;
      }

   return *prefix == 0;   
}
//----------------------------------------------------------------------------
bool 
UCS_string::starts_iwith(const UCS_string & prefix) const
{
   if (prefix.size() > size())   return false;

   loop(p, prefix.size())
      {
        int c1 = at(p);
        int c2 = prefix[p];
        if (c1 >= 'a' && c1 <= 'z')   c1 -= 'a' - 'A';
        if (c2 >= 'a' && c2 <= 'z')   c2 -= 'a' - 'A';
        if (c1 != c2)   return false;
      }

   return true;
}
//----------------------------------------------------------------------------
UCS_string
UCS_string::no_pad() const
{
UCS_string ret;
   loop(s, size())
      {
        Unicode uni = at(s);
        if (is_iPAD_char(uni))   uni = UNI_SPACE;
        ret.append(uni);
      }

   return ret;
}
//----------------------------------------------------------------------------
void
UCS_string::map_pad()
{
   loop(s, size())
      {
        if (is_iPAD_char(at(s)))   at(s) = UNI_SPACE;
      }
}
//----------------------------------------------------------------------------
UCS_string
UCS_string::remove_pad() const
{
UCS_string ret;
   loop(s, size())
      {
        Unicode uni = at(s);
        if (!is_iPAD_char(uni))   ret.append(uni);
      }

   return ret;
}
//----------------------------------------------------------------------------
UCS_string
UCS_string::reverse() const
{
UCS_string ret;
   for (int s = size(); s > 0;)   ret.append(at(--s));
   return ret;
}
//----------------------------------------------------------------------------
bool
UCS_string::is_comment_or_label() const
{
   if (size() == 0)                          return false;
   if (at(0) == UNI_NUMBER_SIGN)       return true;   // comment
   if (at(0) == UNI_COMMENT)                 return true;   // comment
   loop(t, size())
       {
         if (at(t) == UNI_COLON)       return true;   // label
         if (!Avec::is_symbol_char(at(t)))   return false;
       }

   return false;
}
//----------------------------------------------------------------------------
ShapeItem
UCS_string::double_quote_count(bool in_quote2) const
{
ShapeItem count = 0;
bool in_quote1 = false;
   loop(s, size())
       {
        const Unicode uni = at(s);
        switch(uni)
           {
             case UNI_SINGLE_QUOTE:
                  if (!in_quote2)   in_quote1 = ! in_quote1;
                  break;

             case UNI_DOUBLE_QUOTE:
                  if (!in_quote1)
                     {
                       ++count;
                       in_quote2 = ! in_quote2;
                     }
                  break;

             case UNI_BACKSLASH:
                  if (in_quote2)    ++s;   // ignore next char inside ""
                  break;

             case UNI_NUMBER_SIGN:
             case UNI_COMMENT:
                  if (!(in_quote1 || in_quote2))   return count;

             default:                            ;
           }
       }

   return count;
}
//----------------------------------------------------------------------------
ShapeItem
UCS_string::double_quote_first() const
{
bool in_quote1 = false;
bool in_quote2 = true;
   loop(s, size())
       {
        const Unicode uni = at(s);
        switch(uni)
           {
             case UNI_SINGLE_QUOTE:
                  if (!in_quote2)   in_quote1 = ! in_quote1;
                  break;

             case UNI_DOUBLE_QUOTE:
                  if (!in_quote1)   return s;
                  break;

             case UNI_BACKSLASH:
                  if (in_quote2)    ++s;   // ignore next char inside ""
                  break;

             case UNI_NUMBER_SIGN:
             case UNI_COMMENT:
                  if (in_quote1 || in_quote2)   ; // ignore # and ⍝ in atrings
                  else                          s = size();
                  break;

             default:                            ;
           }
       }


   return -1;   // no un-commented and un-escaped " found
}
//----------------------------------------------------------------------------
ShapeItem
UCS_string::double_quote_last() const
{
ShapeItem ret = -1;
bool in_quote1 = false;
bool in_quote2 = false;
   loop(s, size())
       {
        const Unicode uni = at(s);
        switch(uni)
           {
             case UNI_SINGLE_QUOTE:
                  if (!in_quote2)   in_quote1 = ! in_quote1;
                  break;

             case UNI_DOUBLE_QUOTE:
                  if (!in_quote1)   ret = s;
                  break;

             case UNI_BACKSLASH:
                  if (in_quote2)    ++s;   // ignotr next char inside ""
                  break;

             case UNI_NUMBER_SIGN:
             case UNI_COMMENT:
                  if (in_quote1 || in_quote2)   ; // ignore # and ⍝ in atrings
                  else                          s = size();
                  break;

             default:                            ;
           }
       }

   return ret;
}
//----------------------------------------------------------------------------
void
UCS_string::append_UTF8(const UTF8 * str)
{
const size_t len = strlen(charP(str));
const UTF8_string utf(str, len);
const UCS_string ucs(utf);

   append(ucs);
}
//----------------------------------------------------------------------------
void
UCS_string::append_quoted(const UCS_string & other)
{
   append(UNI_DOUBLE_QUOTE);
   loop(s, other.size())
       {
          const Unicode uni = other[s];
          if (uni == UNI_DOUBLE_QUOTE)   append(UNI_BACKSLASH);
          append(uni);
       }
   append(UNI_DOUBLE_QUOTE);
}
//----------------------------------------------------------------------------
void
UCS_string::append_number(ShapeItem num)
{
char cc[40];
   SPRINTF(cc, "%lld", static_cast<long long>(num));
   loop(c, sizeof(cc))
      {
        if (char digit = cc[c])   append(Unicode(digit));
        else                      break;
      }
}
//----------------------------------------------------------------------------
void
UCS_string::append_double(double num)
{
char cc[40];
   SPRINTF(cc, "%g", num);
   loop(c, sizeof(cc))
      {
        if (char digit = cc[c])   append(Unicode(digit));
        else                      break;
      }
}
//----------------------------------------------------------------------------
void
UCS_string::append_hex(ShapeItem num, bool uppercase)
{
const char * format = uppercase ? "%llX" : "%llx";
char cc[40];
   SPRINTF(cc, format, static_cast<long long>(num));
   loop(c, sizeof(cc))
      {
        if (char hex_digit = cc[c])   append(Unicode(hex_digit));
        else         break;
      }
}
//----------------------------------------------------------------------------
void
UCS_string::append_shape(const Shape & shape)
{
   if (shape.get_rank() == 0)   // empty shape (scalar)
      {
        append(UNI_ZILDE);   //  ⍬
        return;
      }

   loop(r, shape.get_rank())
       {
         if (r)   append(UNI_SPACE);
         ShapeItem s = shape.get_shape_item(r);
         if (s < 0)
            {
              s = -s;
              append(UNI_OVERBAR);
            }
         append_number(s);
       }
}
//----------------------------------------------------------------------------
void
UCS_string::append_members(const basic_string<const UCS_string *> & members,
                           int m)
{
   for (int mm = members.size() - 1; mm >= m; --mm)
       {
         if (mm < int(members.size() - 1))   *this += Unicode('.');
         append(*members[mm]);
       }
}
//----------------------------------------------------------------------------
void
UCS_string::append_float(APL_Float num)
{
char cc[60];
   SPRINTF(cc, "%lf", double(num));
   loop(c, sizeof(cc))
      {
        if (cc[c])   append(Unicode(cc[c]));
        else         break;
      }
}
//----------------------------------------------------------------------------
UCS_string
UCS_string::un_escape(bool double_quoted, bool keep_LF) const
{
const char * hex = "0123456789abcdef";
UCS_string ret;
   ret.reserve(size());

   if (double_quoted)
      {
        loop(s, size())
            {
             const Unicode uni = at(s);
             if (uni != UNI_BACKSLASH)   // normal char
                {
                  ret.append(uni);
                  continue;
                }

             if (s >= (size() - 1))   // \ at end of string
                {
                  ret.append(UNI_BACKSLASH);
                  break;
                }

             const Unicode uni1 = at(++s);
             switch(uni1)
                 {
                  case UNI_a:            ret << UNI_BEL;   continue;
                  case UNI_b:            ret << UNI_BS;    continue;
                  case UNI_f:            ret << UNI_FF;    continue;
                  case UNI_n:            if (keep_LF)   break;
                                               ret << UNI_LF;    continue;
                  case UNI_r:            ret << UNI_CR;    continue;
                  case UNI_t:            ret << UNI_BS;    continue;
                  case UNI_v:            ret << UNI_VT;    continue;
                  case UNI_DOUBLE_QUOTE:
                  case UNI_BACKSLASH:
                                               ret << uni1;            continue;
                  default:                     break;
                 }

             int max_len = 0;
             if (uni1 == UNI_u)
                {
                  max_len = 4;
                }
             else if (uni1 == UNI_x)
                {
                  max_len = 2;
                }
             else   // \n or \": keep them escaped
                {
                  ret.append(uni);
                  ret.append(uni1);
                  continue;
                }

               // \x or \u
               //
               int value = 0;
               loop(m, max_len)
                   {
                     if (s >= (size() - 1))   break;
                     const int dig = at(s+1);
                     const char * pos = strchr(hex, dig);
                     if (pos == 0)   break;   // non-hex character

                     value = value << 4 | (pos - hex);
                     ++s;
                   }
               ret.append(Unicode(value));
            }
      }
   else
      {
        bool got_quote = false;
        loop(s, size())
           {
             const Unicode uni = at(s);
             if (uni == UNI_SINGLE_QUOTE)
                {
                  if (got_quote)
                     {
                        ret.append(UNI_SINGLE_QUOTE);
                        got_quote = false;
                     }
                  else
                     {
                        ret.append(UNI_SINGLE_QUOTE);
                        got_quote = true;
                     }
                }
             else
                {
                  if (got_quote)   ret.append(UNI_SINGLE_QUOTE);   // mal-formed
                  ret.append(uni);
                  got_quote = false;
                }
           }
      }

   return ret;
}
//----------------------------------------------------------------------------
UCS_string
UCS_string::do_escape(bool double_quoted) const
{
const char * hex = "0123456789abcdef";
UCS_string ret;
   ret.reserve(size());

   if (double_quoted)
      {
        loop(s, size())
           {
             const Unicode uni = at(s);
             switch(uni)
                {
                  case UNI_BEL:            ret << "\\a";    continue;
                  case UNI_BS:             ret << "\\b";    continue;
                  case UNI_HT:             ret << "\\t";    continue;
                  case UNI_LF:             ret << "\\n";    continue;
                  case UNI_VT:             ret << "\\v";    continue;
                  case UNI_FF:             ret << "\\f";    continue;
                  case UNI_CR:             ret << "\\r";    continue;
                  case UNI_DOUBLE_QUOTE:   ret << "\\\"";   continue;
                  case UNI_BACKSLASH:      ret << "\\\\";   continue;
                  default:                       break;
                }

             // none of the above
             //
             if (uni >= UNI_SPACE && uni < UNI_DELETE)
                {
                  ret.append(uni);
                  continue;
                }

             if (uni <= 0x0F)   // small ASCII
                {
                  ret << "\\x0";
                  ret << Unicode(hex[uni]);
                }
             else
                {
                  ret.append(uni);
                }
           }
      }
   else   // single-quoted
      {
        loop(s, size())
           {
             const Unicode uni = at(s);
             ret.append(uni);
             if (uni == UNI_SINGLE_QUOTE)   ret.append(uni);   // another '
           }
      }

   return ret;
}
//----------------------------------------------------------------------------
size_t
UCS_string::to_vector(UCS_string_vector & result) const
{
size_t max_len = 0;

   result.clear();
   if (size() == 0)   return max_len;

   result.push_back(UCS_string());
   loop(s, size())
      {
        const Unicode uni = at(s);
        if (uni == UNI_LF)    // line done
           {
             const size_t len = result.back().size();
             if (max_len < len)   max_len = len;

             if (s < size() - 1)   // more coming
                result.push_back(UCS_string());
           }
        else
           {
             if (uni != UNI_CR)         // ignore \r.
                result.back().append(uni);
           }
      }

   // if the last line lacked a \n we check max_len here again.
const size_t len = result.back().size();
   if (max_len < len)   max_len = len;

   return max_len;
}
//----------------------------------------------------------------------------
int
UCS_string::atoi() const
{
int ret = 0;
bool negative = false;

   loop(s, size())
      {
        const Unicode uni = at(s);

        if (!ret && Avec::is_white(uni))   continue;   // leading whitespace

        if (uni == UNI_MINUS || uni == UNI_OVERBAR)
           {
             negative = true;
             continue;
           }

        if (uni < UNI_0)                break;      // non-digit
        if (uni > UNI_9)                break;      // non-digit

        ret *= 10;
        ret += uni - UNI_0;
      }

   return negative ? -ret : ret;
}
//----------------------------------------------------------------------------
ostream &
operator << (ostream & os, Unicode uni)
{
   if (uni < 0x80)      return os << char(uni);
        
   if (uni < 0x800)     return os << char(0xC0 | (uni >> 6))
                                  << char(0x80 | (uni & 0x3F));

   if (uni < 0x10000)    return os << char(0xE0 | (uni >> 12))
                                   << char(0x80 | (uni >>  6 & 0x3F))
                                   << char(0x80 | (uni       & 0x3F));

   if (uni < 0x200000)   return os << char(0xF0 | (uni >> 18))
                                   << char(0x80 | (uni >> 12 & 0x3F))
                                   << char(0x80 | (uni >>  6 & 0x3F))
                                   << char(0x80 | (uni       & 0x3F));

   if (uni < 0x4000000)  return os << char(0xF8 | (uni >> 24))
                                   << char(0x80 | (uni >> 18 & 0x3F))
                                   << char(0x80 | (uni >> 12 & 0x3F))
                                   << char(0x80 | (uni >>  6 & 0x3F))
                                   << char(0x80 | (uni       & 0x3F));

   return os << char(0xFC | (uni >> 30))
             << char(0x80 | (uni >> 24 & 0x3F))
             << char(0x80 | (uni >> 18 & 0x3F))
             << char(0x80 | (uni >> 12 & 0x3F))
             << char(0x80 | (uni >>  6 & 0x3F))
             << char(0x80 | (uni       & 0x3F));
}
//----------------------------------------------------------------------------
ostream &
operator << (ostream & os, const UCS_string & ucs)
{
const int fill_len = os.width() - ucs.size();

   if (fill_len > 0)
      {
        os.width(0);
        loop(u, ucs.size())   os << ucs[u];
        loop(f, fill_len)     os << os.fill();
      }
   else
      {
        loop(u, ucs.size())   os << ucs[u];
      }

   return os;
}
//----------------------------------------------------------------------------
bool
UCS_string::lexical_before(const UCS_string other) const
{
   loop(u, size())
      {
        if (u >= other.size())   return false;   // other is a prefix of this
        if (at(u) < other.at(u))   return true;
        if (at(u) > other.at(u))   return false;
      }

   // at this point the common part of this and other is equal, If other
   // is longer then this is a prefix of other (and this comes before other)
   return other.size() > size();
}
//----------------------------------------------------------------------------
ostream &
UCS_string::dump(ostream & out) const
{
   out << right << hex << uppercase << setfill('0');
   loop(s, size())
      {
        out << " U+" << setw(4) << int(at(s));
      }

   return out << left << dec << nouppercase << setfill(' ');
}
//----------------------------------------------------------------------------
UCS_string
UCS_string::from_int(int64_t value)
{
   if (value >= 0)   return from_uint(value);

UCS_string ret(UNI_OVERBAR);
   return ret + from_uint(- value);
}
//----------------------------------------------------------------------------
UCS_string
UCS_string::from_uint(uint64_t value)
{
   if (value == 0)   return UCS_string(UNI_0);

int digits[40];
int * d = digits;

   while (value)
      {
        const uint64_t v_10 = value / 10;
        *d++ = value - 10*v_10;
        value = v_10;
      }

UCS_string ret;
   while (d > digits)   ret.append(Unicode(UNI_0 + *--d));
   return ret;
}
//----------------------------------------------------------------------------
UCS_string
UCS_string::from_big(APL_Float & val)
{
   Assert(val >= 0.0);

long double value = val;
int digits[320];   // DBL_MAX is 1.79769313486231470E+308
int * d = digits;

const long double initial_fract = modf(value, &value);
long double fract;
   for (; value >= 1.0; ++d)
      {
         fract = modf(value / 10.0, &value);   // U.x -> .U
         *d = int((fract + .02) * 10.0);
         fract -= 0.1 * *d;
      }

   val = initial_fract;

UCS_string ret;
   if (d == digits)   ret.append(UNI_0);   // 0.xxx

   while (d > digits)   ret.append(Unicode(UNI_0 + *--d));
   return ret;
}
//----------------------------------------------------------------------------
UCS_string
UCS_string::from_double_to_expo(APL_Float v, int fract_digits)
{
UCS_string ret;

   if (v == 0.0)
      {
        ret.append(UNI_0);
        if (fract_digits)   // unless integer only
           {
             ret.append(UNI_FULLSTOP);
             loop(f, fract_digits)   ret.append(UNI_0);
           }
        ret.append(UNI_E);
        ret.append(UNI_0);
        return ret;
      }

   if (v < 0.0)   { ret.append(UNI_OVERBAR);   v = -v; }

int expo = 0;
   while (v >= 1.0E1)   // scale large v ≥ 10 down to [1:10)
      {
        if (v >= 1.0E9)
           if (v >= 1.0E13)
              if (v >= 1.0E15)
                 if     (v >= 1.0E16)      { v = v * 1.0E-16;   expo += 16; }
                 else /* v >= 1.0E15 */    { v = v * 1.0E-15;   expo += 15; }
              else
                 if     (v >= 1.0E14)      { v = v * 1.0E-14;   expo += 14; }
                 else /* v >= 1.0E13 */    { v = v * 1.0E-13;   expo += 13; }
           else
              if (v >= 1.0E11)
                 if     (v >= 1.0E12)      { v = v * 1.0E-12;   expo += 12; }
                 else /* v >= 1.0E11 */    { v = v * 1.0E-11;   expo += 11; }
              else
                 if     (v >= 1.0E10)      { v = v * 1.0E-10;   expo += 10; }
                 else /* v >= 1.0E9 */     { v = v * 1.0E-9;    expo += 9;  }
        else
           if (v >= 1.0E5)
              if (v >= 1.0E7)
                 if     (v >= 1.0E8)       { v = v * 1.0E-8;    expo += 8;  }
                 else /* v >= 1.0E7 */     { v = v * 1.0E-7;    expo += 7;  }
              else
                 if     (v >= 1.0E6)       { v = v * 1.0E-6;    expo += 6;  }
                 else /* v >= 1.0E5 */     { v = v * 1.0E-5;    expo += 5;  }
           else
              if (v >= 1.0E3)
                 if     (v >= 1.0E4)       { v = v * 1.0E-4;    expo += 4;  }
                 else /* v >= 1.0E3 */     { v = v * 1.0E-3;    expo += 3;  }
              else
                 if     (v >= 1.0E2)       { v = v * 1.0E-2;    expo += 2;  }
                 else /* v >= 1.0E1 */     { v = v * 1.0E-1;    expo += 1;  }
      }

   while (v < 1.0E0)   // scale small v < 1 down to [1:10)
      {
        if (v < 1.0E-8)
           if (v < 1.0E-12)
              if (v < 1.0E-14)
                 if     (v < 1.0E-15)      { v = v * 1.0E16;   expo += -16; }
                 else /* v < 1.0E-14 */    { v = v * 1.0E15;   expo += -15; }
              else
                 if     (v < 1.0E-13)      { v = v * 1.0E14;   expo += -14; }
                 else /* v < 1.0E-12 */    { v = v * 1.0E13;   expo += -13; }
           else
              if (v < 1.0E-10)
                 if     (v < 1.0E-11)      { v = v * 1.0E12;   expo += -12; }
                 else /* v < 1.0E-10 */    { v = v * 1.0E11;   expo += -11; }
              else
                 if     (v < 1.0E-9 )      { v = v * 1.0E10;   expo += -10; }
                 else /* v < 1.0E-8 */     { v = v * 1.0E9;    expo += -9;  }
        else
           if (v < 1.0E-4)
              if (v < 1.0E-6)
                 if     (v < 1.0E-7)       { v = v * 1.0E8;    expo += -8;  }
                 else /* v < 1.0E-6 */     { v = v * 1.0E7;    expo += -7;  }
              else
                 if     (v < 1.0E-5)       { v = v * 1.0E6;    expo += -6;  }
                 else /* v < 1.0E-4 */     { v = v * 1.0E5;    expo += -5;  }
           else
              if (v < 1.0E-2)
                 if     (v < 1.0E-3)       { v = v * 1.0E4;    expo += -4;  }
                 else /* v < 1.0E-2 */     { v = v * 1.0E3;    expo += -3;  }
              else
                 if     (v < 1.0E-1)       { v = v * 1.0E2;    expo += -2;  }
                 else /* v < 1.0E0  */     { v = v * 1.0E1;    expo += -1;  }
      }

   Assert(v >= 1.0);
   Assert(v < 10.0);

   // print the mantissa in fixed format. Normally the mantissa is ≥ 1.0 and
   // < 10.0, i.e. excluding 10.0. However, rounding up to fract_digits could
   // make it 10 (including)
   //
UCS_string mantissa = from_double_to_fixed(v, fract_digits);
   if (mantissa.size() >= 2 && mantissa[0] == UNI_1 && mantissa[1] == UNI_0)
      {
        // mantissa starts with 10
        //
        if (mantissa.size() == 2)   // 10Ee → 1.0yyyEe+1
           {
             mantissa[0] = UNI_1;
             mantissa.resize(1);
             if (fract_digits)
                {
                  mantissa += UNI_FULLSTOP;
                  loop(z, fract_digits)   mantissa += UNI_0;
                }
             ++expo;
           }
        else if (mantissa[2] == UNI_FULLSTOP)   // 10.yyyEe →  1.0yyyEe+1
           {
             mantissa[1] = UNI_FULLSTOP;
             mantissa[2] = UNI_0;
             mantissa.pop_back();
             ++expo;
           }
      }

   ret.append(mantissa);
   ret.append(UNI_E);
   ret.append(from_int(expo));

   return ret;
}
//----------------------------------------------------------------------------
UCS_string
UCS_string::from_double_to_fixed(APL_Float v, int fract_digits)
{
   // fract_digits shall be the number of digitsd AFTER the decimal point

UCS_string ret;
   if (v < 0.0)   { ret.append(UNI_OVERBAR);   v = - v; }

   Assert(v >= 0.0);

   // store the integer part of v in ret, leaving the fract part in v.
   //
   ret.append(from_big(v));   // from_big() leaves fractional part of v in v
   ret.append(UNI_FULLSTOP);

   // append one more fractional digit than needed (the last one will be
   // rounded
   loop(f, fract_digits + 1)
      {
        v = v * 10.0;
        const int vv = v;
        ret.append(Unicode(UNI_0 + vv));
        v -= vv;
      }

   // round to last digit may increase the length
const int ret_len = ret.size();
   ret.round_last_digit();
   while (ret.size() > ret_len)   ret.pop_back();
   return ret;
}
//----------------------------------------------------------------------------
void
UCS_string::round_last_digit()
{
   Assert(size() > 1);

   // if the number ends with digits 0..4 then we simply discard
   // the trailing digit and return
   //
const bool round_down = back() <= UNI_4;   // remember direction
   pop_back();   // discard the digit that is rounded up or down
   if (size() && back() == UNI_FULLSTOP)   pop_back();   // trailing .

   Assert(size() > 0);
   if (round_down)   // round down (= discard last digit)
      {
        return;
      }

bool carry = true;   // from the rounded up digit

   rev_loop (q, size())
       {
         const Unicode uni = at(q);
         if (uni < UNI_0)   continue;   // not a digit, e.g. ¯ or .
         if (uni > UNI_9)   continue;   // not a digit, e.g. ¯ or .
         if (uni == UNI_9)
            {
              at(q) = UNI_0;   // 9 → 0 and keep carry
            }
         else
            {
              at(q) = Unicode(uni + 1);   // round cc up and clear carry
              carry = false;
              break;
            }
       }

   if (size() && back() == UNI_FULLSTOP)   pop_back();
   if (carry)
      {
        insert(at(0) == UNI_OVERBAR || at(0) == UNI_MINUS ? 1 : 0, UNI_1);
      }
}
//----------------------------------------------------------------------------
bool
UCS_string::contains(Unicode uni) const
{
   loop(u, size())   if (at(u) == uni)   return true;
   return false;
}
//----------------------------------------------------------------------------
UCS_string
UCS_string::sort() const
{
UCS_string ret(*this);
   Heapsort<Unicode>::sort(&ret.front(), ret.size(), 0, greater_uni);
   return ret;
}
//----------------------------------------------------------------------------
UCS_string
UCS_string::unique() const
{
   if (size() <= 1)   return UCS_string(*this);

UCS_string sorted = sort();
UCS_string ret;
   ret.reserve(sorted.size());

   ret.append(sorted[0]);
   for (ShapeItem j = 1; j < size(); ++j)
       {
         if (sorted[j] != ret.back())   ret.append(sorted[j]);
       }

   Heapsort<Unicode>::sort(&ret[0], ret.size(), 0, greater_uni);
   return ret;
}
//----------------------------------------------------------------------------
UCS_string
UCS_string::to_HTML(int offset, bool preserve_ws) const
{
UCS_string ret;
   for (;offset < size(); ++offset)
      {
        const Unicode uni = at(offset);
        switch(uni)
           {
             case ' ':  if (preserve_ws)   ret.append_ASCII("&nbsp;");
                        else               ret.append(uni);
                        break;
             case '#':  ret.append_ASCII("&#35;");   break;
             case '%':  ret.append_ASCII("&#37;");   break;
             case '&':  ret.append_ASCII("&#38;");   break;
             case '<':  ret.append_ASCII("&lt;");    break;
             case '>':  ret.append_ASCII("&gt;");    break;
             default:   ret.append(uni);
           }
      }

   return ret;
}
//----------------------------------------------------------------------------
#if UCS_tracking
UCS_string::~UCS_string()
{
   --total_count;
   cerr << setfill('0') << endl << "@@ " << setw(5) << instance_id
        << " DEL ##" << total_count
        << " c= " << Backtrace::caller(3) << setfill(' ') << endl;
}
//----------------------------------------------------------------------------
void UCS_string::create(const char * loc)
{
   ++total_count;
   instance_id = ++total_id;
   cerr << setfill('0') << endl << "@@ " << setw(5) << instance_id
        << " NEW ##" << total_count << " " << loc
        << " c= " << Backtrace::caller(3) << setfill(' ') << endl;
}

#endif
//============================================================================
UCS_ASCII_string::UCS_ASCII_string(TokenTag tag)
{
   switch(tag)
      {
        case TOK_NONE:   append_ASCII("TOK_NONE");             return;
#define TD(tag, _tc, _tv, _id) case tag: append_ASCII(#tag);   return;
#include "Token.def"
      }

   // tag was not defined
   //
char cc[40];
   SPRINTF(cc, "TOK_%4.4X", tag);
   append_ASCII(cc);
}
//----------------------------------------------------------------------------
UCS_ASCII_string::UCS_ASCII_string(TokenClass tc)
{
   switch(tc)
      {
        case TC_ASSIGN:   append_ASCII("TC_ASSIGN");     return;
        case TC_R_ARROW:  append_ASCII("TC_R_ARROW");    return;
        case TC_L_BRACK:  append_ASCII("TC_L_BRACK");    return;
        case TC_R_BRACK:  append_ASCII("TC_R_BRACK");    return;
        case TC_END:      append_ASCII("TC_END");        return;
        case TC_FUN0:     append_ASCII("TC_FUN0");       return;
        case TC_FUN12:    append_ASCII("TC_FUN12");      return;
        case TC_INDEX:    append_ASCII("TC_INDEX");      return;
        case TC_OPER1:    append_ASCII("TC_OPER1");      return;
        case TC_OPER2:    append_ASCII("TC_OPER2");      return;
        case TC_L_PARENT: append_ASCII("TC_L_PARENT");   return;
        case TC_R_PARENT: append_ASCII("TC_R_PARENT");   return;
        case TC_RETURN:   append_ASCII("TC_RETURN");     return;
        case TC_SYMBOL:   append_ASCII("TC_SYMBOL");     return;
        case TC_VALUE:    append_ASCII("TC_VALUE");      return;
        case TC_PINDEX:   append_ASCII("TC_PINDEX");     return;
        case TC_VOID:     append_ASCII("TC_VOID");       return;
        case TC_OFF:      append_ASCII("TC_OFF");        return;
        case TC_SI_LEAVE: append_ASCII("TC_SI_LEAVE");   return;
        case TC_LINE:     append_ASCII("TC_LINE");       return;
        case TC_DIAMOND:  append_ASCII("TC_DIAMOND");    return;
        case TC_NUMERIC:  append_ASCII("TC_NUMERIC");    return;
        case TC_SPACE:    append_ASCII("TC_SPACE");      return;
        case TC_NEWLINE:  append_ASCII("TC_NEWLINE");    return;
        case TC_COLON:    append_ASCII("TC_COLON");      return;
        case TC_QUOTE:    append_ASCII("TC_QUOTE");      return;
        case TC_L_CURLY:  append_ASCII("TC_L_CURLY");    return;
        case TC_R_CURLY:  append_ASCII("TC_R_CURLY");    return;

         default: ;
      }

   // tc was not defined
   //
char cc[40];
   SPRINTF(cc, "TC_%4.4X", tc);
   append_ASCII(cc);
}
//----------------------------------------------------------------------------
UCS_ASCII_string::UCS_ASCII_string(TokenValueType tvt)
{
   switch(tvt)
      {
        case TV_MASK:  append_ASCII("TV_MASK" );   return;
        case TV_NONE:  append_ASCII("TV_NONE" );   return;
        case TV_CHAR:  append_ASCII("TV_CHAR" );   return;
        case TV_INT:   append_ASCII("TV_INT"  );   return;
        case TV_FLT:   append_ASCII("TV_FLT"  );   return;
        case TV_CPX:   append_ASCII("TV_CPX"  );   return;
        case TV_SYM:   append_ASCII("TV_SYM"  );   return;
        case TV_LIN:   append_ASCII("TV_LIN"  );   return;
        case TV_VAL:   append_ASCII("TV_VAL"  );   return;
        case TV_INDEX: append_ASCII("TV_INDEX");   return;
        case TV_FUN:   append_ASCII("TV_FUN"  );   return;

         default: ;
      }

   // tvt was not defined
   //
char cc[40];
   SPRINTF(cc, "TV_%4.4X", tvt);
   append_ASCII(cc);
}
//----------------------------------------------------------------------------

