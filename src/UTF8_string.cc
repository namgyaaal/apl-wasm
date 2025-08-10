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

#include <string.h>
#include <stdio.h>

#include "Avec.hh"
#include "Common.hh"
#include "Error.hh"
#include "Output.hh"
#include "PrintOperator.hh"
#include "UTF8_string.hh"
#include "Workspace.hh"

/* from RFC 3629 / STD 63:

   Char. number range  |        UTF-8 octet sequence
      (hexadecimal)    |              (binary)
   --------------------+---------------------------------------------
   0000 0000-0000 007F | 0xxxxxxx
   0000 0080-0000 07FF | 110xxxxx 10xxxxxx
   0000 0800-0000 FFFF | 1110xxxx 10xxxxxx 10xxxxxx
   0001 0000-0010 FFFF | 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx

   RFC 2279, the predecessor of RFC 3629, also allowed these:

   0000 0000-0000 007F   0xxxxxxx
   0000 0080-0000 07FF   110xxxxx 10xxxxxx
   0000 0800-0000 FFFF   1110xxxx 10xxxxxx 10xxxxxx
   0001 0000-001F FFFF   11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
   0020 0000-03FF FFFF   111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
   0400 0000-7FFF FFFF   1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx

   In order to be more general, we follow RFC 2279 and also allow 5-byte and
   6-byte encodings
*/

//----------------------------------------------------------------------------
UTF8_string::UTF8_string(const UCS_string & ucs)
{
   Log(LOG_char_conversion)
      CERR << "UTF8_string::UTF8_string(ucs = " << ucs << ")" << endl;

   loop(i, ucs.size())
      {
        int uni = ucs[i];
        if (uni < 0x80)            // 1-byte unicode (ASCII)
           {
             *this += uni;
           }
        else if (uni < 0x800)      // 2-byte unicode
           {
             const uint8_t b1 = uni & 0x3F;   uni >>= 6; 
             *this += uni | 0xC0;
             *this += b1  | 0x80;
           }
        else if (uni < 0x10000)    // 3-byte unicode
           {
             const uint8_t b2 = uni & 0x3F;   uni >>= 6; 
             const uint8_t b1 = uni & 0x3F;   uni >>= 6; 
             *this += uni | 0xE0;
             *this += b1  | 0x80;
             *this += b2  | 0x80;
           }
        else if (uni < 0x200000)   // 4-byte unicode
           {
             const uint8_t b3 = uni & 0x3F;   uni >>= 6; 
             const uint8_t b2 = uni & 0x3F;   uni >>= 6; 
             const uint8_t b1 = uni & 0x3F;   uni >>= 6; 
             *this += uni | 0xF0;
             *this += b1  | 0x80;
             *this += b2  | 0x80;
             *this += b3  | 0x80;
           }
        else if (uni < 0x4000000)   // 5-byte unicode
           {
             const uint8_t b4 = uni & 0x3F;   uni >>= 6; 
             const uint8_t b3 = uni & 0x3F;   uni >>= 6; 
             const uint8_t b2 = uni & 0x3F;   uni >>= 6; 
             const uint8_t b1 = uni & 0x3F;   uni >>= 6; 
             *this += uni | 0xF8;
             *this += b1  | 0x80;
             *this += b2  | 0x80;
             *this += b3  | 0x80;
             *this += b4  | 0x80;
           }
        else if (size_t(uni) < 0x80000000)   // 6-byte unicode
           {
             const uint8_t b5 = uni & 0x3F;   uni >>= 6; 
             const uint8_t b4 = uni & 0x3F;   uni >>= 6; 
             const uint8_t b3 = uni & 0x3F;   uni >>= 6; 
             const uint8_t b2 = uni & 0x3F;   uni >>= 6; 
             const uint8_t b1 = uni & 0x3F;   uni >>= 6; 
             *this += uni | 0xFC;
             *this += b1  | 0x80;
             *this += b2  | 0x80;
             *this += b3  | 0x80;
             *this += b4  | 0x80;
             *this += b5  | 0x80;
           }
        else
           {
             CERR << "Bad Unicode: " << UNI(uni) << endl
                  << "The offending ucs string is:";
             loop(ii, ucs.size()) CERR << " " << HEX(ucs[ii]);
             CERR << endl;

             BACKTRACE
             Assert(0 && "Error in UTF8_string::UTF8_string(ucs)");
           }
      }

   Log(LOG_char_conversion)
      CERR << "UTF8_string::UTF8_string(): utf = " << *this << endl;
}
//----------------------------------------------------------------------------
ostream &
UTF8_string::dump_hex(ostream & out, int max_bytes) const
{
   loop(b, size())
      {
        if (b)   out << " ";
        if (b >= max_bytes)   return out << "...";

         out << HEX2(at(b));
      }

   return out;
}
//----------------------------------------------------------------------------
Unicode
UTF8_string::toUni(const UTF8 * string, int & len, bool verbose)
{
const uint32_t b0 = *string++;
   if (b0 < 0x80)                  { len = 1;   return Unicode(b0); }

uint32_t bx = b0;   // the "significant" bits in b0
   if      ((b0 & 0xE0) == 0xC0)   { len = 2;   bx &= 0x1F; }
   else if ((b0 & 0xF0) == 0xE0)   { len = 3;   bx &= 0x0F; }
   else if ((b0 & 0xF8) == 0xF0)   { len = 4;   bx &= 0x07; }
   else if ((b0 & 0xFC) == 0xF8)   { len = 5;   bx &= 0x03; }
   else if ((b0 & 0xFE) == 0xFC)   { len = 6;   bx &= 0x01; }
   else if (verbose)
      {
        CERR << "Bad UTF8 sequence: " << HEX(b0);
        loop(j, 6)
           {
             const uint32_t bx = string[j];
             if (bx & 0x80)   CERR << " " << HEX(bx);
             else              break;
           }

        CERR <<  " at " LOC << endl;

        BACKTRACE
        Assert(0 && "Internal error in UTF8_string::toUni()");
      }
   else
      {
        len = 0;
        return Invalid_Unicode;
      }

uint32_t uni = 0;
   loop(l, len - 1)
       {
         const UTF8 subc = *string++;
         if ((subc & 0xC0) != 0x80)
            {
              CERR << "Bad UTF8 sequence: " << HEX(b0) << "... at " LOC << endl;
              Assert(0 && "Internal error in UTF8_string::toUni()");
            }

         bx  <<= 6;
         uni <<= 6;
         uni |= subc & 0x3F;
       }

   return Unicode(bx | uni);
}
//----------------------------------------------------------------------------
int
UTF8_string::bytes_chars(const void * string)
{
const UTF8 * str = utf8P(string);
int ret = 0;

   while (*str)   { if (0x80 == (0xC0 & *str++))   ++ret; }
   return ret;
}
//----------------------------------------------------------------------------
Unicode
UTF8_string::getc(istream & in)
{
const uint32_t b0 = in.get() & 0xFF;
   if (!in.good())         return Invalid_Unicode;
   if      (b0 < 0x80)   { return Unicode(b0);    }

uint32_t bx = b0;   // the "significant" bits in b0
int len;

   if      ((b0 & 0xE0) == 0xC0)   { len = 1;   bx &= 0x1F; }
   else if ((b0 & 0xF0) == 0xE0)   { len = 2;   bx &= 0x0F; }
   else if ((b0 & 0xF8) == 0xF0)   { len = 3;   bx &= 0x0E; }
   else
      {
        CERR << "Bad UTF8 sequence: " << HEX(b0) << "... at " LOC << endl;
        BACKTRACE
        return Invalid_Unicode;
      }

char cc[4];
   in.get(cc, len + 1);   // read subsequent characters + terminating 0
   if (!in.good())   return Invalid_Unicode;

uint32_t uni = 0;
   loop(l, len)
       {
         const UTF8 subc = cc[l];
         if ((subc & 0xC0) != 0x80)
            {
              CERR << "Bad UTF8 sequence: " << HEX(b0) << "... at " LOC << endl;
              return Invalid_Unicode;
            }

         bx  <<= 6;
         uni <<= 6;
         uni |= subc & 0x3F;
       }

   return Unicode(bx | uni);
}
//----------------------------------------------------------------------------
bool
UTF8_string::starts_with(const char * path) const
{
   if (path == 0)   return false;   // no path provided

const size_t path_len = strlen(path);
   if (path_len > size())   return false;   // path_len longer than this string

   // can't use strncmp() because this string may not be 0-terminated
   //
   loop(p, path_len)   if (at(p) != path[p])   return false;
   return true;
}
//----------------------------------------------------------------------------
bool
UTF8_string::ends_with(const char * ext) const
{
   if (ext == 0)   return false;   // no ext provided

const size_t ext_len = strlen(ext);
   if (ext_len > size())   return false;   // ext longer than this string

   // can't use strncmp() because this string may not be 0-terminated
   //
   loop(e, ext_len)   if (at(size() - ext_len + e) != ext[e])   return false;
   return true;
}
//----------------------------------------------------------------------------
int
UTF8_string::un_HTML(int in_HTML)
{
int dest = 0;
bool got_tag = false;
   loop(src, size())
      {
        const char cc = at(src);
        if (in_HTML == 2)   // inside HTML tag, i.e. < seen
           {
             if (cc == '>')   in_HTML = 1;   // in HTML but not in HTML tag
             continue;
           }

        if (cc == '<')   // start of HTML tag
           {
             in_HTML = 2;   // now inside HTML tag
             got_tag = true;
             continue;
           }

        if (cc != '&')   // unless HTML-escaped char
           {
             at(dest++) = cc;
             continue;
           }

        // at this point cc == '&' which is the start of an HTML-escaped
        // character. This can be:
        //
        // &NN;  with decimal digits NN...,        or
        // &#XX;  with hexadecimal digits XX...,   or (incomplete list)
        // &gt;   for >,                           or
        // &lt;   for <
        //
        // There exist more HTML escapes, but the function producing them
        // (i.e. UCS_string::to_HTML()) does not emit them.
        //
        const int rest = size() - src;
        if (rest > 3 && at(src + 1) == '#' &&
            strchr("0123456789", at(src + 2)))
           {
             src += 2;   // skip "&#"
             int val = 0;
             char * end = 0;
             if (at(src) == 'x')   // hex value
                {
                  ++src;   // skip "x"
                  val = strtoll(c_str() + src, &end, 16);
                }
             else                 // decimal
                {
                  val = strtoll(c_str() + src, &end, 10);
                }
             at(dest++) = val;
             src = end - c_str();   // skip hex or decimal digits
             // ";" skipped by loop()
           }
        else if (rest > 3 && at(src + 1) == 'g' &&
                             at(src + 2) == 't' &&
                             at(src + 3) == ';')
           {
             at(dest++) = '>';
             src += 3;   // skip "gt;"
           }
        else if (rest > 3 && at(src + 1) == 'l' &&
                             at(src + 2) == 't' &&
                             at(src + 3) == ';')
           {
             at(dest++) = '<';
             src += 3;   // skip "lt;"
           }
        else if (rest > 4 && at(src + 1) == 'n' &&
                             at(src + 2) == 'b' &&
                             at(src + 3) == 's' &&
                             at(src + 4) == 'p' &&
                             at(src + 5) == ';')
           {
             at(dest++) = ' ';
             src += 5;   // skip "nbsp;"
           }
        else
           {
             at(dest++) = cc;
           }
      }

   if (got_tag)   dest = 0;
   resize(dest);
   return in_HTML;
}
//----------------------------------------------------------------------------
uint64_t
UTF8_string::long_value() const
{
uint64_t ret = 0;
   loop(j, size())   ret = 10*ret + (at(j) - UNI_0);
   return ret;
}
//----------------------------------------------------------------------------
bool
UTF8_string::round_0_1()
{
   if (back() >= '5')   // round up
      {
        // rounding up of the last digit creates a carry
        //
        for (int j = size() - 2; j >= 0; --j)
            {
              if (at(j) == '9')   // propagate carry
                 {
                   at(j) = '0';
                   continue;      // carry survived
                 }

              // eat carry
              //
              ++at(j);
              pop_back();   // discard last digit.
              return false;   // no carry
            }
      }

   // if carry survived then digits were 0.999... and were then rounded
   // up to 1.000...
   //
   at(0) = '1';
   pop_back();
   return true;   // 1.0 → 0.1
}
//----------------------------------------------------------------------------
/// declared in PrintOperator.hh
ostream &
operator <<(ostream & os, const UTF8_string & utf)
{
   loop(c, utf.size())   os << utf[c];
   return os;
}
//============================================================================
int
UTF8_filebuf::overflow(int c)
{
   data += c;
   return 0;
}
//============================================================================
