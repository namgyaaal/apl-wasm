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

#ifndef __UTF8_STRING_HH_DEFINED__
#define __UTF8_STRING_HH_DEFINED__

#include <iostream>
#include <stdint.h>
#include <string>

#include "Common.hh"

using namespace std;

class UCS_string;
class Value;
//----------------------------------------------------------------------------
/// one byte (= 7-bit character !) of an ASCII string
typedef char ASCII;

/// one byte (not character !( of a UTF8 encoded Unicode (RFC 3629) string
typedef char8_t UTF8;

//----------------------------------------------------------------------------
/// frequently used cast to const UTF8 *
inline const UTF8 *
utf8P(const void * vp)
{
  return reinterpret_cast<const UTF8 *>(vp);
}
//----------------------------------------------------------------------------
/// frequently used cast to UTF8 *
inline UTF8 *
utf8P(ASCII * cp)
{
  return reinterpret_cast<UTF8 *>(cp);
}
//----------------------------------------------------------------------------
/// an UTF8 encoded Unicode (RFC 3629) string
class UTF8_string : public std::basic_string<UTF8>
{
public:
   /// constructor: empty UTF8_string
   UTF8_string()
   {}

   /// constructor: UTF8_string from 0-terminated C string.
   UTF8_string(const ASCII * str)
      { while (const ASCII cc = *str++)   *this += cc; }

   /// constructor: copy of C string, at most len bytes, possibly less if
   /// \b str is 0-terminated.
   UTF8_string(const UTF8 * str, size_t len)
      {
        loop(l, len)
            if (const UTF8 cc = *str++)   *this += cc;
            else        break;
      }

   /// constructor: copy of UCS string. The Unicodes in ucs
   /// will be UTF8-encoded
   UTF8_string(const UCS_string & ucs);

   /// return true iff \b this is equal to \b other
   bool operator ==(const UTF8_string & other) const
      {
        if (size() != other.size())   return false;
        loop(c, size())   if (at(c) != other.at(c))   return false;
        return true;
      }

   /// return \b this string as a 0-terminated C string
   const char * c_str() const
      { return reinterpret_cast<const char *>
                               (std::basic_string<UTF8>::c_str()); }

   /// prevent basic_string::erase() with its dangerous default value for
   /// the number of erased character.
   void erase(size_t pos)
      { basic_string<UTF8>::erase(pos, 1); }

   /// return the last byte in this string
   UTF8 back() const
      { Assert(size());   return at(size() - 1); }

   /// discard the last byte in this string
   void pop_back()
      { Assert(size());   resize(size() - 1); }

   /// append a 0-terminated C string
   void append_ASCII(const char * ascii)
      { while (*ascii)   *this += *ascii++; }

   /// append the UTF8_string \b suffix
   void append_UTF8(const UTF8_string & suffix)
      { loop(s, suffix.size())   *this += suffix[s]; }

   /// display bytes in this UTF string
   ostream & dump_hex(ostream & out, int max_bytes) const;

   /// return true iff string ends with ext (usually a file name extension)
   bool ends_with(const char * ext) const;

   /// return true iff string starts with path (usually a file path)
   bool starts_with(const char * path) const;

   /// skip over < ... > and expand &lt; and friends
   int un_HTML(int in_HTML);


   /// essentially strtol(this, 0, 10)
   uint64_t long_value() const;

   /// round a digit string is the fractional part of a number between
   /// 0.0... and 0.9... up or down according to its last digit, return true
   /// if the exponent shall be increased (because 1.0 -> 0.1)
   bool round_0_1();

   /// convert the first char in UTF8-encoded string to Unicode,
   /// setting len to the number of bytes in the UTF8 encoding of the char
   static Unicode toUni(const UTF8 * string, int & len, bool verbose);

   /// return the (always positive) difference between the number of bytes
   /// and the number of chars in the (UTF8 encoded) \b string.
   static int bytes_chars(const void * string);

   /// return the next UTF8 encoded char from an input file
   static Unicode getc(istream & in);
};
//============================================================================
/// A UTF8 string to be used as filebuf in UTF8_ostream
class UTF8_filebuf : public filebuf
{
public:
   /// return the data in this filebuf
   const UTF8_string & get_data()
      { return data; }

protected:
   /// insert \b c into this filebuf
   virtual int overflow(int c);

   /// the data in this filebuf
   UTF8_string data;
};
//============================================================================
/// a UTF8 string that can be used as ostream
class UTF8_ostream : public ostream
{
public:
   /// An UTF8_string that can be used like an ostream to format data
   UTF8_ostream()
   : ostream(&utf8_filebuf)
   {}

   /// return the data in this UTF8_string
   const UTF8_string & get_data()
      { return utf8_filebuf.get_data(); }

protected:
   /// the filebuf of this ostream
   UTF8_filebuf utf8_filebuf;
};
//============================================================================

#endif // __UTF8_STRING_HH_DEFINED__
