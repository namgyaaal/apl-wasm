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

#ifndef __UCS_STRING_HH_DEFINED__
#define __UCS_STRING_HH_DEFINED__

#include <stdint.h>
#include <stdio.h>
#include <string>
#include <vector>

#include "Assert.hh"
#include "Avec.hh"
#include "Common.hh"
#include "Heapsort.hh"
#include "TokenEnums.hh"
#include "Unicode.hh"
#include "UTF8_string.hh"

using namespace std;

class PrintBuffer;
class PrintContext;
class Shape;
class Value;
class UCS_string_vector;

/// track construction and destruction of UCS_strings
#define UCS_tracking 0

//============================================================================
/// A string of Unicode characters (32-bit)
class UCS_string : public UnicodeString
{
public:
   /// default constructor: empty string
   UCS_string();

   /// constructor: one-element string
   UCS_string(Unicode uni);

   /// constructor: \b len Unicode characters, starting at \b data
   UCS_string(const Unicode * data, size_t len);

   /// constructor: \b len times \b uni
   UCS_string(size_t len, Unicode uni);

   /// constructor: copy of another UCS_string
   UCS_string(const UCS_string & ucs);

   /// constructor: copy of another UCS_string \b ucs, starting at \b pos
   /// in \b ucs and length \b len
   UCS_string(const UCS_string & ucs, size_t pos, size_t len);

   /// constructor: UCS_string from UTF8_string
   UCS_string(const UTF8_string & utf);

   /// constructor: UCS_string from print buffer
   UCS_string(const PrintBuffer & pb, sRank rank, int quad_PW);

   /// constructor: UCS_string from a double with quad_pp valid digits.
   /// (eg. 3.33 has 3 digits), In standard APL format.
   UCS_string(APL_Float value, bool & scaled, const PrintContext & pctx);

   /// constructor: read one line from UTF8-encoded file.
   UCS_string(istream & in);

   /// constructor: UCS_string from simple character vector value.
   UCS_string(const Value & value);

   /// constructor: UCS_string from a pointer Cell pointing to a
   /// character vector value (Asserts if not)
   UCS_string(const Cell & cell);

#if UCS_tracking
   /// common part of all constructors
   void create(const char * loc);

   /// destructor
   ~UCS_string();
#else
   /// common part of all constructors
   void create(const char * loc)   { ++total_count; }

   /// destructor
   ~UCS_string()                   { --total_count; }
#endif

   /// cast to an array of items with the same size as Unicode. This is for
   /// interfacing to libraries that have typedef'ed Unicodes differently.
   template<typename T>
   const T * raw() const
      {
        Assert(sizeof(T) == sizeof(Unicode));
        return reinterpret_cast<const T *>(&at(0));
      }

   /// return this string with the first \b drop_count characters removed
   UCS_string drop(int drop_count) const
      {
        if (drop_count <= 0)        return UCS_string(*this, 0, size());
        if (size() <= drop_count)   return UCS_string();
        return UCS_string(*this, drop_count, size() - drop_count);
      }

   /// return true if every character in \b this string is the digit '0'
   bool all_zeroes() const
      { loop(s, size())   if ((*this)[s] != UNI_0)   return false;
        return true;
      }

   /// return the last character in \b this string
   Unicode back() const
    { return size() ? (*this)[size() - 1] : Invalid_Unicode; }

   /// compute the length of an output row
   int compute_chunk_length(int quad_PW, int col) const;

   /// skip leading whitespaces starting at idx, append the following
   /// non-whitespaces (if any) to \b dest, and skip trailing whitespaces.
   /// return the idx after that.
   size_t copy_black(UCS_string & dest, int idx) const;

   /// return the FNV (Fowler-Noll-Vo) hash of \b this_string
   uint32_t FNV_hash() const
      { enum { FNV_Offset_32 = 0x811C9DC5, FNV_Prime_32  = 16777619 };
        uint32_t hash = FNV_Offset_32;
        loop(s, size()) hash = (hash * FNV_Prime_32) ^ at(s);
        return hash;
      }

   /// return the number of LF characters in \b this string
   ShapeItem LF_count() const;

   /// return the start position of \b sub in \b this string or -1 if \b sub
   /// is not contained in \b this string
   ShapeItem substr_pos(const UCS_string & sub) const;

   /// return the start position of """ in \b this string or -1 if \b """
   /// is not contained in \b this string
   ShapeItem multi_pos(bool expect_end) const;

   /// return true if \b this string contains any non-whitespace characters
   bool has_black() const;

   /// return true if \b this starts with prefix (ASCII, case sensitive).
   bool starts_with(const char * prefix) const;

   /// return true if \b this ends with suffix (ASCII, case sensitive).
   bool ends_with(const char * suffix) const;

   /// return true if \b this starts with \b prefix (case sensitive).
   bool starts_with(const UCS_string & prefix) const;

   /// return true if \b this starts with \b prefix (ASCII, case insensitive).
   bool starts_iwith(const char * prefix) const;

   /// return true if \b this starts with \b prefix (case insensitive).
   bool starts_iwith(const UCS_string & prefix) const;

   /// return a string like this, but with pad chars mapped to spaces
   UCS_string no_pad() const;

   /// return this string reversed (i.e. characters from back to front).
   UCS_string reverse() const;

   /// return true if \b this string starts with # or ⍝ or x:
   bool is_comment_or_label() const;

   /// return the number of unescaped and un-commented " in this string
   ShapeItem double_quote_count(bool in_quote2) const;

   /// return the position of the first (leftmost) unescaped " in \b this
   /// string (if any), or else -1
   ShapeItem double_quote_first() const;

   /// return the position of the last (rightmost) unescaped " in \b this
   /// string (if any), or else -1
   ShapeItem double_quote_last() const;

   /// return integer value for a string starting with optional whitespaces,
   /// followed by digits.
   int atoi() const;

   /// return a string like this, but with pad chars removed
   UCS_string remove_pad() const;

   /// split \b this multi-line string into individual lines,
   /// removing the CR and NL chars in \b this string.
   size_t to_vector(UCS_string_vector & result) const;

   /// return \b this string with "escape sequences" replaced by their real
   /// characters ('' → ' if single quoted and \\r, \\n, \\xNNN etc. otherwise.
   UCS_string un_escape(bool double_quoted, bool keep_LF) const;

   /// the inverse of \b un_escape().
   UCS_string do_escape(bool double_quoted) const;

   /// case-sensitive comparison: Return true iff \b this comes before \b other
   bool lexical_before(const UCS_string other) const;

   /// dump \b this string to out (like U+nnn U+mmm ... )
   ostream & dump(ostream & out) const;

   /// sort the characters in this string by their Unicode
   UCS_string sort() const;

   /// return the characters in this string (sorted and duplicates removed)
   UCS_string unique() const;

   /// return \b this string HTML-escaped, starting at offset, and using &nbsp;
   /// for blanks
   UCS_string to_HTML(int offset, bool preserve_ws) const;

   /// remove comment (if any) from \b this string
   void remove_comment();

   /// remove trailing pad characters
   void remove_trailing_padchars();

   /// remove trailing blanks, tabs, etc
   void remove_trailing_whitespaces();

   /// remove leading blanks, tabs, etc
   void remove_leading_whitespaces();

   /// remove leading and trailing whitespaces
   void remove_leading_and_trailing_whitespaces()
      {
        remove_trailing_whitespaces();
        remove_leading_whitespaces();
      }

   /// \b this is a command with optional args. Remove leading and trailing
   /// whitespaces, append args to rest, and remove args from this.
   void split_ws(UCS_string & rest);

   /// return the last character in \b this string
   Unicode & back()
    { Assert(size());   return at(size() - 1); }

   /// replace pad chars in \b this string by spaces
   void map_pad();

   /// remove the last character in \b this string
   void pop_back()
   { Assert(size() > 0); UnicodeString::pop_back(); }

   /// append UCS_string suffix to \b this string
   void append(const UCS_string & suffix)
      {  UnicodeString::append(suffix); }

   /// append 0-terminated ASCII string \b str to this string.
   /// \b str is NOT interpreted as UTF8 string (use append_UTF8() instead of
   /// append_ASCII() if such interpretation is desired)
   void append_ASCII(const char * ascii)
      { while (const char cc = *ascii++)   *this += Unicode(cc); }

   /// append 0-terminated UTF8 string str to \b this UCS_string.
   // This is different from append_ASCII((const char * str):
   ///
   /// append_ascii() appends one Unicode per byte (i.e. strlen(str) in total),
   /// without checking for UTF8 sequences.
   ///
   /// append_UTF8() appends one Unicode per UTF8 sequence (which is the same
   /// if all characteras are ASCII, but less if not.
   void append_UTF8(const UTF8 * str);

   /// same as app(const UTF8 * str)
   void append_UTF8(const char * str)
      { append_UTF8(utf8P(str)); }

   /// more intuitive insert() function
   void insert(ShapeItem pos, Unicode uni)
      {  UnicodeString::insert(pos, 1, uni); }

   /// prepend character \b uni
   void prepend(Unicode uni)
      { insert(0, uni); }

   /// return \b this string and \b other concatenated
   UCS_string operator +(const UCS_string & other) const
      { UCS_string ret(*this);   ret += other;   return ret; }

   /// append C-string \b str
   UCS_string & operator <<(const char * str)
      { append_UTF8(str);   return *this; }

   /// append number \b num
   UCS_string & operator <<(ShapeItem num)
      { append_number(num);   return *this; }

   /// append character \b uni
   UCS_string & operator <<(Unicode uni)
      { *this += uni;   return *this; }

   /// append UCS_string \b other
   UCS_string & operator <<(const UCS_string & other)
      { UnicodeString::append(other);   return *this; }

   /// compare \b this with UCS_string \b other
   Comp_result compare(const UCS_string & other) const
      {
        if (*this < other)   return COMP_LT;
        if (*this > other)   return COMP_GT;
        return COMP_EQ;
      }

   /// append \b other in (single) quotes, doubling single quotes in \b other
   void append_quoted(const UCS_string & other);

   /// append number (in ASCII encoding like %d) to this string
   void append_number(ShapeItem num);

   /// append floating a point number (in ASCII encoding like %f) to this string
   void append_double(double num);

   /// append number (in ASCII encoding like %X or %x) to this string
   void append_hex(ShapeItem num, bool uppercase);

   /// append shape (in APL encoding tke left arg of ↑) this string
   void append_shape(const Shape & shape);

   /// append members (like x.y.z) starting at members[m] and going backwards
   /// from the end of \b members to \b this string.
   void append_members(const basic_string<const UCS_string *> & members, int m);

   /// append number (in ASCII encoding like %lf) to this string
   void append_float(APL_Float num);

   /// overload basic_string::size() so that it returns a signed length
   ShapeItem size() const
      { return  ShapeItem(UnicodeString::size()); }

   /// an iterator for UCS_strings
   class iterator
      {
        public:
           /// constructor: start at position p
           iterator(const UCS_string & ucs)
           : s(ucs),
             pos(0)
           {}

           /// return the next char (without pos increment)
           Unicode lookup() const
              { Assert(pos < s.size());    return s[pos]; }

           /// return the next char
           Unicode next()
              { Assert(pos < s.size());   return s[pos++]; }

           /// undo a prior next();
           void un_next()
              { Assert(pos);   --pos; }

           /// return true iff there are more chars available
           bool has_more() const
              { return pos < s.size(); }

        /// skip whitespace
        void skip_white()
           { while (pos < s.size() && Avec::is_white(s[pos]))   ++pos; }

        protected:
           /// the string
           const UCS_string & s;

           /// the current position in the string
           int pos;
      };

   /// round last digit and discard it. Note that \b this is always expected
   /// to be in floating point format and never in exponential format
   void round_last_digit();

   /// return true if \b this string contains \b uni
   bool contains(Unicode uni) const;

   /// erase 1 (!) character at pos
   void erase(ShapeItem pos)
      {  UnicodeString::erase(pos, 1); }

   /// helper function for Heapsort<Unicode>::sort()
   static bool greater_uni(const Unicode & u1, const Unicode & u2,
                            const void *)
      { return u1 > u2; }

   /// convert a signed integer value to an UCS_string (like snprintf("%d"))
   static UCS_string from_int(int64_t value);

   /// convert an unsigned integer value to an UCS_string (like snprintf("%u"))
   static UCS_string from_uint(uint64_t value);

   /// convert the integer part of value to an UCS_string and remove it
   /// from value
   static UCS_string from_big(APL_Float & value);

   /// convert the double \b value to an UCS_string scaled (exponential) format
   /// (with \b fract_digits fractional digits).
   static UCS_string from_double_to_expo(APL_Float value, int fract_digits);

   /// convert the double \b value to an UCS_string in fixed point format
   /// (with \b fract_digits fractional digits).
   static UCS_string from_double_to_fixed(APL_Float value, int fract_digits);

   /// convert double \b value to an UCS_string with \b quad_pp significant
   /// digits in scaled (exponential) format
   static UCS_string from_double_expo_pp(APL_Float value, int quad_pp);

   /// convert double \b value to an UCS_string with \b quad_pp significant
   /// digits in fixed point format
   static UCS_string from_double_fixed_pp(APL_Float value, int quad_pp);

   /// return the total number of UCS_strings
   static ShapeItem get_total_count()
      { return total_count; }

   /// return true if n1 < n2
   static bool compare_names(const UCS_string & n1,
                             const UCS_string & n2, const void *)
      { return n2.compare(n1) == COMP_LT; }

protected:
   /// the total number of UCS_strings
   static ShapeItem total_count;

   /// the next unique instance_id
   static ShapeItem total_id;

#if UCS_tracking
   /// a unique number for \b this  UCS_string
   ShapeItem instance_id;
#endif

private:
   /// prevent accidental allocation
   void * operator new[](size_t size);

   /// prevent accidental de-allocation
   void operator delete[](void *);

private:
   /// prevent accidental usage of the rather dangerous default len parameter
   /// in basic_strng::erase(pos, len = npos)
    UnicodeString & erase(size_type pos, size_type len);

   /// constructor: UCS_string from 0-terminated C string.
   /// Made private because it was far too often used incorrectly.

   UCS_string(const char * cstring);
};
//----------------------------------------------------------------------------
/// an UCS_string that contains only ASCII characters,
class UCS_ASCII_string : public UCS_string
{
public:
   /// constructor. The caller MUST have checked that all characters in
   /// cstring are ASCII. Only use it with C literals, not with const char *s.
   UCS_ASCII_string(const char * ascii)
      { append_ASCII(ascii); }

   /// return \b tag in a readable form (e.g. TOK_LINE, TOK_SYMBOL, ...).
   UCS_ASCII_string(TokenTag tag);

   /// return \b tc in a readable form (e.g. TC_ASSIGN, TC_R_ARROW, ...).
   UCS_ASCII_string(TokenClass tc);

   /// return \b tvt in a readable form (e.g. TV_NONE, TV_CHAR, ...).
   UCS_ASCII_string(TokenValueType tvt);
};
//----------------------------------------------------------------------------
inline void
Hswap(const UCS_string * & usp1, const UCS_string * & usp2)
{
const UCS_string * tmp = usp1;
   usp1 = usp2;
   usp2 = tmp;
}
//----------------------------------------------------------------------------
inline void
Hswap(UCS_string & u1, UCS_string & u2)
{
   swap(u1, u2);
}
//----------------------------------------------------------------------------

#endif // __UCS_STRING_HH_DEFINED__

