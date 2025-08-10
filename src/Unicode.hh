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

#ifndef __UNICODE_HH_DEFINED__
#define __UNICODE_HH_DEFINED__

#ifndef __COMMON_HH_DEFINED__
# error This file shall NOT be #included directly, but by #including Common.hh
#endif

#include "Common.hh"
#include <string>

/// One Unicode character
enum Unicode
{
#define char_def(name, uni, _tag, _flags, _av_pos) UNI_ ## name = uni,
#define char_uni(name, uni, _tag, _flags)          UNI_ ## name = uni,
#include "Avec.def"

   Unicode_0       = 0,            ///< End of unicode string
   Invalid_Unicode = 0x55AA55AA,   ///< An invalid Unicode.

   // cursor control characters for line editing
   //
   UNI_EOF         = -1,
   UNI_CursorUp    = -2,
   UNI_CursorDown  = -3,
   UNI_CursorRight = -4,
   UNI_CursorLeft  = -5,
   UNI_CursorEnd   = -6,
   UNI_CursorHome  = -7,
   UNI_InsertMode  = -8,

   /// internal pad characters - will be removed before printout
#ifdef cfg_VISIBLE_MARKERS_WANTED
   UNI_iPAD_U2    = UNI_PAD_U2,   // blank on the right after notchar column
   UNI_iPAD_U3    = UNI_PAD_U3,   // blank on the left before notchar column
   UNI_iPAD_U1    = UNI_PAD_U1,   // not (yet) a pad char

   UNI_iPAD_U0    = UNI_PAD_U0,   // not (yet) a pad char
   UNI_iPAD_U4    = UNI_PAD_U4,   // blank on the left to separate values
   UNI_iPAD_U5    = UNI_PAD_U5,   // blank on the right to separate values
   UNI_iPAD_U6    = UNI_PAD_U6,   // empty line below to reach max_row_height
   UNI_iPAD_U7    = UNI_PAD_U7,   // pad to the right to achieve max_spacing
   UNI_iPAD_U8    = UNI_PAD_U8,   // blank on the left to indicate depth
   UNI_iPAD_U9    = UNI_PAD_U9,   // blank on the right to indicate depth

   UNI_iPAD_L0    = UNI_PAD_L0,   // dimension separator row (rank > 2)
   UNI_iPAD_L1    = UNI_PAD_L1,   // new line padding to achieve column width
   UNI_iPAD_L2    = UNI_PAD_L2,   // old lines padding to achieve column width
   UNI_iPAD_L3    = UNI_PAD_L3,   // blanks on the left to pad integer part
   UNI_iPAD_L4    = UNI_PAD_L4,   // blanks on the right to pad fract part
   UNI_iPAD_L5    = UNI_PAD_L5,   // blanks on the left to pad strings
   UNI_iPAD_L9    = UNI_PAD_L9,   // not (yet) a pad char
#else
   UNI_iPAD_U2    = 0xEEEE,
   UNI_iPAD_U3    = 0xEEEF,
   UNI_iPAD_U1    = 0xEEF0,

   UNI_iPAD_U0    = 0xEEF1,
   UNI_iPAD_U4    = 0xEEF2,
   UNI_iPAD_U5    = 0xEEF3,
   UNI_iPAD_U6    = 0xEEF4,
   UNI_iPAD_U7    = 0xEEF5,
   UNI_iPAD_U8    = 0xEEF6,
   UNI_iPAD_U9    = 0xEEF7,

   UNI_iPAD_L0    = 0xEEF8,
   UNI_iPAD_L1    = 0xEEF9,
   UNI_iPAD_L2    = 0xEEFA,
   UNI_iPAD_L3    = 0xEEFB,
   UNI_iPAD_L4    = 0xEEFC,
   UNI_iPAD_L5    = 0xEEFD,
   UNI_iPAD_L9    = 0xEEFE,
#endif
};

/// value 0-15 of hex digit, or -1 if uni not in "023456789ABCDEFabcdef"
extern int nibble(Unicode uni);

/// value 0-63 of base64 digit, or -1 if uni not base64 (RFC 4648)
extern int sixbit(Unicode uni);

/// swap Unicodes u1 and u2 (used by UCS_string::sort())
inline void
Hswap(Unicode & u1, Unicode & u2)
{ const Unicode tmp = u1;   u1 = u2;   u2 = tmp; }


struct UnicodeCharTraits 
{
   using char_type = Unicode; 
   using int_type = std::char_traits<char32_t>::int_type; 
   using off_type = std::char_traits<char32_t>::off_type; 
   using pos_type = std::char_traits<char32_t>::pos_type;
   using state_type = std::char_traits<char32_t>::state_type;

   static void assign(char_type &c1, char_type c2) noexcept;
   static char_type *assign(char_type *s, std::size_t n, char_type c);
   static bool eq(char_type c1, char_type c2) noexcept;
   static bool lt(char_type c1, char_type c2) noexcept;
   static char_type* move(char_type* s1, const char_type* s2, std::size_t n);
   static char_type* copy(char_type* s1, const char_type* s2, std::size_t n);
   static int  compare(const char_type* s1, const char_type* s2, std::size_t n);
   static std::size_t   length(const char_type* s);
   static const char_type* find(const char_type* s, std::size_t n, const char_type& c);
   static char_type  to_char_type(int_type i) noexcept;
   static int_type   to_int_type(char_type c) noexcept;
   static bool eq_int_type(int_type i1, int_type i2) noexcept;
   static int_type   eof() noexcept;
   static int_type   not_eof(int_type i) noexcept;
};

using UnicodeString = std::basic_string<Unicode, UnicodeCharTraits>;


#endif // __UNICODE_HH_DEFINED__
