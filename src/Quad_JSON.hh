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

#ifndef __Quad_JSON_DEFINED__
#define __Quad_JSON_DEFINED__

#include "QuadFunction.hh"

//----------------------------------------------------------------------------
/**
   The system function ⎕JSON
 */
/// The class implementing ⎕JSON
class Quad_JSON : public QuadFunction
{
public:
   /// Constructor.
   Quad_JSON()
      : QuadFunction(TOK_Quad_JSON)
   {}

   static Quad_JSON  fun;          ///< Built-in function.

protected:
   /// overloaded Function::eval_B()
   Token eval_AB(Value_P A, Value_P B) const;

   /// overloaded Function::eval_B()
   Token eval_B(Value_P B) const;

   /// return JSON file (-name in B) converted to APL structured value
   Token convert_file(const Value & B) const;

   /// convert APL value to JSON string
   static Value_P APL_to_JSON(const Value & B, bool sorted);

   /// append APL value to JSON string \b result
   static void APL_to_JSON_string(UCS_string & result, const Value & B,
                                  bool level, bool sorted);

   /// append Cell value to JSON string \b result
   static void APL_to_JSON_string(UCS_string & result, const Cell & B,
                                  bool level, bool sorted);

   /// convert JSON string to APL associative array
   static Value_P JSON_to_APL(const Value & B);

   /** skip the string literal that starts at ucs_B[b].
       Return the content length and increment b.
       at start: ucs_B[b] = the left " of the string
       at return: ucs_B[b] = the right " of the string  */
   static size_t skip_string(const UCS_string & ucs_B, ShapeItem & b);

   /// return the length-1 of the number(-token) starting at \b b in \b ucs_B
   inline static size_t number_len(const UCS_string & ucs_B, ShapeItem b);

   /// parse a JSON value (false, null, true, object, array, number, or string)
   /// and increment along the way.
   static void parse_value(Value & Z, const UCS_string & ucs_B,
                           const std::vector<ShapeItem> & tokens_B,
                           size_t & token0);

   /// parse a JSON array: [ value (, value)* ] and increment token0
   /// along the way.
   static void parse_array(Value & Z, const UCS_string & ucs_B,
                           const std::vector<ShapeItem> & tokens_B,
                           size_t & token0);

   /// parse a JSON object: { member ( , member)* } and increment token0
   static void parse_object(Value & Z, const UCS_string & ucs_B,
                           const std::vector<ShapeItem> & tokens_B,
                           size_t & token0);

   /// parse a JSON object member: "name" : value , ; and increment token0
   /// along the way.
   static void parse_object_member(Value & Z, const UCS_string & ucs_B,
                                   const std::vector<ShapeItem> & tokens_B,
                                   size_t & token0);

   /// parse a JSON number
   static void parse_number(Value & Z, const UCS_string & ucs_B, ShapeItem b);

   /// parse a JSON string
   static void parse_string(Value & Z, const UCS_string & ucs_B, ShapeItem b);

   /// decode a \\uUUUU sequence, return non-Unicode_0 on success and
   /// increment b, or else return Unicode_0 and leave b as is.
   static Unicode decode_UUUU(const UCS_string & ucs_B, ShapeItem b);

   /// return true iff uni is a high surrogate Unicode (0xD8xx)
   static bool is_high_surrogate(Unicode uni)
      { return (uni & ~0x03FF) == 0xD800; }

   /// return true iff uni is a low surrogate Unicode (0xDCxx)
   static bool is_low_surrogate(Unicode uni)
      { return (uni & ~0x03FF) == 0xDC00; }

   /// parse a JSON literal (false, null, or true)
   static void parse_literal(Value & Z, const UCS_string & ucs_B,
                             ShapeItem b, const char * expected_literal);

   /// return the number of name-separators or value-separators at the
   /// top-level of the object or array starting at token0 and increment token0
   /// along the way.
   static size_t comma_count(const UCS_string & ucs_B,
                             const std::vector<ShapeItem> & tokens_B,
                             size_t & token0);
};

#endif // __Quad_JSON_DEFINED__

