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

#ifndef __QUAD_TF_HH_DEFINED__
#define __QUAD_TF_HH_DEFINED__

#include "QuadFunction.hh"

//----------------------------------------------------------------------------
/** The system function Quad-TF (Transfer Form).  */
/// The class implementing ⎕TF
class Quad_TF : public QuadFunction
{
public:
   /// Constructor.
   Quad_TF() : QuadFunction(TOK_Quad_TF) {}

   /// Overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const;

   static Quad_TF  fun;          ///< Built-in function.

   /// return true if val contains an 1⎕TF or 2⎕TF record
   static bool is_inverse(const UCS_string & maybe_name);

   /// return B in transfer format 1 (old APL format)
   static Value_P tf1(const UCS_string & symbol_name);

   /// return B in transfer format 2 (new APL2 format)
   static Token tf2(const UCS_string & symbol_name);

   /// return B in transfer format 3 (APL2 CDR format)
   static Value_P tf3(const UCS_string & symbol_name);

   /// append ravel of \b value in tf2_format to \b ucs.
   /// Return true iff the value should be enclosed in parentheses when grouped.
   static void tf2_value(int level, UCS_string & ucs,
                                    const Value & value, ShapeItem nesting);

   /// store B in transfer format 2 (new APL format) into \b ucs
   static void tf2_fun_ucs(UCS_string & ucs, const UCS_string & fun_name,
                           const Function & fun);

   /// store simple character vector \b vec in \b ucs, either as
   /// 'xxx' or as (UCS nn nnn ...)
   static void tf2_char_vec(UCS_string & ucs, const UCS_string & vec);

   /// undo ⎕UCS() created by tf2_char_vec
   static UCS_string no_UCS(const UCS_string & ucs);

   /// try inverse ⎕TF2 of ucs, set \b new_var_or_fun if successful
   static UCS_string tf2_inverse(const UCS_string & ravel);

   /// return B in transfer format 2 (new APL format) for a variable
   static Token tf2_var(const UCS_string & var_name, const Value & val);

protected:
   /// append \b shape in tf2_format to \b ucs.
   static void tf2_shape(UCS_string & ucs, const Shape & shape,
                         ShapeItem nesting);

   /// append ravel \b cells in tf2_format to \b ucs.
   static void tf2_ravel(int level, UCS_string & ucs, const ShapeItem len,
                           const Cell * cells);

   /// append the ravel of a simple character array (of any rank)
   static void tf2_all_char_ravel(int level, UCS_string & ucs,
                                  const Value & value);

   /// return B in transfer format 1 (old APL format) for variable B
   static Value_P tf1(const UCS_string & var_name, Value_P B);

   /// return B in transfer format 1 (old APL format) for function B
   static Value_P tf1(const UCS_string & fun_name, const Function & B);

   /// return inverse transfer format 1 (old APL format) for a variable
   static Value_P tf1_inv(const UCS_string &  ravel);

   /// simplify tos by removing UCS nnn etc.
   static void tf2_reduce(Token_string & tos);

   /// replace pattern ⊂ B  in \b tos with the single token (⊂B);
   /// return true iff done so.
   static bool tf2_reduce_ENCLOSE(Token_string & tos);

   /// replace pattern A ⍴ B in \b tos with the single token (A⍴B);
   /// return true iff done so.
   static bool tf2_reduce_RHO(Token_string & tos);

   /// replace pattern N - ⎕IO - ⍳ K  in \b tos with N N+1 ... N+K-1;
   /// return true iff done so.
   static bool tf2_reduce_sequence(Token_string & tos);

   /// replace pattern N - M × ⎕IO - ⍳ K  in \b tos with M × (N N+1 ... N+K-1);
   /// return true iff done so.
   static bool tf2_reduce_sequence1(Token_string & tos);

   /// replace pattern ( B ) in tos with B; return true iff done so.
   static bool tf2_reduce_parentheses(Token_string & tos);

   /// replace pattern value1 value2... in tos with the single token
   //  (value1 value2...); return true iff done so.
   static bool tf2_glue(Token_string & tos);

   /// replace pattern , B in tos with single token (,B);
   /// return true iff done so.
   static bool tf2_reduce_COMMA(Token_string & tos);

   /// return B in transfer format 2 (new APL format) for a function
   static UCS_string tf2_fun(const UCS_string & fun_name, const Function & fun);

   /// replace pattern ⊂ ⊂ B in tos with the single token (⊂⊂B)
   static bool tf2_reduce_ENCLOSE_ENCLOSE(Token_string & tos);

   /// replace pattern ⊂ B in tos with the single token (⊂B)
   static bool tf2_reduce_ENCLOSE1(Token_string & tos);

   /// replace ⎕UCS n... with the corresponding Unicodes,
   static void tf2_reduce_UCS(Token_string & tos);

   /// in-place exchange of IntCells and CharCells
   static ShapeItem tf2_toggle_UCS(Value & val);
};
//----------------------------------------------------------------------------
#endif // __QUAD_TF_HH_DEFINED__
