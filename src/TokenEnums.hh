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

#ifndef __TOKENENUMS_HH_DEFINED__
#define __TOKENENUMS_HH_DEFINED__

#include "IdEnums.hh"
/**
    The class tag of a token. Token classes < TC_MAX_PERM are permanent
    (e.g. used in user defined functions) while higher classes are
    temporary (used while parsing),
 **/
enum TokenClass
{
   // token classes.
   //

   // permanent token classes. Only Token of these classes can appear in
   // the body of a defined function.
   //
   TC_ASSIGN        = 0x01,   ///< ←
   TC_R_ARROW       = 0x02,   ///< →N
   TC_L_BRACK       = 0x03,   ///< [ or ;
   TC_R_BRACK       = 0x04,   ///< ]
   TC_END           = 0x05,   ///< left end of statement
   TC_FUN0          = 0x06,   ///< niladic function
   TC_FUN12         = 0x07,   ///< ambivalent function
   TC_INDEX         = 0x08,   ///< [...]
   TC_OPER1         = 0x09,   ///< monadic operator
   TC_OPER2         = 0x0A,   ///< dyadic operator
   TC_L_PARENT      = 0x0B,   ///< (
   TC_R_PARENT      = 0x0C,   ///< )
   TC_RETURN        = 0x0D,   ///< return from defined function
   TC_SYMBOL        = 0x0E,   ///< user defined name
   TC_VALUE         = 0x0F,   ///< APL value

   TC_MAX_PERM,               ///< permanent token are < TC_MAX_PERM

   // token class aliases. We sometimes want to use TC_FUN1 or TC_FUN2
   // for clarification, but both are TC_FUN12.
   //
   TC_FUN1          = TC_FUN12,   ///< monadic function
   TC_FUN2          = TC_FUN12,   ///< dyadic function

   // temporary Token classes. Token of these classes only appear as
   // intermediate results during tokenization and prefix parsing
   //
   TC_PINDEX        = 0x10,   ///< partial index
   TC_VOID          = 0x11,
   TC_MAX_PHRASE,             ///< token in phrases are < TC_MAX_PHRASE
   TC_MAX_PHRASE_2 = TC_MAX_PHRASE*TC_MAX_PHRASE,     // TC_MAX_PHRASE ^ 2
   TC_MAX_PHRASE_3 = TC_MAX_PHRASE*TC_MAX_PHRASE_2,   // TC_MAX_PHRASE ^ 3
   TC_MAX_PHRASE_4 = TC_MAX_PHRASE*TC_MAX_PHRASE_3,   // TC_MAX_PHRASE ^ 4

   TC_OFF           = 0x12,
   TC_SI_LEAVE      = 0x13,
   TC_LINE          = 0x14,
   TC_DIAMOND       = 0x15,   // ◊
   TC_NUMERIC       = 0x16,   // 0-9, ¯
   TC_SPACE         = 0x17,   // space, tab, CR (but not LF)
   TC_NEWLINE       = 0x18,   // LF
   TC_COLON         = 0x19,   // :
   TC_QUOTE         = 0x1A,   // ' or "
   TC_L_CURLY       = 0x1B,   // {
   TC_R_CURLY       = 0x1C,   // }

   TC_MASK          = 0xFF,
   TC_INVALID       = 0xFF,

   // shorter token class aliases for the phrase table
   //
   SN_A             = TC_VALUE,
   SN_ASS           = TC_ASSIGN,
   SN_B             = TC_VALUE,
   SN_C             = TC_INDEX,
   SN_D             = TC_OPER2,
   SN_END           = TC_END,
   SN_F             = TC_FUN12,
   SN_G             = TC_FUN12,
   SN_GOTO          = TC_R_ARROW,
   SN_I             = TC_PINDEX,
   SN_LBRA          = TC_L_BRACK,
   SN_LPAR          = TC_L_PARENT,
   SN_M             = TC_OPER1,
   SN_N             = TC_FUN0,
   SN_RETC          = TC_RETURN,
   SN_RBRA          = TC_R_BRACK,
   SN_RPAR          = TC_R_PARENT,
   SN_V             = TC_SYMBOL,
   SN_VOID          = TC_VOID,
   SN_              = TC_INVALID
};

   /// return true if \b tcl is a niladic, monadic or dyadic function
   inline bool is_function_class(TokenClass tcl)
      {
        enum { BITS = 1 << TC_FUN0 | 1 << TC_FUN1 | 1 << TC_FUN2 };
        return BITS & 1 << tcl;
      }

   /// return true if \b tcl is a monadic or dyadic operator
   inline bool is_operator_class(TokenClass tcl)
      {
        enum { BITS = 1 << TC_OPER1 | 1 << TC_OPER2 };
        return BITS & 1 << tcl;
      }

   inline bool is_function_or_operator_class(TokenClass tcl)
      {
        enum { BITS = 1 << TC_FUN0  | 1 << TC_FUN1 | 1 << TC_FUN2
                    | 1 << TC_OPER1 | 1 << TC_OPER2 };
        return BITS & 1 << tcl;
      }

   /// return if tcl is a token class that needs a right argument
   inline bool needs_B(TokenClass tcl)
      {
        enum { BITS = 1 << TC_FUN1  | 1 << TC_FUN2
                    | 1 << TC_OPER1 | 1 << TC_OPER2 };
        return BITS & 1 << tcl;
      }

   /// binding strengths between token classes
enum Binding_Strength
{
   BS_ANY     =  0,
   BS_ASS_B   = 10,   ///< ← ANY   : ← to what is on its right
   BS_F_B     = 20,   ///< F ANY   : function to its right argument
   BS_A_F     = 30,   ///< ANY F   : function to its left argument
   BS_LO_OP   = 40,   ///< ANY OP  : operator to its left function
   BS_VAL_VAL = 50,   ///< VEC VEC : vector to vector
   BS_OP_RO   = 60,   ///< OP ANY  : (dyadic) operator to its right function
   BS_V_ASS   = 70,   ///< ANY ←   : ← to what is on its left
   BS_ANY_BRA = 80    ///< ANY []  : [] to what is on its left
};

   /// token class groups (class bitmaps)
enum TokenClassGroup
{
   /// function with arguments
   TCG_FUN12_OPER12 = 1 << TC_FUN12
                    | 1 << TC_OPER1
                    | 1 << TC_OPER2,

   ///  something that has (or may produce) a value
   TCG_MAY_GLUE     = 1 << TC_SYMBOL   // variable
                    | 1 << TC_FUN0     // niladic function
                    | 1 << TC_VALUE,   // value

   /// ) or ]
   TCG_R_PAR_BRA    = 1 << TC_R_PARENT
                    | 1 << TC_R_BRACK,

   /// classes X for which FUN in "X FUN B" can only be monadic.
   TCG_NO_A         = 1 << TC_ASSIGN            //  ← FUN B
                    | 1 << TC_R_ARROW           //  → FUN B
                    | 1 << TC_L_BRACK           //  [ FUN B
                    | 1 << TC_END               //  ◊ FUN B
                    | 1 << TC_FUN12             //  + FUN B
                    | 1 << TC_OPER1             // f/ FUN B
                    | 1 << TC_L_PARENT,         // ( FUN B
};
   
/// The value type of a token

enum TokenValueType
{
   // token value types. The token value type defines the type of the
   // token in the union 'value'.
   //                              Type              union member
   TV_MASK          = 0xFF00,
   TV_NONE          = 0x0000,   // value not used
   TV_CHAR          = 0x0100,   // Unicode            .char_val
   TV_INT           = 0x0200,   // uint64_t           .int_val;
   TV_FLT           = 0x0300,   // APL_Float          .flt_val;
   TV_CPX           = 0x0400,   // cdouble            .complex_val;
   TV_SYM           = 0x0500,   // Symbol *           .sym_ptr;
   TV_LIN           = 0x0600,   // Function_Line      .fun_line;
   TV_VAL           = 0x0700,   // Value_P            .apl_val;
   TV_INDEX         = 0x0800,   // IndexExpr *        .index_val;
   TV_FUN           = 0x0900,   // Function_P         .function;
};

/**
     A token tag. It is comprised of 3 fields: a 16 bit Id, an 8 bit
     TokenValueType, and an 8 bit Token class.

     Bit:  31............................16 15.............8 7..............0
          ├────────────────────────────────┼────────────────┼────────────────┤
          │               Id               │ TokenValueType │   TokenClass   │
          └────────────────────────────────┴────────────────┴────────────────┘
 **/
enum TokenTag
{

#define TD(tok, tc, tv, id) tok = tc | tv | (id << 16),
#include "Token.def"

   TOK_FUN1 = TOK_FUN2,
   TOK_NONE = -1,         // invalid tag
};

/// exchange TokenTags \b t1 and \b t2
inline void
swap(TokenTag & tag1, TokenTag & tag2)
{ const TokenTag tmp = tag1;
   tag1 = tag2;
   tag2 = tmp;
}

/// return true for / ⌿ \ and ⍀
inline bool
is_SLASH_or_BACKSLASH(TokenTag tag)
{
   return tag == TOK_OPER1_REDUCE  || tag == TOK_OPER1_SCAN ||
          tag == TOK_OPER1_REDUCE1 || tag == TOK_OPER1_SCAN1;
}

#endif // __TOKENENUMS_HH_DEFINED__
