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

#ifndef __PREFIX_HH_DEFINED__
#define __PREFIX_HH_DEFINED__

#include "Common.hh"
#include "PrintOperator.hh"
#include "Token.hh"

class Prefix;
class StateIndicator;
class Token_string;
struct ReduceArg;

/// the max. number of token in one reduction
enum { MAX_REDUCTION_LEN = 6 };

//----------------------------------------------------------------------------
/// how to continue after return from a reduce_XXX() function
enum R_action
{
   /** repeat phrase matching with current stack. Called after the current
       stack was changed and phrase matching should be repeated without
       fetching a new token. This is the "normal" case after a phrase has
       reduced the stack. **/
   RA_CONTINUE = 0,

   /// push next token and repeat phrase matching with current stack
   RA_PUSH_NEXT = 1,   // aka. SHIFT

   /// return from parser with result arg[0]
   RA_RETURN = 2,

   /// return from parser and continue in pushed SI
   RA_SI_PUSHED = 3,

   /// internal error: action was not set by reduce_XXX() function
   RA_FIXME  = 4,
};
//----------------------------------------------------------------------------
/// a class for reducing all statements of an Executable
class Prefix
{
   friend class XML_Loading_Archive;
   friend class XML_Saving_Archive;

public:
   /// constructor
   Prefix(StateIndicator & _si, const Token_string & _body);

   /// destructor
   void clean_up();

   /// max. number of lookahead token
   enum { MAX_CONTENT   = 10*MAX_REDUCTION_LEN,
          MAX_CONTENT_1 = MAX_CONTENT - 1 };

   /// return true if ufun is on the stack
   bool uses_function(const UserFunction * ufun) const;

   /// return true if the content of \b this Prefix has ⎕R (and maybe ⎕L or ⎕X).
   bool has_quad_LRX() const;

   /// print the state of this parser
   void print(ostream & out, int indent) const;

   /// throw an E_LEFT_SYNTAX_ERROR or an E_SYNTAX_ERROR
   void syntax_error(const char * loc);

   /// clear the mark flag of all values in \b this Prefix
   void unmark_all_values() const;

   /// print all owners of \b value
   int show_owners(const char * prfx, ostream & out, const Value & value) const;

   /// highest PC in current statement
   Function_PC get_range_high() const
      { return put ? content[put- 1].get_PC() : get_lookahead_PC(); }

   /// lowest PC in current statement
   Function_PC get_range_low() const
      { return put ? content[0].get_PC() : get_lookahead_PC(); }

   /// lookahead is a complete index. return \b true if it belongs to a value
   /// (as opposed to a function axis).
   bool value_expected() const;

   /// print the current range (PC from - PC to) on out
   void print_range(ostream & out) const;

   /// return the left argument of a failed primitive function (if any)
   Value_P * locate_L(UCS_string & function) const;

   /// return the right argument of a failed primitive function (if any)
   Value_P * locate_R(UCS_string & function) const;

   /// return the axis argument of a failed primitive function (if any)
   Value_P * locate_X(UCS_string & function) const;

   /// return the current monadic function (if any)
   const Function * get_dyadic_fun() const
      { return at1().get_ValueType() == TV_FUN ? at1().get_function() : 0; }

   /// return the current dyadic function (if any)
   const Function * get_monadic_fun() const
      { return at0().get_ValueType() == TV_FUN ? at0().get_function() : 0; }

   /// execute one context (user defined function or operator, execute,
   /// or immediate execution)
   Token reduce_statements();

   /// return the number of token currently in the FIFO
   int size() const
      { return put; }

   /// return the leftmost token (in APL order, e.g. A in A←B or in A+B)
   const Token & at0() const
      { Assert1(put);   return content[put - 1].get_token(); }

   /// return the leftmost token (in APL order, e.g. A in A←B or in A+B)
   Token & at0()
      { Assert1(put);   return content[put - 1].get_token(); }

   /// return the second token from the left (e.g. ← in A←B)
   const Token & at1() const
      { Assert1(put >= 2);   return content[put - 2].get_token(); }

   /// return the second token from the left (e.g. ← in A←B)
   Token & at1()
      { Assert1(put >= 2);   return content[put - 2].get_token(); }

   /// return the third token from the left (e.g. B in A←B)
   Token & at2()
      { Assert1(put >= 3);   return content[put - 3].get_token(); }

   /// return the fourth token from the left (e.g. B in A+/B)
   Token & at3()
      { Assert1(put >= 4);   return content[put - 4].get_token(); }

   /// return true if the next token binds stronger than the best match
   bool do_shift(TokenClass next) const;

   /// return the current assignment state
   Assign_state get_assign_state() const
      { return assign_state; }

   /// set the current assignment state
   void set_assign_state(Assign_state new_state)
      { assign_state = new_state; }

   /* return the highest PC seen in the current statement. This includes the
      lookahead token, therefore it could be (a little) > get_range_high().
      More importantly, it can be Function_PC_invalid.
    */
   Function_PC get_lookahead_PC() const
      { return saved_MISC.get_PC(); }

   /// set the lookahead PC
   void set_lookahead_PC(Function_PC pc)
      { saved_MISC.set_PC(pc); }

   /// read one more token (don't store yet)
   Token_loc lookahead()
      {
        if (PC >= body.size())   return Token_loc(Token(), PC);  // end of fun
        const Function_PC old_PC = PC++;
        return Token_loc(body[old_PC], old_PC);
      }

   /// store one more token. push(A) in e.g. F B would produce A F B.
   /// IOW, push() works right-to-left of XXX in reduce_XXX().
   void push(const Token_loc & tl)
      {
        if (put >= MAX_CONTENT_1)   LIMIT_ERROR_PREFIX;
        content[put++].copy(tl, LOC);
      }

   /// if \b tok is an error token (from some eval_XXX() function) then
   /// push its Token_loc and return \b true. Otherwise return \b false.
   bool push_error(const Token & tok)
      {
        if (tok.get_tag() != TOK_ERROR)   return false;   // no error pushed
        const Token_loc tl(tok, get_range_low());
        push(tl);
        set_action(RA_RETURN);   // return from context;
        return true;             // error pushed
      }

   /// pop \b prefix_len items
   void pop_args()
        {
          Assert1(put >= prefix_len);
          put -= prefix_len;
        }

   /// pop \b prefix_len items and push \b result.
   /* For example:

      Before:   A + B    prefix_len=3, put→A,
                └─PC─┐
      After:         Z   prefix_len=1, put→Z, PC(Z_ = PC(A)
    */
   void pop_args_push_result(const Token & result)
        {
          Assert1(put >= prefix_len);
          const int ZB = put - prefix_len;             // positions of B and Z
          content[ZB].get_token().copy(result, LOC);   // replace B with Z
          content[ZB].set_PC(at(0).get_PC());          // PC(Z) = PC(A)
          put -= prefix_len - 1;                       // discard A and +
        }

   /// remove the leftmost token (e,g, A in A←B) from the stack and return it.
   /// In e.g. reduce_A_F_B_(), the first pop() would remove A.
   /// IOW, pop() works left-to-right of XXX in reduce_XXX().
   Token_loc & pop()
      {  Assert1(put);   return content[--put]; }

   /// discard the leftmost token (e,g, A in A←B) from the stack
   void pop_and_discard()
      {  Assert1(put);   --put; }

   /// return TOK_LSYMB2 tokens ahead (excluding the leftmost one
   /// already read into \b content). The result \b symbols is empty
   /// for selective specifications and non-empty for vector specifications.
   void collect_symbols(basic_string<Symbol *> & symbols);

   /// clear the saved_MISC token
   void clear_MISC(const char * loc)
      {
        saved_MISC.set_PC(Function_PC_invalid);
        saved_MISC.get_token().clear(loc);
      }

   /// reset statement to empty state (e.g. after →N)
   void reset(const char * loc)
      { clean_up();   put = 0;   assign_state = ASS_none;
        clear_MISC(loc);
        prefix_len = 0;
      }

   /// print the current stack
   void print_stack(ostream & out, const char * loc) const;

   /// jump to new PC
   void goto_PC(Function_PC new_pc)
      { reset(LOC);   PC = new_pc; }

   /// return the current PC
   Function_PC get_PC() const
      { return PC; }

   /// set the prefix parser action
   void set_action(R_action ra)
      {
        action = ra;
      }

   /// set the prefix parser action according to (result-) Token type.
   // Called (typically after some eval_XXX()) if the return class can not
   // be predicted
   // token classes.
   void set_action(const Token & result)
      {
        switch(result.get_Class())
           {
             case TC_VALUE:
             case TC_VOID:
             case TC_END:
             case TC_FUN2:
                  set_action(RA_CONTINUE);
                  return;

             case TC_RETURN:
                  // result was one of TOK_RETURN_EXEC, TOK_RETURN_STATS,
                  // TOK_RETURN_VOID, or TOK_RETURN_SYMBOL.
                  // The current context is complete and may or may not
                  // have produced a value.
                  //
                  set_action(RA_RETURN);
                  return;

             case TC_SI_LEAVE:
                  //
                  // result was TOK_SI_PUSHED or TOK_ERROR
                  if (result.get_tag() == TOK_ERROR)   set_action(RA_RETURN);
                  else                                 set_action(RA_SI_PUSHED);
                  return;

             default: CERR << "CLASS = " << result.get_Class()
                           << " at " << LOC << endl;
                      FIXME;
           }
      }

   /// read and resolve the token class left of [ ... ], PC is at ']'
   bool is_value_bracket() const;

   /// reeturn true if \b this prefix has a valid lookahead token
   int has_MISC() const
      { return saved_MISC.get_token().get_tag() == TOK_VOID ? 0 : 1; }

   /// read and resolve the token class left of )
   bool is_value_parenthesis(int pc) const;

   /// return the leftmost (top-of-stack) Token_loc (at put position)
   Token_loc & tos()
       { Assert1(put);   return content[put - 1]; }

   /// return the idx'th Token_loc (from put position)
   Token_loc & at(int idx)
       { Assert1(idx < put);   return content[put - idx - 1]; }

   /// return the TokenClass of \b at(idx)
   const TokenClass Class_at(int idx) const
       { Assert1(idx < put);
         return content[put - idx - 1].get_token().get_Class(); }
 
   /// return the idx'th Token_loc (from put position)
   const Token_loc & at(int idx) const
       { Assert1(idx < put);   return content[put - idx - 1]; }

   /// one phrase in the phrase table
   struct Phrase
      {
        const char *   phrase_name;     ///< phrase name
        const char *   reduce_name;     ///< reduce function name
        void (Prefix::*reduce_fun)();   ///< reduce function
        unsigned int   phrase_hash;     ///< phrase hash
        int            prio;            ///< phrase priority
        int            misc;            ///< 1 if MISC phrase
        int            phrase_len;      ///< phrase length
        uint8_t        sub_nodes[TC_MAX_PHRASE+1];   ///< parent nodes
      };

   /// adjust the right caret after a SYNTAX_ERROR
   static void adjust_right_caret(Function_PC2 & range,
                                  const Token_string & failed_statement);

protected:
   /// push the next token onto the stack. Return \b true iff )SI was pushed.
   //  called often but from the same place in reduce_statements()
   inline bool push_next_token();

   /// find a phrase that matches the current stack. Return \b true iff found.
   //  called often but from the same place in reduce_statements()
   inline void find_best_phrase();

   /// return true iff the next token binds stronger (so we need to shift).
   inline bool check_next_binding();

   /// check if ^C or attention was raised (and throw if so)
   void check_interrupt_or_attention(bool end_of_line);

   /// perform a branch within a function. 
   void branch_within_function(bool end_of_line)
      {
        check_interrupt_or_attention(end_of_line);
        set_action(RA_PUSH_NEXT);
      }

   /// construct the )MORE info for a SYNTAX_ERROR.
   //  called rarely, so not inlined
   void push_END_error();

   /// push a symbol token (of class TC_SYMBOL). Return true iff )SI was pushed.
   inline bool push_Symbol(Token_loc & tl);

   /// return true if the left (back-)slash in M M means F M.
   inline bool MM_is_FM(Function_PC PC);

   /// a unique identifier
   const uint64_t instance;

   // member declarations of all 'void reduce_XXX()' functions...
   //
#define P_(_name, _suffix, _idx, _prio, _misc, _len)
#include "Prefix.def"

   /// the StateIndicator that contains this parser
   StateIndicator & si;

   /// put pointer (for the next token at PC). Since content is a stack,
   /// its put position is also its size.
   int put;

   /** the lookahead tokens (tokens that were shifted but not yet reduced).
       \b content is in body order, that is, content[0] is the oldest 
       (= rightmost in APL order) token and content[put - 1] is the latest.
    **/
   Token_loc content[MAX_CONTENT];

   /// the X token (leftmost token in MISC phrase, if any)
   Token_loc saved_MISC;

   /// the function body. \b this parser parses tokens body[pc_from] ...
   const Token_string & body;

   /// the current pc (+1)
   Function_PC PC;

   /// assignment state
   Assign_state assign_state;

   /// length of matched prefix (before calling a reduce_XXX() function)
   int prefix_len;

   /// the action to be taken after returning from a reduce_XXX() function
   R_action action;

   /// the best (longest) phrase that matches the current stack,
   /// or 0 if no phrase matches (so the parser will SHIFT)
   const Phrase * best_phrase;

   /// a generator for unique identifiers
   static uint64_t instance_counter;
};
//----------------------------------------------------------------------------

#endif // __PREFIX_HH_DEFINED__
