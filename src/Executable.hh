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

#ifndef __EXECUTABLE_HH_DEFINED__
#define __EXECUTABLE_HH_DEFINED__

#include "Avec.hh"
#include "Token.hh"
#include "UCS_string_vector.hh"

class Error;
class UCS_string;
class UserFunction;

//----------------------------------------------------------------------------
/**
     A sequence of APL token. An executable is created for one of 3 purposes:
     - an APL expression for execute (⍎), or
     - the statements of one line in immediate execution, or
     - all lines of a user defined function.
 **/
/// Base class for ExecuteList, StatementList, and UserFunction
class Executable
{
public:
   /// constructor
   Executable(const UCS_string & ucs, bool multi_line,
              ParseMode pm, const char * loc);

   /// constructor for lambdas
   Executable(Fun_signature sig, int lambda_num, 
              const UCS_string & lambda_text, const char * loc);

   /// destructor: release values held by the body
   virtual ~Executable();

   /// the parse mode of \b this Executable
   ParseMode get_parse_mode() const
      { return pmode; }

   /// delete values in body and (maybe) remove lambdas
   void clear_body();

   /// return the name of this function, or ◊ or ⍎
   virtual UCS_string get_name() const = 0;

   /// return true iff this Executable cannot be suspended
   virtual bool cannot_suspend() const
      { return false; }

   /// return the result symbol iff this is a user defined function or
   /// operator that returns a value
   virtual Symbol * get_sym_Z() const
      { return 0; }

   /// return a UserFunction * (if \b this is one) or else 0.
   virtual const UserFunction * get_exec_ufun() const
      { return 0; }

   /// return true if this Executable localizes Symbol \b sym
   virtual bool pushes_sym(const Symbol * sym) const
      { return false; }

   /// return a UserFunction * (if \b this is one) or else 0.
   virtual UserFunction * get_exec_ufun()
   { return 0; }

   /// get the line number for pc
   virtual Function_Line get_line(Function_PC pc) const
      { return Function_Line_0; }

   /// print this user defined executable to \b out
   virtual ostream & print(ostream & out) const
      { print_token(out, false);   return out; }

   /// print a body range (in APL order)
   ostream & print_range(ostream & out, Function_PC2 from_to) const;

   /// print this user defined executable to \b out
   void print_token(ostream & out, int details) const;

   /// print the text of this user defined executable to \b out
   void print_text(ostream & out) const;

   /// execute the body of this executable
   Token execute_body() const;

   /// return the body of this executable
   const Token_string & get_body() const
      { return body; }

   /// return number of function lines (including header line 0)
   const int get_text_size() const 
      { return text.size(); }

   /// return function line n (0 = header line)
   const UCS_string & get_text(int l) const
      { return text[l]; }

   /// clear the text of line n (0 = header line)
   void clear_text(int l)
      { text[l].clear(); }

   /// set the text of line n (0 = header line)
   void set_text(int l, const UCS_string & new_line)
      { text[l] = new_line; }

   /// return the text of statement at \b pc
   UCS_string statement_text(Function_PC pc) const;

   /// return the first PC of the \b line
   virtual Function_PC line_start(Function_Line line) const
      { return Function_PC_0; }

   /// remove all TOK_VOID token from the body.
   /// Return the number of tokens removed
   virtual VoidCount remove_TOK_VOID();

   /// compute error lines 2 and 3 in \b error
   void set_error_info(Error & error, Function_PC2 range) const;

   /// helper for set_error_info(Error & error, Function_PC2 range).
   void set_error_info(Error & error, const Token_string & failed_statement,
                       Function_PC2 range) const;

   /// restore the original (un-optimized) tokens for a failed statement
   void reparse(Token_string & original, Function_PC low_PC) const;

   /// return the end of the statement (excluding) to which pc belongs
   Function_PC get_statement_end(Function_PC pc) const;

   /// return the start of the statement to which pc belongs
   Function_PC get_statement_start(Function_PC pc) const;

   /// clear marked flag in all body token
   void unmark_all_values() const;

   /// print all owners of \b value
   int show_owners(const char * prefix, ostream & out,
                   const Value & value) const;

   /// say where this SI entry was allocated
   const char * get_loc() const
      { return alloc_loc; }

   /// increment the reference counter
   void increment_refcount(const char * loc);

   /// decrement the reference counter
   void decrement_refcount(const char * loc);

protected:
   /// the body positions of the ←← and ←→ tokens of a congitional
   struct conditional
      {
        Function_PC if_THEN;   ///< position of the ←← token (end of condition)
        Function_PC if_ELSE;   ///< position of the ←→ token (before else)
      };


   /// return true if the body contains { ... }. The parser has already
   /// complained if the curly brackets do not match, therefore we only check
   /// for } (which comes  last in APL, but first in the (reversed) body.
   inline bool body_has_curly() const
      {
        loop(b, body.size())
           {
             if (body[b].get_tag() == TOK_R_CURLY)   return true;
           }
        return false;
      }

   /// recursively extract all lambda expressions from body and store
   /// them in lambdas
   void setup_lambdas();

   /// reverse the token order in each statement of tos (statement by
   /// statement reversal). The order pf statements is not changed).
  static void reverse_each_statement(Token_string & tos);

   /// reverse the token order of the entire Token_string (i.e. not statement
   /// by statement)
  static void reverse_all_token(Token_string & tos);

   /// extract one lambda expressions from body and store it in lambdas
   /// body[b] is { and body[bend] is }.
   ShapeItem setup_one_lambda(ShapeItem b, ShapeItem nend, int lambda_num);

   /// body[b ... bend] is a lambda. Move these token from this body to the body
   /// of the lambda and clear them in \b this body.
   Fun_signature compute_lambda_body(Token_string & rev_lambda_body,
                                     ShapeItem b, ShapeItem bend);

   /// extract the skip'th { ... } from the function text
   UCS_string extract_lambda_text(Fun_signature signature, int skip) const;

   /// where this SI entry was allocated
   const char * alloc_loc;

   /// parse the body line number \b line of \b this function
   ErrorCode parse_body_line(Function_Line line, const UCS_string & ucs,
                             bool trace, bool tolerant, const char * loc,
                             bool macro);

   /// parse the body line number \b line of \b this function
   ErrorCode parse_body_line(Function_Line line, const Token_string & tos,
                             bool trace, bool tolerant, const char * loc);

   /// compute the targets for if/else aka →→ ←→ and ←←.
   /// Maybe set MORE() info and return true on error.
   bool compute_if_else_targets();

   /// the mode \b this Executable
   const ParseMode pmode;

   /// the program text from which \b body was created
   UCS_string_vector text;

   /// The token to be executed. They are organized line by line and
   /// statement by statement, but the token within a statement reversed
   /// due to the right-to-left execution of APL.
   Token_string body;

   /// reference counter (for lambdas)
   int refcount;
};
//----------------------------------------------------------------------------
/**
   The token of an execute expression (⍎'...')
 **/
/// The token of an execute expression ⍎'...'
class ExecuteList : public Executable
{
   friend class XML_Loading_Archive;

public:
   /// compute body token from text \b data
   static ExecuteList * fix(const UCS_string & data, const char * loc);

   /// constructor
   ExecuteList(const UCS_string & txt, const char * loc)
   : Executable(txt, false, PM_EXECUTE, loc)
   {}

protected:
   /// overloaded Executable::get_name()
   virtual UCS_string get_name() const
      { return UCS_string(UNI_EXECUTE); }
};
//----------------------------------------------------------------------------
/**
   The token of an statement list (cmd ◊ cmd ... ◊ cmd)
 **/
/// The token of an statement list cmd ◊ cmd ... ◊ cmd
class StatementList : public Executable
{
   friend class XML_Loading_Archive;

public:
   /// compute body token from text \b data
   static StatementList * fix(const UCS_string & data, const char * loc);

protected:
   /// constructor
   StatementList(const UCS_string txt, const char * loc)
   : Executable(txt, false, PM_STATEMENT_LIST, loc)
   {}

   /// overloaded Executable::get_name()
   virtual UCS_string get_name() const
      { return UCS_string(UNI_DIAMOND); }
};
//----------------------------------------------------------------------------

#endif // __EXECUTABLE_HH_DEFINED__
