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

#ifndef __STATE_INDICATOR_HH_DEFINED__
#define __STATE_INDICATOR_HH_DEFINED__

#include "Common.hh"
#include "DerivedFunction.hh"
#include "Executable.hh"
#include "Error.hh"
#include "Function.hh"
#include "Parser.hh"
#include "Prefix.hh"
#include "PrintOperator.hh"

//----------------------------------------------------------------------------
/**
    One entry of the state indicator (SI) of the APL interpreter.
    Compared to e.g.  C++, the state indicator is (one element of) the
    function call stack of the interpreter.
 */
/// One entry of the state indicator (SI) of the APL interpreter
class StateIndicator
{
   friend class XML_Loading_Archive;
   friend class XML_Saving_Archive;

public:
   /// constructor
   StateIndicator(const Executable * exec, StateIndicator * _par);

   /// destructor
   ~StateIndicator();

   /// continue this StateIndicator (to line N after →N back into it)
   void goon(Function_Line N, const char * loc);

   /// retry this StateIndicator (after →'')
   void retry(const char * loc);

   /// return true iff
   ///  (1) this SI entry is executing \b funname, or
   ///  (2) has resolved \b funname on its prefix parser stack
   bool uses_function(const UserFunction * ufun) const;

   /// Return the function name, or "*" for an immediate execution context
   UCS_string function_name() const;

   /// list the stack entry (for commands ]SI, )SI, and )SIS)
   void list(ostream & out, SI_mode mode) const;

   /// list the stack entry (for command ]SI)
   void print(ostream & out) const;

   /// print spaces according to level
   ostream & indent(ostream & out) const;

   /// return pointer to the current user function, statements, or execute
   const Executable * get_executable() const
      { return executable; }

   /// return the name of the parse mode
   Unicode get_parse_mode_name() const;

   /// return the current PC
   Function_PC get_PC() const
      { return current_stack.get_PC(); }

   /// return the mode of this entry
   ParseMode get_parse_mode() const
      { return executable->get_parse_mode(); }

   /// evaluate a →B statement. Update PC if needed, maybe do nothing (→'')
   Token jump(const Value * B);

   /// do a jump to function line \b line
   Token jump_to_line(Function_Line line);

   /// return the nesting level (oldest SI has level 0, next has level 1, ...)
   SI_level get_level() const   { return level; };

   /// return the current line number
   Function_Line get_line() const;

   /// Maybe print B (according to tag) and erase B
   void statement_result(Token & result, bool trace);

   /// Escape from \b user function (exit from each invocation until
   /// immediate execution is reached)
   void escape();

   /// execute token in body...
   Token run();

   /// clear the marked bit in the executable, in the fun_oper_cache, and
   /// in all parsers
   void unmark_all_values() const;

   /// print all owners of \b value
   int show_owners(ostream & out, const Value & value) const;

   /// print a short debug info
   void info(ostream & out, const char * loc) const;

   /// return the error related info in this context
   static Error & get_error(StateIndicator * si)
       { return si ? si->error : top_level_error; }

   /// return the error related info in this context
   static const Error & get_error(const StateIndicator * si)
       { return si ? si->error : top_level_error; }

   /// return left arg (and set \b function)
   Value_P get_L(UCS_string & function) const;

   /// change the left argument (if any) of a failed primitive
   void set_L(Value_P value);

   /// return the right arg (and set \b function)
   Value_P get_R(UCS_string & function) const;

   /// change right argument of a failed primitive
   void set_R(Value_P value);

   /// return axis arg (and set \b function)
   Value_P get_X(UCS_string & function) const;

   /// change the axis left argument (if any) of a failed primitive
   void set_X(Value_P value);

   /// return true if \b this )SI entry has entered safe execution mode.
   /// That is, \b this )SI entry has initiated  ⎕EC)
   bool is_safe_execution_start() const
      { if (!parent)   return safe_execution_count > 0;
        return safe_execution_count > parent->safe_execution_count;
      }

   /// return the number of pending ⎕ECs (or other safe execution contexts)
   int get_safe_execution_count() const
      { return safe_execution_count; }

   /// set safe_execution mode
   void set_safe_execution_count()
      {
        if (parent)  safe_execution_count = parent->safe_execution_count + 1;
        else         safe_execution_count = 1;
      }

   /// clear safe_execution mode
   void clear_safe_execution()
      {
        if (parent)  safe_execution_count = parent->safe_execution_count;
        else         safe_execution_count = 0;
      }

   /// get the current prefix parser
   Prefix & get_prefix()
      { return current_stack; }

   /// get the current prefix parser
   const Prefix & get_prefix() const
      { return current_stack; }

   /// return the SI entry that has called \b this one
   StateIndicator * get_parent() const
      { return parent; }

   /// return the immediate child SI (if any) of \b this )SI entry
   const StateIndicator * find_child() const;

   /// return the level at which sym is pushed for the nth. time
   SI_level nth_push(const Symbol * sym, int from_tos) const;

   /// a small storage for DerivedFunction objects.
   DerivedFunctionCache fun_oper_cache;

   /// error when )SI is empty
   static Error top_level_error;

protected:
   /// the user function that is being executed
   const Executable * executable;

   /** track ⎕EC calls. The first )SI entry that calls ⎕ES sets
       \b safe_execution_count to 1 and every child )SI increments it.
       Therefore:

       1. safe_execution_count != 0 tells if ⎕ES is in effect, and the
       2. the parent with safe_execution_count == 1 is the one that has
          initiated ⎕ES (and to which the )SI stack shall be popped on error.
    */
   int safe_execution_count;

   /// The nesting level (of sub-executions)
   const SI_level level;

   /// details of the last error in this context.
   Error error;

   /// the current-stack of this context.
   Prefix current_stack;

   /// the StateIndicator that has called this one
   StateIndicator * const parent;
};
//----------------------------------------------------------------------------

#endif // __STATE_INDICATOR_HH_DEFINED__
