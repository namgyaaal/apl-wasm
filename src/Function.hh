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

#ifndef __FUNCTION_HH_DEFINED__
#define __FUNCTION_HH_DEFINED__

#include <sys/types.h>

#include "Error.hh"
#include "NamedObject.hh"
#include "Parser.hh"
#include "PrintBuffer.hh"
#include "Value.hh"

class Workspace;
class UserFunction;
class RavelIterator;

/* Naming conventions:

   A: left argument
   L: left function
   R: right function
   X: axis
   B: right argument

   default implementation for axis function: ignore axis
*/
//----------------------------------------------------------------------------
/** The base class for all functions (user defined or system functions).  */
/// Base class for all APL functions and operators (system or defined)
class Function : public NamedObject
{
public:
   /// constructor for most functions
   Function(TokenTag _tag)
   : NamedObject(Id(_tag >> 16)),
     creation_time(0),
     tag(_tag)
   { parallel_thresholds[0] = parallel_thresholds[1] = -1; }

   /// constructor for functions whose Id does not match their tag
   Function(Id id, TokenTag _tag)
   : NamedObject(id),
     creation_time(0),
     tag(_tag)
   { parallel_thresholds[0] = parallel_thresholds[1] = -1; }

   /// destructor
   virtual ~Function()
   {}

   /// return \b true iff \b this is an operator.
   virtual bool is_operator() const   { return false; }

   /// return \b true iff \b this is a native function
   virtual bool is_native() const   { return false; }

   /// return \b true iff \b this function is a lambda
   virtual bool is_lambda() const   { return false; }

   /// return \b true iff \b this function is a macro
   virtual bool is_macro() const   { return false; }

   /// return \b true iff \b this function is defined (= has an APL body)
   virtual bool is_defined() const   { return false; }

   /// return \b true iff \b this function is a derived function
   virtual bool is_derived() const
      { return false; }

   /// a monadic Cell function that returns bool
   typedef bool (*bool_f1)(const Cell & B);

   /// a dyadic Cell function that returns bool
   typedef bool (*bool_f2)(const Cell & A, const Cell & B);

   /// a monadic function that returns packed bool for packed A
   typedef uint64_t (*bool_f1_bool)(uint64_t A);

   /// a dyadic function that returns packed bool for packed A and packed B
   typedef uint64_t (*bool_f2_bool)(uint64_t A, uint64_t B);

   /// return \b a bool_f1() (if any), otherwise 0.
   virtual bool_f1 get_bool_f1() const
      { return 0; }

   /// return \b a bool_f2() (if any), otherwise 0.
   virtual bool_f2 get_bool_f2() const
      { return 0; }

   /// return \b a bool_f1() (if any), otherwise 0.
   virtual bool_f1_bool get_bool_f1_bool() const
      { return 0; }

   /// return \b a bool_f2() (if any), otherwise 0.
   virtual bool_f2_bool get_bool_f2_bool() const
      { return 0; }

   /// return \b true iff \b this function returns a (possibly nested)
   /// boolean result and takes (only) boolean arguments.
   /// For example: ∧, ∨, ∼, ...
   virtual bool is_bool_bool() const
      { return 0; }

   /// return \b true if \b eval_XXX may push the SI. True for ⍎, user defined
   /// functions, and operators derived from user defined functions
   virtual bool may_push_SI() const   { return false; }

   /// return the number of value arguments (0, 1, or 2) of a user defined
   /// function, For non-user defined functions, throw DOMAIN_ERROR.
   virtual int get_fun_valence() const
      {
        const TokenClass tc = TokenClass(tag & TC_MASK);
        if (tc == TC_FUN2)   return 2;
        if (tc == TC_FUN1)   return 1;
        return 0;
      }

   /// return the number of function arguments (1 or 2) of an operator,
   /// or 0 for a "normal" function that isn't an operator.
   virtual int get_oper_valence() const   { return 0; }

   /// return 1 if the function returns a result, otherwise 0.
   /// For non-user defined functions, throw DOMAIN_ERROR.
   virtual bool has_result() const = 0;

   /// return true iff the function has and axis (system defined functions
   /// always have an axis even thogh most of them ignore it).
   virtual bool has_axis() const    { return true; }

   /// overloaded NamedObject::get_function()
   virtual const Function * get_function() const   { return this; }

   /// the monadic inverse function of \b this function (if any)
   virtual cFunction_P get_monadic_inverse() const   { return 0; }

   /// the dyadic inverse function of \b this function (if any)
   virtual cFunction_P get_dyadic_inverse() const   { return 0; }

   /// GMT when this function was created; 0 for system functions
   APL_time_us get_creation_time() const
      {  return creation_time; }

   /// set the time when the function was created (0 for built-in functions)
   void set_creation_time(APL_time_us t)
      { creation_time = t; }

   /// return the execution properties (3 ⎕AT) for this function
   virtual const int * get_exec_properties() const
      { static int ep[] = { 1, 1, 1, 0 };   return ep; }

   /// print the properties of this function
   virtual void print_properties(ostream & out, int indent) const = 0;

   /// store the attributes (as per ⎕AT) of symbol in Z, ...
   virtual void get_attributes(int mode, Value & Z) const;

   /// return a pointer to \b this UserFunction (if it is one)
   virtual const UserFunction * get_func_ufun() const   { return 0; }

   /// return true if this function has a name with alphabetic chars,
   /// i.e. the function is either a defined function or a ⎕-function
   virtual bool has_alpha() const   { return false; }

   /// Print \b this function.
   virtual ostream & print(ostream & out) const = 0;

   /// return the dyadic scalar primitive if \b this function
   virtual prim_f2 get_scalar_f2() const
      { return 0; }

   /// an associative cell function
   typedef ErrorCode (Cell::*assoc_f2)(Cell *, const Cell *) const;

   /// if this function is an associative scalar function then return
   /// its associative cell function, otherwise 0.
   virtual assoc_f2 get_assoc() const
      { return 0; }

   /// return a \b Token for \b this function.
   Token get_token() const { return Token(tag, this); }

   /// return the \b Token Tag for \b this function.
   TokenTag get_tag() const { return tag; }

   /// plain function, 0 arguments
   virtual Token eval_() const;

   /// plain function, 2 arguments
   virtual Token eval_AB(Value_P A, Value_P B) const;

   /// monadic operator, 2 arguments
   virtual Token eval_ALB(Value_P A, Token & LO, Value_P B) const;

   /// plain function, 2 arguments, with axis
   virtual Token eval_AXB(Value_P A, Value_P X, Value_P B) const
      { AXIS_ERROR; }

   /// dyadic operator, 2 arguments
   virtual Token eval_ALRB(Value_P A, Token & LO, Token & RO, Value_P B) const;

   /// monadic operator, 2 arguments, with axis
   virtual Token eval_ALXB(Value_P A, Token & LO, Value_P X, Value_P B) const
      { AXIS_ERROR; }

   /// dyadic operator, 2 arguments, with axis
   virtual Token eval_ALRXB(Value_P A, Token & LO, Token & RO,
                            Value_P X, Value_P B) const
      { AXIS_ERROR; }

   /// plain function, 1 argument
   virtual Token eval_B(Value_P B) const;

   /// monadic operator, 1 argument
   virtual Token eval_LB(Token & LO, Value_P B) const;

   /// plain function, 1 arguments, with axis
   virtual Token eval_XB(Value_P X, Value_P B) const
      { AXIS_ERROR; }

   /// monadic operator, 1 arguments, with axis
   virtual Token eval_LXB(Token & LO, Value_P X, Value_P B) const
      { AXIS_ERROR; }

   /// dyadic operator, 1 arguments
   virtual Token eval_LRB(Token & LO, Token & RO, Value_P B) const;

   /// dyadic operator, 1 arguments, with axis
   virtual Token eval_LRXB(Token & LO, Token & RO, Value_P X, Value_P B) const
      { AXIS_ERROR; }

   /// Evaluate \b the fill function.
   virtual Token eval_fill_B(Value_P B) const
      { DOMAIN_ERROR; }

   /// Evaluate \b the fill function.
   virtual Token eval_fill_AB(Value_P A, Value_P B) const;

   /** Evaluate \b the identity function. B is empty and the result is a
   /// value f/B0 with (B0 f ↑B) ≡ B for all B (and f is \b this function).
   /// If such a B0 does not exist then raise a DOMAIN ERROR.

       NOTE that eval_identity_fun() returns f/B0 and not B0 !!!
    **/
   virtual Token eval_identity_fun(Value_P B, sAxis axis) const;

   /// Delete this function (do nothing, overloaded by UserFunction).
   virtual void destroy() {}

   /// Quad_CR of this function.
   virtual UCS_string canonical(bool with_lines) const
      { NeverReach("Function::canonical() called"); }

   /// return axis (non-0 only for derived functions)
   virtual Value_P * locate_X() const
      { return 0; }

   /// return true if this function has (named) sub-functions, i.e.
   /// this function is either ⎕FIO or ⎕CR.
   virtual bool has_subfuns() const
      { return false; }

   /// return a (pseudo-) axis number for subfunction \b name
   virtual sAxis subfun_to_axis(const UCS_string & name) const
      { return -1; }

   /// return the signature of this function (currently only valid
   /// for user-defined functions)
   Fun_signature get_signature() const;

   /// return break-even point for monadic parallel execution
   ShapeItem get_monadic_threshold() const
      { return parallel_thresholds[0]; }

   /// set the break-even point for monadic parallel execution
   void set_monadic_threshold(ShapeItem new_threshold)
      { parallel_thresholds[0] = new_threshold; }

   /// return break-even point for dyadic parallel execution
   ShapeItem get_dyadic_threshold() const
      { return parallel_thresholds[1]; }

   /// set the break-even point for dyadic parallel execution
   void set_dyadic_threshold(ShapeItem new_threshold)
      { parallel_thresholds[1] = new_threshold; }

   /// when this function was created (0.0 for system functions)
   APL_time_us creation_time;

protected:
   /// the tag for token pointing to \b this function
   TokenTag tag;

   /// the thresholds for parallel execution
   ShapeItem parallel_thresholds[2];
};
//----------------------------------------------------------------------------

#endif // __FUNCTION_HH_DEFINED__
