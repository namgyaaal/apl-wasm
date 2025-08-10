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

#ifndef __Quad_FUNCTION_HH_DEFINED__
#define __Quad_FUNCTION_HH_DEFINED__

#include "PrimitiveFunction.hh"

//----------------------------------------------------------------------------
/** The various Quad functions.  */
/// Base class for all system functions
class QuadFunction : public PrimitiveFunction
{
public:
   /// Constructor.
   QuadFunction(TokenTag tag) : PrimitiveFunction(tag) {}

   /// overloaded Function::has_alpha()
   virtual bool has_alpha() const   { return true; }

   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const
      { VALENCE_ERROR; }

   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { VALENCE_ERROR; }

   /// overloaded Function::has_result()
   virtual bool has_result() const   { return true; }
};
//----------------------------------------------------------------------------
/** The system function ⎕AF (Atomic Function) */
/// The class implementing ⎕AF
class Quad_AF : public QuadFunction
{
public:
   /// Constructor.
   Quad_AF() : QuadFunction(TOK_Quad_AF) {}

   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const;

   static Quad_AF  fun;          ///< Built-in function.

protected:
};
//----------------------------------------------------------------------------
/** The system function ⎕AT (Attributes) */
/// The class implementing ⎕AI
class Quad_AT : public QuadFunction
{
public:
   /// Constructor.
   Quad_AT() : QuadFunction(TOK_Quad_AT) {}

   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const;

   static Quad_AT  fun;          ///< Built-in function.

protected:
};
//----------------------------------------------------------------------------
/** The system function ⎕DL (Delay).  */
/// The class implementing ⎕DL
class Quad_DL : public QuadFunction
{
public:
   /// Constructor.
   Quad_DL() : QuadFunction(TOK_Quad_DL) {}

   static Quad_DL  fun;          ///< Built-in function.

protected:
   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const;
};
//----------------------------------------------------------------------------
/**
   The system function ⎕EA (Execute Alternate)
 */
/// The class implementing ⎕EA
class Quad_EA : public QuadFunction
{
public:
   /// Constructor.
   Quad_EA() : QuadFunction(TOK_Quad_EA) {}

   /// overladed Function::may_push_SI()
   virtual bool may_push_SI() const   { return true; }

   static Quad_EA  fun;          ///< Built-in function.

protected:
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const;
};
//----------------------------------------------------------------------------
/**
   The system function ⎕EB (Execute Both)
 */
/// The class implementing ⎕EB
class Quad_EB : public QuadFunction
{
public:
   /// Constructor.
   Quad_EB() : QuadFunction(TOK_Quad_EB) {}

   /// overladed Function::may_push_SI()
   virtual bool may_push_SI() const   { return true; }

   static Quad_EB  fun;          ///< Built-in function.

protected:
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const;
};
//----------------------------------------------------------------------------
/**
   The system function ⎕EC (Execute Controlled)
 */
/// The class implementing ⎕EC
class Quad_EC : public QuadFunction
{
public:
   /// Constructor.
   Quad_EC() : QuadFunction(TOK_Quad_EC) {}

   /// overladed Function::may_push_SI()
   virtual bool may_push_SI() const   { return true; }

   static Quad_EC  fun;          ///< Built-in function.

   /// end of context handler for ⎕EC
   static void eoc(Token & token);

protected:
   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const;

   /// overloaded Function::eval_fill_B().
   virtual Token eval_fill_B(Value_P B) const;
};
//----------------------------------------------------------------------------
/**
   The system function ⎕ENV (ENvironment Variables)
 */
/// The class implementing ⎕ENV
class Quad_ENV : public QuadFunction
{
public:
   /// Constructor.
   Quad_ENV() : QuadFunction(TOK_Quad_ENV) {}

   static Quad_ENV  fun;          ///< Built-in function.

protected:
   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const;
};
//----------------------------------------------------------------------------
/**
   The system function ⎕ES (Event Simulate).
 */
/// The class implementing ⎕ES
class Quad_ES : public QuadFunction
{
public:
   /// Constructor.
   Quad_ES() : QuadFunction(TOK_Quad_ES) {}

   static Quad_ES  fun;          ///< Built-in function.

protected:
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const;

   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const;

   /// common inplementation for eval_AB() and eval_B()
   static Token event_simulate(const UCS_string * A, Value_P B, Error & error);

   /// compute error code for B
   static ErrorCode get_error_code(Value_P B);
};
//----------------------------------------------------------------------------
/**
   The system function ⎕EX (Expunge).
 */
/// The class implementing ⎕EX
class Quad_EX : public QuadFunction
{
public:
   /// Constructor.
   Quad_EX() : QuadFunction(TOK_Quad_EX) {}

   static Quad_EX  fun;          ///< Built-in function.

   /// disassociate name from value, return 0 on failure or 1 on success.
   static int expunge(const UCS_string & name);

protected:
   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const;
};
//----------------------------------------------------------------------------
/**
   The system function ⎕INP (input from script, aka. HERE document)
 */
/// The class implementing ⎕INP
class Quad_INP : public QuadFunction
{
public:
   /// constructor.
   Quad_INP()
   : QuadFunction(TOK_Quad_INP)
     {
       Quad_INP_running = false;
     }

   static Quad_INP  fun;          ///< Built-in function.

protected:
   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const;

   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const;

   /// overloaded Function::eval_XB().
   virtual Token eval_XB(Value_P X, Value_P B) const;

   /// extract the esc1 and esc2 strings from \b A
   static void get_esc(Value_P A, UCS_string & esc1, UCS_string & esc2);

   /// read \b raw_lines from stdin or file, stop at end_marker
   static void read_strings();

   /// split \b raw_lines into \b prefixes, \b escapes, and \b suffixes
   static void split_strings();

   /// the end merker for APL escapes (dyadic ⎕INP only)
   /// the start merker for APL escapes (dyadic ⎕INP only)
   static UCS_string esc1;

   /// the end merker for APL escapes (dyadic ⎕INP only)
   static UCS_string esc2;

   /// the end merker for the entire ⎕INP input
   static UCS_string end_marker;

   /// the raw lines read from stdin
   static UCS_string_vector raw_lines;

   /// the line parts left of the escapes
   static UCS_string_vector prefixes;

   /// the line parts to exe executed
   static UCS_string_vector escapes;

   /// the line parts right of the escapes
   static UCS_string_vector suffixes;

   /// bool to prevent recursive ⎕INP calls
   static bool Quad_INP_running;
};
//----------------------------------------------------------------------------
/**
   The system function ⎕NA (Name Association).
 */
/// The class implementing ⎕NA
class Quad_NA : public QuadFunction
{
public:
   /// Constructor.
   Quad_NA() : QuadFunction(TOK_Quad_NA) {}

   static Quad_NA  fun;          ///< Built-in function.

protected:
   /// overloaded Function::eval_AB()
   virtual Token eval_AB(Value_P A, Value_P B) const
      { TODO; }

   /// overloaded Function::eval_B()
   virtual Token eval_B(Value_P B) const
      { TODO; }
};
//----------------------------------------------------------------------------
/**
   The system function ⎕NC (Name class).
 */
/// The class implementing ⎕NC
class Quad_NC : public QuadFunction
{
public:
   /// Constructor.
   Quad_NC() : QuadFunction(TOK_Quad_NC) {}

   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const;

   /// return the ⎕NC for variable name \b var
   static APL_Integer get_NC(const UCS_string var);

   static Quad_NC  fun;          ///< Built-in function.

protected:
};
//----------------------------------------------------------------------------
/**
   The system function ⎕NL (Name List).
 */
/// The class implementing ⎕NL
class Quad_NL : public QuadFunction
{
public:
   /// Constructor.
   Quad_NL() : QuadFunction(TOK_Quad_NL) {}

   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const
      { return do_quad_NL(Value_P(), B); }

   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const
      { return do_quad_NL(A, B); }

   static Quad_NL  fun;          ///< Built-in function.

protected:
   /// return A ⎕NL B
   static Token do_quad_NL(Value_P A, Value_P B);
};
//----------------------------------------------------------------------------
/**
   The system function ⎕SI (State Indicator)
 */
/// The class implementing ⎕SI
class Quad_SI : public QuadFunction
{
public:
   /// Constructor.
   Quad_SI() : QuadFunction(TOK_Quad_SI) {}

   /// overloaded Function::eval_AB().
   virtual Token eval_AB(Value_P A, Value_P B) const;

   /// overloaded Function::eval_AB().
   virtual Token eval_B(Value_P B) const;

   static Quad_SI  fun;          ///< Built-in function.

protected:
};
//----------------------------------------------------------------------------
/**
   The system function ⎕UCS (Universal Character Set)
 */
/// The class implementing ⎕UCS
class Quad_UCS : public QuadFunction
{
public:
   /// Constructor.
   Quad_UCS() : QuadFunction(TOK_Quad_UCS) {}

   /// overloaded Function::eval_B().
   virtual Token eval_B(Value_P B) const;

   static Quad_UCS  fun;          ///< Built-in function.

protected:
};
//----------------------------------------------------------------------------
/// Base class for ⎕STOP and ⎕TRACE
class Stop_Trace : public QuadFunction
{
protected:
   /// constructor
   Stop_Trace(TokenTag tag)
   : QuadFunction (tag)
   {}

   /// find UserFunction named \b fun_name
   static const UserFunction * locate_fun(const Value & fun_name);

   /// return integers in lines
   static Token reference(const std::basic_string<Function_Line> & lines,
                          bool assigned);

   /// return assign lines in new_value to stop or trace vector in ufun
   static void assign(UserFunction * ufun, const Value & new_value, bool stop);
};
//----------------------------------------------------------------------------
/// The class implementing ⎕STOP
class Quad_STOP : public Stop_Trace
{
public:
   /// Constructor
   Quad_STOP()
   : Stop_Trace(TOK_Quad_STOP)
   {}

   /// Overloaded Function::eval_AB()
   virtual Token eval_AB(Value_P A, Value_P B) const;

   /// Overloaded Function::eval_B()
   virtual Token eval_B(Value_P B) const;

   static Quad_STOP  fun;          ///< Built-in function.
};
//----------------------------------------------------------------------------
/// The class implementing ⎕TRACE
class Quad_TRACE : public Stop_Trace
{
public:
   /// Constructor
   Quad_TRACE()
   : Stop_Trace(TOK_Quad_TRACE)
   {}

   /// Overloaded Function::eval_AB()
   virtual Token eval_AB(Value_P A, Value_P B) const;

   /// Overloaded Function::eval_B()
   virtual Token eval_B(Value_P B) const;

   static Quad_TRACE  fun;          ///< Built-in function.
};
//----------------------------------------------------------------------------

#endif // __Quad_FUNCTION_HH_DEFINED__
