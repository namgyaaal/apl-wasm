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

#ifndef __SYSTEM_VARIABLE_HH_DEFINED__
#define __SYSTEM_VARIABLE_HH_DEFINED__

#include "Id.hh"
#include "Parallel.hh"
class RavelIterator;
#include "Symbol.hh"

#include <sys/time.h>

//----------------------------------------------------------------------------
/**
    Base class for all system variables (Quad variables).
 */
///  Base class for all APL system variables
class SystemVariable : public Symbol
{
public:
   /// Construct a \b SystemVariable with \b Id \b id
   SystemVariable(Id id)
   : Symbol(id)
   {}

   /// return the TokenTag for this system variable
   Token get_token()
      { return Token(ID::get_token_tag(get_Id()), this); }

   /// overloaded Symbol::print().
   virtual ostream & print(ostream & out) const;

   /// overloaded Symbol::assign().
   virtual void assign(Value_P B, bool clone, const char * loc);

   /// overloaded Symbol::assign_indexed().
   virtual void assign_indexed(const Value * X, Value_P B);

   /// overloaded Symbol::get_attributes().
   virtual void get_attributes(int mode, Value & Z) const;

   /// system vars cannot be expunged.
   virtual int expunge() { return 0; }

   /// do not allow (...⎕xxx)←...
   virtual Token resolve_lv(const char * loc)
      { SYNTAX_ERROR; }
};
//----------------------------------------------------------------------------
/**
    A system variable that cannot be localized (push and pop have no effect).
 */
/// Base class for all APL system variables that cannot be localized
class NL_SystemVariable : public SystemVariable
{
public:
public:
   /// Constructor.
   NL_SystemVariable(Id id)
   : SystemVariable(id)
   {}
protected:
   /// overloaded Symbol::push()
   virtual void push() {}

   /// overloaded Symbol::push_label()
   virtual void push_label(Function_Line label) {}

   /// overloaded Symbol::push_function()
   virtual void push_function(cFunction_P function) {}

   /// overloaded Symbol::push_value()
   virtual void push_value(Value_P B) {}

   /// overloaded Symbol::pop()
   virtual void pop() { }
};
//----------------------------------------------------------------------------
/**
    A read-only system variable (push, pop, and assign are ignored).
 */
/// Base class for all read-only APL system variables
class RO_SystemVariable : public NL_SystemVariable
{
public:
   /// Constructor.
   RO_SystemVariable(Id id)
   : NL_SystemVariable(id)
   {}

protected:
   /// overloaded Symbol::assign()
   virtual void assign(Value_P B, bool clone, const char * loc) {}

   /// overloaded Symbol::assign_indexed()
   virtual void assign_indexed(const Value * X, Value_P B) {}

   /// overloaded Symbol::is_readonly()
   virtual bool is_readonly() const   { return true; }

   /// overloaded Symbol:resolve_left(). Invalid for read-only variables
   virtual void resolve_left(Token & token, Function_PC & PC) const
      { MORE_ERROR() << "Assignment to read-only system variable "
                     << get_name();
        SYNTAX_ERROR; }

   /// overloaded Symbol::resolve_right(). Since this variable is read-only,
   /// push(), pop(), and assign() do nothing, and therefore we may call
   /// get_apl_value() directly.
   virtual void resolve_right(Token & token, Function_PC & PC) const
      { new (&token) Token(TOK_APL_VALUE1, get_apl_value()); }
};
//----------------------------------------------------------------------------
/**
   System variable Quad-AI (Account Information)
 */
/// The class implementing ⎕AI
class Quad_AI : public RO_SystemVariable
{
public:
   /// Constructor.
   Quad_AI();

   /// increase the time waiting for user input
   void add_wait(APL_time_us diff)
   { user_wait += diff; }

protected:
   /// when the interpreter was started
   const APL_time_us session_start;

   /// time waiting for user input.
   APL_time_us user_wait;

   /// overloaded Symbol::get_apl_value().
   virtual Value_P get_apl_value() const;
};
//----------------------------------------------------------------------------
/**
   System variable Quad-ARG (command line arguments of the interpreter)
 */
/// The class implementing ⎕ARG
class Quad_ARG : public RO_SystemVariable
{
public:
   /// Constructor.
   Quad_ARG();

protected:
   /// overloaded Symbol::get_apl_value()
   virtual Value_P get_apl_value() const;
};
//----------------------------------------------------------------------------
/**
   System variable Quad-AV (Atomic Vector)
 */
/// The class implementing ⎕AV
class Quad_AV : public RO_SystemVariable
{
public:
   /// Constructor.
   Quad_AV();

   /// return ⎕AV[pos - ⎕IO]
   static Unicode indexed_at(uint32_t pos);

   /// a static ⎕AV
   static Unicode qav[Avec::MAX_AV];
};
//----------------------------------------------------------------------------
/**
   System variable Quad-CT (Comparison Tolerance)
 */
/// The class implementing ⎕CT
class Quad_CT : public SystemVariable
{
public:
   /// constructor
   Quad_CT();

   /// return the current comparison tolerance. MUST NOT CALL get_apl_value()
   /// because it can be called from parallel Cell functions
   APL_Float current() const
      { return get_first_cell()->get_real_value(); }

protected:
   /// overloaded Symbol::assign()
   virtual void assign(Value_P B, bool clone, const char * loc);

   // overloaded Symbol::push()
   virtual void push()
      {
        Symbol::push();
        Symbol::assign(FloatScalar(DEFAULT_Quad_CT, LOC), false, LOC);
      }
};
//----------------------------------------------------------------------------
/**
   System variable Quad-EM (Event Message)
 */
/// The class implementing ⎕EM
class Quad_EM : public RO_SystemVariable
{
public:
   /// Constructor.
   Quad_EM()
   : RO_SystemVariable(ID_Quad_EM)
   {}

protected:
   /// overloaded Symbol::get_apl_value()
   virtual Value_P get_apl_value() const;
};
//----------------------------------------------------------------------------
/**
   System variable Quad-ET (Event Type).
 */
/// The class implementing ⎕ET
class Quad_ET : public RO_SystemVariable
{
public:
   /// Constructor.
   Quad_ET() : RO_SystemVariable(ID_Quad_ET)
   {}

protected:
   /// overloaded Symbol::get_apl_value().
   virtual Value_P get_apl_value() const;
};
//----------------------------------------------------------------------------
/**
   System variable Quad-FC (Format Control).
 */
/// The class implementing ⎕FC
class Quad_FC : public SystemVariable
{
public:
   /// Constructor.
   Quad_FC();

   /// return the current format control (used by Workspace::get_FC())
   const UCS_string current() const
      { return UCS_string(*get_apl_value()); }

   // overloaded Symbol::push()
   virtual void push();

protected:
   /// overloaded Symbol::assign().
   virtual void assign(Value_P B, bool clone, const char * loc);

   /// overloaded Symbol::assign_indexed().
   virtual void assign_indexed(const Value * X, Value_P B);

   /// overloaded Symbol::assign_indexed().
   virtual void assign_indexed(const IndexExpr & IX, Value_P B);
};
//----------------------------------------------------------------------------
/**
   System variable Quad-IO (Index Origin).
 */
/// The class implementing ⎕IO
class Quad_IO : public SystemVariable
{
public:
   /// Constructor.
   Quad_IO();

   /// Return the current index origin. MUST NOT CALL get_apl_value()
   /// because it can be called from parallel Cell functions
   APL_Integer current() const
      { return get_first_cell()->get_int_value(); }

protected:
   /// overloaded Symbol::assign().
   virtual void assign(Value_P B, bool clone, const char * loc);

   // overloaded Symbol::push()
   virtual void push()
      {
        Symbol::push();
        Symbol::assign(IntScalar(1, LOC), false, LOC);
      }
};
//----------------------------------------------------------------------------
/**
   System variable Quad-L (Left Argument).
 */
/// The class implementing ⎕L
class Quad_L : public NL_SystemVariable
{
public:
   /// Constructor.
   Quad_L();

   /// overloaded Symbol::save()
   virtual void save(ostream & out) const {}

protected:
   /// overloaded Symbol::assign()
   virtual void assign(Value_P B, bool clone, const char * loc);

   /// overloaded Symbol::get_apl_value().
   virtual Value_P get_apl_value() const;
};
//----------------------------------------------------------------------------
/**
   System variable Quad-LC (Line Counter).
 */
/// The class implementing ⎕LC
class Quad_LC : public RO_SystemVariable
{
public:
   /// Constructor.
   Quad_LC();

protected:
   /// overloaded Symbol::get_apl_value().
   virtual Value_P get_apl_value() const;
};
//----------------------------------------------------------------------------
/**
   System variable Quad-LX (Latent Expression).
 */
/// The class implementing ⎕LX
class Quad_LX : public NL_SystemVariable
{
public:
   /// Constructor.
   Quad_LX();

protected:
   /// overloaded Symbol::assign().
   virtual void assign(Value_P B, bool clone, const char * loc);

   /// overloaded Symbol::assign_indexed()
   virtual void assign_indexed(const Value * X, Value_P B) {}
};
//----------------------------------------------------------------------------
/**
   System variable Quad-PP (Printing Precision).
 */
/// The class implementing ⎕PP
class Quad_PP : public SystemVariable
{
public:
   /// Constructor.
   Quad_PP();

   /// Return the current print precision. MUST NOT CALL get_apl_value()
   /// because it can be called from parallel Cell functions
   APL_Integer current() const
      { return get_first_cell()->get_int_value(); }

protected:
   /// overloaded Symbol::assign().
   virtual void assign(Value_P B, bool clone, const char * loc);

   // overloaded Symbol::push()
   virtual void push()
      {
        Symbol::push();
        Symbol::assign(IntScalar(DEFAULT_Quad_PP, LOC), false, LOC);
      }
};
//----------------------------------------------------------------------------
/**
   System variable Quad-PR (Prompt Replacement).
 */
/// The class implementing ⎕PR
class Quad_PR : public SystemVariable
{
public:
   /// Constructor.
   Quad_PR();

   /// Return the current prompt replacement, aka. ⎕PR.
   const UCS_string current() const
      { return  UCS_string(*get_apl_value()); }

protected:
   /// overloaded Symbol::assign().
   virtual void assign(Value_P B, bool clone, const char * loc);

   // overloaded Symbol::push()
   virtual void push()
      {
        Symbol::push();
        Symbol::assign(CharScalar(UNI_SPACE, LOC), false, LOC);
      }
};
//----------------------------------------------------------------------------
/**
   System variable Quad-PS (Print Style). This variable controls the formatting
   of APL values (classical APL or DISPLAY style).
 */
/// The class implementing ⎕PS
class Quad_PS : public SystemVariable
{
public:
   /// Constructor.
   Quad_PS();

   /// Return the current print style. MUST NOT CALL get_apl_value()
   /// because it can be called from parallel Cell functions
   PrintStyle current() const
      { switch (style)
           {
             case 0: return PR_APL;
             case 1: return PR_APL_FUN;
             case 2: return PR_BOXED_CHAR;
             case 3: return PR_BOXED_GRAPHIC;
             default: return PST_NONE;
           }
      }

   /// return the quotient print style
   bool get_print_quotients() const
      { return print_quotients; }

protected:
   /// overloaded Symbol::assign().
   virtual void assign(Value_P B, bool clone, const char * loc);

   /// overloaded Symbol::assign_indexed()
   virtual void assign_indexed(const Value * X, Value_P B);

   /// overloaded Symbol::push()
   virtual void push()
      {
        Symbol::push();
        Symbol::assign(IntScalar(0, LOC), false, LOC);
      }

   /// true if quotients shall be printed as A÷B
   bool print_quotients;

   /// the current style
   int style;
};
//----------------------------------------------------------------------------
/**
   System variable Quad-PW (Print Width).
 */
/// The class implementing ⎕PW
class Quad_PW : public SystemVariable
{
public:
   /// Constructor.
   Quad_PW();

   /// return the current ⎕PW. MUST NOT CALL get_apl_value()
   /// because it can be called from parallel Cell functions
   APL_Integer current() const
      { return get_first_cell()->get_int_value(); }

   /// overloaded Symbol::assign().
   virtual void assign(Value_P B, bool clone, const char * loc);

protected:
   // overloaded Symbol::push()
   virtual void push()
      {
        Symbol::push();
        Symbol::assign(IntScalar(DEFAULT_Quad_PW, LOC), false, LOC);
      }
};
//----------------------------------------------------------------------------
/**
   System variable Quad-Quad (Evaluated Input/Output aka. ⎕).
 */
/// The class implementing ⎕
class Quad_Quad : public SystemVariable
{
public:
   /// Constructor.
   Quad_Quad();

   /// overloaded Symbol::resolve_left().
   virtual void resolve_left(Token & token, Function_PC & PC) const   { }   // ⎕←xxx

   /// overloaded Symbol::resolve_right().
   virtual void resolve_right(Token & token, Function_PC & PC) const;

protected:
   virtual void assign(Value_P B, bool clone, const char * loc);

   // should never be called due to overloaded resolve()
   virtual Value_P get_apl_value() const
      { Assert(0);   /* not reached */ return Value_P(); }
};
//----------------------------------------------------------------------------
/**
   System variable Quote-Quad (Character Input/Output aka. ⍞).
 */
/// The class that implements ⍞
class Quad_QUOTE : public SystemVariable
{
public:
   /// Constructor.
   Quad_QUOTE();

   /// end of ⍞ (output, but no input: clear prompt)
   static void done(bool with_LF, const char * loc);

protected:
   /// overloaded Symbol::assign().
   virtual void assign(Value_P B, bool clone, const char * loc);

   /// overloaded Symbol::get_apl_value().
   virtual Value_P get_apl_value() const;

   /// last line of output
   static UCS_string buffer;
};
//----------------------------------------------------------------------------
/**
   System variable Quad-R (Right Argment).
 */
/// The class implementing ⎕R
class Quad_R : public NL_SystemVariable
{
public:
   /// Constructor.
   Quad_R();

   /// overloaded Symbol::save()
   virtual void save(ostream & out) const {}

protected:
   /// overloaded Symbol::assign()
   virtual void assign(Value_P B, bool clone, const char * loc);

   /// overloaded Symbol::get_apl_value().
   virtual Value_P get_apl_value() const;
};
//----------------------------------------------------------------------------
/**
   System variable Quad-SYL (System Limits).
 */
/// The class implementing ⎕SYL
class Quad_SYL : public NL_SystemVariable
{
public:
   /// Constructor.
   Quad_SYL() : NL_SystemVariable(ID_Quad_SYL)
      { assign(Value_P(), false, LOC); }

   /// overloaded Symbol::assign()
   virtual void assign(Value_P B, bool clone, const char * loc);

   /// overloaded Symbol::assign_indexed()
   virtual void assign_indexed(const Value * X, Value_P B);

   /// overloaded Symbol::assign_indexed()
   virtual void assign_indexed(const IndexExpr & IDX, Value_P B);

   virtual Value_P get_apl_value() const;

   /// maximum depth of SI stack
   static ShapeItem si_depth_limit;

   /// maximum number of values
   static ShapeItem value_count_limit;

   /// maximum number of ravel bytes
   static ShapeItem ravel_count_limit;

   /// maximum number of ravel bytes in APL printout
   static ShapeItem print_length_limit;

   /// the system limits
   enum SYL_INDEX
      {
#define syl3(n, e, v) syl1(n, e, v)
#define syl2(n, e, v) syl1(n, e, v)
#define syl1(_n, e, _v) SYL_ ## e, ///< dito
#include "SystemLimits.def"

        SYL_MAX                      ///< max. system limit value (excluding)
      };

protected:
};
//----------------------------------------------------------------------------
/**
   System variable Quad-TC (Terminal Control Characters)
 */
/// The class implementing ⎕TC
class Quad_TC : public RO_SystemVariable
{
public:
   /// Constructor.
   Quad_TC();
};
//----------------------------------------------------------------------------
/**
   System variable Quad-TS (Time Stamp).
 */
/// The class implementing ⎕TS
class Quad_TS : public RO_SystemVariable
{
public:
   /// Constructor.
   Quad_TS();

   /// overloaded Symbol::get_apl_value().
   virtual Value_P get_apl_value() const;
};
//----------------------------------------------------------------------------
/**
   System variable Quad-TZ (Time Zone).
 */
/// The class implementing ⎕TZ
class Quad_TZ : public SystemVariable
{
public:
   /// Constructor.
   Quad_TZ();

   /// return the offset from GMT of the current timezone (in seconds).
   /// we make this int64_t rather than int so that conversion to usec
   /// does not overflow
   int64_t get_offset() const   { return offset_seconds; }

   /// set the timezone offset
   void set_offset(int offset)
      { offset_seconds = offset; }

   /// compute the offset (in seconds) from GMT
   static int compute_offset();

   /// print e.g. 2017-05-17  15:09:12 (GMT+2) to out (no trailing endl)
   ostream & print_timestamp(ostream & out, APL_time_us when) const;

protected:
   /// overloaded Symbol::assign().
   virtual void assign(Value_P B, bool clone, const char * loc);

   // overloaded Symbol::push()
   virtual void push()
      {
        Symbol::push();
        offset_seconds = compute_offset();
        if (offset_seconds % 3600 == 0)   // full hour
           Symbol::assign(IntScalar(offset_seconds/3600, LOC), false, LOC);
        else
           Symbol::assign(FloatScalar(offset_seconds/3600, LOC), false, LOC);
      }

   virtual void pop()
      {
        Symbol::pop();
        offset_seconds = 3600 * get_apl_value()->get_cscalar().get_int_value();
      }

   /// the offset from GMT of the current timezone (in seconds)
   int offset_seconds;
};
//----------------------------------------------------------------------------
/**
   System variable Quad-UL (User Load).
 */
/// The class implementing ⎕UL
class Quad_UL : public RO_SystemVariable
{
public:
   /// Constructor.
   Quad_UL();

protected:
   /// overloaded Symbol::get_apl_value().
   virtual Value_P get_apl_value() const;
};
//----------------------------------------------------------------------------
/**
   System variable Quad-X (Axis Argument).
 */
/// The class implementing ⎕X
class Quad_X : public NL_SystemVariable
{
public:
   /// Constructor.
   Quad_X();

   /// overloaded Symbol::save()
   virtual void save(ostream & out) const {}

protected:
   /// overloaded Symbol::assign()
   virtual void assign(Value_P B, bool clone, const char * loc);

   /// overloaded Symbol::get_apl_value().
   virtual Value_P get_apl_value() const;
};
//----------------------------------------------------------------------------
#endif // __SYSTEM_VARIABLE_HH_DEFINED__
