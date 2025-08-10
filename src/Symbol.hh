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

#ifndef __SYMBOL_HH_DEFINED__
#define __SYMBOL_HH_DEFINED__

#include <stdint.h>

#include "ErrorCode.hh"
#include "Function.hh"
#include "NamedObject.hh"
#include "Parser.hh"
#include "SystemLimits.hh"
#include "Svar_DB.hh"

class IndexExpr;
class RavelIterator;
class UserFunction;

//----------------------------------------------------------------------------
/** One entry in the value stack of a symbol. The value stack
    is pushed/poped when the symbol is localized on entry/return of
    a user defined function.
 */
/// One entry in the value stack of a Symbol
class ValueStackItem
{
   friend class Symbol;

public:
   /// return the name class for \b this ValueStackItem
   NameClass get_NC() const
      { return name_class; }

   /// set the name class for \b this ValueStackItem
   void set_NC(NameClass nc)
      { name_class = nc; }

   /// set \b this ValueStackItem to APL value new_value
   void set_apl_value(Value_P new_value)
      {
        name_class = NC_VARIABLE;
        apl_val = new_value;
      }

   /// reset (clear) the APL value in \b this ValueStackItem
   void reset_apl_value()
      { apl_val.reset(); }

   /// return the line number of a label (caller must have checked NC_LABEL)
   const Function_Line get_label() const
      { return sym_val.label; }

   /// return the function (caller must have checked NC_FUNOPER).
   cFunction_P get_function() const
      { return sym_val.function; }

   /// set function to \b fun
   void set_function(cFunction_P fun)
      {
        name_class = fun->is_operator() ? NC_OPERATOR : NC_FUNCTION;
        sym_val.function = fun;
      }

   /// delete the function
   void clear_function()
      {
        sym_val.function = 0;
        name_class = NC_UNUSED_USER_NAME;
      }

   /// return the address of function (caller must have checked NC_FUNOPER).
   cFunction_P * get_function_P()
      { return & sym_val.function; }

   /// return the shared variable key (caller must have checked NC_SYSTEM_VAR).
   SV_key get_key() const
      { return sym_val.sv_key; }

   /// set the shared variable key
   void set_key(SV_key key)
      { name_class = NC_SYSTEM_VAR;   sym_val.sv_key = key; }

   /// return the APL value of \b this ValueStackItem, or 0 if it has none.
   /// Only used to iterate over the ValueStack in Doxy.cc and in Quad_RL to
   /// quickly access the current ⎕RL
   const Value * get_val_cptr() const
      { return apl_val.get(); }

   /// return the APL value  of \b this ValueStackItem, or 0 if it has none
   /// Only used in ⎕RL for quick in-place update of the current ⎕RL.
   Value * get_val_wptr()
      { return apl_val.get(); }

   /// flags of this ValueStackItem
   enum VS_flags
      {
        VSF_NONE = 0,      ///< no flags
        VSF_COW  = 0x01,   ///< Clone On Write
      };

   /// return the flags of \b this ValueStackItem
   VS_flags get_vs_flags() const
      { return flags; }

   /// set the flags of \b this ValueStackItem
   void set_vs_flags(VS_flags flg)
      { flags = flg; }

   /// make \b apl_val the sole owner of apl_val.value
   void isolate(const char * loc)
      { if (+apl_val)   apl_val.isolate(loc); }

protected:
   /// constructor: ValueStackItem for an unused symbol
   ValueStackItem()
   : name_class(NC_UNUSED_USER_NAME),
     flags(VSF_NONE)
      { memset(&sym_val, 0, sizeof(sym_val)); }

   /// constructor: ValueStackItem for a label (function line)
   ValueStackItem(Function_Line lab)
   : name_class(NC_LABEL),
     flags(VSF_NONE)
      { sym_val.label = lab; }

   /// constructor: ValueStackItem for a variable
   ValueStackItem(Value_P val)
   : apl_val(val),
     name_class(NC_VARIABLE),
     flags(VSF_NONE)
   {}

   /// constructor: ValueStackItem for a shared variable
   ValueStackItem(SV_key key)
   : name_class(NC_SYSTEM_VAR),
     flags(VSF_NONE)
      { sym_val.sv_key = key; }

   /// reset \b this ValueStackItem to being unused
   void clear()
      {
        // sym_val contains only POD variables, so it can be memset()
        memset(&sym_val, 0, sizeof(sym_val));
        if (!!apl_val)   apl_val.reset();
        name_class = NC_UNUSED_USER_NAME;
         flags = VSF_NONE;
      }

   /// the possible "values" of a symbol
   union _sym_val
      {
        cFunction_P    function;   ///< if \b Symbol is a function
        Function_Line label;       ///< if \b Symbol is a label
        SV_key        sv_key;      ///< if \b Symbol is a shared variable
      };

   /// the (current) value of this symbol (unless variable)
   _sym_val sym_val;

   /// the (current) value of this symbol (if variable)
   Value_P apl_val;

   /// the (current) name class (like ⎕NC, unless shared variable)
   NameClass name_class;

   /// flags of \b this ValueStackItem
   VS_flags flags;
};
//----------------------------------------------------------------------------
/// Base class for variables, defined functions, and distinguished names
class Symbol : public NamedObject
{
   friend class SymbolTable;

public:
   /// create a system symbol with Id \b id
   Symbol(Id id);

   /// create a symbol with name \b ucs
   Symbol(const UCS_string & ucs, Id id);

   /// destructor
   virtual ~Symbol()
      { clear_vs(); }

   /// List \b this \b Symbol ( for )VARS, )FNS )
   ostream & list(ostream & out) const;

   /// write \b this symbol in )OUT format to file \b out
   void write_OUT(FILE * out, uint64_t & seq) const;

   /// set \b token according to the current NC/sym_val of \b this \b Symbol.
   /// Decrement PC if the name class of \b token is either
   /// NC_UNUSED_USER_NAME or unexpected (supposedly to re-read the
   /// the resolved token).
   virtual void resolve_left(Token & token, Function_PC & PC) const;

   /// set \b token according to the current NC/sym_val of \b this \b Symbol.
   /// Decrement PC if the name class of \b token is either
   /// NC_UNUSED_USER_NAME or unexpected (supposedly to re-read the
   /// the resolved token).
   virtual void resolve_right(Token & token, Function_PC & PC) const;

   /// set \b token according to the current NC/sym_val of \b this shared var
   void resolve_shared_variable(Token & token) const;

   /// resolve a variable name for an assignment (left of ←)
   virtual Token resolve_lv(const char * loc);

   /// return the token class of \b this \b Symbol WITHOUT calling resolve()
   TokenClass resolve_class(bool left) const;

   /// set current NameClass of this Symbol to NC_UNUSED_USER_NAME and remove
   /// any values associated with this symbol
   virtual int expunge();

   /// Set current NameClass of this Symbol to \b nc
   void set_NC(NameClass nc);

   /// Set current NameClass of this Symbol to \b nc and function fun
   void set_NC(NameClass nc, cFunction_P fun);

   /// share variable with \b proc
   void share_var(SV_key key);

   /// unshare a shared variable
   SV_Coupling unshare_var();

   /// clear the value stack of \b this symbol
   void clear_vs();

   /// Compare name of \b this value with \b other
   int compare(const Symbol & other) const
       { return name.compare(other.name); }

   /// return true iff this variable is read-only
   /// (overloaded by RO_SystemVariable)
   virtual bool is_readonly() const   { return false; }

   /// Assign \b value to \b this \b Symbol
   virtual void assign(Value_P value, bool clone, const char * loc);

   /// Assign \b value to \b this \b Symbol (which is a shared variable)
   void assign_shared_variable(Value_P value, const char * loc);

   /// Indexed (multi-dimensional) assign \b value to \b this \b Symbol
   virtual void assign_indexed(const IndexExpr & index, Value_P value);

   /// Indexed (one-dimensional) assign \b value to \b this \b Symbol
   virtual void assign_indexed(const Value * X, Value_P value);

   /// assign lambda, eg. V←{ ... }
   virtual bool assign_named_lambda(cFunction_P lambda, const char * loc);

   /// Print \b this \b Symbol to \b out
   virtual ostream & print(ostream & out) const;

   /// Print \b this \b Symbol and its stack to \b out
   ostream & print_verbose(ostream & out) const;

   /// Pop latest entry from the stack of \b this \b Symbol
   virtual void pop();

   /// Push an undefined entry onto the stack of \b this \b Symbol
   virtual void push();

   /// push a label onto the stack of \b this \b Symbol
   virtual void push_label(Function_Line label);

   /// push a function onto the stack of \b this \b Symbol
   virtual void push_function(cFunction_P function);

   /// Push an APL value onto the stack of \b this \b Symbol
   virtual void push_value(Value_P value);

   /// return the depth (global == 0) of \b ufun on the stack. Use the largest
   /// depth if ufun is pushed multiple times
   int get_exec_ufun_depth(const UserFunction * ufun);

   /// return the current APL value (or throw a VALUE_ERROR)
   virtual Value_P get_apl_value() const;

   /// return the first Cell of this value without creating a value
   const Cell * get_first_cell() const;

   /// return true, iff this Symbol can be assigned
   bool can_be_assigned() const;

   /// return the current SV_key (or throw a VALUE_ERROR)
   SV_key get_SV_key() const;

   /// set the current SV_key
   void set_SV_key(SV_key key);

   /// return true, iff this Symbol is not used (i.e. erased)
   bool is_erased() const
      { return value_stack_size() == 0 ||
              (value_stack_size() == 1 &&
               value_stack[0].get_NC() == NC_UNUSED_USER_NAME); }

   /// helper function for Prefix::MM_is_FM(). \b this is the symbol left
   /// of an operator M. Return \b true for function-like name classes and
   /// \b false for name-like name classes.
   bool M_is_F() const;

   /// Return the current function (or throw a VALUE_ERROR)
   virtual const Function * get_function() const;

   /// Return the function at SI level si, or 0 if none.
   cFunction_P get_function(unsigned int si) const;

   /// The name of \b this \b Symbol
   virtual UCS_string get_name() const   { return name; }

   /// overloaded NamedObject::get_name_ptr()
   const UCS_string * get_name_ptr() const   { return &name; }

   /// overloaded NamedObject::get_function(), 0 for non-functions
   virtual cFunction_P get_function();

   /// get the value if a variable, 0 if none
   Value_P get_var_value();

   /// return a const pointer to the current APL value
   const Value * get_val_cptr() const
      { return value_stack.back().get_val_cptr(); }

   /// return a pointer to the current APL value
   Value * get_val_wptr()
      { return value_stack.back().get_val_wptr(); }

   /// return a reason why this symbol cant become a defined function
   const char * cant_be_defined() const;

   /// overloaded NamedObject::get_symbol()
   virtual Symbol * get_symbol() 
      { return this; }

   /// overloaded NamedObject::get_symbol()
   virtual const Symbol * get_symbol() const
      { return this; }

   /// store the attributes (as per ⎕AT) of symbol in Z...
   virtual void get_attributes(int mode, Value & Z) const;

   /// return the size of the value stack
   const int value_stack_size() const
      { return value_stack.size(); }

   /// return the top-most item on the value stack
   const ValueStackItem * top_of_stack() const
      { return value_stack.size() ? &value_stack.back() : 0; }

   /// return the top-most item on the value stack
   ValueStackItem * top_of_stack()
      { return value_stack.size() ? &value_stack.back() : 0; }

   /// return the idx'th item on stack (higher index = newer item)
   const ValueStackItem & operator [](int idx) const
      { return value_stack[idx]; }

   /// return the idx'th item on stack (higher index = newer item)
   ValueStackItem & operator [](int idx)
      { return value_stack[idx]; }

   /// set a callback function for symbol events
   void set_monitor_callback(void (* callback)(const Symbol &, Symbol_Event))
      { monitor_callback = callback; }

   /// clear the marked flag of all entries
   void unmark_all_values() const;

   /// print variables owning value
   int show_owners(ostream & out, const Value & value) const;

   /// perform a vector assignment (like (A B C)←1 2 3) for variables in
   /// \b symbols with values \b values
   static void vector_assignment(std::basic_string<Symbol *> & symbols,
                                 Value_P values);

   /// dump this symbol to out
   void dump(ostream & out) const;

   /// call the monitor callback function (if any) with event \b ev
   void call_monitor_callback(Symbol_Event ev)
      { if (monitor_callback)   monitor_callback(*this, ev); }

   /// Compare the name of \b this \b Symbol with \b ucs
   bool equal(const UCS_string & ucs) const
      { return (name.compare(ucs) == COMP_EQ); }

   /// return the level of fun on the stack of \b this Symbol) on the SI stack
   int get_SI_level(cFunction_P fun) const;

   /// return the SI stack level of val on the stack of \b this Symbol)
   int get_SI_level(const Value & val) const;

   /// The next Symbol with the same hash value as \b this \b Symbol
   Symbol * next;

protected:
   /// the name of \b this \b Symbol
   UCS_string name;

   /// called on symbol events (if non-0)
   void (*monitor_callback)(const Symbol &, Symbol_Event sev);

   /// the value stack of \b this \b Symbol
   std::vector<ValueStackItem> value_stack;
};

inline void
Hswap(const Symbol * & u1, const Symbol * & u2)
{ const Symbol * tmp = u1;   u1 = u2;   u2 = tmp; }

//----------------------------------------------------------------------------
/// lambda result λ
class LAMBDA : public Symbol
{
public:
   /// constructor
   LAMBDA()
   : Symbol(ID_LAMBDA)
   {}

   /// overloaded Symbol::assign(), suppressing assignment if not localized
   virtual void assign(Value_P value, bool clone, const char * loc)
      { if (value_stack_size() > 1)   Symbol::assign(value, clone, loc); }

   /// destroy variable (don't)
   void destroy_var() {}
};
//----------------------------------------------------------------------------
/// lambda variable ⍺
class ALPHA : public Symbol
{
public:
   /// constructor
   ALPHA()
   : Symbol(ID_ALPHA)
   {}

   /// overloaded Symbol::assign(), suppressing assignment if not localized
   virtual void assign(Value_P value, bool clone, const char * loc)
      { if (value_stack_size() > 1)   Symbol::assign(value, clone, loc); }

   /// destroy variable (don't)
   void destroy_var() {}
};
//----------------------------------------------------------------------------
/// lambda variable ⍶
class ALPHA_U : public Symbol
{
public:
   /// constructor
   ALPHA_U()
   : Symbol(ID_ALPHA_U)
   {}

   /// overloaded Symbol::assign(), suppressing assignment if not localized
   virtual void assign(Value_P value, bool clone, const char * loc)
      { if (value_stack_size() > 1)   Symbol::assign(value, clone, loc); }

   /// destroy variable (don't)
   void destroy_var() {}
};
//----------------------------------------------------------------------------
/// lambda variable χ
class CHI : public Symbol
{
public:
   /// constructor
   CHI()
   : Symbol(ID_CHI)
   {}

   /// overloaded Symbol::assign(), suppressing assignment if not localized
   virtual void assign(Value_P value, bool clone, const char * loc)
      { if (value_stack_size() > 1)   Symbol::assign(value, clone, loc); }

   /// destroy variable (don't)
   void destroy_var() {}
};
//----------------------------------------------------------------------------
/// lambda variable ⍵
class OMEGA : public Symbol
{
public:
   /// constructor
   OMEGA()
   : Symbol(ID_OMEGA)
   {}

   /// overloaded Symbol::assign(), suppressing assignment if not localized
   virtual void assign(Value_P value, bool clone, const char * loc)
      { if (value_stack_size() > 1)   Symbol::assign(value, clone, loc); }

   /// destroy variable (don't)
   void destroy_var() {}
};
//----------------------------------------------------------------------------
/// lambda variable ⍹
class OMEGA_U : public Symbol
{
public:
   /// constructor
   OMEGA_U()
   : Symbol(ID_OMEGA_U)
   {}

   /// overloaded Symbol::assign(), suppressing assignment if not localized
   virtual void assign(Value_P value, bool clone, const char * loc)
      { if (value_stack_size() > 1)   Symbol::assign(value, clone, loc); }

   /// destroy variable (don't)
   void destroy_var() {}
};
//----------------------------------------------------------------------------

#endif // __SYMBOL_HH_DEFINED__
