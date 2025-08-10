
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

#ifndef __Quad_CR_HH_DEFINED__
#define __Quad_CR_HH_DEFINED__

#include "QuadFunction.hh"

/**
   The system function ⎕CR (Character Representation).
 */
/// The class implementing ⎕CR
class Quad_CR : public QuadFunction
{
public:
   /// Constructor
   Quad_CR() : QuadFunction(TOK_Quad_CR) {}

   /// overloaded Function::eval_B()
   virtual Token eval_B(Value_P B) const;

   /// overloaded Function::eval_AB()
   virtual Token eval_AB(Value_P A, Value_P B) const;

   /// overloaded Function::eval_XB().
   /// ⎕CR[X] B  ←→  X ⎕CR B
   virtual Token eval_XB(Value_P X, Value_P B) const
      { return eval_AB(X, B); }

   /// overloaded Function::has_subfuns()
   virtual bool has_subfuns() const
      { return true; }

   /// overloaded Function::subfun_to_axis()
   virtual sAxis subfun_to_axis(const UCS_string & name) const;

   /// compute \b a ⎕CR \b B
   static Value_P do_CR(APL_Integer a, const Value * B, PrintContext pctx);

   /// compute a good default type and value for the top-level ⍴ of 10 ⎕CR.
   /// Return true for INT and false for CHAR.
   static bool figure_default(const Value * value, Unicode & default_char,
                              APL_Integer & default_int);

   /// compute the prolog (e,g, ((⎕IO+2)⊃X)←)  for the pick of \b left
   static UCS_string compute_prolog(int pick_level, const UCS_string & left,
                                    const Value * value);

   static Quad_CR  fun;          ///< Built-in function.

   /// portable variable encoding of value \b name (varname or varname ⊂)
   static void do_CR10_variable(UCS_string_vector & result,
                                const UCS_string & var_name,
                                const Value * value);

   /// compute \b 35 ⎕CR \b B
   static Value_P do_CR35(const Value * B);

protected:
   /// a mapping between function names and function numbers
   struct _sub_fun
      {
        unsigned int val;   ///< the function number
        const char * key;   ///< the name for it
      };

   /// a mapping between function names and function numbers
   static _sub_fun sub_functions[];

   /// compare two axis strings (function names)
   static int fun_compare(const void * key, const void * sf);

   /// list all ⎕CR functions
   static Token list_functions(ostream & out, bool mapping);

   /// do eval_B() with extra spaces removed
   static Token do_eval_B(const Value * B, bool remove_extra_spaces);

   /// compute \b 5 ⎕CR \b B or \b 6 ⎕CR \b B
   static Value_P do_CR5_6(int A56, const Value * B);

   /// compute \b 10 ⎕CR \b B
   static Value_P do_CR10(const Value * B);

   /// compute \b 11 ⎕CR \b B
   static Value_P do_CR11(const Value * B);

   /// compute \b 12 ⎕CR \b B
   static Value_P do_CR12(const Value * B);

   /// compute \b 13 ⎕CR \b B
   static Value_P do_CR13(const Value * B);

   /// compute \b 14 ⎕CR \b B
   static Value_P do_CR14(const Value * B);

   /// compute \b 15 ⎕CR \b B
   static Value_P do_CR15(const Value * B);

   /// compute \b 16 ⎕CR \b B
   static Value_P do_CR16(const Value * B);

   /// compute \b 17 ⎕CR \b B
   static Value_P do_CR17(const Value * B);

   /// compute \b 18 ⎕CR \b B
   static Value_P do_CR18(const Value * B);

   /// compute \b 19 ⎕CR \b B
   static Value_P do_CR19(const Value * B);

   /// compute \b 26 ⎕CR \b B
   static Value_P do_CR26(const Value * B);

   /// compute \b 27 ⎕CR \b B or \b 28 ⎕CR \b B
   static Value_P do_CR27_28(int A_27_28, const Value * B);

   /// compute \b 30 ⎕CR \b B
   static Value_P do_CR30(const Value * B);

   /// compute \b 31 ⎕CR \b B or \b 32 ⎕CR \b B
   static Value_P do_CR31_32(int A_31_32, const Value * B);

   /// compute \b 33 ⎕CR \b B
   static Value_P do_CR33(const Value * B);

   /// compute \b 34 ⎕CR \b B
   static Value_P do_CR34(const Value * B);

   /// compute \b 36 ⎕CR \b B
   static Value_P do_CR36(const Value * B);

   /// compute \b 37 ⎕CR \b B
   static Value_P do_CR37(const Value * B)
      { return do_eval_B(B, false).get_apl_val(); }

   /// compute \b 38 ⎕CR \b B
   static Value_P do_CR38(const Value * B);

   /// compute \b 39 ⎕CR \b B
   static Value_P do_CR39(const Value * B);

   /// compute \b 40 ⎕CR \b B
   static Value_P do_CR40(const Value * B);

   /// compute \b 41 ⎕CR \b B
   static Value_P do_CR41(const Value * B);

   /// compute \b 42 ⎕CR \b B (tokenize) or 43 ⎕CR \b B (parse); return tags
   static Value_P do_CR42_43(const Value * B, bool parse);

   /// compute \b 44 ⎕CR \b B
   static Value_P do_CR44(const Value * B);

   /// decode token or token tag \b cB into \b result
   static void decode_CR44(UCS_string & result, const Cell & cB);

   /// append \b value to result
   static void value_CR44(UCS_string & result, const Value & value);

   /// the left argument of Pick (⊃) which selects a sub-item of a variable
   /// being constructed
   class Picker
      {
        public:
           /// constructor: Picket at top (= variable) level
           Picker(const UCS_string & vname)
           : var_name(vname)
           {}

          /// push \b sh on \b shapes stack and \b pidx on \b indices stack
          void push(const Shape & sh, ShapeItem pidx)
             {
               shapes.push_back(sh);
               indices.push_back(pidx);
             }

          /// pop \b shapes and \b indices
          void pop()
             {
               shapes.pop_back();
               indices.pop_back();
             }

           /// varname or pick of varname
           void get(UCS_string & result);

           /// indexed varname or pick of varname
           void get_indexed(UCS_string & result, ShapeItem pos, ShapeItem len);

           /// the pick depth (⍴A for A⊃B)
           int get_level() const
              { return shapes.size(); }

        protected:
           /// the name of the variable from which we pick
           const UCS_string & var_name;

           /// the shapes along the pick
           std::vector<Shape> shapes;

           /// the indices along the pick
           std::vector<ShapeItem> indices;
      };

   /// compute 10 ⎕CR recursively
   static void do_CR10_value(UCS_string_vector & result, const Value * value,
                             Picker & picker, ShapeItem pidx);

   /// compute 10 ⎕CR recursively (structured variable)
   static const char * do_CR10_structured(UCS_string_vector & result,
                                          const UCS_string & varname,
                                          const Value * value);

   /// try to emit \b value in short format. Retrun true if that is not
   /// possible.
   static bool short_ravel(UCS_string_vector & result, bool & nested,
                           const Value * value, const Picker & picker,
                           const UCS_string & left,
                           const UCS_string & shape_rho);

   /// the state of the current output line
   enum V_mode
      {
        Vm_NONE,   ///< initial state
        Vm_NUM,    ///< in numeric vector.          e.g. 1 2 3
        Vm_QUOT,   ///< in quoted character vector, e.g. 'abc'
        Vm_UCS     ///< in ⎕UCS(),                  e.g. ⎕UCS(97 98 99)
      };

   /// 10 ⎕CR symbol_name (variable or function name). Also used for )OUT
   static void do_CR10(UCS_string_vector & result, const Value * symbol_name);

   /// one ravel item in 10 ⎕CR symbol_name
   static V_mode do_CR10_item(UCS_string & item, const Cell & cell,
                              V_mode mode, bool may_quote);

   /// decide if 'xxx' or ⎕UCS(xxx) shall be used
   static bool use_quote(V_mode mode, const Value * value, ShapeItem pos);

   /// close current mode (ending ' for '' or ) for ⎕UCS())
   static void close_mode(UCS_string & line, V_mode mode);

   /// print item separator
   static void item_separator(UCS_string & line,
                              V_mode from_mode, V_mode to_mode);
};
//----------------------------------------------------------------------------

#endif //__Quad_CR_HH_DEFINED__

