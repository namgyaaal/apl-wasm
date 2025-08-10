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

#include "Assert.hh"
#include "CDR.hh"
#include "CharCell.hh"
#include "ComplexCell.hh"
#include "FloatCell.hh"
#include "IntCell.hh"
#include "PointerCell.hh"
#include "Quad_FX.hh"
#include "Quad_TF.hh"
#include "Symbol.hh"
#include "Tokenizer.hh"
#include "Workspace.hh"

Quad_TF   Quad_TF::fun;

enum { Quad_PP_TF = 17 };

//----------------------------------------------------------------------------
Token
Quad_TF::eval_AB(Value_P A, Value_P B) const
{
   // A should be an integer scalar or 1-element_vector with value 1 or 2
   //
   if (A->get_rank() > 0)         RANK_ERROR;
   if (A->element_count() != 1)   LENGTH_ERROR;

const APL_Integer mode = A->get_cfirst().get_int_value();
const UCS_string symbol_name(*B.get());

Value_P Z;

   if (mode == 1)
      {
        const bool inverse = is_inverse(symbol_name);
        if (inverse)   Z = tf1_inv(symbol_name);
        else           Z = tf1(symbol_name);
      }
   else if (mode == 2)
      {
        const bool inverse = is_inverse(symbol_name);
        if (!inverse)   return tf2(symbol_name);
        Z = Value_P(tf2_inverse(symbol_name), LOC);
      }
   else if (mode == 3)
      {
        Z = tf3(symbol_name);
      }
   else
      {
        DOMAIN_ERROR;
      }

   if (!Z)   DOMAIN_ERROR;

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
bool
Quad_TF::is_inverse(const UCS_string & maybe_name)
{
   // a forward ⎕TF contains only a function or variable name.
   // an inverse ⎕TF contains at least one non-symbol character
   //
   loop(n, maybe_name.size())
      {
        if (n == 0 && Avec::is_quad(maybe_name[n]))   continue;
        if (Avec::is_symbol_char(maybe_name[n]))      continue;

        return true;   // inverse ⎕TF
      }

   // all chars in val were symbol characters. Therefore it is a forward ⎕TF
   return false;
}
//----------------------------------------------------------------------------
Value_P
Quad_TF::tf1(const UCS_string & name)
{
const NamedObject * obj = Workspace::lookup_existing_name(name);
   if (obj == 0)   return Str0(LOC);

const Function * function = obj->get_function();

   if (function)
      {
        if (obj->is_user_defined())   return tf1(name, *function);

        // quad function or primitive: return ''
        return Str0(LOC);
      }


   if (const Symbol * symbol = obj->get_symbol())
      {
        Value_P value = symbol->get_apl_value();
        if (+value)   return tf1(name, value);

        /* not a variable: fall through */
      }

   return Str0(LOC);
}
//----------------------------------------------------------------------------
Value_P
Quad_TF::tf1(const UCS_string & var_name, Value_P val)
{
const bool is_char_array = val->get_cfirst().is_character_cell();
UCS_string ucs(is_char_array ? UNI_C : UNI_N);

   ucs.append(var_name);
   ucs.append(UNI_SPACE);
   ucs.append_number(val->get_rank());   // rank

   loop(r, val->get_rank())
      {
        ucs.append(UNI_SPACE);
        ucs.append_number(val->get_shape_item(r));   // shape
      }

const ShapeItem ec = val->element_count();

   if (is_char_array)
      {
        ucs.append(UNI_SPACE);

        loop(e, ec)
           {
             const Cell & cell = val->get_cravel(e);
             if (!cell.is_character_cell())
                {
                  return  Str0(LOC);
                }

             ucs.append(cell.get_char_value());
           }
      }
   else   // number
      {
        loop(e, ec)
           {
             ucs.append(UNI_SPACE);

             const Cell & cell = val->get_cravel(e);
             if (cell.is_integer_cell())
                {
                  const int sign = ucs.size();
                  ucs.append_number(cell.get_int_value());
                  if (ucs[sign] == '-')   ucs[sign] = UNI_OVERBAR;
                }
             else if (cell.is_near_real())
                {
                  PrintContext pctx(PR_APL_MIN, Quad_PP_TF, MAX_Quad_PW);
                  bool scaled = true;
                  UCS_string ucs1(cell.get_real_value(), scaled, pctx);
                  ucs.append(ucs1);
                }
             else if (cell.is_numeric())
                {
                  PrintContext pctx(PR_APL_MIN, Quad_PP_TF, MAX_Quad_PW);
                  bool scaled = true;
                  UCS_string ucs1(cell.get_real_value(), scaled, pctx);
                  ucs.append(ucs1);

                  ucs.append(UNI_J);

                  ucs1 = UCS_string(cell.get_imag_value(), scaled, pctx);
                  ucs.append(ucs1);
                }
             else
                {
                  MORE_ERROR() << "Non-number in 1 ⎕TF N record";
                  return Value_P();
                }
           }
      }

   return Value_P(ucs, LOC);
}
//----------------------------------------------------------------------------
Value_P
Quad_TF::tf1(const UCS_string & fun_name, const Function & fun)
{
const UCS_string text = fun.canonical(false);
UCS_string_vector lines;
const size_t max_len = text.to_vector(lines);

UCS_string ucs;
   ucs.append(UNI_F);
   ucs.append(fun_name);
   ucs.append(UNI_SPACE);
   ucs.append_number(2);   // rank
   ucs.append(UNI_SPACE);
   ucs.append_number(lines.size());   // rows
   ucs.append(UNI_SPACE);
   ucs.append_number(max_len);        // cols
   ucs.append(UNI_SPACE);

   loop(l, lines.size())
      {
       ucs.append(lines[l]);
       loop(c, max_len - lines[l].size())   ucs.append(UNI_SPACE);
      }

   return Value_P(ucs, LOC);
}
//----------------------------------------------------------------------------
Value_P
Quad_TF::tf1_inv(const UCS_string & ravel)
{
const int len = ravel.size();

   if (len < 2)
      {
        MORE_ERROR() << "1 ⎕TF record too short";
        return Value_P();
      }

   // mode should be 'F', 'N', or 'C'.
const Unicode mode = ravel[0];
   if (mode != 'C' && mode != 'F' && mode != 'N')
      {
        MORE_ERROR() << "1 ⎕TF record type not F, N, or C";
        return Value_P();
      }

UCS_string name(ravel[1]);
   if (!Avec::is_quad(name[0]) && !Avec::is_first_symbol_char(name[0]))
      {
        MORE_ERROR() << "Bad variable name in 1 ⎕TF record";
        return Value_P();
      }

ShapeItem idx = 2;
   while (idx < len && Avec::is_symbol_char(ravel[idx]))
         name.append(ravel[idx++]);

   if (Avec::is_quad(name[0]) && name.size() == 1)
      {
        MORE_ERROR() << "Bad variable name in 1 ⎕TF record";
        return Value_P();
      }

   if (ravel[idx++] != UNI_SPACE)
      {
        MORE_ERROR() << "missing space (before rank) in 1 ⎕TF record";
        return Value_P();
      }

ShapeItem idx0 = idx;
sRank rank = 0;
   while (idx < len && Avec::is_digit(ravel[idx]))
      rank = 10 * rank + ravel[idx++] - UNI_0;

   if (rank == 0 && idx0 == idx)
      {
        MORE_ERROR() << "missing rank in 1 ⎕TF record";
        return Value_P();
      }

   if (ravel[idx++] != UNI_SPACE)
      {
        MORE_ERROR() << "missing space (after rank) in 1 ⎕TF record";
        return Value_P();
      }

   if (rank > MAX_RANK)
      {
        MORE_ERROR() << "max. rank exceeded in 1 ⎕TF record";
        return Value_P();
      }

Shape shape;
   loop(r, rank)
      {
        idx0 = idx;
        ShapeItem sh = 0;
        while (idx < len && Avec::is_digit(ravel[idx]))
           sh = 10 * sh + ravel[idx++] - UNI_0;
        if (sh == 0 && idx0 == idx)   // no shape
           {
             MORE_ERROR() << "too few shape items in 1 ⎕TF record";
             return Value_P();
           }

        if (ravel[idx++] != UNI_SPACE)
           {
             MORE_ERROR() << "missing space (in shape) in 1 ⎕TF record";
             return Value_P();
           }
        shape.add_shape_item(sh);
      }

const Symbol * symbol = 0;
NameClass nc = NC_UNUSED_USER_NAME;
const NamedObject * sym_or_fun = Workspace::lookup_existing_name(name); 
   if (sym_or_fun)   // existing name
      {
        symbol = sym_or_fun->get_symbol();
        nc = sym_or_fun->get_NC();
        if (symbol && symbol->is_readonly())
           {
             MORE_ERROR() << "symbol cannot be modified in 1 ⎕TF record";
             return Value_P();
           }
     }

const int data_chars = len - idx;

   if (mode == UNI_F)   // function record
      {
        if (rank != 2)
           {
             MORE_ERROR() << "function text is not a matrix in 1 ⎕TF record";
             return Value_P();
           }

        if (nc != NC_UNUSED_USER_NAME &&
            nc != NC_FUNCTION         &&
            nc != NC_OPERATOR)
           {
             MORE_ERROR() << "symbol is an existing variable in 1 ⎕TF F record";
             return Value_P();
           }

        if (data_chars != shape.get_volume())
           {
             MORE_ERROR() << "item count mismatch in 1 ⎕TF N record";
             return Value_P();
           }

        Value_P new_val(shape, LOC);
        loop(d, data_chars)   new_val->next_ravel_Char(ravel[idx + d]);
        new_val->check_value(LOC);

         Token t = Quad_FX::do_eval_B(new_val.get());
      }
   else if (mode == UNI_C)   // char array
      {
        if (data_chars != shape.get_volume())
           {
             MORE_ERROR() << "item count mismatch in 1 ⎕TF C record";
             return Value_P();
           }
        if (nc != NC_UNUSED_USER_NAME && nc != NC_VARIABLE)
           {
             MORE_ERROR() <<
             "symbol is existing and not a variable in 1 ⎕TF C record";
             return Value_P();
           }

        Value_P new_val(shape, LOC);
        loop(d, data_chars)   new_val->next_ravel_Char(ravel[idx + d]);

        new_val->check_value(LOC);

        if (symbol == 0)   symbol = Workspace::lookup_symbol(name);
        const_cast<Symbol *>(symbol)->assign(new_val, false, LOC);
      }
   else if (mode == UNI_N)   // numeric array
      {
        if (nc != NC_UNUSED_USER_NAME && nc != NC_VARIABLE)
           {
             MORE_ERROR() << "symbol cannot be assigned in 1 ⎕TF N record";
             return Value_P();
           }

        UCS_string data(ravel, idx, len - idx);
        Tokenizer tokenizer(PM_EXECUTE, LOC, false);
        Token_string tos;
        if (tokenizer.tokenize(data, tos) != E_NO_ERROR)
           {
             MORE_ERROR() << "tokenization failed in 1 ⎕TF N record";
             return Value_P();
           }
        if (size_t(tos.size()) != size_t(shape.get_volume()))
           {
             MORE_ERROR() << "item count mismatch in 1 ⎕TF N record";
             return Value_P();
           }

        // check that all token are numeric...
        //
        loop(t, tos.size())
           {
             const TokenTag tag = tos[t].get_tag();
             if (tag == TOK_INTEGER)   continue;
             if (tag == TOK_REAL)      continue;
             if (tag == TOK_COMPLEX)   continue;
             MORE_ERROR() <<  "Non-number in 1 ⎕TF N record";
             return Value_P();
           }

        // at this point, we have a valid inverse 1 ⎕TF.
        //
        Value_P ZZ(shape, LOC);

        loop(t, tos.size())
           {
             const Token & tok = tos[t];
             const TokenTag tag = tok.get_tag();
             if (tag == TOK_INTEGER)
                ZZ->next_ravel_Int(tok.get_int_val());
             else if (tag == TOK_REAL)
                ZZ->next_ravel_Float(tok.get_flt_val());
             else if (tag == TOK_COMPLEX)
                ZZ->next_ravel_Complex(tok.get_cpx_real(),
                                            tok.get_cpx_imag());
             else Assert(0);   // since checked above
           }

        ZZ->check_value(LOC);

        if (!symbol)   symbol = Workspace::lookup_symbol(name);

        const_cast<Symbol *>(symbol)->assign(ZZ, false, LOC);
      }
   else Assert(0);   // since checked above

   return Value_P(name, LOC);
}
//----------------------------------------------------------------------------
Token
Quad_TF::tf2(const UCS_string & name)
{
const NamedObject * obj = Workspace::lookup_existing_name(name);
   if (obj == 0)
      {
        Log(LOG_Quad_TF)   CERR << "bad name in tf2(" << name << ")" << endl;
        return Token(TOK_APL_VALUE1, Str0(LOC));
      }

const Function * function = obj->get_function();

   if (function)
      {
        if (obj->is_user_defined())
           {
             UCS_string ucs = tf2_fun(name, *function);
             Value_P Z(ucs, LOC);
             Z->check_value(LOC);
             return Token(TOK_APL_VALUE1, Z);
           }

        // quad function or primitive: return ''
        return Token(TOK_APL_VALUE1, Str0(LOC));
      }

const Symbol * symbol = obj->get_symbol();
   if (symbol)
      {
        Value_P value = symbol->get_apl_value();
        if (+value)   return tf2_var(name, *value);

        /* not a variable: fall through */
      }

   Log(LOG_Quad_TF)   CERR << "error in tf2(" << name << ")" << endl;
   return Token(TOK_APL_VALUE1, Str0(LOC));
}
//----------------------------------------------------------------------------
Token
Quad_TF::tf2_var(const UCS_string & var_name, const Value & B)
{
   Log(LOG_Quad_TF)   CERR << "tf2_var(" << var_name << ")" << endl;

UCS_string ucs_value; /// the right hand side of VAR←VALUE
   if (B.is_scalar() && !B.is_simple_scalar())
      {
        const Cell & cell = B.get_cfirst();
        Assert(cell.is_pointer_cell());
        tf2_value(0, ucs_value, *cell.get_pointer_value(), 1);
      }
   else
      {
        tf2_value(0, ucs_value, B, 0);
      }

   Assert(ucs_value[0] == UNI_L_PARENT);
   Assert(ucs_value[ucs_value.size() - 1] == UNI_R_PARENT);

UCS_string ucs(var_name);
   ucs.append(UNI_LEFT_ARROW);

   // copy ucs_value except the outer parrentheses
   for (ShapeItem v = 1; v < (ucs_value.size() - 1); ++v)
       ucs.append(ucs_value[v]);

   Log(LOG_Quad_TF)   CERR << "success in tf2_var(): " << ucs << endl;
Value_P Z(ucs, LOC);
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
UCS_string
Quad_TF::tf2_inverse(const UCS_string & ravel)
{
Token_string tos;
   tos.push_back(Token(TOK_L_PARENT, int64_t(0)));

   Log(LOG_Quad_TF)   CERR << "inverse ⎕TF2: " << ravel << endl;

   try
      {
        UCS_string ucs1 = no_UCS(ravel);
        const Parser parser(PM_EXECUTE, LOC, false);
        parser.parse(ucs1, tos, true);
      }
   catch(...)
      {
        Log(LOG_Quad_TF)   CERR << "parse error in tf2_inverse()" << endl;
        return UCS_string();
      }

   // tos is now ( VAR ← ... ) or: ( ⎕FX val ...
   // make it VAR ← ( ... )    or: leave as is
   //
   {
     if (tos.size() >= 3 && tos[1].get_tag() != TOK_Quad_FX)
        {
          tos[0] = tos[1];   // move VAR
          tos[1] = tos[2];   // move ←
          tos[2] = Token(TOK_L_PARENT, int64_t(0));
        }
   }
    tos.push_back(Token(TOK_R_PARENT, int64_t(0)));

   // simplify tos as much as possible. We replace A⍴B by reshaped B
   // and glue values together.
   //
   tf2_reduce(tos);

   Log(LOG_Quad_TF)
      {
        CERR << "inverse ⎕TF2: tos[" << tos.size() << "] after reduce() is: ";
        tos.print(CERR, false);
        CERR << endl;
      }

   if (tos.size() < 2)
      {
        Log(LOG_Quad_TF)
           CERR << "short tos in tf2_inverse()" << tos.size() << endl;
        return UCS_string();   // too short for an inverse 2⎕TF
      }

   // it could happen that some system variable ⎕XX which is not known by
   // GNU APL is read. This case is parsed as ⎕ XX ← ... 
   // Issue a warning in that case
   //
   if (tos[0].get_tag() == TOK_Quad_Quad &&
       tos[1].get_Class() == TC_SYMBOL)
      {
        UCS_string new_var_or_fun = tos[1].get_sym_ptr()->get_name();
        CERR << "*** Unknown system variable ⎕" << new_var_or_fun
             << " in 2⎕TF / )IN (assignment ignored)" << endl;
        return new_var_or_fun;
      }

   /* we expect either   VAR ← VALUE
      or:              ( ⎕FX fun-text    )

      Try VAR ← VALUE first.
    */
   if (tos[0].get_Class() == TC_SYMBOL && tos[1].get_Class() == TC_ASSIGN)
      {
        // at this point, we expect SYM ← VALUE.
        //
        if (tos.size() != 3)                  return UCS_string();
        if (tos[2].get_Class() != TC_VALUE)   return UCS_string();

         tos[0].get_sym_ptr()->assign(tos[2].get_apl_val(), true, LOC);
         Log(LOG_Quad_TF)   CERR << "valid inverse 2 ⎕TF" << endl;
         return tos[0].get_sym_ptr()->get_name();   // valid 2⎕TF
      }

   // try dyadic ⎕FX (native function)
   //
   // Then tos[5] is: ( VALUE ⎕FX VALUE )
   //
   if (tos.size() == 5                   &&
       tos[1].get_Class() == TC_VALUE    &&
       tos[2].get_tag()   == TOK_Quad_FX &&
       tos[3].get_Class() == TC_VALUE)
      {
        const Value * fname   =  tos[1].get_apl_val().get();
        const Value * so_path =  tos[3].get_apl_val().get();

        const Token tok = Quad_FX::do_eval_AB(so_path, fname);
        if (tok.get_Class() == TC_VALUE)   // ⎕FX successful
           {
             Value_P val = tok.get_apl_val();
        Log(LOG_Quad_TF)
           CERR << "valid inverse 2 ⎕TF (native function):" << endl;
             return UCS_string(*val.get());
           }

        Log(LOG_Quad_TF)
           CERR << "invalid inverse 2 ⎕TF (native function):" << endl;
        return UCS_string();
      }

   /* monadic ⎕FX. at this point we should have:

      tos[4] =  ( ⎕FX Value )
    */

   Log(LOG_Quad_TF)
      CERR << "inverse 2 ⎕TF (monadic ⎕FX):" << endl;
   if (tos.size() != 4)                   return UCS_string();
   if (tos[1].get_tag() != TOK_Quad_FX)   return UCS_string();
   if (tos[2].get_Class() != TC_VALUE)    return UCS_string();

static const int eprops[] = { 0, 0, 0, 0 };
const Token tok = Quad_FX::do_quad_FX(eprops, tos[2].get_apl_val().get(),
                                      UTF8_string("2 ⎕TF"), true);

   if (tok.get_Class() != TC_VALUE)
      {
        Log(LOG_Quad_TF)
           CERR << "Quad_FX::do_quad_FX() failed in tf2_inverse()" << endl;
        return UCS_string();   // error in ⎕FX
      }

   // ⎕FX succeeded (and the token returned is the name of the function)
   //
   return UCS_string(*tok.get_apl_val().get());
}
//----------------------------------------------------------------------------
void
Quad_TF::tf2_shape(UCS_string & ucs, const Shape & shape, ShapeItem nesting)
{
   ucs.append(UNI_L_PARENT);
   loop(n, nesting)   ucs.append(UNI_SUBSET);   // ⊂...

   // scalars are ''⍴SCALAR but ''⍴ has no effect and can be omitted

   if (shape.get_rank())   // non-scalar
      {
        loop(r, shape.get_rank())
            {
              if (r)   ucs.append(UNI_SPACE);
              ucs.append_number(shape.get_shape_item(r));
            }

        ucs.append(UNI_RHO);
      }
}
//----------------------------------------------------------------------------
void
Quad_TF::tf2_value(int level, UCS_string & ucs, const Value & value,
                   ShapeItem nesting)
{
   Log(LOG_Quad_TF)
      {
        char cc[100];
        SPRINTF(cc, "tf2_value() initial level %u\n", level);
        CERR << "tf2_value(): ucs before at level " << level << ": "
             << ucs << endl << cc;
        value.print_boxed(CERR, 0);
      }


   // some (but not all) empty vectors
   if (value.is_empty())
      {
        const Cell & cell = value.get_cfirst();
        if (cell.is_character_cell())
           {
             ucs.append(UNI_L_PARENT);
             ucs.append(UNI_SINGLE_QUOTE);
             ucs.append(UNI_SINGLE_QUOTE);
             ucs.append(UNI_R_PARENT);
             return;
           }
      }

const ShapeItem ec = value.nz_element_count();

   // emit e.g. ( shape ⍴
   //
   tf2_shape(ucs, value.get_shape(), nesting);
   if (value.NOTCHAR())   tf2_ravel(level, ucs, ec, &value.get_cfirst());
   else                   tf2_all_char_ravel(level, ucs, value);
   ucs.append(UNI_R_PARENT);   // close corresponding '(' from tf2_shape()

   Log(LOG_Quad_TF)
      {
        CERR << "tf2_value(): ucs after at level " << level
             << ": " << ucs << endl;
      }
   return;
}
//----------------------------------------------------------------------------
void
Quad_TF::tf2_ravel(int level, UCS_string & ucs, const ShapeItem len,
                   const Cell * cells)
{
   Assert(len > 0);

   loop(e, len)
       {
         if (e)   ucs.append(UNI_SPACE);
         const Cell & cell = *cells++;

         if (cell.is_pointer_cell())
            {
              Value_P sub_val = cell.get_pointer_value();
              ShapeItem nesting = 0;
              while (sub_val->is_scalar())
                    {
                      Assert(sub_val->get_cfirst().is_pointer_cell());
                      ++nesting;
                      sub_val = sub_val->get_cfirst().get_pointer_value();
                    }

              tf2_value(level + 1, ucs, *sub_val, nesting);
           }
        else if (cell.is_lval_cell())
           {
             DOMAIN_ERROR;
           }
        else if (cell.is_complex_cell())
           {
             PrintContext pctx(PR_APL_MIN, Quad_PP_TF, MAX_Quad_PW);
             bool scaled = true;
             UCS_string ucs1(cell.get_real_value(), scaled, pctx);
             UCS_string ucs2(cell.get_imag_value(), scaled, pctx);
             ucs.append(ucs1);
             ucs.append(UNI_J);
             ucs.append(ucs2);
           }
        else if (cell.is_float_cell())
           {
             PrintContext pctx(PR_APL_MIN, Quad_PP_TF, MAX_Quad_PW);
             bool scaled = true;
             UCS_string ucs1(cell.get_real_value(), scaled, pctx);
             ucs.append(ucs1);
           }
        else
           {
             PrintContext pctx(PR_APL_FUN);
             const PrintBuffer pb = cell.character_representation(pctx);

             ucs.append(pb.l1());
           }
        }
}
//----------------------------------------------------------------------------
void
Quad_TF::tf2_all_char_ravel(int level, UCS_string & ucs, const Value & value)
{
const ShapeItem ec = value.nz_element_count();

   // check if ⎕UCS is needed. 
   //
   // ⎕UCS is needed if the string contains a character that is:
   //
   // 1. not in our ⎕AV, or
   // 2. not in IBM's ⎕AV
   //
   bool use_UCS = false;
   loop(e, ec)
       {
         const Unicode uni = value.get_cravel(e).get_char_value();
         if (Avec::need_UCS(uni))   { use_UCS = true;   break; }
       }

   if (use_UCS)
      {
        ucs.append_UTF8("⎕UCS");
        loop(e, ec)
            {
              ucs.append(UNI_SPACE);
              const Unicode uni = value.get_cravel(e).get_char_value();
              ucs.append_number(uni);
            }
      }
   else
      {
        ucs.append(UNI_SINGLE_QUOTE);
        loop(e, ec)
            {
              const Unicode uni = value.get_cravel(e).get_char_value();
              ucs.append(uni);
              if (uni == UNI_SINGLE_QUOTE)   ucs.append(UNI_SINGLE_QUOTE);
            }
        ucs.append(UNI_SINGLE_QUOTE);
      }
}
//----------------------------------------------------------------------------
void
Quad_TF::tf2_reduce(Token_string & tos)
{
   tf2_reduce_UCS(tos);
   for (bool progress = true; progress;)
       {
         progress = false;

         Log(LOG_Quad_TF)   tos.print(CERR, true);

         if ((progress = tf2_reduce_RHO(tos)))               continue;
         if ((progress = tf2_reduce_COMMA(tos)))             continue;
         if ((progress = tf2_reduce_ENCLOSE_ENCLOSE(tos)))   continue;
         if ((progress = tf2_reduce_ENCLOSE(tos)))           continue;
         if ((progress =  tf2_reduce_ENCLOSE1(tos)))         continue;
         if ((progress = tf2_reduce_sequence(tos)))          continue;
         if ((progress = tf2_reduce_sequence1(tos)))         continue;
         if ((progress = tf2_reduce_parentheses(tos)))       continue;
         if ((progress = tf2_glue(tos)))                     continue;
       }
}
//----------------------------------------------------------------------------
void
Quad_TF::tf2_reduce_UCS(Token_string & tos)
{
ShapeItem skipped = 0;

   loop(s, tos.size())
      {
        if (s < ShapeItem(tos.size() - 1) && tos[s].get_tag() == TOK_Quad_UCS)
           {
             s += 1;     skipped += 1;    // skip ⎕UCS
             for (; s < ShapeItem(tos.size()) &&
                    tos[s].get_Class() == TC_VALUE; ++s)
                {
                  tf2_toggle_UCS(*tos[s].get_apl_val().get());
                  tos[s - skipped].move(tos[s], LOC);
                }
           }

        if (skipped)   tos[s - skipped].move(tos[s], LOC);

      }

   if (skipped)
      {
        Log(LOG_Quad_TF)
           CERR << "tf2_reduce_UCS() has skipped "
                << skipped << " token" << endl;

        tos.resize(tos.size() - skipped);
      }

}
//----------------------------------------------------------------------------
ShapeItem
Quad_TF::tf2_toggle_UCS(Value & val)
{
ShapeItem error_count = 0;

   loop(e, val.nz_element_count())
       {
        const Cell & cell = val.get_cravel(e);
        if (cell.is_character_cell())       // char → integer
           {
             val.set_ravel_Int(e, cell.get_char_value());
           }
        else if (cell.is_integer_cell())   // integer → char
           {
             val.set_ravel_Char(e, Unicode(cell.get_int_value()));
           }
        else if (cell.is_pointer_cell())   // nested
           {
             error_count += tf2_toggle_UCS(*cell.get_pointer_value());
           }
        else
           {
             ++error_count;
           }
      }

   if (val.is_empty())   val.to_type(false);   // unlikely

   return error_count;
}
//----------------------------------------------------------------------------
bool
Quad_TF::tf2_reduce_RHO(Token_string & tos)
{
ShapeItem skipped = 0;

   loop(s, tos.size())
      {
        // we replace (A⍴B) by B reshaped to A.
        //
        if ((s + 3) >= ShapeItem(tos.size())        ||   // too short
            tos[s    ].get_Class() != TC_VALUE      ||   // not A
            tos[s + 1].get_tag()   != TOK_F12_RHO   ||   // not ⍴
            tos[s + 2].get_Class() != TC_VALUE      ||   // not B 
            tos[s + 3].get_tag()   != TOK_R_PARENT)      // not )
           {
             // no match: copy the token (but never to itself)
             //
             if (skipped)   tos[s - skipped].move(tos[s], LOC);
             continue;
           }

        Shape sh;
        {
          const Value & aval = *tos[s].get_apl_val();
          sh = Shape(aval, /* ⎕IO */ 0);
          tos[s].release_apl_val(LOC);
        }
        s += 2;     skipped += 2;    // skip ( A

        Value_P bval = tos[s].get_apl_val();
        if (sh.get_volume() == bval->element_count())   // same volume
           {
             tos[s - skipped].move(tos[s], LOC);
             tos[s - skipped].ChangeTag(TOK_APL_VALUE1);
             bval->set_shape(sh);
           }
        else
           {
             Token t = Bif_F12_RHO::do_reshape(sh, *bval);
             tos[s - skipped].move(t, LOC);
           }
      }

   if (skipped)
      {
        Log(LOG_Quad_TF)
           CERR << "tf2_reduce_RHO() has skipped "
                << skipped << " token" << endl;

        tos.resize(tos.size() - skipped);
      }

   return skipped > 0;
}
//----------------------------------------------------------------------------
bool
Quad_TF::tf2_reduce_sequence(Token_string & tos)
{
ShapeItem skipped = 0;

//   tos.print(CERR);

   // replace N - ⎕IO - ⍳ K  by  N N+1 ... N+K-1
   //         0 1 2   3 4 5
   //
   loop(s, tos.size())
      {
        if ((s + 5) >= ShapeItem(tos.size())           ||   // too short
            tos[s    ].get_Class() != TC_VALUE         ||   // not N
            tos[s + 1].get_tag()   != TOK_F12_MINUS    ||   // not -
            tos[s + 2].get_tag()   != TOK_Quad_IO      ||   // not ⎕IO
            tos[s + 3].get_tag()   != TOK_F12_MINUS    ||   // not -
            tos[s + 4].get_tag()   != TOK_F12_INDEX_OF ||   // not ⍳
            tos[s + 5].get_Class() != TC_VALUE         ||   // not K
           !tos[s    ].get_apl_val()->is_int_scalar()  ||   // N not integer
           !tos[s + 5].get_apl_val()->is_int_scalar())      // K not integer
           {
             if (skipped)   // dont copy to itself
                tos[s - skipped].move(tos[s], LOC);
             continue;
           }

        const APL_Integer N = tos[s].get_apl_val()->get_cfirst()
                                    .get_int_value();
        const APL_Integer K = tos[s + 5].get_apl_val()->get_cfirst()
                                        .get_int_value();

        loop(j, 6)   tos[s + j].clear(LOC);

        Value_P sequence(K, LOC);
        loop(k, K)   sequence->next_ravel_Int(N + k);
        sequence->check_value(LOC);
        Token tok(TOK_APL_VALUE1, sequence);
        tos[s - skipped].move(tok, LOC);
        s += 5;   skipped += 5;
      }

   if (skipped)
      {
        Log(LOG_Quad_TF)
           CERR << "tf2_reduce_sequence() has skipped "
                << skipped << " token" << endl;

        tos.resize(tos.size() - skipped);
      }

   return skipped > 0;
}
//----------------------------------------------------------------------------
bool
Quad_TF::tf2_reduce_sequence1(Token_string & tos)
{
ShapeItem skipped = 0;

//   tos.print(CERR);

   // replace N - M × ⎕IO - ⍳ K by M(N N+1 ... N+K-1)
   //         0 1 2 3 4   5 6 7
   //
   loop(s, tos.size())
      {
        if ((s + 7) >= ShapeItem(tos.size())           ||   // too short
            tos[s    ].get_Class() != TC_VALUE         ||   // not N
            tos[s + 1].get_tag()   != TOK_F12_MINUS    ||   // not -
            tos[s + 2].get_Class() != TC_VALUE         ||   // not K
            tos[s + 3].get_tag()   != TOK_F12_TIMES    ||   // not ×
            tos[s + 4].get_tag()   != TOK_Quad_IO      ||   // not ⎕IO
            tos[s + 5].get_tag()   != TOK_F12_MINUS    ||   // not -
            tos[s + 6].get_tag()   != TOK_F12_INDEX_OF ||   // not ⍳
            tos[s + 7].get_Class() != TC_VALUE         ||   // not K
           !tos[s    ].get_apl_val()->is_int_scalar()  ||   // N not integer
           !tos[s + 2].get_apl_val()->is_int_scalar()  ||   // M not integer
           !tos[s + 7].get_apl_val()->is_int_scalar())      // K not integer
           {
             if (skipped)   // dont copy to itself
                tos[s - skipped].move(tos[s], LOC);
             continue;
           }

        const APL_Integer N = tos[s].get_apl_val()->get_cfirst()
                                    .get_int_value();
        const APL_Integer M = tos[s + 2].get_apl_val()->get_cfirst()
                                        .get_int_value();
        const APL_Integer K = tos[s + 7].get_apl_val()->get_cfirst()
                                        .get_int_value();

        loop(j, 6)   tos[s + j].clear(LOC);

        Value_P sequence(K, LOC);
        loop(k, K)   sequence->next_ravel_Int(M * (N + k));
        sequence->check_value(LOC);
        Token tok(TOK_APL_VALUE1, sequence);
        tos[s - skipped].move(tok, LOC);
        s += 7;   skipped += 7;
      }

   if (skipped)
      {
        Log(LOG_Quad_TF)
           CERR << "tf2_reduce_sequence1() has skipped "
                << skipped << " token" << endl;

        tos.resize(tos.size() - skipped);
      }
   return skipped > 0;
}
//----------------------------------------------------------------------------
bool
Quad_TF::tf2_reduce_ENCLOSE(Token_string & tos)
{
ShapeItem skipped = 0;

   // replace  ⊂ B ) by an enclosed B )
   //
   loop(s, tos.size())
      {
        if ((s + 2) >= ShapeItem(tos.size())            ||
            tos[s    ].get_tag()   != TOK_F12_PARTITION ||   // not ⊂
            tos[s + 1].get_Class() != TC_VALUE          ||   // not B
            tos[s + 2].get_tag()   == TOK_F12_RHO)           // followed by ⍴
           {
             if (skipped)   // dont copy to itself
                tos[s - skipped].move(tos[s], LOC);
             continue;
           }

        Value_P B = tos[s + 1].get_apl_val();
        if (B->is_simple_scalar())
           {
             tos[s - skipped].move(tos[s + 1], LOC);
           }
        else
           {
             Value_P enc_B(LOC);
             enc_B->next_ravel_Pointer(B.get());
             enc_B->check_value(LOC);
             Token tok(TOK_APL_VALUE1, enc_B);
             tos[s - skipped].move(tok, LOC);
             tos[s + 1].clear(LOC);   // B
           }
        s += 1;   skipped += 1;
      }

   if (skipped)
      {
        Log(LOG_Quad_TF)
           CERR << "tf2_reduce_ENCLOSE() has skipped "
                << skipped << " token" << endl;

        tos.resize(tos.size() - skipped);
      }

   return skipped > 0;
}
//----------------------------------------------------------------------------
bool
Quad_TF::tf2_reduce_COMMA(Token_string & tos)
{
ShapeItem skipped = 0;

   loop(s, tos.size())
      {
        // we replace , B by B reshaped or A , B by AB
        //
        if (s < ShapeItem(tos.size() - 1)     &&
            tos[s].get_tag() == TOK_F12_COMMA &&   // ,
            tos[s + 1].get_Class() == TC_VALUE)    // B 
           {
             const ShapeItem d_1 = s - skipped - 1;
             if (s > 0 && tos[d_1].get_tag() == TOK_R_PARENT)   // value to be
                {
                  // TOK_R_PARENT will become a value: just copy it for now
                  //
                  if (skipped)   tos[s - skipped].move(tos[s], LOC);
                  continue;
                }

             if (s > 0 && tos[d_1].get_Class() == TC_VALUE)   // value
                {
                  Value_P A = tos[d_1].get_apl_val();
                  Value_P B = tos[s + 1].get_apl_val();
                  Assert(A->get_rank() <= 1);
                  Assert(B->get_rank() <= 1);
                  Value_P Z(A->element_count() + B->element_count(), LOC);
                  loop(a, A->element_count())
                     Z->next_ravel_Cell(A->get_cravel(a));
                  loop(b, B->element_count())
                     Z->next_ravel_Cell(B->get_cravel(b));

                  tos[s + 1].clear(LOC);
                  Token tok_AB(TOK_APL_VALUE1, Z);
                  tos[d_1].move(tok_AB, LOC);
                  s += 1;   skipped += 2;   // skip , (of A , B) but not B
                  continue;   // don't move() below
                }

             // monadic , B
             s += 1;   skipped += 1;    // skip ,

             Value_P bval = tos[s].get_apl_val();
             const Shape sh(bval->element_count());
             bval->set_shape(sh);

             tos[s].ChangeTag(TOK_APL_VALUE1);
             tos[s - skipped].move(tos[s], LOC);
             continue;
           }

        if (skipped)   tos[s - skipped].move(tos[s], LOC);
      }

   if (skipped)
      {
        Log(LOG_Quad_TF)
           CERR << "tf2_reduce_COMMA() has skipped "
                << skipped << " token" << endl;

        tos.resize(tos.size() - skipped);
      }

   return skipped > 0;
}
//----------------------------------------------------------------------------
bool
Quad_TF::tf2_reduce_ENCLOSE_ENCLOSE(Token_string & tos)
{
ShapeItem skipped = 0;

   loop(s, tos.size())
      {
        if ((s + 2) >= ShapeItem(tos.size())            ||
            (s && tos[s - 1].get_Class() == TC_VALUE)   ||   // dyadic ⊂
            tos[s    ].get_tag()   != TOK_F12_PARTITION ||   // not ⊂
            tos[s + 1].get_tag()   != TOK_F12_PARTITION ||   // not ⊂
            tos[s + 2].get_Class() != TC_VALUE          ||   // not B 
            skipped)            // remove at most one ⊂ (may enable ⍴)
           {
             if (skipped)   // dont copy to itself
                tos[s - skipped].move(tos[s], LOC);
             continue;
           }

        skipped++;   // ignore ⊂ at tos[s]
      }

   if (skipped)
      {
        Log(LOG_Quad_TF)
           CERR << "tf2_reduce_ENCLOSE_ENCLOSE() has skipped "
                << skipped << " token" << endl;

        tos.resize(tos.size() - skipped);
      }
   return skipped > 0;
}
//----------------------------------------------------------------------------
bool
Quad_TF::tf2_reduce_ENCLOSE1(Token_string & tos)
{
ShapeItem skipped = 0;

   // replace  ⍴ ⊂ B  by ⍴ enclosed B
   //
   loop(s, tos.size())
      {
        if ((s + 2) >= ShapeItem(tos.size())            ||
            tos[s    ].get_tag()   != TOK_F12_RHO       ||   // not ⍴
            tos[s + 1].get_tag()   != TOK_F12_PARTITION ||   // not ⊂
            tos[s + 2].get_Class() != TC_VALUE)              // not B 
           {
             if (skipped)   // dont copy to itself
                tos[s - skipped].move(tos[s], LOC);
             continue;
           }

        if (skipped)   // dont copy to itself
           tos[s - skipped].move(tos[s], LOC);   // move ⍴

        Value_P B = tos[s + 2].get_apl_val();
        if (B->is_scalar())
           {
             tos[s - skipped + 1].move(tos[s + 2], LOC);
           }
        else
           {
             Value_P enc_B(LOC);
             enc_B->next_ravel_Pointer(B.get());
             Token tok(TOK_APL_VALUE1, enc_B);
             tos[s - skipped + 1].move(tok, LOC);
             tos[s + 2].clear(LOC);   // B
           }
        s += 2;   ++skipped;
      }

   if (skipped)
      {
        Log(LOG_Quad_TF)
           CERR << "tf2_reduce_ENCLOSE1() has skipped "
                << skipped << " token" << endl;

        tos.resize(tos.size() - skipped);
      }

   return skipped > 0;
}
//----------------------------------------------------------------------------
bool
Quad_TF::tf2_reduce_parentheses(Token_string & tos)
{
ShapeItem skipped = 0;

   loop(s, tos.size())
      {
        // we replace ( B ) by B 
        //
        if ((s + 2) < ShapeItem(tos.size())        &&
            tos[s].get_tag()       == TOK_L_PARENT &&
            tos[s + 1].get_Class() == TC_VALUE     &&
            tos[s + 2].get_tag()   == TOK_R_PARENT)   // ( B )
           {
             Token & dest = tos[s - skipped];   // '(' of: '(' B ')'
             dest.move(tos[s + 1], LOC);      // move B to '('
             if (dest.get_tag() == TOK_APL_VALUE3)
                {
                  // final result of strand expression: enclose it
                  //
                  Value_P B = dest.get_apl_val();
                  if (!B->is_simple_scalar())   // unless simple scalar
                     {
                       dest.release_apl_val(LOC);    // we will override it
                       Value_P Z(LOC);
                       Z->next_ravel_Pointer(B.get());
                       Z->check_value(LOC);
                       new (&dest) Token(TOK_APL_VALUE3, Z);
                     }
                }
             s += 2;   skipped += 2;
             continue;
           }

        if (skipped)   // dont copy to itself
           tos[s - skipped].move(tos[s], LOC);
      }

   if (skipped)
      {
        Log(LOG_Quad_TF)
           CERR << "tf2_reduce_parentheses() has skipped "
                << skipped << " token" << endl;
        tos.resize(tos.size() - skipped);
      }

   return skipped > 0;
}
//----------------------------------------------------------------------------
bool
Quad_TF::tf2_glue(Token_string & tos)
{
ShapeItem skipped = 0;

   loop(s, tos.size())   // ⍴ must not be first or last
      {
        // the first value (if any) survives
        //
        if (skipped)   // dont copy to itself
           tos[s - skipped].move(tos[s], LOC);

        if (tos[s - skipped].get_Class() != TC_VALUE)   continue;

         // subsequent values (if any) are glued to the first value which
         // is now tos[s - skipped]
         //
         while ((s + 1) < ShapeItem(tos.size()) &&
                tos[s + 1].get_Class() == TC_VALUE)
            {
               Token t;
               t.move(tos[s - skipped], LOC);
               Value::glue(tos[s - skipped], t, tos[s + 1], LOC);
               s += 1;   skipped += 1;
            }
      }

   if (skipped)
      {
        Log(LOG_Quad_TF)
           CERR << "tf2_glue() has skipped " << skipped << " token" << endl;

        tos.resize(tos.size() - skipped);
      }

   return skipped > 0;
}
//----------------------------------------------------------------------------
UCS_string
Quad_TF::tf2_fun(const UCS_string & fun_name, const Function & fun)
{
UCS_string ucs;
   tf2_fun_ucs(ucs, fun_name, fun);

   return ucs;
}
//----------------------------------------------------------------------------
void
Quad_TF::tf2_fun_ucs(UCS_string & ucs, const UCS_string & fun_name,
                    const Function & fun)
{
const UCS_string text = fun.canonical(false);

   if (fun.is_native())
      {
        CERR << "Warning: the workspace contains a native function '"
             << fun_name << "', making the" << endl
             << "   .atf output file incompatible with other APL interpreters."
             << endl;

        ucs.append_UTF8("'");
        ucs.append(fun_name);
        ucs.append_UTF8("' ⎕FX '");
        ucs.append(text);
        ucs.append_UTF8("'");
        return;
      }

UCS_string_vector lines;
   text.to_vector(lines);

   ucs.append_UTF8("⎕FX");

   loop(l, lines.size())
      {
        ucs.append(UNI_SPACE);
        tf2_char_vec(ucs, lines[l]);
      }
}
//----------------------------------------------------------------------------
void
Quad_TF::tf2_char_vec(UCS_string & ucs, const UCS_string & vec)
{
   if (vec.size() == 0)   return;

bool in_UCS = false;
   ucs.append_UTF8("'");

   loop(v, vec.size())
       {
         const Unicode uni = vec[v];
         const bool need_UCS = Avec::need_UCS(uni);
         if (in_UCS != need_UCS)   // mode changed
            {
              if (in_UCS)   ucs.append_UTF8("),'");      // UCS() → 'xxx'
              else          ucs.append_UTF8("',(⎕UCS");   // 'xxx' → UCS()
              in_UCS = need_UCS;
            }

         if (in_UCS)
            {
              ucs.append_UTF8(" ");
              ucs.append_number(uni);
            }
         else
            {
              ucs.append(uni);
              if (uni == UNI_SINGLE_QUOTE)   ucs.append(uni);
            }
       }

   if (in_UCS)   ucs.append_UTF8("),''");
   else          ucs.append_UTF8("'");
}
//----------------------------------------------------------------------------
UCS_string
Quad_TF::no_UCS(const UCS_string & ucs)
{
UCS_string ret;

   loop(u, ucs.size())
      {
        if ( u < ucs.size() - 6 &&
             ucs[u]     == '\'' &&
             ucs[u + 1] == ','  &&
             ucs[u + 2] == '('  &&
             ucs[u + 3] == UNI_Quad_Quad &&
             ucs[u + 4] == 'U'  &&
             ucs[u + 5] == 'C'  &&
             ucs[u + 6] == 'S')
           {
             u += 7;   // skip '(⎕UCS

             for (;;)
                 {
                  while (u < ucs.size() && ucs[u] == ' ')   ++u;
                  if (ucs[u] == ')')   { u += 2;   break; }

                  int num = 0;
                  while (u < ucs.size() && ucs[u] >= '0' && ucs[u] <= '9')
                     { num *= 10;   num += ucs[u++] - '0'; }

                  ret.append(Unicode(num));
                 }
           }
        else ret.append(ucs[u]);
      }

   return ret;
}
//----------------------------------------------------------------------------
Value_P
Quad_TF::tf3(const UCS_string & name)
{
const NamedObject * obj = Workspace::lookup_existing_name(name);
   if (obj == 0)   return Str0(LOC);

   if (obj->get_function())
      {
        // 3⎕TF is not defined for functions.
        //
        return Str0(LOC);
      }

const Symbol * symbol = obj->get_symbol();
   if (symbol)
      {
        if (const Value * value = symbol->get_apl_value().get())
           {
             CDR_string cdr;
             CDR::to_CDR(cdr, value);

             return Value_P(cdr, LOC);
           }

        /* not a variable: fall through */
      }

   return Str0(LOC);
}
//----------------------------------------------------------------------------
