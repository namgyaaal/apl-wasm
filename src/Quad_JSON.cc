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

#include <errno.h>
#include <string.h>

#include <sys/fcntl.h>   // for O_RDONLY
#include <sys/stat.h>

#include "Avec.hh"
#include "Quad_FIO.hh"
#include "Quad_JSON.hh"
#include "Value.hh"
#include "Workspace.hh"

Quad_JSON  Quad_JSON::fun;

//============================================================================
Token
Quad_JSON::eval_AB(Value_P A, Value_P B) const
{
   if (A->get_rank() > 0)   RANK_ERROR;

const int function_number = A->get_cfirst().get_int_value();
   switch(function_number)
      {
        case 0:   // same as monadic ⎕JSON
             {
               return eval_B(B);
             }

         case 1:   // read and convert a JSOM file
              {
                return convert_file(*B);
              }

         case 2:   // read and convert a JSOM file (unsorted)
              {
                return Token(TOK_APL_VALUE1, APL_to_JSON(*B, false));
              }

         case 3:   // read and convert a JSOM file (sorted)
              {
                return Token(TOK_APL_VALUE1, APL_to_JSON(*B, true));
              }

      }

   MORE_ERROR() << "A ⎕JSON B: Bad function number A=" << function_number;
   DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
Token
Quad_JSON::convert_file(const Value & B) const
{
const UCS_string filename_ucs(B);
const UTF8_string filename_utf(filename_ucs);

   errno = 0;
const int fd = open(filename_utf.c_str(), O_RDONLY);
  if (fd == -1)
      {
        MORE_ERROR() << "1 ⎕JSON B: error reading " << B
                     << ": " << strerror(errno);
       DOMAIN_ERROR;
      }

struct stat st;
   if (fstat(fd, &st))
      {
        MORE_ERROR() << "1 ⎕JSON B: error in fstat(" << B
                     << "): " << strerror(errno);
        ::close(fd);
        DOMAIN_ERROR;
      }

UTF8 * buffer = new UTF8[st.st_size];
   if (buffer == 0)
      {
        ::close(fd);
        WS_FULL;
      }

const ssize_t bytes_read = read(fd, buffer, st.st_size);
   if (bytes_read != st.st_size)
      {
        MORE_ERROR() << "1 ⎕JSON B: error in reading " << B
                     << "): " << strerror(errno);
        ::close(fd);
        DOMAIN_ERROR;
        ::close(fd);
        delete [] buffer;
      }

const UTF8_string json_string_utf8(buffer, bytes_read);
   delete[] buffer;
   ::close(fd);

UCS_string json_string_ucs(json_string_utf8);
Value_P json_string_value(json_string_ucs, LOC);

   return eval_B(json_string_value);
}
//----------------------------------------------------------------------------
Token
Quad_JSON::eval_B(Value_P B) const
{
   if (B->get_rank() != 1)   RANK_ERROR;

Value_P Z = JSON_to_APL(*B);
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Value_P
Quad_JSON::APL_to_JSON(const Value & B, bool sorted)
{
UCS_string ucs_Z;
   ucs_Z.reserve(2*B.get_enlist_count());
   APL_to_JSON_string(ucs_Z, B, 0, sorted);
   if (ucs_Z.size() == 0)   DOMAIN_ERROR;

Value_P Z(ucs_Z, LOC);
   return Z;
}
//----------------------------------------------------------------------------
void
Quad_JSON::APL_to_JSON_string(UCS_string & result, const Cell & cell,
                              bool level, bool sorted)
{
   if (cell.is_integer_cell())
      {
        char cc[40];
        SPRINTF(cc, "%lld", static_cast<long long>(cell.get_int_value()));
        result.append_UTF8(cc);
        return;
      }

   if (cell.is_float_cell())
      {
        char cc[40];
        SPRINTF(cc, "%lg", cell.get_real_value());
        result.append_UTF8(cc);
        return;
      }

   if (cell.is_complex_cell())
      {
        char cc[50];
        SPRINTF(cc, "%lgJ%lg", cell.get_real_value(), cell.get_imag_value());
        result.append_UTF8(cc);
        return;
      }

   if (!cell.is_pointer_cell())
      {
        MORE_ERROR() << "2 ⎕JSON B: Unexpected/unsupported Celltype "
                     << Cell::get_cell_type_name(cell.get_cell_type());
        DOMAIN_ERROR;
      }

   // at this point the cell should end up as char vector. Determint its depth.
   //
const Value * Z = cell.get_pointer_value().get();
   if (!(Z->get_cfirst().is_pointer_cell() && Z->is_scalar()))
      {
        APL_to_JSON_string(result, *Z, level, sorted);
        return;
      }

   Z = Z->get_cfirst().get_pointer_value().get();
   if (!Z->is_char_vector())
      {
FIXME;
      }

const UCS_string lit_ucs(*Z);
   if (lit_ucs.compare(UCS_ASCII_string("null"))  == COMP_EQ ||
       lit_ucs.compare(UCS_ASCII_string("true"))  == COMP_EQ ||
       lit_ucs.compare(UCS_ASCII_string("false")) == COMP_EQ)
      {
        result.append(lit_ucs);
        return;
      }

   MORE_ERROR() << "2 ⎕JSON B: bad JSON literal ⊂'" << lit_ucs
                << "'. Expecting ⊂'true', ⊂'false', or ⊂'null'";
   result.clear();   // indicate error
}
//----------------------------------------------------------------------------
void
Quad_JSON::APL_to_JSON_string(UCS_string & result, const Value & B,
                              bool level, bool sorted)
{
   if (B.is_scalar())   // number or literal
      {
        APL_to_JSON_string(result, B.get_cfirst(), level, sorted);
        return;
      }

   if (B.is_char_vector())
      {
        const UCS_string ucs_string(B);
        result += UNI_DOUBLE_QUOTE;
        loop(u, ucs_string.size())
            {
              switch(const Unicode uni = ucs_string[u])
                 {
                   case UNI_BS:           result.append_UTF8("\\b");   break;
                   case UNI_HT:           result.append_UTF8("\\t");   break;
                   case UNI_LF:           result.append_UTF8("\\n");   break;
                   case UNI_FF:           result.append_UTF8("\\f");   break;
                   case UNI_CR:           result.append_UTF8("\\r");   break;
                   case UNI_DOUBLE_QUOTE: result.append_UTF8("\\\"");   break;

                   default: if (uni < UNI_SPACE)
                               {
                                 char cc[10];
                                 SPRINTF(cc, "\\u%4.4X", int(uni));
                                 result.append_UTF8(cc);
                               }
                            else
                               {
                                 result += uni;
                               }
                 }
            }
        result += UNI_DOUBLE_QUOTE;
        return;
      }

   if (B.get_rank() == 1)   // JSON array
      {
        UCS_string array(UTF8_string("[ "));
        const ShapeItem ec = B.element_count();
        loop(e, ec)
            {
              if (e)   array.append_ASCII(", ");
              if (array.size() > 60)
                 {
                   array.back() = UNI_LF;
                   result.append(array);
                   array = UCS_string(2*level + 2, UNI_SPACE);
                 }

              APL_to_JSON_string(array, B.get_cravel(e), level + 1, sorted);
              if (array.size() == 0)   // error in APL_to_JSON_string()
                 {
                   result.clear();   // indicate error
                   return;
                 }
            }
        array.append_UTF8(" ]");
        result.append(array);
        return;
      }

   if (B.is_structured())   // JSON object
      {
        std::basic_string<ShapeItem> member_indices;
        B.used_members(member_indices, sorted);

        loop(m, member_indices.size())
           {
             const Cell & member_name = B.get_cravel(2*member_indices[m]);
             const Cell & member_data = B.get_cravel(2*member_indices[m] + 1);

             result.append(UCS_string(2*level, UNI_SPACE));   // level indent
             if (m)   result.append_UTF8("  \"");
             else     result.append_UTF8("{ \"");
             UCS_string member(*member_name.get_pointer_value());
             result.append(member_name);
             result.append_UTF8("\": ");
             APL_to_JSON_string(result, member_data, level + 1, sorted);
             if (result.size() == 0)   return;   // error converting member_data
             if (size_t(m) < (member_indices.size() - 1))
                {
                  result += UNI_COMMA;
                  result += UNI_LF;
                }
             else
                {
                  result += UNI_SPACE;
                }
           }

        result.append_UTF8("}");
        return;
      }

   // unexpected rank for a JSON value
   //
   MORE_ERROR() << "⎕JSON B: bad rank " << B.get_rank();
   result.clear();   // indicate error
}
//----------------------------------------------------------------------------
size_t
Quad_JSON::skip_string(const UCS_string & ucs_B, ShapeItem & b)
{
const ShapeItem B0 = b++;   // the leading "
   Assert(ucs_B[B0] == UNI_DOUBLE_QUOTE);

ShapeItem content_len = 0;
   for (; b < ucs_B.size(); ++b)
       {
         const Unicode uni = ucs_B[b];
         if (uni == UNI_DOUBLE_QUOTE)   return content_len;

        ++content_len;
        if (uni == UNI_BACKSLASH)   // skip the escaped part...
           {
             if (ucs_B[b + 1] == UNI_u)   // \uUUUU
                {
                  const Unicode u1 = decode_UUUU(ucs_B, b);
                  if (u1 == Unicode_0)   // decode_UUUU() failed
                     {
                       MORE_ERROR() << "⎕JSON B: bad escape sequence "
                                    << ucs_B[b    ] << ucs_B[b + 1]
                                    << ucs_B[b + 2] << ucs_B[b + 3]
                                    << ucs_B[b + 4] << ucs_B[b + 5]
                                    << " at " << b << "↓B";
                       DOMAIN_ERROR;
                     }

                  if (is_high_surrogate(u1))
                     {
                       const Unicode u2 = decode_UUUU(ucs_B, b + 6);
                       if (u2 == Unicode_0)   // decode_UUUU() failed
                          {
                            MORE_ERROR() << "⎕JSON B: bad escape sequence "
                                         << ucs_B[b +  6] << ucs_B[b +  7]
                                         << ucs_B[b +  8] << ucs_B[b +  9]
                                         << ucs_B[b + 10] << ucs_B[b + 11]
                                         << " at " << (b + 6) << "↓B";
                            DOMAIN_ERROR;
                          }
                       b += 11;
                     }
                  else   // u1 is not a high surrogate (normal \uUUUU)
                     {
                       b += 5;
                     }
                }
             else                     // 1-character escape
                {
                  ++b;   // the escaped character
                }
           }
       }

   // end of string reached without seeing "
   //
   MORE_ERROR() <<
   "⎕JSON B: No string end for string starting at " << B0 << "↓B";
   DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
size_t
Quad_JSON::number_len(const UCS_string & ucs_B, ShapeItem b)
{
const ShapeItem B0 = b;

   // leading sign
   if (ucs_B[b] == UNI_MINUS)          ++b;
   else if (ucs_B[b] == UNI_OVERBAR)   ++b;

   // mandatory integral part
   while (uint32_t(ucs_B[b] - UNI_0) < 10)   ++b;

   if (ucs_B[b] == UNI_FULLSTOP)   // optional fractional part
      {
        ++b;   // skip UNI_FULLSTOP
       while (uint32_t(ucs_B[b] - UNI_0) < 10)   ++b;
      }

   if (ucs_B[b] == UNI_E || ucs_B[b] == UNI_e)   // optional exponent
      {
        ++b;   // skip UNI_E / UNI_e
        if (ucs_B[b] == UNI_MINUS)          ++b;
        else if (ucs_B[b] == UNI_OVERBAR)   ++b;
       while (uint32_t(ucs_B[b] - UNI_0) < 10)   ++b;
      }

   return b - B0;
}
//----------------------------------------------------------------------------
Value_P
Quad_JSON::JSON_to_APL(const Value & B)
{
const ShapeItem len_B = B.element_count();

UCS_string ucs_B;
   ucs_B.reserve(len_B + 1);
   loop(b, len_B)   ucs_B += B.get_cravel(b).get_char_value();
   ucs_B += Unicode_0;   // 0-terminate ucs_B to avoid too many length checks

   // tokenize ucs_B
   //
std::basic_string<ShapeItem> tokens_B;
   tokens_B.reserve(10 + len_B/2);

   loop(b, ucs_B.size())
       {
         const Unicode uni = ucs_B[b];
         if (uni <= ' ')   continue;   // skip leading whitespace

         tokens_B.push_back(b);
         switch(uni)
            {
              case UNI_DOUBLE_QUOTE:
                   skip_string(ucs_B, b);
                   continue;

              case UNI_COMMA:
              case UNI_COLON:
              case UNI_L_BRACK:
              case UNI_R_BRACK:
              case UNI_L_CURLY:
              case UNI_R_CURLY: continue;       // the six structural chars

              case UNI_f: b += 4;   continue;   // literal false
              case UNI_n:                       // literal true
              case UNI_t: b += 3;   continue;   // literal null

              case UNI_MINUS:
              case UNI_OVERBAR:
              case UNI_FULLSTOP:
              case UNI_0 ... UNI_9:
                   b += number_len(ucs_B, b) - 1;
                   continue;

              default:
                   MORE_ERROR() << "⎕JSON B: Got '" << uni <<
                   "' when expecting a JSON token at " << b << "↓B";
                   DOMAIN_ERROR;
                   MORE_ERROR() << "⎕JSON B: Got '" << uni <<
                   "' when expecting a JSON token at " << b << "↓B";
                   DOMAIN_ERROR;
            }
       }

Value_P Z(LOC);
size_t token0 = 0;
   parse_value(*Z, ucs_B, tokens_B, token0);
   Z->check_value(LOC);

   if (token0 != tokens_B.size())
      {
        MORE_ERROR() <<
        "⎕JSON B: there were extra token in B (tokenized: " << tokens_B.size()
        << ", but processed: " << token0 <<
        ").\n    The JSON string must be one serialized value.";
        LENGTH_ERROR;
      }

   if (Z->is_simple_scalar())
      {
        Assert(Z->get_cfirst().is_numeric());
        return Z;   // number
      }

   Assert(Z->is_scalar());
   Assert(Z->get_cfirst().is_pointer_cell());
   return Z->get_cfirst().get_pointer_value();
}
//----------------------------------------------------------------------------
void
Quad_JSON::parse_value(Value & Z, const UCS_string & ucs_B,
                      const std::basic_string<ShapeItem> & tokens_B, size_t & token0)
{
const ShapeItem b = tokens_B.at(token0);
   switch(ucs_B[b])
      {
        // unexpected token...
        //
        case UNI_COMMA:
             MORE_ERROR() << "⎕JSON: Got ',' when expecting a value";
             DOMAIN_ERROR;

        case UNI_COLON:
             MORE_ERROR() << "⎕JSON: Got ':' when expecting a value";
             DOMAIN_ERROR;

        case UNI_R_BRACK:
             MORE_ERROR() << "⎕JSON: Got ']' when expecting a value";
             DOMAIN_ERROR;

        case UNI_R_CURLY:
             MORE_ERROR() << "⎕JSON: Got '}' when expecting a value";
             DOMAIN_ERROR;

        // single token (we increment token0)...
        //
        case '-':
        case UNI_OVERBAR:
        case UNI_FULLSTOP:
        case '0' ... '9': parse_number(Z, ucs_B, b);       ++token0;   break;
        case UNI_DOUBLE_QUOTE: parse_string(Z, ucs_B, b);  ++token0;   break;
        case UNI_f: parse_literal(Z, ucs_B, b, "false");   ++token0;   break;
        case UNI_n: parse_literal(Z, ucs_B, b, "null");    ++token0;   break;
        case UNI_t: parse_literal(Z, ucs_B, b, "true");    ++token0;   break;

        // multi token (they increment token0)...
        //
        case UNI_L_BRACK: parse_array (Z, ucs_B, tokens_B,  token0);   break;
        case UNI_L_CURLY: parse_object(Z, ucs_B, tokens_B,  token0);   break;

        default: FIXME;
      }
}
//----------------------------------------------------------------------------
void
Quad_JSON::parse_array(Value & Z, const UCS_string & ucs_B,
                       const std::basic_string<ShapeItem> & tokens_B,
                       size_t & token0)
{
size_t token_from = token0;
   Assert(ucs_B[tokens_B[token_from]] == UNI_L_BRACK);   // [

const size_t commas = comma_count(ucs_B, tokens_B, token0);
   Assert(ucs_B[tokens_B[token0]] == UNI_R_BRACK);   // always ]

   ++token_from;   // skip [
   if (commas == 0)   // [ ] or [ item ]
      {
        if ((token0 - token_from) == 0)   // [ ]
           {
             // CERR << "empty ARRAY" << std::endl;
             Value_P Zsub = Idx0(LOC);
             Z.next_ravel_Pointer(Zsub.get());
           }
        else
           {
             // CERR << "One element ARRAY" << std::endl;
             Value_P Zsub(1, LOC);
             parse_value(*Zsub, ucs_B, tokens_B, token_from);
             Zsub->check_value(LOC);
             Z.next_ravel_Value(Zsub.get());
           }
        ++token_from;   // skip ]
      }
   else               // [ item , item... ]
      {
        const size_t len = commas + 1;
        // CERR << "ARRAY with " << len << " elements" << endl;

        Value_P Zsub(len, LOC);
        loop(l, len)
            {
              parse_value(*Zsub, ucs_B, tokens_B, token_from);
              const Unicode uni = ucs_B[tokens_B[token_from]];
              if (uni == UNI_COMMA)           ++token_from;
              else if (uni == UNI_R_BRACK)    ++token_from;
              else
                 {
                   MORE_ERROR() << "⎕JSON B: Got '" << uni
                                << "' when expecting ',' or ']' at "
                                <<  token_from << "↓B";
                   DOMAIN_ERROR;
                 }
            }
        Zsub->check_value(LOC);
        Z.next_ravel_Value(Zsub.get());
      }

   ++token0;   // skip final ]
   Assert(token0 == token_from);
}
//----------------------------------------------------------------------------
void
Quad_JSON::parse_object(Value & Z, const UCS_string & ucs_B,
                        const std::basic_string<ShapeItem> & tokens_B,
                        size_t & token0)
{
size_t token_from = token0;
   Assert(ucs_B[tokens_B[token_from]] == UNI_L_CURLY);   // {

const size_t commas = comma_count(ucs_B, tokens_B, token0);
   Assert(ucs_B[tokens_B[token0]] == UNI_R_CURLY);   // always }

Value_P assoc_array = EmptyStruct(LOC);
   Z.next_ravel_Pointer(assoc_array.get());

   ++token_from;   // skip {
   if (commas == 0)   // { } or { 'name' : value }
      {
        if ((token0 - token_from) == 0)   // { }
           {
             // CERR << "empty OBJECT" << std::endl;
             ++token_from;   // skip }
           }
        else
           {
             // CERR << "One element OBJECT" << std::endl;
             parse_object_member(*assoc_array, ucs_B, tokens_B, token_from);
           }
      }
   else               // { 'name' : value , 'name' : value... }
      {
        const size_t items = commas + 1;
        // CERR << "OBJECT with " << items << " elements" << endl;

        loop(it, items)
            {
              parse_object_member(*assoc_array, ucs_B, tokens_B, token_from);
            }
      }

   ++token0;   // skip final }
   Assert(token0 == token_from);
}
//----------------------------------------------------------------------------
void
Quad_JSON::parse_object_member(Value & Z, const UCS_string & ucs_B,
                               const std::basic_string<ShapeItem> & tokens_B,
                               size_t & token_from)
{
const size_t B_start =  tokens_B[token_from];
   if (ucs_B[B_start] != UNI_DOUBLE_QUOTE)
      {
        MORE_ERROR() << "⎕JSON B: Got '" << ucs_B[B_start] <<
        "' when expecting (double-quoted) object member name at " <<
        B_start << "↓B";
        DOMAIN_ERROR;
      }
   ++token_from;   // skip "

   // parse "member-name" :
   //
UCS_string member_name;
   {
     const ShapeItem ucs_B_name = B_start + 1;

     const ShapeItem ucs_B_colon = tokens_B[token_from];
     if (ucs_B[ucs_B_colon] != UNI_COLON)
        {
          MORE_ERROR() <<
          "⎕JSON B: Got '" << ucs_B[ucs_B_colon] <<
          "' when expecting ':' after object member name at " <<
          B_start << "↓B";
          DOMAIN_ERROR;
        }
     ++token_from;   // skip :

     // find trailing " of "member-name" :  by searching backwards from ':'
     //
     ShapeItem name_len = ucs_B_colon - ucs_B_name;
     while (name_len && ucs_B[ucs_B_name + name_len - 1] != UNI_DOUBLE_QUOTE)
           --name_len;
     --name_len;   // skip the trailing "
     if (name_len < 1)
        {
          FIXME;   // since skip_string() should have failed
        }

     member_name = UCS_string(ucs_B, ucs_B_name, name_len);
   }

   // check that member_name does not yet exist in Z.
   //
   if (Z.get_member_data(member_name))
      {
        // member name exists already
        //
        MORE_ERROR() << "⎕JSON: duplicate member name '" << member_name
                     << "' at " << B_start << "↓B";
        DOMAIN_ERROR;
      }

   // parse the member value
   {
     Value_P Zsub(LOC);
     parse_value(*Zsub, ucs_B, tokens_B, token_from);
     Zsub->check_value(LOC);

     Cell * member_data = Z.get_new_member(member_name);
     member_data->init(Zsub->get_cfirst(), Z, LOC);
   }

   // check member-seperator (or end of object).
   {
     const ShapeItem ucs_B_sepa = tokens_B[token_from++];   // skip , or }
     const Unicode sepa = ucs_B[ucs_B_sepa];
     if (sepa != UNI_COMMA && sepa != UNI_R_CURLY)
        {
          MORE_ERROR() << "⎕JSON B: Got '" << sepa <<
               "' when expecting ',' or '}' after \"" << member_name
               << "\" : at " << B_start << "↓B";
          DOMAIN_ERROR;
        }
   }
}
//----------------------------------------------------------------------------
void
Quad_JSON::parse_number(Value & Z, const UCS_string & ucs_B, ShapeItem b)
{
   enum { MAX_NUMLEN = 90 };
char cc[MAX_NUMLEN + 10];
size_t cc_len = 0;
bool need_fract = false;
bool have_expo = false;
double dval = 0;

   if (ucs_B[b] == UNI_MINUS || ucs_B[b] == UNI_OVERBAR)
      { ++b;   cc[cc_len++] = '-'; }

   // copy integer part to cc...
   //
   while (ucs_B[b] >= UNI_0 && ucs_B[b] <= UNI_9)
         {
           if (cc_len >= MAX_NUMLEN)   goto number_too_long;
           cc[cc_len++] = ucs_B[b++];
         }

   // maybe copy fractional part to cc...
   //
   if (ucs_B[b] == UNI_FULLSTOP)   // fractional part
      {
        need_fract = true;   // force FloatCell
        if (cc_len >= MAX_NUMLEN)   goto number_too_long;
        cc[cc_len++] = '.';
        ++b;
        while (ucs_B[b] >= UNI_0 && ucs_B[b] <= UNI_9)
              {
                if (cc_len >= MAX_NUMLEN)   goto number_too_long;
                cc[cc_len++] = ucs_B[b++];
              }
      }

   // maybe copy exponent part to cc...
   //
   if (ucs_B[b] == UNI_E || ucs_B[b] == UNI_e)   // exponent part
      {
        have_expo = true;
        if (cc_len >= MAX_NUMLEN)   goto number_too_long;
        cc[cc_len++] = 'e';
        ++b;
        if (ucs_B[b] == UNI_MINUS || ucs_B[b] == UNI_OVERBAR)   // negative expo
           {
             need_fract = true;
             ++b;   cc[cc_len++] = '-'; }
             while (ucs_B[b] >= UNI_0 && ucs_B[b] <= UNI_9)
                   {
                     if (cc_len >= MAX_NUMLEN)   goto number_too_long;
                      cc[cc_len++] = ucs_B[b++];
                   }
      }
   cc[cc_len++] = 0;

   // always scan the double value
   //
   dval = strtod(cc, 0);

   if (dval > LARGE_INT || dval < SMALL_INT)   need_fract = true;

   if (need_fract)
      {
        Z.next_ravel_Float(dval);
        return;
      }
   else if (have_expo)   // so strtoll() won't work
      {
        if (dval < 0)   Z.next_ravel_Int(dval - 0.5);   // round negative → 0
        else            Z.next_ravel_Int(dval + 0.5);   // round positive → 0
        return;
      }
   else
      {
        const long long int ival = strtoll(cc, 0, 10);
        Z.next_ravel_Int(ival);
        return;
      }

number_too_long:
   MORE_ERROR() << "⎕JSON B: number too long at " << b
                << "↓B (max. length is " << MAX_NUMLEN << ")";
   DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
void
Quad_JSON::parse_string(Value & Z, const UCS_string & ucs_B, ShapeItem b)
{
ShapeItem bb = b + 1;   // NOTE: skip_string() will increment b
const ShapeItem content_len = skip_string(ucs_B, b);

Value_P Zsub(content_len, LOC);
   loop(l, content_len)
       {
         Unicode uni = ucs_B[bb++];
         if (uni == UNI_BACKSLASH)
            {
              Unicode surr = Unicode_0;

              uni = ucs_B[bb++];  // default: escaped = character itself
              switch(uni)
                 {
                   case UNI_b: uni = UNI_BS;   break;   // \b
                   case UNI_f: uni = UNI_FF;   break;   // \f
                   case UNI_n: uni = UNI_LF;   break;   // \n
                   case UNI_r: uni = UNI_CR;   break;   // \r
                   case UNI_t: uni = UNI_HT;   break;   // \t
                   case UNI_u:                          // \uUUUU
                        uni  = decode_UUUU(ucs_B, bb - 2);
                        surr = decode_UUUU(ucs_B, bb + 4);
                        if (is_high_surrogate(uni) && is_low_surrogate(surr))
                           {
                             uni = Unicode(0x10000 + (surr & 0x03FF)
                                                   + ((uni  & 0x03FF) << 10));
                             bb += 10;
                           }
                        else if (is_high_surrogate(uni))
                           {
                             MORE_ERROR() <<
                             "⎕JSON B: No low surrogate at " << bb << "↓B";
                             DOMAIN_ERROR;
                           }
                        else if (uni == Unicode_0)   // decode_UUUU() failed
                           {
                             FIXME;   // since skip_string() should have failed
                           }
                        else   // normal (non-surrogate) \uUUUU
                           {
                             bb += 4;
                           }
                        break;   // case UNI_u

                   default: break;
                 }
            }

         Zsub->next_ravel_Char(uni);
       }

   Zsub->check_value(LOC);
   Z.next_ravel_Pointer(Zsub.get());
}
//----------------------------------------------------------------------------
Unicode
Quad_JSON::decode_UUUU(const UCS_string & ucs_B, ShapeItem b)
{
   if (ucs_B[b++] != UNI_BACKSLASH)   return Unicode_0;
   if (ucs_B[b++] != UNI_u)           return Unicode_0;

char cc[5];
   cc[0] = ucs_B[b++];
   cc[1] = ucs_B[b++];
   cc[2] = ucs_B[b++];
   cc[3] = ucs_B[b++];
   cc[4] = 0;

   return Unicode(strtoll(cc, 0, 16));
}
//----------------------------------------------------------------------------
void
Quad_JSON::parse_literal(Value & Z, const UCS_string & ucs_B,
                         ShapeItem b, const char * expected_literal)
{
const size_t len = strlen(expected_literal);
   loop(l, len)
       {
         if (ucs_B[b + l] != expected_literal[l])
            {
              MORE_ERROR() << "⎕JSON B: misspelled JSON literal "
                           << UCS_string(ucs_B, b, len) << " at " << b
                           << "↓B (expecting " << expected_literal << ")";
              DOMAIN_ERROR;
            }
       }

Value_P Zsubsub(len, LOC);
   loop(l, len)
       Zsubsub->next_ravel_Char(Unicode(expected_literal[l]));
   Zsubsub->check_value(LOC);

Value_P Zsub(LOC);
   Zsub->next_ravel_Pointer(Zsubsub.get());
   Zsub->check_value(LOC);

   Z.next_ravel_Pointer(Zsub.get());
}
//----------------------------------------------------------------------------
size_t
Quad_JSON::comma_count(const UCS_string & ucs_B,
                       const std::basic_string<ShapeItem> & tokens_B,
                       size_t & token0)
{
   // ucs_B is the JSON text string being parsed,
   // tokens_B are the start positions (in ucs_B) of the tokenized ucs_B,
   // token0 is the current token in tokens_B which is either [ (start of a
   // JSON array) or { (start of a JSON object).
   //
const Unicode start = ucs_B[tokens_B[token0]];   // either [ or {/
Unicode end;
   if      (start == UNI_L_BRACK)   end = UNI_R_BRACK;
   else if (start == UNI_L_CURLY)   end = UNI_R_CURLY;
   else FIXME;

UCS_string stack(end);   // a stack of ] amd } to track nested [...] and {...}
size_t commas = 0;       // the numebr of (top-level-) commas
bool expect_comma = false;
bool expect_colon = true;

   for (++token0; token0 < tokens_B.size(); ++token0)
       {
         const Unicode uni = ucs_B[tokens_B[token0]];
         switch(uni)
               {
                 case UNI_L_BRACK:            // [
                      stack += UNI_R_BRACK;   // push ]
                      continue;

                 case UNI_L_CURLY:            //      {
                      stack += UNI_R_CURLY;   // push }
                      continue;

                 case UNI_COMMA:
                      if (stack.size() > 1)   continue;
                      if (!expect_comma)
                         {
                           MORE_ERROR() << "⎕JSON B: Got unexpected ',' at "
                                        <<  tokens_B[token0] << "↓B";
                           DOMAIN_ERROR;
                         }

                      ++commas;
                      expect_comma = false;
                      expect_colon = true;
                      continue;

                 case UNI_COLON:
                      if (stack.size() > 1)   continue;
                      if (!expect_comma)
                         {
                           MORE_ERROR() << "⎕JSON B: Got unexpected ':' at "
                                        <<  tokens_B[token0] << "↓B";
                           DOMAIN_ERROR;
                         }

                      expect_comma = false;
                      if (!expect_colon)
                         {
                           MORE_ERROR() << "⎕JSON B: Got ':' "
                                           "instead of ',' at "
                                        <<  tokens_B[token0] << "↓B";
                           DOMAIN_ERROR;
                         }
                      expect_colon = false;
                      continue;

                 case UNI_R_BRACK:
                      //
                      // UNI_R_BRACK must match stack.back()
                      //
                      if (stack.back() != UNI_R_BRACK)
                         {
                           MORE_ERROR() << "Mismatch ]";
                           DOMAIN_ERROR;
                         }
                      stack.pop_back();
                      if (stack.size() == 0)   return commas;
                      expect_comma = true;
                      continue;

                 case UNI_R_CURLY:
                      //
                      // UNI_R_CURLY must match stack.back()
                      //
                      if (stack.back() != UNI_R_CURLY)
                         {
                           MORE_ERROR() << "Mismatch ]";
                           DOMAIN_ERROR;
                         }
                      stack.pop_back();
                      if (stack.size() == 0)   return commas;
                      expect_comma = true;
                      continue;

                 default:
                      if (stack.size() > 1)   continue;   // not of interest
                      if (expect_comma)
                         {
                           MORE_ERROR() << "⎕JSON B: Got '" << uni
                                        << "' when expecting ',' at "
                                        <<  tokens_B[token0] << "↓B";
                           DOMAIN_ERROR;
                         }
                      expect_comma = true;
               }

       }

   MORE_ERROR() << "⎕JSON B: No matching " << end << " for " << start
                << " at " << tokens_B[token0] << "↓B ";
   DOMAIN_ERROR;
}
//============================================================================
