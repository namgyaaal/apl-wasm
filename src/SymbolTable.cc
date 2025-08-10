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

#include <string.h>
#include <strings.h>

#include "CDR.hh"
#include "Command.hh"
#include "Function.hh"
#include "IndexExpr.hh"
#include "IntCell.hh"
#include "Output.hh"
#include "PrintOperator.hh"
#include "Symbol.hh"
#include "SymbolTable.hh"
#include "SystemVariable.hh"
#include "Tokenizer.hh"
#include "UserFunction.hh"
#include "Value.hh"
#include "Workspace.hh"

#include "Workspace.icc"

//----------------------------------------------------------------------------
Symbol *
SymbolTable::lookup_symbol(const UCS_string & sym_name)
{
   if (sym_name.size() == 0)   return 0;
   if (Avec::is_quad(sym_name[0]))   // should not be called for ⎕xx
      {
        CERR << "Symbol is: '" << sym_name << "' at " << LOC << endl;
        FIXME;
      }

const uint32_t hash = compute_hash(sym_name);

   if (symbol_table[hash] == 0)   // unused hash value
      {
        // sym_name is the first symbol with this hash.
        // create a new symbol, insert it into symbol_table, and return it.
        //
        Log(LOG_SYMBOL_lookup_symbol)
           {
             CERR << "Symbol " << sym_name << " has hash " << HEX(hash) << endl;
           }

        Symbol * new_symbol = new Symbol(sym_name, ID_USER_SYMBOL);
        symbol_table[hash] =  new_symbol;
        return new_symbol;
      }

   // One or more symbols with this hash already exist. The number of symbols
   // with the same name is usually very short, so we walk the list twice.
   // The first walk checks for a (possibly erased) symbol with the
   // same name and return it if found.
   //
   for (Symbol * sym = symbol_table[hash]; sym; sym = sym->next)
       {
         if (sym->equal(sym_name))   // found
            {
              return sym;
            }
       }

   // no symbol with name sym_name exists. The second walk:
   //
Symbol * sym = new Symbol(sym_name, ID_USER_SYMBOL);
   add_symbol(sym);
   return sym;
}
//----------------------------------------------------------------------------
UCS_string
SymbolTable::find_lambda_name(const UserFunction * lambda)
{
   loop(s, SYMBOL_HASH_TABLE_SIZE)
       {
         for (Symbol * sym = symbol_table[s]; sym; sym = sym->next)
             {
               if (sym->is_erased())   continue;
               if (sym->get_exec_ufun_depth(lambda) != -1)
                  return sym->get_name();
             }
       }

   return UCS_string();
}
//----------------------------------------------------------------------------
ostream &
SymbolTable::list_symbol(ostream & out, const UCS_string & buf1) const
{
UCS_string buf(buf1);
   buf.remove_leading_and_trailing_whitespaces();
   if (buf1.size() == 1)   switch(buf1[0])
      {
        case UNI_ALPHA:  return Workspace::get_v_ALPHA().print_verbose(out);
        case UNI_ALPHA_UNDERBAR:
                         return Workspace::get_v_ALPHA_U().print_verbose(out);
        case UNI_OMEGA:  return Workspace::get_v_OMEGA().print_verbose(out);
        case UNI_CHI:    return Workspace::get_v_CHI().print_verbose(out);
        case UNI_OMEGA_UNDERBAR:
                         return Workspace::get_v_OMEGA_U().print_verbose(out);
        case UNI_LAMBDA: return Workspace::get_v_LAMBDA().print_verbose(out);
        default:         break;
      }

const Symbol * sym = Workspace::lookup_existing_symbol(buf);

   if (sym)   return sym->print_verbose(out);

   if (buf1[0] == UNI_Quad_Quad)   return out << "System Function" << endl;

   return out << "no symbol '" << buf1 << "'" << endl;
}
//----------------------------------------------------------------------------
void
SymbolTable::list(ostream & out, ListCategory which, UCS_string from_to) const
{
UCS_string from;
UCS_string to;
   {
     const bool bad_from_to = Command::parse_from_to(from, to, from_to);
     if (bad_from_to)
        {
          CERR << "bad range argument" << endl;
          MORE_ERROR() << "bad range argument " << from_to
               << ", expecting from-to";
          return;
        }
   }

   // put those symbols into 'list' that satisfy 'which'
   //
std::basic_string<Symbol *> list;
int symbol_count = 0;
   loop(s, SYMBOL_HASH_TABLE_SIZE)
       {
         for (Symbol * sym = symbol_table[s]; sym; sym = sym->next)
             {
               if (sym->get_name()[0] == UNI_MUE)   continue;   // macro
               ++symbol_count;

               // check range
               //
               if (from.size() && sym->get_name().lexical_before(from))
                  {
                    // CERR << "'" << sym->get_name() << "' comes before '"
                    //       << from << "'" << endl;
                    continue;
                  }
               if (to.size() && to.lexical_before(sym->get_name()))
                  {
                    // CERR << "'" << to << "' comes before '"
                    //      << sym->get_name() << "'" << endl;
                    continue;
                  }

               if (sym->value_stack.size() == 0)
                  {
                    if (which == LIST_ALL)   list.push_back(sym);
                    continue;
                  }

               if (sym->is_erased() && !(which & LIST_ERASED))   continue;

               const NameClass nc = sym->value_stack.back().get_NC();
               if (((nc == NC_VARIABLE)         && (which & LIST_VARS))    ||
                   ((nc == NC_FUNCTION)         && (which & LIST_FUNS))    ||
                   ((nc == NC_OPERATOR)         && (which & LIST_OPERS))   ||
                   ((nc == NC_LABEL)            && (which & LIST_LABELS))  ||
                   ((nc == NC_LABEL)            && (which & LIST_VARS))    ||
                   ((nc == NC_INVALID)          && (which & LIST_INVALID)) ||
                   ((nc == NC_UNUSED_USER_NAME) && (which & LIST_UNUSED)))
                   {
                     list.push_back(sym);
                   }
             }
       }

   if (which == LIST_NONE)   // )SYMBOLS: display total symbol count
      {
        // this could be:
        //
        // 1. )SYMBOLS or   (show symbol count)
        // 2. )SYMBOLS N    (set symbol count, ignored by GNU APL)
        //
        if (from_to.size())   return;   // case 2
        out << "IS " << symbol_count << endl;
        return;
      }

const int count = list.size();
UCS_string_vector names;
   loop(l, count)
      {
        UCS_string name = list[l]->get_name();
        if (which == LIST_NAMES)   // append .NC
           {
             name.append(UNI_FULLSTOP);
             name.append_number(list[l]->value_stack.back().get_NC());
           }
        names.push_back(name);
      }

   names.sort();

   // figure column widths
   //
   enum { tabsize = 4 };
std::basic_string<int> col_widths;
   names.compute_column_width(tabsize, col_widths);

   loop(c, count)
      {
        const size_t col = c % col_widths.size();
        out << names[c];
        if (col == (col_widths.size() - 1) || c == (count - 1))
           {
             // last column or last item: print newline
             //
             out << endl;
           }
        else
           {
             // intermediate column: print spaces
             //
             const int len = tabsize*col_widths[col] - names[c].size();
             Assert(len > 0);
             loop(l, len)   out << " ";
           }
      }
}
//----------------------------------------------------------------------------
void
SymbolTable::unmark_all_values() const
{
   loop(s, SYMBOL_HASH_TABLE_SIZE)
       {
         for (Symbol * sym = symbol_table[s]; sym; sym = sym->next)
             {
               sym->unmark_all_values();
             }
       }
}
//----------------------------------------------------------------------------
int
SymbolTable::show_owners(ostream & out, const Value & value) const
{
int count = 0;
   loop(s, SYMBOL_HASH_TABLE_SIZE)
       {
         for (Symbol * sym = symbol_table[s]; sym; sym = sym->next)
             {
               count += sym->show_owners(out, value);
             }
       }
   return count;
}
//----------------------------------------------------------------------------
void
SymbolTable::write_all_symbols(FILE * out, uint64_t & seq) const
{
   loop(s, SYMBOL_HASH_TABLE_SIZE)
       {
         for (Symbol * sym = symbol_table[s]; sym; sym = sym->next)
             {
               sym->write_OUT(out, seq);
             }
       }
}
//----------------------------------------------------------------------------
void
SymbolTable::erase_symbols(ostream & out, const UCS_string_vector & symbols)
{
size_t error_count = 0;
   loop(s, symbols.size())
       {
         if (erase_one_symbol(symbols[s]))
            {
              if (error_count == 0)   out << "NOT ERASED:";
              ++error_count;
              out << " " << symbols[s];
            }
       }

   if (error_count)   out << endl;
}
//----------------------------------------------------------------------------
void
SymbolTable::clear(ostream & out)
{
   // SymbolTable::clear() should only be called after Workspace::clear_SI()
   //
   Assert(Workspace::SI_entry_count() == 0);

   loop(hash, max_symbol_count)   clear_slot(out, hash);
}
//----------------------------------------------------------------------------
void
SymbolTable::clear_slot(ostream & out, int hash)
{
Symbol * sym = symbol_table[hash];
   if (sym == 0)   return;   // no symbol with this hash

   symbol_table[hash] = 0;

Symbol * next;   // the symbol after sym
   for (; sym; sym = next)
       {
         next = sym->next;

         // keep system-defined symbols
         //
         if (sym->is_user_defined() && sym->get_name()[0] != UNI_MUE)
            {
              sym->call_monitor_callback(SEV_ERASED);
              delete sym;
            }
         else
            {
              sym->next = symbol_table[hash];
              symbol_table[hash] = sym;
            }
       }
}
//----------------------------------------------------------------------------
bool
SymbolTable::erase_one_symbol(const UCS_string & sym)
{
   if (sym.contains(UNI_FULLSTOP))   // member access
      {
         const int result = Quad_EX::expunge(sym);
         return result != 1;
      }

Symbol * symbol = lookup_existing_symbol(sym);

   if (symbol == 0)
      {
        MORE_ERROR() << "Can't )ERASE symbol '" << sym << "': unknown symbol ";
        return true;
      }

   if (symbol->is_erased())
      {
        if (symbol->value_stack.size() == 1)
           {
             MORE_ERROR() << "Can't )ERASE symbol '"
                          << sym << "': already erased";
             return true;
           }
        else   // still holding a value
           {
             symbol->clear_vs();
             return false;
           }
      }

   if (symbol->value_stack.size() != 1)
      {
        MORE_ERROR() << "Can't )ERASE symbol '" << sym
                     << "': symbol is localized";
        return true;
      }

   if (Workspace::is_called(sym))
      {
        MORE_ERROR() << "Can't )ERASE symbol '" << sym
                     << "': symbol is called (is on SI)";
        return true;
      }

ValueStackItem & tos = symbol->value_stack[0];   // APL top-level

   switch(tos.get_NC())
      {
        case NC_LABEL:
             Assert(0 && "should not happen since stack height == 1");
             return true;

        case NC_VARIABLE:
             symbol->expunge();
             return false;

        case NC_UNUSED_USER_NAME:
             return true;

        case NC_FUNCTION:
        case NC_OPERATOR:
             Assert(tos.get_function());
             if (tos.get_function()->is_native())
                {
                  symbol->expunge();
                  return false;
                }

             if (tos.get_function()->is_lambda())
                {
                  symbol->expunge();
                  return false;
                }

             {
               const UserFunction * ufun = tos.get_function()->get_func_ufun();
               Assert(ufun);
               if (Workspace::oldest_exec(ufun))
                  {
                    MORE_ERROR() << "Can't )ERASE symbol '" << sym
                                 << "':  pushed on SI-stack";
                    return true;
                  }
             }

             delete tos.get_function();
             tos.clear_function();
             return false;

        default: break;
      }

    Assert(0 && "Bad name_class in SymbolTable::erase_one_symbol()");
   return true;
}
//----------------------------------------------------------------------------
std::basic_string<const Symbol *>
SymbolTable::get_all_symbols() const
{
std::basic_string<const Symbol *> ret;
   ret.reserve(1000);

   loop(hash, SYMBOL_HASH_TABLE_SIZE)
      {
        for (const Symbol * sym = symbol_table[hash]; sym; sym = sym->next)
            {
              ret.push_back(sym);
            }
      }

   return ret;
}
//----------------------------------------------------------------------------
void
SymbolTable::dump(ostream & out, int & fcount, int & vcount) const
{
std::basic_string<const Symbol *> symbols;
   loop(hash, SYMBOL_HASH_TABLE_SIZE)
      {
        for (const Symbol * sym = symbol_table[hash]; sym; sym = sym->next)
            {
              if (sym->is_erased())              continue;
              if (sym->value_stack_size() < 1)   continue;
              symbols.push_back(sym);
            }
      }

   // sort symbols by name
   //
   loop(d, symbols.size())
      {
        for (ShapeItem j = d + 1; j < ShapeItem(symbols.size()); ++j)
            {
              if (symbols[d]->get_name().compare(symbols[j]->get_name()) > 0)
                 {
                   const Symbol * ss = symbols[d];
                   symbols[d] = symbols[j];
                   symbols[j] = ss;
                 }
            }
      }

   // pass 1: functions
   //
   loop(s, symbols.size())
      {
        const Symbol & sym = *symbols[s];
        const ValueStackItem & vs = sym[0];
         if      (vs.get_NC() == NC_FUNCTION)   { ++fcount;   sym.dump(out); }
         else if (vs.get_NC() == NC_OPERATOR)   { ++fcount;   sym.dump(out); }
      }

   // pass 2: variables
   //
   loop(s, symbols.size())
      {
        const Symbol & sym = *symbols[s];
        const ValueStackItem & vs = sym[0];
        if (vs.get_NC() == NC_VARIABLE)   { ++vcount;   sym.dump(out); }
      }
}
//============================================================================
void
SystemSymTab::clear(ostream & out)
{
   // SymbolTable::clear() should only be called after Workspace::clear_SI()
   //
   Assert(Workspace::SI_entry_count() == 0);

   loop(hash, max_symbol_count)   clear_slot(out, hash);
}
//----------------------------------------------------------------------------
void
SystemSymTab::clear_slot(ostream & out, int hash)
{
SystemName * sym = symbol_table[hash];
   symbol_table[hash] = 0;

   while (sym)
       {
         // remember sym->next (since sym is being deleted)
         //
         SystemName * next = sym->next;

         delete sym;
         sym = next;
       }
}
//----------------------------------------------------------------------------
void
SystemSymTab::add_fun_or_var(const UCS_string & name, Id id,
                       QuadFunction * function, SystemVariable * variable)
{
   // name should not yet exist
   //
   if (lookup_existing_symbol(name))
      {
        Q1(name);   FIXME;
      }

SystemName * dist_name = new SystemName(name, id, function, variable);
   add_symbol(dist_name);
   if (max_name_len < name.size())   max_name_len = name.size();
}
//----------------------------------------------------------------------------
