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
#ifndef __DOXY_HH_DEFINED__
#define __DOXY_HH_DEFINED__

#include "UCS_string.hh"
#include "UCS_string_vector.hh"
#include "UTF8_string.hh"

#include <ostream>

using namespace std;

class UserFunction;

//----------------------------------------------------------------------------
/// one endge in a (directed) function call graph
struct fcall_edge
{
   /// default constructor
   fcall_edge()
   : caller(0),
     callee(0),
     value(0)
   {}

   /// constructor
   fcall_edge(cFunction_P cer, const UCS_string & cer_name,
              cFunction_P cee, const UCS_string & cee_name )
   : caller(cer),
     caller_name(cer_name),
     callee(cee),
     callee_name(cee_name),
     value(0)
   {}

   /// the calling function
   cFunction_P caller;

   /// the (Symbol-) name of the calling function
   UCS_string caller_name;

   /// the called function
   cFunction_P callee;

   /// the (Symbol-) name of called function
   UCS_string callee_name;

   /// some arbitrary int used in graph algorithms
   int      value;
};

typedef std::vector<fcall_edge> CallGraph;

//----------------------------------------------------------------------------
/// implementation of the ]Doxy command.
class Doxy
{
public:
   /// Constructor: create (and remember) the root directory
   Doxy(ostream & out, const UCS_string & root_dir);

   /// generate the entire documentation
   void gen();

   /// HTML-print a table containing all defined functions to \b page
   void functions_table(const std::vector<const Symbol *> & functions,
                       ofstream & page);

   /// HTML-print one line og a table containing all defined functions
   /// to \b page
   void functions_table_entry(const Symbol * function, ofstream & page,
                              size_t & total_lines);

   /// HTML-print a table with all variables to 'page'
   void variables_table(const std::vector<const Symbol *> & variables,
                        ofstream & page);

   /// HTML-print a table with the SI stack to 'page'
   void SI_table(ofstream & page);

   /// return the number of errors that have occurred
   int get_errors() const
      { return errors; }

   /// return the directory into which all documenattion files will be written
   const UTF8_string & get_root_dir() const
      { return root_dir; }

protected:
   /// write a fixed CSS file
   void write_css();

   /// (HTML-)print a function header with the name in bold to file of
   void bold_name(ostream & of, const UserFunction * ufun) const;

   /// write the page for one defined function. If the define function is a
   /// named lambda, then lambda_owner is the Symbol to which the lambda was
   /// assigned.
   void function_page(const UserFunction * ufun, const UCS_string & alias);

   /// write the page for one native function.
   void native_page(const Function * fun, const UCS_string & alias);

   /// create the call graph
   void make_call_graph(const std::vector<const Symbol *> & all_funs);

   /// add one symbol to the call graph. Note that one symbol can have
   /// different UserFunctions (at different SI levels).
   void add_fun_to_call_graph(const Symbol * sym, const UserFunction * ufun);

   /// make the call graph start from function \b ufun and set \b nodes to
   /// those nodes that are reachable from ufun
   void set_call_graph_root(cFunction_P fun);

   /// write the call graph (if caller == false), or else the caller graph
   int write_call_graph(cFunction_P fun, const UCS_string & alias, bool caller);

   /// swap callers and callees (reverse the direction of an edge)
   void swap_caller_calee();

   /// return the index of \b ufun in \b nodes[] or -1 if not found
   int node_ID(cFunction_P fun);

   /// return an HTML-anchor for function \b name (in the output files)
   static UCS_string fun_anchor(const UCS_string & name);

   /// convert a graphviz .gv file to a .png file using program 'dot'
   int gv_to_png(const char * gv_filename, const char * png_filename,
                 bool cmapx);

   /// the command output channel (COUR or CERR)
   ostream & out;

   /// the name of the workspace (for HTML output)
   UCS_string ws_name;

   /// the directory that shall contain all documentation files
   UTF8_string root_dir;

   /// the nodes for the current root.
   std::vector<const Function *> nodes;

   /// the nodes for all function symbols (independent of the current root).
   std::vector<const Symbol *> all_functions;

   /// the real names for the current root.
   UCS_string_vector aliases;

   /// a directed graph telling which function calles which
   CallGraph call_graph;

   /// the number of errors that have occurred
   int errors;
};
//----------------------------------------------------------------------------
#endif // __DOXY_HH_DEFINED__
