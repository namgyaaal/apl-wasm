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

#include <stdint.h>

#include <fstream>
#include <string.h>

#include "DerivedFunction.hh"
#include "SystemLimits.hh"
#include "UCS_string.hh"
#include "UTF8_string.hh"
#include "Value.hh"
#include "Workspace.hh"

class Cell;
class Function;
class Executable;
class Prefix;
class Symbol;
class StateIndicator;
class SymbolTable;
class Token;
class Token_loc;
class Value;
class ValueStackItem;

using namespace std;

/// class for )SAVEing and )LOADing APL Workspaces in an XML file format
class XML_Archive
{
protected:
   /** archive syntax version, hopefully stepped up after Archive.hh or
      Archive.cc were changed:

      ASX_OTHER: increment to indicate change in Archive.hh/cc that DOES NOT
                 change the XML file format,

      ASX_MINOR: increment to indicate a backward-compatible change in the XML
                 file format, i.e. older Archive.hh/hh versions can still read
                 it but may not benefit from the change,

      ASX_MAJOR: increment to indicate an incompatible change in the XML file
                 format
    **/
   enum ArchiveSyntax
      {
        ASX_MAJOR = 1,   ///< ++ if XML file format change (incompatible)
        ASX_MINOR = 3,   ///< ++ if XML file format change (backward compatible)
        ASX_OTHER = 7,   ///< ++ XML file format not changed (e.g. code cleanup)
      };
};
//----------------------------------------------------------------------------
/// a helper class for saving an APL workspace
class XML_Saving_Archive: public XML_Archive
{
public:
   /// constructor: remember output stream and  workspace
   XML_Saving_Archive(ofstream & of)
   : indent(0),
     out(of),
     val_pars(0),
     value_count(0),
     char_mode(false)
   {}

   /// destructor
   ~XML_Saving_Archive()   { out.close();  delete [] val_pars; }

   /// an index for \b values
   enum Vid { INVALID_VID = -1 };

   /// write user-defined function \b fun
   void save_Function(const Function & fun);

   /// write either the name and SI-level of user-defined function or the
   /// id of a system function. Return number of attribute= items written
   int save_Function_name(const Function & fun);

   /// write the Prefix parser of \b si (and its derived functions)
   void save_Parser(const StateIndicator & si);

   /// write derived functions cache
   void save_Derived(const DerivedFunctionCache & fns);

   /// write Symbol \b sym
   void save_Symbol(const Symbol & sym);

   /// write all function
   void save_functions();

   /// write all user defined commands
   void save_user_commands(const std::vector<Command::user_command> & cmds);

   /// write SymbolTable \b symtab
   void save_symtab(const SymbolTable & symtab);

   /// write StateIndicator entry \b si
   void save_SI_entry(const StateIndicator & si);

   /// write Token_loc \b tloc
   void save_token_loc(const Token_loc & tloc);

   /// write ValueStackItem \b vsi
   void save_vstack_item(const ValueStackItem & vsi);

   /// write UCS_string \b str
   void save_UCS(const UCS_string & str);

   /// write Value \b v except its ravel
   XML_Saving_Archive & save_shape(Vid vid);

   /// write ravel of Value \b v
   XML_Saving_Archive & save_Ravel(Vid vid);

   /// write entire workspace
   XML_Saving_Archive & save();

   /// a value and its parent (if the parent is nested)
   struct _val_par
      {
         /// default constructor
         _val_par()
         : _val(0),
           _par(INVALID_VID)
         {}

         /// constructor
         _val_par(const Value * v, Vid par)
         : _val(v),
           _par(par)
         {}

         /// the value
         const Value * _val;

         /// the optional parent
         Vid _par;

         /// assign \b other
         void operator=(const _val_par & other)
            { new (this) _val_par(other._val, other._par); }

         /// compare function for Heapsort::sort()
         static bool compare_val_par(const _val_par & A, const _val_par & B,
                                     const void *);

         /// compare function for bsearch()
         static int compare_val_par1(const void * key, const void * B);
      };

protected:
   /// width of one indentation level
   enum { INDENT_LEN = 2 };

   /// indent by \b indent levels, return space left on line
   int do_indent();

   /// return the index of \b val in values
   Vid find_vid(const Value * val);

   /// emit one unicode character (inside "...")
   void emit_unicode(Unicode uni, int & space);

   /// emit one ravel cell \b cell
   void emit_cell(const Cell & cell, int & space);

   /// emit a token value up to (excluding) the '>' of the end token
   void emit_token_val(const Token & tok);

   /// enter char mode. maybe print ² and return the number of chars printed
   int enter_char_mode()
      { if (char_mode)   return 0;   // already in char mode
        out << UNI_PAD_U2;   char_mode = true;   return 1; }

   /// leave char mode. maybe print ⁰ and return the number of chars printed
   int leave_char_mode()
      { if (!char_mode)   return 0;   // not in char mode
        out << UNI_PAD_U0;   char_mode = false;   return 1; }

   /// decrement \b space by length of \b str and return \b str
   static const char * decr(int & space, const char * str);

   /// return true if \b uni prints nicely and is allowed in XML "..." strings
   static bool xml_allowed(Unicode uni);

   /// current indentation level
   int indent;

   /// output XML file
   std::ofstream & out;

   /// an array of values and the Vid of its parent (if value is a
   ///sub-value of a nested parent). The top-level of an APL value has
   /// has no parents (i.e. INVALID_VID).
   /// all values in the workspace
   _val_par * val_pars;

   /// the number of (non-stale) values
   ShapeItem value_count;

   /// true iff ² is pending
   bool char_mode;

   /// functions saved so far
   basic_string<const Function *> saved_Functions;

   /// return true iff (the definition of) \b fun was already saved.
   bool is_saved(const Function * fun) const;
};
//----------------------------------------------------------------------------
inline void
Hswap(XML_Saving_Archive::_val_par & vp1, XML_Saving_Archive::_val_par & vp2)
{
const XML_Saving_Archive::_val_par tmp = vp1;
   vp1 = vp2;
   vp2 = tmp;
}
//----------------------------------------------------------------------------
/// a helper class for loading an APL workspace
class XML_Loading_Archive: public XML_Archive
{
public:
   /// constructor: remember file name and workspace
   XML_Loading_Archive(ostream & _out, const char * _filename, int & dump_fd);

   /// destructor (unmap()s file).
   ~XML_Loading_Archive();

   /// return true iff constructor could open the file
   bool is_open() const   { return fd != -1; }

   /// read an entire workspace, throw DOMAIN_ERROR on failure
   void read_Workspace(bool silent);

   /// check compatibility information in the workspace and maybe warn the
   /// user
   void check_compatibility();

   /// set copying and maybe set protection
   void set_protection(bool prot, const UCS_string_vector & allowed)
      { copying = true;   protection = prot;   allowed_objects = allowed;
        have_allowed_objects = allowed_objects.size() > 0; }

   /// reset archive to the start position
   void reset();

   /// skip to tag \b tag, return EOF if end of file
   bool skip_to_tag(const char * tag);

   /// read vids of top-level variables
   void read_vids();

   /// move to next tag, return true if EOF
   bool next_tag(const char * loc);

protected:
   /// where to send the "SAVED..." message
   ostream & out;

   /// a value ID in a )SAVEd workspace
   enum Vid { NO_VID = int(-1) };   ///< no (invalid) value ID

   /// the address of a function in a )SAVEd workspace
   enum Fid { NO_FID = int(-1) };   ///< no (invalid) function ID

   /// read next Value element
   void read_Value();

   /// read one or more Cell(s) from input into the ravel of Z;
   /// return the  UTF8 * after the cells.
   const UTF8 * read_Cells(Value & Z, const UTF8 * input);

   /// input is a UTF8-encoded sequence of bytes, terminated with ".
   /// UTF8-decode the sequence, thereby removing the escape tagging
   /// with ⁰ (aka. UNI_PAD_U0), ¹ (UNI_PAD_U1), ²(UNI_PAD_U2), and \n.
   /// return the  UTF8 * pointing to the terminating ".
   const UTF8 * read_XML_string(UCS_string & ucs, const UTF8 * input);

   /// read next Ravel element
   void read_Ravel();

   /// read next unused-name element
   void read_unused_name(int d, Symbol & symbol);

   /// read next Variable element
   void read_Variable(int d, Symbol & symbol);

   /// read next Function element
   void read_Function();

   /// read next Function element
   void read_Function(int d, Symbol & symbol);

   /// read next derived function
   void read_Derived(StateIndicator & si, int lev);

   /// read next Label element
   void read_Label(int d, Symbol & symbol);

   /// read next Shared-Variable element
   void read_Shared_Variable(int d, Symbol & symbol);

   /// read next Symbol element
   void read_SymbolTable();

   /// read next Symbol element
   void read_Symbol();

   /// read all user-defined commands
   void read_Commands();

   /// read next Command element
   void read_Command();

   /// read next StateIndicator element
   void read_StateIndicator();

   /// read next StateIndicator entry
   void read_SI_entry(int level);

   /// read parsers in SI entry
   void read_Parser(StateIndicator & si, int lev);

   /// read ⍎ Executable
   const Executable * read_Execute();

   /// read ◊ Executable
   const Executable * read_Statement();

   /// read a user defined Executable
   const Executable * read_UserFunction();

   /// read a lambda
   Executable * read_lambda(const UTF8 * lambda_name);

   /// read an UCS string
   UCS_string read_UCS();

   /// read a token
   bool read_Token(Token_loc & tloc);

   /// read a system function with attribute id_prefix-id or a user defined
   /// functions with attributes 'ufun_prefix-ufun' and 'level_prefix-prefix'
   cFunction_P read_Function_name();

   /// find a lambda in the current SI entry
   cFunction_P find_lambda(const UCS_string & lambda);

   /// return true iff there is more data in the file
   bool more() const   { return data < file_end; }

   /// show some characters starting at the current position
   void where(ostream & out);

   /// show attributes of current tag
   void where_att(ostream & out);

   /// set \b current_char to next (UTF-8 encoded) char, return true if EOF
   bool get_uni();

   /// return true iff \b tagname starts with prefix
   bool is_tag(const char * prefix) const;

   /// check that is_tag(prefix) is true and print error info if not
   void expect_tag(const char * prefix, const char * loc) const;

   /// print current tag
   void print_tag(ostream & out) const;

   /// find attribute \b att_name and return: a pointer to its value if found,
   /// 0 if optional is true, and throw DOMAIN ERROR if optional is false.
   const UTF8 * find_attr(const char * att_name, bool optional);

   /// find mandatory attribute \b att_name. Return a pointer to its value
   ///  if found and throw DOMAIN ERROR if not.
   const UTF8 * find_mandatory_attr(const char * att_name)
      { return find_attr(att_name, false); }

   /// find optional attribute \b att_name. Return a pointer to its value
   /// if found and 0 if not.
   const UTF8 * find_optional_attr(const char * att_name)
      { return find_attr(att_name, true); }

   /// return integer value of attribute \b att_name
   int64_t find_int_attr(const char * att_name, bool optional, int base);

   /// return Fid value of attribute \b att_name
   Fid find_Fid_attr(const char * att_name, bool optional, int base)
      { return Fid(find_int_attr(att_name, optional, base)); }

   /// return Fid value of attribute \b att_name
   Vid find_Vid_attr(const char * att_name, bool optional, int base)
      { return Vid(find_int_attr(att_name, optional, base)); }

   /// return floating point value of attribute \b att_name
   APL_Float find_float_attr(const char * att_name);

   /// the file descriptor for the mmap()ed workspace.xml file
   int fd;

   /// the start of the file (for mmap())
   void * map_start;

   /// the length of the file (for mmap())
   size_t map_length;

   /// the start of the file
   const char * file_start;

   /// the start of the current line
   const UTF8 * line_start;

   /// the current line
   int line_no;

   /// the current char
   Unicode current_char;

   /// the next char
   const UTF8 * data;

   /// the name of the current tag, e.g. "Symbol" or "/Symbol"
   const UTF8 * tag_name;

   /// the attributes of the current tag
   const UTF8 * attributes;

   /// the end of attributes
   const UTF8 * end_attr;

   /// the end of the file
   const UTF8 * file_end;

   /// all values in the workspace
   std::vector<Value_P> values;

   /// true for )COPY and )PCOPY, false for )LOAD
   bool copying;

   /// true for )PCOPY, false for )COPY and )LOAD
   bool protection;

   /// true if reading vids (preparation for )COPY or )PCOPY)
   bool reading_vids;

   /// the vids to be copied (empty if all)
   std::basic_string<Vid> vids_COPY;

   /// the names of objects (empty if all)
   UCS_string_vector allowed_objects;

   /// true for selective copy (COPY with symbol names)
   bool have_allowed_objects;

   /// a value ID and the ID of its parent
   struct _vid_pvid
     {
       Vid vid;    ///< value ID
       Vid pvid;   ///< parent's value ID
     };

   /// parents[vid] is the parent of vid, or NO_VID if vid is a top-level value
   std::basic_string<Vid> parents;

   /// the file name from which this archive was read
   const char * filename;

   /// true if file contains a <\/Workspace> tag at the end
   bool file_is_complete;

   /// one mapping from an fid in the old (SAVEed) workspace to the function
   /// address in the new (LOADing) workspace
   struct fun_map
      {
        Fid old_fid;           ///< the fid in the )SAVEed workspace
        cFunction_P new_fun;   ///< address in the )LOADing workspace
        const char * loc;      ///< where allocated
      };

   /// all mappings from fids to functions
   std::vector<fun_map> fid_to_function;

   /// return fun_map for fid, or 0 if not found.
   fun_map * find_fun_map(Fid fid);

   /// return function for fid, or 0 if not found.
   cFunction_P find_function(Fid fid);

   /// add fid and function to find_fun_map. Either fid must be new, or else
   /// an existing fid must have its new_fun == 0 (forward declaration).
   void add_fid_function(Fid fid, cFunction_P new_fun, const char * loc);

   /// properties of a derived function
   struct _derived_todo
      {
        Function * cache;       ///< the new()-address of the function
        cFunction_P * symptr;   ///< the address of a function * to be set
        Fid fid;                ///< the address of \b this derived function
        Fid LO_fid;             ///< the address of \b LO of this function
        Fid OPER_fid;           ///< the address of \b OPER of this function
        Fid RO_fid;             ///< the address of \b RO of this function
        Vid AXIS_vid;           ///< the AXIS of \b this function
        const char * loc;       ///< where \b this was initialized
      };

      /// derived functions that need to be instantiated
      vector<_derived_todo> derived_todos;

   /// instantiate the derived functions in \b derived_todos
   void instantiate_derived_functions(bool allocate);
};
//----------------------------------------------------------------------------

