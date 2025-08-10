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

#include "Common.hh"
#include "LibPaths.hh"
#include "TabExpansion.hh"
#include "Workspace.hh"

//----------------------------------------------------------------------------
TabExpansion::TabExpansion(UCS_string & line)
   : have_trailing_blank(line.size() && line.back() == UNI_SPACE)
{
}
//----------------------------------------------------------------------------
ExpandResult
TabExpansion::expand_tab(UCS_string & user_input)
{
   // the user has entered user_input and then pressed TAB.
   // Try to expand user_input. At this level we distinguish between:
   //
   // user-defined names  (variables and functions),
   // distinguished names (⎕xxx variables and functions), and
   // APL commands,

   // skip leading and trailing blanks
   //
   user_input.remove_leading_and_trailing_whitespaces();

   if (user_input.size() == 0)                      // nothing entered yet
      return expand_user_name(user_input);          // assume defined name

   if (Avec::is_first_symbol_char(user_input[0]))   // start of -defined name
      return expand_user_name(user_input);

   if (user_input[0] == ')' || user_input[0] == ']')   // APL command
      return expand_APL_command(user_input);

   return expand_distinguished_name(user_input);       // ⎕xxx
}
//----------------------------------------------------------------------------
ExpandResult
TabExpansion::expand_user_name(UCS_string & user_input)
{
std::basic_string<const Symbol *> symbols = Workspace::get_all_symbols();

UCS_string_vector matches;
   loop(s, symbols.size())
      {
        const Symbol * sym = symbols[s];
        if (sym->is_erased())    continue;

        const UCS_string & sym_name = sym->get_name();
        if (!sym_name.starts_with(user_input))   continue;
        matches.push_back(sym_name);
      }

   if (matches.size() == 0)   return ER_IGNORE;   // no match

   if (matches.size() > 1)    // multiple names match user input
      {
        const int user_len = user_input.size();
        user_input.clear();
        return show_alternatives(user_input, user_len, matches);
      }

   // unique match
   //
   if (user_input.size() < matches[0].size())
      {
        // the name is longer than user_input, so we expand it.
        //
        user_input = matches[0];
        return ER_REPLACE;
      }

   return ER_IGNORE;
}
//----------------------------------------------------------------------------
ExpandResult
TabExpansion::expand_APL_command(UCS_string & user_input)
{
ExpandHint ehint = EH_NO_PARAM;
const char * shint = 0;
UCS_string cmd = user_input;   // command and argument
UCS_string arg;                // argument
   cmd.split_ws(arg);          // command only

   // scan Command.def for all commands that start with cmd.
   // have_trailing_blank tells if user_input is a command prefix or a
   // complete command.
   //
UCS_string_vector matches;
   if (have_trailing_blank || arg.size())   // cmd is a complete command
      {
#define cmd_def(cmd_str, code, arg, hint)                           \
   { UCS_string ustr(UTF8_string(cmd_str));                         \
     if (cmd.size() == strlen(cmd_str) && ustr.starts_iwith(cmd))   \
        { matches.push_back(ustr); ehint = hint; shint = arg; } }
#include "Command.def"

      }
   else                       // command prefix
      {
#define cmd_def(cmd_str, code, arg, hint)                           \
   { UCS_string ustr(UTF8_string(cmd_str));                         \
     if (ustr.starts_iwith(cmd))                                    \
        { matches.push_back(ustr); ehint = hint; shint = arg; } }
#include "Command.def"
      }

   // no match was found: ignore the TAB
   //
   if (matches.size() == 0)   return ER_IGNORE;

   // if multiple matches were found then either expand the common part
   // or list all matches
   //
   if (matches.size() > 1)   // multiple commands match cmd
      {
        Assert(arg.size() == 0);   // due to parti
        user_input.clear();
        return show_alternatives(user_input, cmd.size(), matches);
      }

   // unique match for cmd
   //
const UCS_string & match = matches[0];
   if (cmd.size() < match.size())   // cmd is a (unique) prefix of a command
      {
        // the user input is a prefix of the matched command. Expand it.
        //
        user_input = match;
        if (ehint != EH_NO_PARAM)   user_input.append(UNI_SPACE);
        return ER_REPLACE;
      }

   Assert(cmd.size() == match.size());

   if (ehint == EH_NO_PARAM)   return ER_IGNORE;

   if (arg.size() == 0 && !have_trailing_blank)   // no args yet
      {
        // the entire command was entered but without a blank. If the
        // command has arguments then append a space to indicate that.
        // Otherwiese fall throught to expand_command_arg();
        //
        user_input = match;
        user_input.append(UNI_SPACE);
        return ER_REPLACE;
      }

   return expand_command_arg(user_input, ehint, shint, match, arg);
}
//----------------------------------------------------------------------------
ExpandResult
TabExpansion::expand_command_arg(UCS_string & user_input,
                            ExpandHint ehint, const char * shint,
                            const UCS_string cmd, const UCS_string arg)
{
   switch(ehint)
      {
        case EH_NO_PARAM:
             return ER_IGNORE;

        case EH_oWSNAME:
        case EH_oLIB_WSNAME:
        case EH_oLIB_oPATH:
        case EH_oPATH:
        case EH_FILENAME:
        case EH_DIR_OR_LIB:
        case EH_WSNAME:
             return expand_filename(user_input, ehint, shint, cmd, arg);

        case EH_PRIMITIVE:
             return expand_help_topics(user_input);

        case EH_CONFIG:
             return expand_capability(user_input);

        default:
             CIN << endl;
             CERR << cmd << " " << shint << endl;
             return ER_AGAIN;
      }
}
//----------------------------------------------------------------------------
ExpandResult
TabExpansion::expand_help_topics(UCS_string & user_input)
{
   if (user_input.size() <= 5)
      return expand_help_topics();   // initial display

const UCS_string help(user_input, 0, 6);
UCS_string prefix(user_input, 5, user_input.size() - 5);   // the name prefix
   prefix.remove_leading_whitespaces();

std::basic_string<const Symbol *> symbols = Workspace::get_all_symbols();

UCS_string_vector matches;
   loop(s, symbols.size())
      {
        const Symbol * sym = symbols[s];
        if (sym->is_erased())    continue;

        const UCS_string & sym_name = sym->get_name();
        if (!sym_name.starts_with(prefix))   continue;
        matches.push_back(sym_name);
      }

   if (matches.size() == 0)   return ER_IGNORE;   // no match
   if (matches.size() > 1)   // multiple matches
      {
        matches.sort();

        const int common_len = compute_common_length(prefix.size(), matches);
        if (common_len > prefix.size())
           {
             // all matches can be extended in a unique way
             //
             matches[0].resize(common_len);
             user_input = help + matches[0];
             return ER_REPLACE;
           }

        // all possible extensions are different
        //
        return show_alternatives(user_input, prefix.size(), matches);
      }

   // unique match
   //
   user_input = help + matches[0];
   return ER_REPLACE;
}
//----------------------------------------------------------------------------
/// one help topic
const struct _help
{
  int          valence;   ///< -5..2, see explanation in file Help.def
  const char * prim;      ///< primitive, e.g. "⍬",     "+", ...
  const char * name;      ///< name,      e.g. "Zilde", "Plus", ...
  const char * title;     ///< brief description
  const char * descr;     ///< long description
} help_texts[] = {
#define help_def(ar, pr, na, ti, descr) { ar, pr, na, ti, descr },
#include "Help.def"
};

enum { HELP_count = sizeof(help_texts) / sizeof(_help) };

ExpandResult
TabExpansion::expand_help_topics()
{
   CIN << "\n";
   CERR << "Help topics (APL primitives and user-defined names) are:" << endl;

   // show APL primitives (but only once). For that Help.def must be sorted.
   // Many primitives occur twice (once for their monadic and once for their
   // dyadic form. We filter those duplicates out with strcmp(prim, last)
   //
   // We also remove some duplicates caused by different Unicodes (like ∣ and |)
   //
const char * last = "";
int col = 0;
const int max_col = Workspace::get_PW() - 4;

   loop(h, HELP_count)
       {
         const char * prim = help_texts[h].prim;
         if (!strcmp(prim, last))    continue;
         last = prim;   // remember it.
         if (strchr("|~", *prim))    continue;
         CERR << " " << prim;
         col += 2;
         if (col > max_col)   { CERR << endl;   col = 0; } \
       }

std::basic_string<const Symbol *> symbols = Workspace::get_all_symbols();

UCS_string_vector names;
   loop(s, symbols.size())
      {
        const Symbol * sym = symbols[s];
        if (sym->is_erased())    continue;

        const UCS_string & sym_name = sym->get_name();
        names.push_back(sym_name);
      }

   names.sort();

   // see if printing full names takes more than 5 lines. If so then
   // only print first characters
   //
int rows = 1;   // the current row
int c1 = col;
   loop(n, names.size())
      {
        const int len = 1 + names[n].size();
        c1 += len;
        if (c1 > max_col)   { ++rows;   c1 = len; }
      }

   if (names.size() == 0)   /* nothing to do */;
   else if (rows < 6)
      {
        loop(n, names.size())
            {
              const int len = 1 + names[n].size();
              col += len;
              if (col > max_col)   { CERR << endl;   col = len; }
              CERR << " " << names[n];
            }
      }
   else
      {
        Unicode uni = names[0][0];
        CERR << " " << uni;
        for (ShapeItem n = 1; n < ShapeItem(names.size()); ++n)
            {
              if (names[n][0] == uni)   continue;   // same first character
              uni = names[n][0];
              CERR << " " << uni;   col += 2;
              if (col > max_col)   { CERR << endl;   col = 0; }
            }
      }

   CERR << endl;
   return ER_AGAIN;
}
//----------------------------------------------------------------------------
ExpandResult
TabExpansion::expand_distinguished_name(UCS_string & user_input)
{
   // figure the length of longest ⎕xxx name (probably ⎕TRACE == 5)
   //
unsigned int max_e = 2;
#define ro_sv_def(_q, str, _txt) if (max_e < strlen(str))   max_e = strlen(str);
#define rw_sv_def(_q, str, _txt) if (max_e < strlen(str))   max_e = strlen(str);
#define sf_def(_q, str, _txt)    if (max_e < strlen(str))   max_e = strlen(str);
#include "SystemVariable.def"

   // Search for ⎕ backwards from the end
   //
int qpos = -1;   // the position of ⎕ in user_input

   loop(e, max_e)
      {
        if (e >= user_input.size())   break;
        if (user_input[user_input.size() - e - 1] == UNI_Quad_Quad)
           {
             qpos = user_input.size() - e;
             break;
           }
      }

   if (qpos != -1)   // ⎕xxx at end
      {
        UCS_string qxx(user_input, qpos, user_input.size() - qpos);
        UCS_string_vector matches;

#define ro_sv_def(_q, str, _txt) { UCS_string ustr(UTF8_string(str));  \
   if (ustr.size() && ustr.starts_iwith(qxx)) matches.push_back(ustr); }

#define rw_sv_def(_q, str, _txt) { UCS_string ustr(UTF8_string(str));  \
   if (ustr.size() && ustr.starts_iwith(qxx)) matches.push_back(ustr); }

#define sf_def(_q, str, _txt) { UCS_string ustr(UTF8_string(str));     \
   if (ustr.size() && ustr.starts_iwith(qxx)) matches.push_back(ustr); }

#include "SystemVariable.def"

        if (matches.size() == 0)   return ER_IGNORE;
        if (matches.size() > 1)
           {
            matches.sort();

            const int common_len = compute_common_length(qxx.size(), matches);
            if (common_len == qxx.size())
               {
                 // qxx is already the common part of all matching ⎕xx
                 // display matching ⎕xx
                 //
                 CIN << endl;
                 loop(m, matches.size())
                     {
                        CERR << "⎕" << matches[m] << " ";
                     }
                 CERR << endl;
                 return ER_AGAIN;
               }
            else
               {
                 // qxx is a prefix of the common part of all matching ⎕xx.
                 // expand to common part.
                 //
                 user_input = matches[0];
                 user_input.resize(common_len);
                 return ER_REPLACE;
               }
           }

        // unique match
        //
        user_input = UCS_string(UNI_Quad_Quad);
        user_input.append(matches[0]);
        return ER_REPLACE;
      }

   return ER_IGNORE;
}
//----------------------------------------------------------------------------
ExpandResult
TabExpansion::expand_capability(UCS_string & user_input)
{
   // user has entered ']NEXTFILE ' and possibly the prefix of a capability
   // argument on which ]NEXTFILE may depend. Expand the capability.
   //

   // split user_input into a (fixed) stem and a (partial) prefix.
   // The prefix shall then be extended into a capability. The stem
   // ends after rightmost blank in user_input.
int prefix_len = 0;
   for (int u = user_input.size(); u; --u)
       {
         if (user_input[u - 1] == UNI_SPACE)
            {
              prefix_len = u;
              break;
            }
       }

const char * CAPABILITIES[] = { apl_CAPABILITIES };
enum { capabilities_count = sizeof(CAPABILITIES) / sizeof(*CAPABILITIES) };

UCS_string stem(user_input, 0, prefix_len);
UCS_string prefix = user_input.drop(prefix_len);

#if 0
    CERR << "\nUSER: '" << user_input << "'" << endl;
    CERR << "STEM: '" << stem << "'" << endl;
    CERR << "PREFIX: '" << prefix << "'" << endl;
#endif

   if (prefix_len == 0)   // no capability argument: display all.
      {
        loop(c, capabilities_count)
            {
               const UTF8_string capa_utf(CAPABILITIES[c]);
               UCS_string capa_ucs(capa_utf);
               capa_ucs.append_ASCII("            ");
               if ((c % 3) == 0)   COUT << endl;
               COUT << "have-" << UCS_string(capa_ucs, 0, 9)
                    << "no-"   << UCS_string(capa_ucs, 0, 9);
            }
         COUT << endl;
         return ER_AGAIN;
      }

   // at this point, prefix should start with either HAVE- or NO-.
   // Expand prefix if appropriate
   if ( (prefix.size() == 1 && prefix.starts_iwith("H" ))  ||
        (prefix.size() == 2 && prefix.starts_iwith("HA"))  ||
        (prefix.size() == 3 && prefix.starts_iwith("HAV")) ||
        (prefix.size() == 4 && prefix.starts_iwith("HAVE")))
      {
        user_input = stem + UCS_ASCII_string("HAVE-");
        return ER_REPLACE;
      }

   if ( (prefix.size() == 1 && prefix.starts_iwith("N" )) ||
        (prefix.size() == 2 && prefix.starts_iwith("NO")))
      {
        user_input = stem + UCS_ASCII_string("NO-");
        return ER_REPLACE;
      }

   // got HAVE- or NO-; expand capability...
   //
basic_string<const char *> matches;
   if (prefix.size() >= 5 && prefix.starts_iwith("HAVE-"))
      {
        stem.append_ASCII(" HAVE-");
        const UCS_string suffix(prefix, 5, prefix.size() - 5);   // skip HAVE-
        loop(c, capabilities_count)
            {
              const UTF8_string cap_utf8(CAPABILITIES[c]);
              const UCS_string cap_ucs(cap_utf8);
              if (cap_ucs.starts_iwith(suffix))
                 matches.push_back(CAPABILITIES[c]);
            }
      }
   else if (prefix.size() >= 3 && prefix.starts_iwith("NO-"))
      {
        stem.append_ASCII(" NO-");
        const UCS_string suffix(prefix, 3, prefix.size() - 3);   // skip NO-
        loop(c, capabilities_count)
            {
              const UTF8_string cap_utf8(CAPABILITIES[c]);
              const UCS_string cap_ucs(cap_utf8);
              if (cap_ucs.starts_iwith(suffix))
                 matches.push_back(CAPABILITIES[c]);
            }
      }

   if (matches.size() == 0)   return ER_IGNORE;   // no match
   if (matches.size() > 1)
      {
        COUT << endl;
        loop(m, matches.size())   COUT << matches[m] << " ";
        COUT << endl;

        return ER_AGAIN;    // multiple matches
      }

   // unique match
   //
const UTF8_string match(matches[0]);
   user_input = stem + UCS_string(match);
   return ER_REPLACE;   // unique match
}
//----------------------------------------------------------------------------
ExpandResult
TabExpansion::expand_filename(UCS_string & user_input,
                              ExpandHint ehint, const char * shint,
                              const UCS_string cmd, UCS_string arg)
{
   if (arg.size() == 0)
      {
        // the user has entered a command but nothing else.
        // If the command accepts a library number then we propose one of
        // the existing libraries.
        //
        if (ehint == EH_oLIB_WSNAME || ehint == EH_DIR_OR_LIB)
           {
             // the command accepts a library reference number
             //
             UCS_string libs_present;
             loop(lib, 10)
                 {
                    if (!LibPaths::is_present(LibRef(lib)))
                       continue;

                    libs_present.append(UNI_SPACE);
                    libs_present.append(Unicode(UNI_0 + lib));
                 }
             if (libs_present.size() == 0)   goto nothing;

             CIN << endl;
             CERR << cmd << libs_present << " <workspace-name>" << endl;
             return ER_AGAIN;
           }

        goto nothing;
      }

   if (arg[0] >= '0' && arg[0] <= '9')
      {
        // library number 0-9. EH_DIR_OR_LIB is complete already so
        // EH_oLIB_WSNAME is the only expansion case left
        //
        if (ehint != EH_oLIB_WSNAME)   goto nothing;

        const LibRef lib = LibRef(arg[0] - '0');

        if (arg.size() == 1 && !have_trailing_blank)   // no space yet
           {
             user_input = cmd;
             user_input.append(UNI_SPACE);
             user_input.append(Unicode(arg[0]));
             user_input.append(UNI_SPACE);
             return ER_REPLACE;
           }

         // discard library reference number
         //
         if (arg.size() == 1)   arg.erase(0);
         if (arg.size())        arg.erase(0);
         return expand_wsname(user_input, cmd, lib, arg);
      }

   // otherwise: real file name
   {
     UCS_string dir_ucs;
     const bool slash_at_1 = arg.size() > 1 && arg[1] == UNI_SLASH;
     const bool tilde_at_0 = arg[0] == UNI_TILDE ||
                             arg[0] == UNI_TILDE_OPERATOR;

     if (arg[0] == UNI_SLASH)                     // absolute path /xxx
        {
          dir_ucs = arg;
        }
     else if (arg[0] == UNI_FULLSTOP && slash_at_1) // relative path ./xxx
        {
          const char * pwd = getenv("PWD");
          if (pwd == 0)   goto nothing;
          const UTF8_string dir_utf(pwd);
          dir_ucs = UCS_string(dir_utf);
          dir_ucs.append(arg.drop(1));
        }
     else if (tilde_at_0 && slash_at_1)                 // user's home ~/
        {
          const char * home = getenv("HOME");
          if (home == 0)   goto nothing;
          const UTF8_string home_utf(home);
          dir_ucs = UCS_string(home_utf);
          dir_ucs.append(arg.drop(1));
        }
     else if (tilde_at_0)                               // somebody's home
        {
          dir_ucs = UCS_ASCII_string("/home/");
          dir_ucs.append(arg.drop(1));
        }
     else goto nothing;

     UTF8_string dir_utf = UTF8_string(dir_ucs);

     const char * dir_dirname = dir_utf.c_str();
     const char * dir_basename = strrchr(dir_dirname, '/');
     if (dir_basename == 0)   goto nothing;

     UTF8_string base_utf = UTF8_string(++dir_basename);
     dir_utf.resize(dir_basename - dir_dirname);
     dir_ucs = UCS_string(dir_utf);

     DIR * dir = opendir(dir_utf.c_str());
     if (dir == 0)   goto nothing;

     UCS_string_vector matches;
     read_matching_filenames(dir, dir_utf, base_utf, ehint, matches);
     closedir(dir);

     if (matches.size() == 0)
        {
          CIN << endl;
          CERR << "  no matching filesnames" << endl;
          return ER_AGAIN;
        }

     if (matches.size() > 1)
        {
          UCS_string prefix(base_utf);
          user_input = cmd;                     // e.g. )LOAD
          user_input.append(UNI_SPACE);
          user_input.append(dir_ucs);           // e.g. )LOAD /usr/apl/
          return show_alternatives(user_input, prefix.size(), matches);
        }

     // unique match
     //
     user_input = cmd;
     user_input.append(UNI_SPACE);
     user_input.append(dir_ucs);
     user_input.append(matches[0]);
     return ER_REPLACE;
   }

nothing:
   CIN << endl;
   CERR << cmd << " " << shint << endl;
   return ER_AGAIN;
}
//----------------------------------------------------------------------------
ExpandResult
TabExpansion::expand_wsname(UCS_string & user_input, const UCS_string cmd,
                       LibRef lib, const UCS_string filename)
{
UTF8_string path = LibPaths::get_lib_dir(lib);
   if (path.size() == 0)
      {
        CIN << endl;
        CERR << "Invalib library reference " << lib << endl;
        return ER_AGAIN;
      }

DIR * dir = opendir(path.c_str());
   if (dir == 0)
      {
        CIN << endl;
        CERR
<< "  library reference " << lib
<< " is a valid number, but the corresponding directory " << endl
<< "  " << path << " does not exist" << endl
<< "  or is not readable. " << endl
<< "  The relation between library reference numbers and filenames" << endl
<< "  (aka. paths) can be configured file 'preferences'." << endl << endl
<< "  At this point, you can use a path instead of the optional" << endl
<< "  library reference number and the workspace name." << endl;

        user_input = cmd;
        user_input.append(UNI_SPACE);
        return ER_REPLACE;
      }

UCS_string_vector matches;
UTF8_string arg_utf(filename);
   read_matching_filenames(dir, path, arg_utf, EH_oLIB_WSNAME, matches);
   closedir(dir);

   if (matches.size() == 0)   goto nothing;
   if (matches.size() > 1)
      {
        user_input = cmd;
        user_input.append(UNI_SPACE);
        user_input.append_number(lib);
        user_input.append(UNI_SPACE);
        return show_alternatives(user_input, filename.size(), matches);
      }

   // unique match
   //
   user_input = cmd;
   user_input.append(UNI_SPACE);
   user_input.append_UTF8(path.c_str());
   user_input.append(UNI_SLASH);
   user_input.append(matches[0]);
   return ER_REPLACE;

nothing:
   CIN << endl;
   CERR << cmd << " " << lib << " '" << filename << "'" << endl;
   return ER_AGAIN;
}
//----------------------------------------------------------------------------
int
TabExpansion::compute_common_length(int len, const UCS_string_vector & matches)
{
   // we assume that all matches have the same case

   for (;; ++len)
       {
         loop(m, matches.size())
            {
              if (len >= matches[m].size())   return matches[m].size();
              if (matches[0][len] != matches[m][len])    return len;
            }
       }
}
//----------------------------------------------------------------------------
void
TabExpansion::read_matching_filenames(DIR * dir, UTF8_string dirname,
                                 UTF8_string prefix, ExpandHint ehint,
                                 UCS_string_vector & matches)
{
const bool only_workspaces = (ehint == EH_oLIB_WSNAME) ||
                             (ehint == EH_WSNAME     ) ||
                             (ehint == EH_oWSNAME    );

   for (;;)
       {
          struct dirent * dent = readdir(dir);
          if (dent == 0)   break;

          const size_t dlen = strlen(dent->d_name);
          if (dlen == 1 && dent->d_name[0] == '.')   continue;
          if (dlen == 2 && dent->d_name[0] == '.'
                        && dent->d_name[1] == '.')   continue;

          if (strncmp(dent->d_name, prefix.c_str(), prefix.size()))   continue;

          UCS_string name(UTF8_string(dent->d_name));

          const bool is_dir = Command::is_directory(dent, dirname);
          if (is_dir)   name.append(UNI_SLASH);
          else if (only_workspaces)
             {
               const UTF8_string filename(dent->d_name);
               bool is_wsname = false;
               if (filename.ends_with(".apl"))   is_wsname = true;
               if (filename.ends_with(".xml"))   is_wsname = true;
               if (!is_wsname)   continue;
             }

          matches.push_back(name);
       }
}
//----------------------------------------------------------------------------
ExpandResult
TabExpansion::show_alternatives(UCS_string & user_input, int prefix_len,
                           UCS_string_vector & matches)
{
const int common_len = compute_common_length(prefix_len, matches);

   if (common_len == prefix_len)
      {
        // prefix is already the common part of all matching files
        // display matching items
        //
        CIN << endl;
        loop(m, matches.size())
            {
              CERR << matches[m] << " ";
            }
        CERR << endl;
        return ER_AGAIN;
      }
   else
      {
        // prefix is a prefix of the common part of all matching files.
        // expand to common part.
        //
        const int usize = user_input.size();
        user_input.append(matches[0]);
        user_input.resize(usize + common_len);
        return ER_REPLACE;
      }
}
//----------------------------------------------------------------------------




