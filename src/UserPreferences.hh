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

#ifndef __USER_PREFERENCES_HH_DEFINED__
#define __USER_PREFERENCES_HH_DEFINED__

#include <sys/time.h>
#include <vector>
#include <string>

#include "Parallel.hh"
#include "UTF8_string.hh"

class Function;

/** a structure that contains user preferences from different sources
    (command line arguments, config files, environment variables ...)
 */
/// Various preferences of the user (sorted alphabetically)
struct UserPreferences
{
   UserPreferences()
   :
     append_summary(false),
     auto_OFF(false),
     backup_before_save(false),
     control_Ds_to_exit(0),
     CPU_limit_secs(0),
     daemon(false),
     discard_indentation(false),
#define sec_def(X) X(false),
#include "Security.def"   // all start with disable_
     do_CONT(true),
     do_Color(true),
     do_not_echo(false),
     echo_CIN(false),
     emacs_arg(0),
     emacs_mode(false),
     initial_pw(DEFAULT_Quad_PW),
     line_history_len(500),
     line_history_path(".apl.history"),
     mem_arg(0),
     multi_line_strings(true),
     multi_line_strings_3(true),
     nabla_to_history(1),   // if function was modified
     output_to_cout(false),
     randomize_testfiles(false),
     raw_cin(false),
     requested_cc(CCNT_UNKNOWN),
     requested_id(0),
     requested_par(0),
     safe_mode(false),
     script_argc(0),
     silent(false),
     system_do_svars(true),
     tcp_port(0),
     tcp_websocket(false),
     user_do_svars(true),
     user_profile(0),
     wait_ms(0),
     WINCH_sets_pw(false)
   { gettimeofday(&session_start, 0); }

   /// read a \b preference file and update parameters set there
   void read_config_file(bool sys, bool log_startup);

   /// read a \b parallel_thresholds file and update parameters set there
   void read_threshold_file(bool sys, bool log_startup);

   /// print possible command line options and exit
   static void usage(const char * prog);

   /// return " (default)" if yes is true
   static const char * is_default(bool yes)
      { return yes ? " (default)" : ""; }

   /// print how the interpreter was configured (via ./configure) and exit
   static void show_configure_options();

   /// print the GPL
   static void show_GPL(ostream & out);

   /// show version information
   static void show_version(ostream & out);

   /// parse the original command line parameters to figure start-up Logging
   /// (BEFORe any expansions). Parses (only) the -l option. Return true
   /// iff startup-logging is requested.
   bool parse_argv_0(int argc, const char * argv[]);

   /// parse command line parameters (BEFORE reading preference files).
   /// Parses the -l, -p, -C, and -u options. Return true iff startup-logging
   /// is requested.
   bool parse_argv_1();

   /// parse command line parameters (AFTER reading preference files)
   /// Parses all valid options.
   void parse_argv_2(bool logit);

   /// argv/argc at startup before expand_argv()
   std::vector<const char *> original_argv;

   /// argv/argc after expand_argv()
   std::vector<const char *> expanded_argv;

   /// append test results to summary.log rather than overriding it
   bool append_summary;

   /// true if the interpreter shall )OFF on EOF of the last input file
   bool auto_OFF;

   /// backup on )SAVE
   bool backup_before_save;

   /// number of control-Ds to exit (0 = never)
   int control_Ds_to_exit;

   /// limit (seconds) on the CPU time
   int CPU_limit_secs;

   /// run as deamon
   bool daemon;

   /// true if leading spaces in the ∇-editor shall be dropped
   bool discard_indentation;

#define sec_def(X) \
   bool X;   ///< true if X is disabled for security reasons
#include "Security.def"

   /// load workspace CONTINUE on start-up
   bool do_CONT;

  /// output coloring enabled
   bool do_Color;

   /// true if no input echo is wanted.
   bool do_not_echo;

   /// true to echo input (after editing)
   bool echo_CIN;

   /// an argument for emacs mode
   const char * emacs_arg;

   /// true if emacs mode is wanted
   bool emacs_mode;

   /// expand lumped arguments
   void expand_argv(int argc, const char ** argv);

   /// --eval expressions
   std::vector<const char *> eval_exprs;

   /// initial value of ⎕PW
   int initial_pw;

   /// a workspace to be loaded at startup
   UTF8_string initial_workspace;

   /// something to be executed at startup (--LX)
   UTF8_string latent_expression;

   /// number of lines in the input line history
   int line_history_len;

   /// name of a user-provided keyboard layout file
   UTF8_string keyboard_layout_file;

   /// location of the input line history
   UTF8_string line_history_path;

   /// argument of --mem (if any: 0 if not given, "" if given but empty)
   const char * mem_arg;

   /// true if old-style multi-line strings are allowed (in ∇-defined
   /// functions)
   bool multi_line_strings;

   /// true if new-style multi-line strings are allowed
   bool multi_line_strings_3;

   /// when function body shall go into the history
   int nabla_to_history;

   /// true if output shall go to cout, otherwise to cerr
   bool output_to_cout;

   /// randomize the order of testfiles
   bool randomize_testfiles;

   /// send no ESC sequences on stderr
   bool raw_cin;

   /// desired core count
   CoreCount requested_cc;

   /// desired --id (⎕AI[1] and shared variable functions)
   int requested_id;

   /// desired --par (⎕AI[1] and shared variable functions)
   int requested_par;

   /// true if --safe command line option was given
   bool safe_mode;

   /// the argument number of the APL script name (if run from a script)
   /// in expanded_argv, or 0 if apl is started directly.
   size_t script_argc;

   /// when apl was started
   timeval session_start;

   /// true if no banner/Goodbye is wanted.
   bool silent;

   /// true if shared variables are enabled by the system. This is initially
   /// the same as user_do_svars, but can become false if something goes wrong
   bool system_do_svars;

   /// TCP port to be used (instead of stdin/stderr)
   int tcp_port;

   /// enable websocket protocol on the TCP port
   bool tcp_websocket;

   /// true if shared variables are wanted by the user
   bool user_do_svars;

   /// the profile to be used (in the preferences file)
   int user_profile;

   /// wait at start-up
   int wait_ms;

   /// true if the WINCH signal shall modify ⎕PW
   bool WINCH_sets_pw;

   /// the combined user preferences (from command line arguments and from
   /// \b preferences files) for the APL interpreter instance
   static UserPreferences uprefs;

protected:
   /// decode a byte in a preferences file. The byte can be given as ASCII name
   /// (currently only ESC is understood), a single char (that stands for
   /// itself), or 2 2-character hex value
   /// 
   static int decode_ASCII(const char * strg);

   /// return true if file \b filename is an APL script (has execute permission
   /// and starts with #!
   static bool is_APL_script(const char * filename);

   /// open a user-supplied config file (in $HOME or gnu-apl.d)
   FILE * open_user_file(const char * fname, char * opened_filename,
                         bool sys, bool log_startup);

   /// set the parallel threshold of function \b fun to \b threshold
   static void set_threshold(cFunction_P fun, int padic, int macn,
                             ShapeItem threshold);
};

#endif // __USER_PREFERENCES_HH_DEFINED__
