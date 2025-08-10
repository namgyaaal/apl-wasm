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

/** @file **/

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <string.h>
#include <sys/time.h>

#include "Bif_OPER2_INNER.hh"
#include "Bif_OPER2_OUTER.hh"
#include "Common.hh"
#include "IO_Files.hh"
#include "InputFile.hh"
#include "LibPaths.hh"
#include "Output.hh"
#include "ScalarFunction.hh"
#include "UserPreferences.hh"
#include "Workspace.hh"

#include "Value.hh"

static const char * build_tag[] = { BUILDTAG, 0 };
UserPreferences UserPreferences::uprefs;

/// CYGWIN defines _B

#ifdef _B
#undef _B
#endif

//----------------------------------------------------------------------------
void
UserPreferences::usage(const char * prog)
{
   // skip directory components in prog
   if (const char * prog1 = strrchr(prog, '/'))   prog = prog1 + 1;

   CERR <<
"usage: " << prog << " [options]\n"
"    options: \n"
"    -h, --help           print this help\n"
"    -d                   run in the background (i.e. as daemon)\n"
"    -f file              read APL input from file\n"
"    --id proc            use processor ID proc (default: first unused > 1000)\n"
   ;

#ifdef cfg_DYNAMIC_LOG_WANTED
   CERR <<
"    -l num               turn logging facility num ON (1 ≤ num ≤ "
                             << (LID_MAX - 1) << ")\n";
#else
   CERR <<
"    -l 37                turn logging facility 37 (startup) ON\n";
#endif

#if cfg_CORE_COUNT_WANTED == -2
   CERR <<
"    --cc count           use count cores (default: all)\n";
#endif

   CERR <<
#ifndef WINDOWS
"    -C new_root          do chroot(new_root) before starting APL (root only)\n"
#endif // WINDOWS
"    --cfg                show ./configure options used and exit\n"
"    --noCIN              do not echo input (for scripting)\n"
"    --CPU_limit_secs sec set CPU time limit to 'sec' seconds\n"
"    --echoCIN            echo (final) input to COUT\n"
"    --rawCIN             do not emit escape sequences\n"
"    --to_COUT            redirect CERR to COUT (like 2>&1 in bash)\n"
"    --tcp_port port      redirect CIN/COUT/CERR to/from TCP port 'port')\n"
"    --[no]Color          start with ]XTERM ON [OFF])\n"
"    --noCONT             do not )LOAD CONTINUE or SETUP workspace on startup\n"
"    --emacs              run in (classical) emacs mode\n"
"    --emacs_arg arg      run in emacs mode with argument 'arg'\n"
"    --eval expr          evaluate APL line expr and exit\n"
"    --gpl                show license (GPL) and exit\n"
"    --huge [arg]         memory management\n"
"    -L wsname            )LOAD wsname (and not SETUP or CONTINUE) on startup\n"
"    --LX expr            execute APL expression expr first\n"
"    --OFF                automatically )OFF after last input file\n"
"    -p N                 use profile N in preferences files\n"
"    --par proc           use processor parent ID proc (default: no parent)\n"
"    --PW value           initial value of ⎕PW\n"
"    -q, --silent         do not print the welcome banner\n"
"    -s, --script         shortcut for --silent --noCIN --noCONT --noColor\n"
"    --safe               safe mode (no shared vars, no native functions)\n"
"    --show_bin_dir       show binary directory and exit\n"
"    --show_doc_dir       show documentation directory and exit\n"
"    --show_etc_dir       show system configuration directory and exit\n"
"    --show_lib_dir       show library directory and exit\n"
"    --show_src_dir       show source directory and exit\n"
"    --show_all_dirs      show all directories above and exit\n"
"    --[no]SV             [do not] start APnnn (a shared variable server)\n"
"    -T testcases ...     run testcases\n"
"    --TM mode            test mode (for -T files):\n"
"                         0:   exit after last testcase (default)\n"
"                         1:   exit after last testcase if no error\n"
"                         2:   continue (don't exit) after last testcase\n"
"                         3:   stop testcases after first error (don't exit)\n"
"                         4:   exit after first error\n"
"    --TR                 randomize order of testfiles\n"
"    --TS                 append to (rather than override) summary.log\n"
#ifndef WINDOWS
"    -u UID               run as user UID (root only)\n"
#endif // WINDOWS
"    -v, --version        show version information and exit\n"
"    -w milli             wait milli milliseconds at startup\n"
"    --                   end of options for the " << prog << " binary\n"
"    +APPOPT              ignored (APL application option)\n"
"    ++APPOPT ARG1        ignored (APL application option with 1 argument)\n"
"    +++APPOPT ARG1 ARG2  ignored (APL application option with 2 arguments)\n"
"       ...\n"
"\n"
"Please report problems to: bug-apl@gnu.org\n" << endl;
}
//----------------------------------------------------------------------------
void
UserPreferences::show_GPL(ostream & out)
{
   out <<
"\n"
"---------------------------------------------------------------------------\n"
"\n"
"    This program is GNU APL, a free implementation of the\n"
"    ISO/IEC Standard 13751, \"Programming Language APL, Extended\"\n"
"\n"
"    Copyright © 2008-2023  Dr. Jürgen Sauermann\n"
"\n"
"    This program is free software: you can redistribute it and/or modify\n"
"    it under the terms of the GNU General Public License as published by\n"
"    the Free Software Foundation, either version 3 of the License, or\n"
"    (at your option) any later version.\n"
"\n"
"    This program is distributed in the hope that it will be useful,\n"
"    but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
"    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
"    GNU General Public License for more details.\n"
"\n"
"    You should have received a copy of the GNU General Public License\n"
"    along with this program.  If not, see http://www.gnu.org/licenses/.\n"
"\n"
"    ------------------------------------------------------------------------\n"
"\n"
"    The implementation of the APL matrix division functions (i.e. ⌹B, A⌹B,\n"
"    and A⌹[X]B) are based on a manual translation of some FORTRAN source\n"
"    code and comments from the LApack library (aka. liblapack, see GNU APL\n"
"    source files LApack.hh and LApack.cc for details).\n"
"\n"
"    LApack has the following copyright notices:\n"
"\n"
"    Copyright (c) 1992-2011 The University of Tennessee and The University\n"
"                           of Tennessee Research Foundation.  All rights\n"
"                           reserved.\n"
"   Copyright (c) 2000-2011 The University of California Berkeley. All\n"
"                           rights reserved.\n"
"   Copyright (c) 2006-2011 The University of Colorado Denver.  All rights\n"
"                           reserved.\n"
"\n"
"   See also: http://www.netlib.org/lapack/LICENSE\n"
"\n";
}
//----------------------------------------------------------------------------
bool
UserPreferences::is_APL_script(const char * filename)
{
   /* according to man execve():

       An interpreter script is a text file that has execute permission
       enabled and whose first line is of the form:

           #! interpreter [optional-arg]

    */

   if (access(filename, X_OK))   return false;
int fd = open(filename, O_RDONLY);
   if (fd == -1)   return false;

char buf[2];
const size_t len = read(fd, buf, sizeof(buf));
   close(fd);

   if (len != 2)        return false;
   if (buf[0] != '#')   return false;
   if (buf[1] != '!')   return false;
   return true;
}
//----------------------------------------------------------------------------
bool
UserPreferences::parse_argv_0(int argc, const char * argv[])
{
   for (int a = 1; a < (argc - 1); ++a)
       {
         if (!strcmp(argv[a], "-l") && atoi(argv[a+1]) == LID_startup)
            return true;
       }

   return false;
}
//----------------------------------------------------------------------------
bool
UserPreferences::parse_argv_1()
{
bool log_startup = false;
   for (size_t a = 1; a < expanded_argv.size(); )
       {
         const char * opt = expanded_argv[a++];
         const char * val = (a < expanded_argv.size()) ? expanded_argv[a] : 0;

         if (!strcmp(opt, "-l"))
            {
              ++a;
              if (!val)
                 {
                   CERR << "-l without log level" << endl;
                   exit(a);
                 }

              log_startup = (atoi(val) == LID_startup);
              continue;
            }

         if (!strcmp(opt, "-p"))
            {
              ++a;
              if (!val)
                 {
                   CERR << "-p without profile number" << endl;
                   exit(a);
                 }

              user_profile = atoi(val);
              continue;
            }

         if (!strcmp(opt, "-C"))
            {
              ++a;
              if (!val)
                 {
                   CERR << "-C without directory" << endl;
                   exit(a);
                 }

#ifndef WINDOWS
              if (chroot(val))
                 {
                   CERR << "chroot(" << val << ") failed: "
                        << strerror(errno) << endl;
                   exit(a);
                 }
#endif // WINDOWS

              if (chdir("/"))
                 {
                   CERR << "chdir(\"/\") failed: "
                        << strerror(errno) << endl;
                   exit(a);
                 }

              continue;
            }

#ifndef WINDOWS
         if (!strcmp(opt, "-u"))
            {
              ++a;
              if (!val)
                 {
                   CERR << "-u without user ID" << endl;
                   exit(a);
                 }
              if (setuid(strtoll(val, 0, 10)))
                 {
                   CERR << "setuid(" << val << ") failed: "
                        << strerror(errno) << endl;
                   exit(a);
                 }
              continue;
            }
#endif // WINDOWS
       }

   return log_startup;
}
//----------------------------------------------------------------------------
void
UserPreferences::parse_argv_2(bool logit)
{
   // execve() puts the script name (and optional script arguments at the
   // end of argv.
   // For example, argv might look like this:
   //
   // /usr/bin/apl -apl-options... -- -application-options
   //
   // In general the options are the shebang of the script followed by the
   // command line options (each starting with the interpreter or script name,
   // followed by their optional arguments).

   // if GNU APL is started as aplscript, then --script is implied
   //
   {
      const char * prog = expanded_argv[0];
      if (strlen(prog) >= 6 && !strcmp("script", (prog + strlen(prog) - 6)))
         {
            do_CONT = false;               // --noCONT
            do_not_echo = true;            // -noCIN
            do_Color = false;              // --noColor
            silent = true;                 // --silent
         }
   }

   for (size_t a = 1; a < expanded_argv.size(); )
       {
         if (a == script_argc)   { ++a;   continue; }   // skip scriptname

         const char * opt = expanded_argv[a++];
         const char * val = (a < expanded_argv.size()) ? expanded_argv[a] : 0;

         // at this point, argv[a] is the string after opt, i.e. either the
         // next option or an argument of the current option
         //
         if (!strcmp(opt, "--"))   // end of options marker
            {
              break;   // for (size_t a = 1; ...
            }

         if (*opt == '+')   // APL application option
            {
              ++opt;   // skip first +
              while (*opt == '+')
                    {
                      ++opt;   // skip +
                      ++a;     // skip arg
                    }
              continue;
            }

         if (!strcmp(opt, "-C"))
            {
              ++a;
              continue;   // -C already handled in parse_argv_1()
            }

#if cfg_CORE_COUNT_WANTED == -2
         if (!strcmp(opt, "--cc"))
            {
              ++a;
              if (!val)
                 {
                   CERR << "--cc without core count" << endl;
                   exit(a);
                 }
              requested_cc = (CoreCount)atoi(val);
              continue;
            }
#endif
         if (!strcmp(opt, "--cfg"))
            {
              show_configure_options();
              exit(0);
            }

         if (!strcmp(opt, "--CPU_limit_secs"))
            {
              ++a;
              if (!val)
                 {
                   CERR << "--CPU_limit_secs without seconds" << endl;
                   exit(a);
                 }
              CPU_limit_secs = atoi(val);
              continue;
            }

         if (!strcmp(opt, "--Color"))
            {
              do_Color = true;
              continue;
            }

         if (!strcmp(opt, "-d"))
            {
              daemon = true;
              continue;
            }

         if (!strcmp(opt, "--echoCIN"))
            {
              echo_CIN = true;
              do_not_echo = true;
              do_Color = false;
              continue;
            }

         if (!strcmp(opt, "--emacs"))
            {
              emacs_mode = true;
              continue;
            }

         if (!strcmp(opt, "--emacs_arg"))
            {
              ++a;
              if (!val)
                 {
                   CERR << "--emacs_arg without argument" << endl;
                   exit(a);
                 }

              emacs_mode = true;
              emacs_arg = val;
              continue;
            }

         if (!strcmp(opt, "--eval"))
            {
              ++a;
              if (!val)
                 {
                   CERR << "--eval without APL line" << endl;
                   exit(a);
                 }
              eval_exprs.push_back(val);
              uprefs.silent   = true;
              uprefs.do_Color = false;
              continue;
            }

         if (!strcmp(opt, "-f"))
            {
              ++a;
              if (!val)
                 {
                   CERR << "-f without argument" << endl;
                   exit(a);
                 }

              const UTF8_string & filename(val);
              InputFile fam(filename, 0, false, !do_not_echo, true, no_LX);
              InputFile::files_todo.push_back(fam);
              InputFile::files_orig.push_back(fam);
              continue;
            }

         if (!strcmp(opt, "--gpl"))
            {
              show_GPL(cout);
              exit(0);
            }

         if (!strcmp(opt, "-h") || !strcmp(opt, "--help"))
            {
              usage(expanded_argv[0]);
              exit(0);
            }

         if (!strcmp(opt, "--mem"))
            {
              mem_arg = "";   // assume no value
              if (val)   // --huge with user-supplied value
                 {
                   ++a;   // skip val
                   mem_arg = val;
                 }

              Quad_WA::parse_mem(logit);
              continue;
            }

         if (!strcmp(opt, "--id"))
            {
              ++a;
              if (!val)
                 {
                   CERR << "--id without processor number" << endl;
                   exit(a);
                 }

              requested_id = atoi(val);
              continue;
            }

         if (!strcmp(opt, "-L"))
            {
              ++a;
              if (!val)
                 {
                   CERR << "-L without workspace name" << endl;
                   exit(a);
                 }

              initial_workspace = UTF8_string(val);
              continue;
            }

         if (!strcmp(opt, "--LX"))
            {
              ++a;
              if (!val)
                 {
                   CERR << "--LX without APL expression" << endl;
                   exit(a);
                 }

              latent_expression = UTF8_string(val);
              continue;
            }

         if (!strcmp(opt, "-l"))
            {
              ++a;
              if (val && atoi(val) == LID_startup)   logit = true;
#ifdef cfg_DYNAMIC_LOG_WANTED
              if (val)   Log_control(LogId(atoi(val)), true);
              else
                 {
                   CERR << "-l without log facility" << endl;
                   exit(a);
                 }
#else
   if (val && atoi(val) == LID_startup)   ;
   else  CERR << "the -l option was ignored (requires ./configure "
                   "DYNAMIC_LOG_WANTED=yes)" << endl;
#endif // cfg_DYNAMIC_LOG_WANTED

              continue;
            }

         if (!strcmp(opt, "--noCIN"))
            {
              do_not_echo = true;
              continue;
            }

         if (!strcmp(opt, "--to_COUT"))
            {
              output_to_cout = false;
              continue;
            }

         if (!strcmp(opt, "--noColor"))
            {
              do_Color = false;
              continue;
            }

         if (!strcmp(opt, "--noCONT"))
            {
              do_CONT = false;
              continue;
            }

         if (!strcmp(opt, "--noSV"))
            {
              user_do_svars = false;
              system_do_svars = false;
              continue;
            }

         if (!strcmp(opt, "--OFF"))
            {
              auto_OFF = true;
              continue;
            }

         if (!strcmp(opt, "-p"))
            {
              ++a;
              continue;   // -p already handled in parse_argv_1()
            }

         if (!strcmp(opt, "--par"))
            {
              ++a;
              if (!val)
                 {
                   CERR << "--par without processor number" << endl;
                   exit(a);
                 }
              requested_par = atoi(val);
              continue;
            }

         if (!strcmp(opt, "--PW"))
            {
              ++a;
              if (!val)
                {
                  CERR << "--PW without screen width" << endl;
                  exit(a);
                }

              initial_pw = atoi(val);
              if (initial_pw < MIN_Quad_PW || initial_pw > MAX_Quad_PW)
                {
                  CERR << "bad --PW value (ignored)" << endl; 
                }
              else
                {
                  Workspace::set_PW(initial_pw, LOC);
                }
              continue;
            }

         if (!strcmp(opt, "--rawCIN"))
            {
              raw_cin = true;
              continue;
            }

         if (!strcmp(opt, "--SV"))
            {
              user_do_svars = true;
              system_do_svars = true;
              continue;
            }

         if (!strcmp(opt, "--safe"))
            {
              safe_mode = true;
              user_do_svars = false;
              system_do_svars = false;
              continue;
            }
         if (!strcmp(opt, "-s") || !strcmp(opt, "--script"))
            {
              do_CONT = false;               // --noCONT
              do_not_echo = true;            // -noCIN
              do_Color = false;              // --noColor
              silent = true;                 // --silent
              continue;
            }

         if (!strcmp(opt, "--show_bin_dir"))
            {
              COUT << apl_DIR__bin << endl;
              exit(0);
            }

         if (!strcmp(opt, "--show_doc_dir"))
            {
              COUT << apl_DIR__doc << endl;
              exit(0);
            }

         if (!strcmp(opt, "--show_etc_dir"))
            {
              COUT << apl_DIR__sysconf << endl;
              exit(0);
            }

         if (!strcmp(opt, "--show_lib_dir"))
            {
              COUT << apl_DIR__pkglib << endl;
              exit(0);
            }

         if (!strcmp(opt, "--show_src_dir"))
            {
              COUT << apl_DIR__src << endl;
              exit(0);
            }

         if (!strcmp(opt, "--show_all_dirs"))
            {
              COUT << "bindir: " << apl_DIR__bin     << endl
                   << "docdir: " << apl_DIR__doc     << endl
                   << "etcdir: " << apl_DIR__sysconf << endl
                   << "libdir: " << apl_DIR__pkglib  << endl
                   << "srcdir: " << apl_DIR__src     << endl;
              exit(0);
            }

         if (!strcmp(opt, "--silent") || !strcmp(opt, "-q"))
            {
              silent = true;
              continue;
            }

         if (!strcmp(opt, "-T"))
            {
              do_CONT = false;

              // IO_Files::open_next_file() will exit if it sees "-"
              //
              IO_Files::need_total = true;
              for (; a < expanded_argv.size(); ++a)   // inner for
                  {
                    if (!strcmp(expanded_argv[a], "--"))  // end of -T args
                       {
                         ++a;   // skip --
                         break;           // inner for

                       }
                    const UTF8_string & filename = expanded_argv[a];
                    InputFile fam(filename, 0, true, true, false, no_LX);
                    InputFile::files_todo.push_back(fam);
                    InputFile::files_orig.push_back(fam);
                  }

              // 
              if (IO_Files::need_total)
                 {
                   // truncate summary.log, unless append_summary is desired
                   //
                   if (!append_summary)
                      {
                        ofstream summary("testcases/summary.log",
                                         ios_base::trunc);
                      }
                 }
              continue;
            }

         if (!strcmp(opt, "--TM"))
            {
              ++a;
              if (!val)
                 {
                   CERR << "--TM without test mode" << endl;
                   exit(a);
                 }
              const int mode = 1 << atoi(val);
              IO_Files::test_mode = IO_Files::TestMode(mode);
              continue;
            }

         if (!strcmp(opt, "--TR"))
            {
              randomize_testfiles = true;
              continue;
            }

         if (!strcmp(opt, "--TS"))
            {
              append_summary = true;
              continue;
            }

         if (!strcmp(opt, "--tcp_port"))
            {
              ++a;
              if (!val)
                 {
                   CERR << "--tcp_port without port number (>0)" << endl;
                   exit(a);
                 }
              tcp_port = atoi(val);
              user_do_svars = system_do_svars = false;   // aka. --noSV
              raw_cin = true;                            // aka. --rawCIN
         //   do_not_echo = true;                        // aka. --noCIN
              continue;
            }

         if (!strcmp(opt, "--tcp_websocket"))
            {
              tcp_websocket = true;
              continue;
            }

         if (!strcmp(opt, "-v") || !strcmp(opt, "--version"))
            {
              show_version(cout);
              exit(0);
            }
         if (!strcmp(opt, "-u"))
            {
              ++a;
              continue;   // -u already handled in parse_argv_1()
            }

         if (!strcmp(opt, "-w"))
            {
              ++a;
              if (!val)
                 {
                   CERR << "-w without milli(seconds)" << endl;
                   exit(a);
                 }
              wait_ms = atoi(val);
              continue;
            }

         CERR << "unknown option '" << opt << "'" << endl;
         usage(expanded_argv[0]);
         exit(a);
       }

   if (logit)
      {
        CERR << InputFile::files_todo.size() << " input files:" << endl;
        loop(f, InputFile::files_todo.size())
            CERR << "    " << InputFile::files_todo[f].filename << endl;
      }

   if (randomize_testfiles)   InputFile::randomize_files();

   /* Note. Let the first line of script.apl be:

      #!/home/eedjsa/apl-1.9/src/apl -l 37 --script

     apl may have been started in one off these ways:

      A.  ./apl -f script1.apl   # with script1.apl NOT executable. Then:
               expanded_argv[script_argc] is:  ./apl
              InputFile::files_todo.size():    1   (from -f)
               script_argc is:                 0

      B.  ./apl -f script2.apl   # with script2.apl executable. Then:
              expanded_argv[script_argc] is:   script2.apl
              InputFile::files_todo.size():    1    (from -f)
               script_argc is:                 2

      C.   ./apl
               expanded_argv[script_argc] is:  ./apl
              InputFile::files_todo.size():    0    (since no -f)
               script_argc is:                 0

      D.  ./script.apl   # with script2.apl executable. Then:
              expanded_argv[script_argc] is:   script2.apl
              InputFile::files_todo.size():    0    (since no -f)
              script_argc is:                  1    #!./apl
              script_argc is:                  2    #!./apl --script
              script_argc is:                  4    #!./apl -l 37 --script

      In case D. the interpreter has lost script_argc and the script file
      needs to be appended to files_todo.
    */

   if (InputFile::files_todo.size() == 0)   // no -f (cases C. and D.)
   if (script_argc > 0)                     // (case D.)
      {
        const UTF8_string & filename = expanded_argv[script_argc];
        InputFile fam(filename, 0, false, !do_not_echo, true, no_LX);
        InputFile::files_todo.insert(InputFile::files_todo.begin(), fam);
      }

   // count number of testfiles
   //
   IO_Files::testcase_count = InputFile::testcase_file_count();
}
//----------------------------------------------------------------------------
/**
    GNU APL can be started in a number of ways:

    1. directly - argc/argv have the usual meaning

    2. as a script without arguments on the first script line, e.g.
       /usr/bin/apl SCRIPT.apl [ scriptargs... ]

    3. as a script with arguments on the first script line, e.g.
       /usr/bin/apl 'line 1 arguments' SCRIPT.apl [ scriptargs... ]

    4. On a non-GNU/linux OS; the semantics of the first script line
       is unknown.

    To make this work, 2, should be avoided and 4. should use exactly one
    argument. The following first script lines shall work (assuming GNU APL
    lives in /usr/bin:

    /usr/bin/apl --
    /usr/bin/apl -s
    /usr/bin/apl --script
    /usr/bin/apl -s --             at least on GNU/linux
    /usr/bin/apl --script --       at least on GNU/linux

    expand_argv() does the following:

    1. expand argv[1] into multiple arguments
    2. maybe set script_argc

 **/
void
UserPreferences::expand_argv(int argc, const char ** argv)
{
   // 1. copy the command line arguments into original_argv and expanded_argv.
   //
   loop(a, argc)
       {
         original_argv.push_back(argv[a]);
         expanded_argv.push_back(argv[a]);
       }

   if (argc <= 1)   // program name argv[0] only, hence no arguments to expand
      {
        return;
      }

   if (is_APL_script(argv[1]))   // case 2: script without argument
      {
        script_argc = 1;
        return;
      }

   if (!is_APL_script(argv[2]))   return;   // not run from a script

   /* at this point:

      argv[1] is the optional argument of the interpreter on the shebang line,
      argv[2] is the name of the script, and
      argv[3] ... are the command line arguments given by the user
                  when starting the script.
      expand argv[1], stopping at --
    */
const char * apl_args = argv[1];   // the args after e.g. /usr/bin/apl
   script_argc = 2;
   if (!strchr(apl_args, ' '))   // single option
      {
        return;
      }

   // remove expanded_argv[1] and insert the expanded expanded_argv[1] instead
   expanded_argv.erase(expanded_argv.begin() + 1);
   --script_argc;
   for (int index = 1;;)
       {
         // skip leading whitespace
         //
         while (*apl_args && *apl_args <= ' ')   ++apl_args;

         if (*apl_args == 0)   break;

         if (!strcmp(apl_args, "--"))        // "--" at end of apl_args
            {
              expanded_argv.insert(expanded_argv.begin() + index++, "--");
              ++script_argc;
              break;   // done
            }

         if (!strncmp(apl_args, "-- ", 2) &&
               apl_args[2] <= ' ')   // "--" somewhere in apl_args
            {
              expanded_argv.insert(expanded_argv.begin() + index++, "--");
              ++script_argc;
              apl_args += 2;   // skip "--"
              continue;
            }

         const char * arg_end = strchr(apl_args, ' ');
         if (arg_end)   // more arguments
            {
              const int arg_len = arg_end - apl_args;
              const char * arg = strndup(apl_args, arg_len);
              expanded_argv.insert(expanded_argv.begin() + index++, arg);
              ++script_argc;
              apl_args += arg_len;
            }
         else           // last argument
            {
              const char * arg = strdup(apl_args);
              expanded_argv.insert(expanded_argv.begin() + index++, arg);
              ++script_argc;
              break;
            }
       }
}
//----------------------------------------------------------------------------
void
UserPreferences::show_version(ostream & out)
{
   out << "BUILDTAG:" << endl
       << "---------" << endl;
   for (const char ** bt = build_tag; *bt; ++bt)
       {
         switch(bt - build_tag)
            {
              case 0:  out << "    Project:        ";   break;
              case 1:  out << "    Version / SVN:  ";   break;
              case 2:  out << "    Build Date:     ";   break;
              case 3:  out << "    Build OS:       ";   break;
              case 4:  out << "    config.status:  ";   break;
              default: out << "    [" << bt - build_tag << "] ";
            }
         out << *bt << endl;
       }

   out << "    Archive SVN:   " << ARCHIVE_SVN << endl;

   Output::set_color_mode(Output::COLM_OUTPUT);
}
//----------------------------------------------------------------------------
void
UserPreferences::show_configure_options()
{
   enum { value_size = sizeof(Value),
          cell_size  = sizeof(Cell),
          header_size = value_size - cell_size * cfg_SHORT_VALUE_LENGTH_WANTED,
        };

   CERR << endl << "configurable options:" << endl <<
                   "---------------------" << endl <<

   "    ASSERT_LEVEL_WANTED=" << cfg_ASSERT_LEVEL_WANTED
        << is_default(cfg_ASSERT_LEVEL_WANTED == 1)
   << endl <<

   "    SECURITY_LEVEL_WANTED=" << cfg_SECURITY_LEVEL_WANTED
        << is_default(cfg_SECURITY_LEVEL_WANTED == 0)
   << endl <<

   "    APSERVER_PATH=" << cfg_APSERVER_PATH
        << is_default(!strcmp(cfg_APSERVER_PATH, "/tmp/GNU-APL/APserver"))
   << endl <<

   "    APSERVER_PORT=" << cfg_APSERVER_PORT
        << is_default(cfg_APSERVER_PORT == 16366)
   << endl <<

   "    APSERVER_TRANSPORT=" << cfg_APSERVER_TRANSPORT
        << is_default(cfg_APSERVER_TRANSPORT == 0)
   << endl <<

   "    CORE_COUNT_WANTED=" << cfg_CORE_COUNT_WANTED <<
#if   cfg_CORE_COUNT_WANTED == -3
   "  (= ⎕SYL)"
#elif cfg_CORE_COUNT_WANTED == -2
   "  (= argv (--cc))"
#elif cfg_CORE_COUNT_WANTED == -1
   "  (= all)"
#elif cfg_CORE_COUNT_WANTED == 0
   "  (= sequential) (default)"
#else
   ""
#endif
   << endl <<

#ifdef cfg_DYNAMIC_LOG_WANTED
   "    DYNAMIC_LOG_WANTED=yes"
#else
   "    DYNAMIC_LOG_WANTED=no (default)"
#endif
   << endl <<

   "    MAX_RANK_WANTED="     << cfg_MAX_RANK_WANTED
        << is_default(cfg_MAX_RANK_WANTED == 8)
   << endl <<

#ifdef cfg_RATIONAL_NUMBERS_WANTED
   "    RATIONAL_NUMBERS_WANTED=yes"
#else
   "    RATIONAL_NUMBERS_WANTED=no (default)"
#endif
   << endl <<

   "    SHORT_VALUE_LENGTH_WANTED=" << cfg_SHORT_VALUE_LENGTH_WANTED
        << is_default(cfg_SHORT_VALUE_LENGTH_WANTED == 1)
        << ", therefore:" << endl <<
   "        sizeof(Value)       : "  << value_size  << " bytes" << endl <<
   "        sizeof(Cell)        :  " << cell_size   << " bytes" << endl <<
   "        sizeof(Value header): "  << header_size << " bytes" << endl
   << endl <<

#ifdef cfg_VALUE_CHECK_WANTED
   "    VALUE_CHECK_WANTED=yes"
#else
   "    VALUE_CHECK_WANTED=no (default)"
#endif
   << endl <<

#ifdef cfg_VALUE_HISTORY_WANTED
   "    VALUE_HISTORY_WANTED=yes"
#else
   "    VALUE_HISTORY_WANTED=no (default)"
#endif
   << endl <<

#ifdef cfg_VF_TRACING_WANTED
   "    VF_TRACING_WANTED=yes"
#else
   "    VF_TRACING_WANTED=no (default)"
#endif
   << endl <<

#ifdef cfg_VISIBLE_MARKERS_WANTED
   "    VISIBLE_MARKERS_WANTED=yes"
#else
   "    VISIBLE_MARKERS_WANTED=no (default)"
#endif
   << endl
   << endl
   << "how ./configure was (probably) called:" << endl
   << "--------------------------------------" << endl
   << "    " cfg_CONFIGURE_ARGS << endl
   << endl;

   show_version(CERR);
}
//----------------------------------------------------------------------------
FILE *
UserPreferences::open_user_file(const char * fname, char * filename,
                                bool sys, bool log_startup)
{
   if (sys)   // filename in /etc/gnu-apl.d/preferences
      {
        snprintf(filename, APL_PATH_MAX,
                 "%s/gnu-apl.d/%s", apl_DIR__sysconf, fname);
      }
   else       // try $HOME/.gnu_apl
      {
        const char * HOME = getenv("HOME");
        if (HOME == 0)
           {
             if (log_startup)
                CERR << "environment variable 'HOME' is not defined!" << endl;
             return 0;
           }

        snprintf(filename, APL_PATH_MAX, "%s/.gnu-apl/%s", HOME, fname);
        filename[APL_PATH_MAX] = 0;

        // check that $HOME/.gnu-apl/preferences exist and fall back to
        // $HOME/.config gnu-apl/preferences if not
        //
        if (access(filename, F_OK) != 0)   // file does not exist
           {
             snprintf(filename, APL_PATH_MAX,
                      "%s/.config/gnu-apl/%s", HOME, fname);
           }
      }

   filename[APL_PATH_MAX] = 0;

FILE * f = fopen(filename, "r");
   if (f == 0)
      {
         if (log_startup)
            CERR << "Not reading config file " << filename
                 << " (" << strerror(errno) << ")" << endl;
         return 0;
      }

   if (log_startup)
      CERR << "Reading config file " << filename << " ..." << endl;

   return f;
}
//----------------------------------------------------------------------------
void
UserPreferences::read_config_file(bool sys, bool log_startup)
{
char filename[APL_PATH_MAX + 1];

FILE * f = open_user_file("preferences", filename, sys, log_startup);
   if (f == 0)   return;

int line = 0;
int file_profile = 0;   // the current profile in the preferences file
   for (;;)
       {
         enum { BUFSIZE = 200 };
         char buffer[BUFSIZE + 1];
         const char * s = fgets(buffer, BUFSIZE, f);
         if (s == 0)   break;   // end of file

         buffer[BUFSIZE] = 0;
         ++line;

         // skip leading spaces
         //
         while (*s && *s <= ' ')   ++s;

         if (*s == 0)     continue;   // empty line
         if (*s == '#')   continue;   // comment line

         char opt[BUFSIZE] = { 0 };
         char arg[BUFSIZE] = { 0 };
         int d[100];
         const int count = sscanf(s, "%s"
                   " %s %X %X %X %X %X %X %X %X %X"
                   " %X %X %X %X %X %X %X %X %X %X"
                   " %X %X %X %X %X %X %X %X %X %X"
                   " %X %X %X %X %X %X %X %X %X %X"
                   " %X %X %X %X %X %X %X %X %X %X"
                   " %X %X %X %X %X %X %X %X %X %X"
                   " %X %X %X %X %X %X %X %X %X %X"
                   " %X %X %X %X %X %X %X %X %X %X"
                   " %X %X %X %X %X %X %X %X %X %X"
                   " %X %X %X %X %X %X %X %X %X %X",
              opt, arg,  d+ 1, d+ 2, d+ 3, d+ 4, d+ 5, d+ 6, d+ 7, d+ 8, d+ 9,
                   d+10, d+11, d+12, d+13, d+14, d+15, d+16, d+17, d+18, d+19,
                   d+20, d+21, d+22, d+23, d+24, d+25, d+26, d+27, d+28, d+29,
                   d+30, d+31, d+32, d+33, d+34, d+35, d+36, d+37, d+38, d+39,
                   d+40, d+41, d+42, d+43, d+44, d+45, d+46, d+47, d+48, d+49,
                   d+50, d+51, d+52, d+53, d+54, d+55, d+56, d+57, d+58, d+59,
                   d+60, d+61, d+62, d+63, d+64, d+65, d+66, d+67, d+68, d+69,
                   d+70, d+71, d+72, d+73, d+74, d+75, d+76, d+77, d+78, d+79,
                   d+80, d+81, d+82, d+83, d+84, d+85, d+86, d+87, d+88, d+89,
                   d+90, d+91, d+92, d+93, d+94, d+95, d+96, d+97, d+98, d+99);

         if (count < 2)
            {
              CERR << "bad tag or value at line " << line
                   << " of config file " << filename << " (ignored)" << endl;
              continue;
            }
         d[0] = strtoll(arg, 0, 16);
         const bool yes = !strcasecmp(arg, "YES"     )
                       || !strcasecmp(arg, "ENABLED" )
                       || !strcasecmp(arg, "ON"      );

         const bool no  = !strcasecmp(arg, "NO"      )
                       || !strcasecmp(arg, "DISABLED")
                       || !strcasecmp(arg, "OFF"     ) ;

         const bool yes_no  = yes || no;

         // first check for profile commands and continue if we are in the
         // wrong profile. Profile 0 in the file matches all profiles of
         // the user while Profile N > 0 in the file requires the same
         // profile requested by the user.
         //
         if (!strcasecmp(opt, "PROFILE"))   // Never ignore Profile entries
            {
              file_profile = atoi(arg);
              continue;
            }

         if (file_profile && (file_profile != user_profile))   continue;

         // set up pointers to arguments...
         //
         const char * sargs[100];
         char * p = buffer;
         int sargs_idx = 0;
         for (;;)
            {
              while (*p && *p <= ' ')   ++p;    // discard leading whitespace
              if (*p == 0)   break;

              if (p[0] == '/' && p[1] == '/')   break;   // comment

              const char * arg = p;
              sargs[sargs_idx++] = arg;         // store start of argument
              while (*p > ' ')   ++p;           // skip non-whitespaces
              if (*p)   *p++ = 0;
              else      break;
            }

         // some args could be non-numeric
         //
         if (0 && sargs_idx < count)   // bogus assertion (disable it)
            {
               CERR << "sargs_idx: "          << sargs_idx << endl
                    << "count: "              << count << endl
                    << "file_profile: "       << file_profile << endl
                    << "sargs[" << (sargs_idx-1)
                                << "]: " << sargs[sargs_idx-1] << endl
                    ;

               Assert(0);
            }
         sargs[sargs_idx] = "NUL";              // terminating 0

         if (!strcasecmp(opt, "COLOR"))
            {
              if (yes_no)   // user said "YES" or "NO"
                 {
                   // if the user said "Yes" (which is an obsolete setting)
                   // then use_curses is cleared because at the time when
                   // "Yes" was valid, curses was not yet implemented.
                   //
                   do_Color = yes;
                 }

              if (!strcasecmp(arg, "ANSI"))
                 {
                   do_Color = true;
                 }
              else if (!strcasecmp(arg, "CURSES"))
                 {
                   do_Color = true;
                 }
            }
         else if (!strcasecmp(opt, "KEYBOARD"))
            {
              // obsolete
            }
         else if (yes_no && !strcasecmp(opt, "WELCOME"))
            {
              silent = no;
            }
         else if (yes_no && !strcasecmp(opt, "SHAREDVARS"))
            {
              user_do_svars = yes;
              system_do_svars = yes;
            }
         else if (!strcasecmp(opt, "LOGGING"))
            {
              d[0] = strtoll(arg, 0, 10);   // decimal!
              Log_control(LogId(d[0]), true);
              log_startup && CERR << "    logging facility " << d[0]
                                  << " enabled in " << filename << endl;
            }
         else if (!strcasecmp(opt, "CIN-SEQUENCE"))
            {
              loop(sa, sargs_idx)
                  Output::color_CIN[sa] = decode_ASCII(sargs[sa + 1]);
            }
         else if (!strcasecmp(opt, "COUT-SEQUENCE"))
            {
              loop(sa, sargs_idx)
                  Output::color_COUT[sa] = decode_ASCII(sargs[sa + 1]);
            }
         else if (!strcasecmp(opt, "CERR-SEQUENCE"))
            {
              loop(sa, sargs_idx)
                  Output::color_CERR[sa] = decode_ASCII(sargs[sa + 1]);
            }
         else if (!strcasecmp(opt, "UERR-SEQUENCE"))
            {
              loop(sa, sargs_idx)
                  Output::color_UERR[sa] = decode_ASCII(sargs[sa + 1]);
            }
         else if (!strcasecmp(opt, "RESET-SEQUENCE"))
            {
              loop(sa, sargs_idx)
                 Output::color_RESET[sa] = decode_ASCII(sargs[sa + 1]);
            }
         else if (!strcasecmp(opt, "CLEAR-EOL-SEQUENCE"))
            {
              loop(sa, sargs_idx)
                 Output::clear_EOL[sa] = decode_ASCII(sargs[sa + 1]);
            }
         else if (!strcasecmp(opt, "CLEAR-EOS-SEQUENCE"))
            {
              loop(sa, sargs_idx)
                 Output::clear_EOS[sa] = decode_ASCII(sargs[sa + 1]);
            }
         else if (!strcasecmp(opt, "KEY-CURSOR-UP"))
            {
              loop(sa, sargs_idx)
                 Output::ESC_CursorUp[sa] = decode_ASCII(sargs[sa + 1]);
            }
         else if (!strcasecmp(opt, "KEY-CURSOR-DOWN"))
            {
              loop(sa, sargs_idx)
                 Output::ESC_CursorDown[sa] = decode_ASCII(sargs[sa + 1]);
            }
         else if (!strcasecmp(opt, "KEY-CURSOR-RIGHT"))
            {
              loop(sa, sargs_idx)
                 Output::ESC_CursorRight[sa] = decode_ASCII(sargs[sa + 1]);
            }
         else if (!strcasecmp(opt, "KEY-CURSOR-LEFT"))
            {
              loop(sa, sargs_idx)
                 Output::ESC_CursorLeft[sa] = decode_ASCII(sargs[sa + 1]);
            }
         else if (!strcasecmp(opt, "KEY-CURSOR-END"))
            {
              loop(sa, sargs_idx)
                 Output::ESC_CursorEnd[sa] = decode_ASCII(sargs[sa + 1]);
            }
         else if (!strcasecmp(opt, "KEY-CURSOR-HOME"))
            {
              loop(sa, sargs_idx)
                 Output::ESC_CursorHome[sa] = decode_ASCII(sargs[sa + 1]);
            }
         else if (!strcasecmp(opt, "KEY-INSMODE"))
            {
              loop(sa, sargs_idx)
                 Output::ESC_InsertMode[sa] = decode_ASCII(sargs[sa + 1]);
            }
         else if (!strcasecmp(opt, "KEY-DELETE"))
            {
              loop(sa, sargs_idx)
                 Output::ESC_Delete[sa] = decode_ASCII(sargs[sa + 1]);
            }
         else if (!strcasecmp(opt, "CIN-FOREGROUND"))
            {
              Output::color_CIN_foreground = atoi(arg);
            }
         else if (!strcasecmp(opt, "CIN-BACKGROUND"))
            {
              Output::color_CIN_background = atoi(arg);
            }
         else if (!strcasecmp(opt, "COUT-FOREGROUND"))
            {
              Output::color_COUT_foreground = atoi(arg);
            }
         else if (!strcasecmp(opt, "COUT-BACKGROUND"))
            {
              Output::color_COUT_background = atoi(arg);
            }
         else if (!strcasecmp(opt, "CERR-FOREGROUND"))
            {
              Output::color_CERR_foreground = atoi(arg);
            }
         else if (!strcasecmp(opt, "CERR-BACKGROUND"))
            {
              Output::color_CERR_background = atoi(arg);
            }
         else if (!strcasecmp(opt, "UERR-FOREGROUND"))
            {
              Output::color_UERR_foreground = atoi(arg);
            }
         else if (!strcasecmp(opt, "UERR-BACKGROUND"))
            {
              Output::color_UERR_background = atoi(arg);
            }
         else if (!strncasecmp(opt, "LIBREF-", 7))
            {
              const int lib_ref = atoi(opt + 7);
              if (lib_ref < 0 || lib_ref > 9)
                 {
                   CERR << "bad library reference number " << lib_ref
                        << " at line " << line << " of config file "
                        << filename << " (ignored)" << endl;
                   continue;
                 }

              LibPaths::set_lib_dir(LibRef(lib_ref), arg,
                                    sys ? LibPaths::LibDir::CSRC_PREF_SYS
                                        : LibPaths::LibDir::CSRC_PREF_HOME);
            }
         else if (!strcasecmp(opt, "READLINE_HISTORY_LEN"))
            {
              line_history_len = atoi(arg);
            }
         else if (!strcasecmp(opt, "READLINE_HISTORY_PATH"))
            {
              // If 'arg' starts with "~/", expand ~ -> $HOME
              if (arg[0] == '~' && arg[1] == '/') 
                 {
                   const char * HOME = getenv("HOME");
                   if (HOME == 0)
                      {
                        if (log_startup)
                           CERR << "environment variable 'HOME' is not "
                                   "defined for '~' expansion, will use "
                                << (arg + 2) << endl;
                        line_history_path = UTF8_string(arg + 2);
                      }
                   else
                      {
                        line_history_path = UTF8_string(HOME);
                        line_history_path.append_ASCII(arg + 1);
                      }
                 }
              else line_history_path = UTF8_string(arg);
            }
         else if (!strcasecmp(opt, "MEMORY"))
            {
              mem_arg = strdup(arg);
            }
         else if (!strcasecmp(opt, "NABLA-TO-HISTORY"))
            {
              if      (!strcasecmp(arg, "NEVER"))      nabla_to_history = 0;
              else if (!strcasecmp(arg, "MODIFIED"))   nabla_to_history = 1;
              else if (!strcasecmp(arg, "ALWAYS"))     nabla_to_history = 2;
              else   CERR << "bad value " << arg << " for NABLA-TO-HISTORY"
                        << " at line " << line << " of config file "
                        << filename << " (ignored)" << endl;
            }
         else if (yes_no && !strcasecmp(opt, "BACKUP_BEFORE_SAVE"))
            {
              backup_before_save = yes;
            }
         else if (!strcasecmp(opt, "KEYBOARD_LAYOUT_FILE"))
            {
              keyboard_layout_file = UTF8_string(arg);
            }
         else if (!strcasecmp(opt, "CONTROL-Ds-TO-EXIT"))
            {
              control_Ds_to_exit = atoi(arg);
            }
         else if (!strcasecmp(opt, "INITIAL-⎕PW"))
            {
              if (initial_pw >= MIN_Quad_PW && initial_pw <= MAX_Quad_PW)
                 initial_pw = atoi(arg);
              else
                 CERR << "bad value " << arg << " for INITIAL-⎕PW (ignored)"
                      << endl;
            }
         else if (!strcasecmp(opt, "MULTI-LINE-STRINGS"))
            {
              multi_line_strings = yes;
              multi_line_strings_3 = yes;
            }
         else if (!strcasecmp(opt, "NEW-MULTI-LINE-STRINGS"))
            {
              multi_line_strings_3 = yes;
            }
         else if (!strcasecmp(opt, "OLD-MULTI-LINE-STRINGS"))
            {
              multi_line_strings = yes;
            }
         else if (!strcasecmp(opt, "WINCH-SETS-⎕PW"))
            {
              WINCH_sets_pw = yes;
            }
         else if (!strcasecmp(opt, "AUTO-OFF"))
            {
              auto_OFF = yes;
            }
         else if (!strcasecmp(opt, "DISCARD-INDENTATION"))
            {
              discard_indentation = yes;
            }

         else if (!strcasecmp(opt, "OUTPUT-TO-COUT"))
            {
              output_to_cout = yes;
            }

         // security facilities...
         //
#define sec_def(X) else if (!strcasecmp(opt, #X)) X = yes;
#include "Security.def"

         else
            {
              CERR << "Invalid option '" << opt << " in preferences file "
                   << filename << " (ignored)" << endl;
            }
       }

   fclose(f);
}
//----------------------------------------------------------------------------
int
UserPreferences::decode_ASCII(const char * strg)
{
   if (!strcmp(strg, "NUL"))   return 0x00;
   if (!strcmp(strg, "SOH"))   return 0x01;
   if (!strcmp(strg, "STX"))   return 0x02;
   if (!strcmp(strg, "ETX"))   return 0x03;
   if (!strcmp(strg, "EOT"))   return 0x04;
   if (!strcmp(strg, "ENQ"))   return 0x05;
   if (!strcmp(strg, "ACK"))   return 0x06;
   if (!strcmp(strg, "BEL"))   return 0x07;
   if (!strcmp(strg, "BS"))   return 0x08;
   if (!strcmp(strg, "HT"))    return 0x09;
   if (!strcmp(strg, "LF"))    return 0x0A;
   if (!strcmp(strg, "VT"))    return 0x0B;
   if (!strcmp(strg, "FF"))    return 0x0C;
   if (!strcmp(strg, "CR"))    return 0x0D;
   if (!strcmp(strg, "SO"))    return 0x0E;
   if (!strcmp(strg, "SI"))    return 0x0F;
   if (!strcmp(strg, "DLE"))   return 0x10;
   if (!strcmp(strg, "DC1"))   return 0x11;
   if (!strcmp(strg, "DC2"))   return 0x12;
   if (!strcmp(strg, "DC3"))   return 0x13;
   if (!strcmp(strg, "DC4"))   return 0x14;
   if (!strcmp(strg, "NAK"))   return 0x15;
   if (!strcmp(strg, "SYN"))   return 0x16;
   if (!strcmp(strg, "ETB"))   return 0x17;
   if (!strcmp(strg, "CAN"))   return 0x18;
   if (!strcmp(strg, "EM"))    return 0x19;
   if (!strcmp(strg, "SUB"))   return 0x1A;
   if (!strcmp(strg, "ESC"))   return 0x1B;
   if (!strcmp(strg, "FS"))    return 0x1C;
   if (!strcmp(strg, "GS"))    return 0x1D;
   if (!strcmp(strg, "RS"))    return 0x1E;
   if (!strcmp(strg, "US"))    return 0x1F;

   if (strlen(strg) == 1)   return *strg;                  // single char
   if (strlen(strg) == 2)   return strtoll(strg, 0, 16);   // hex value, e.g. 4C
   CERR << "invalid parameter " << strg << " in preferences file" << endl;
   return -1;
}
//----------------------------------------------------------------------------
void
UserPreferences::read_threshold_file(bool sys, bool log_startup)
{
char filename[APL_PATH_MAX + 1];

FILE * f = open_user_file("parallel_thresholds", filename, sys, log_startup);
   if (f == 0)   return;

int line = 0;
   for (;;)
       {
         enum { BUFSIZE = 200 };
         char buffer[BUFSIZE + 1];
         const char * s = fgets(buffer, BUFSIZE, f);
         if (s == 0)   break;   // end of file

         buffer[BUFSIZE] = 0;
         ++line;

         // skip leading spaces
         //
         while (*s && *s <= ' ')   ++s;

         if (*s == 0)     continue;   // empty line
         if (*s == '#')   continue;   // comment line or #undef

         // eg. perfo_2(F12_MINUS, _AB, "A - B",  25 )
         //
         const char * p = strstr(s, "perfo_");
         if (p == 0)   continue;

         p += 6;   // skip "perfo_"
         const int macro_type = *p++ - '0';
         if (macro_type < 1 || macro_type > 3)
            {
               CERR << "Bad macro in file " << filename
                    << " line " << line << endl;
               continue;
            }

         // macro arguments: (param, p_ab, p_txt, p_be)
         //
         {
           const char * param = strchr(p, '(');
           if (param == 0)   goto param_error;
           skip_spaces(++param);

           const char * p_ab = strchr(param, ',');
           if (p_ab == 0)   goto param_error;
           skip_spaces(++p_ab);
           const int i_ab = p_ab[1] == 'A' ? 2 : 1;

           const char * p_txt = strchr(p_ab, ',');
           if (p_txt == 0)   goto param_error;
           skip_spaces(++p_txt);

           const char * p_be = strchr(p_txt, ',');
           if (p_be == 0)   goto param_error;
           skip_spaces(++p_be);

           const ShapeItem value = strtoll(p_be, 0, 0);

         enum { _B = 1, _AB = 2 };
#define perfo_1(bif, ab, _name, th)   if (!strncmp(param, #bif, strlen(#bif))) \
   set_threshold(&Bif_ ## bif::fun, ab, i_ab, value);

#define perfo_2(bif, ab, _name, thr)  perfo_1(bif, ab, _name, thr)
#define perfo_3(bif, ab, _name, thr)  perfo_1(bif, ab, _name, thr)
#define perfo_4(bif, ab, _name, thr)

#include "Performance.def"
         }
         continue;

         param_error:
            CERR << "Bad macro format in file " << filename
                 << " line " << line << endl;
       }
}
//----------------------------------------------------------------------------
void
UserPreferences::set_threshold(cFunction_P fun, int ab, int i_ab,
                               ShapeItem threshold)
{
   if (fun == 0)    return;
   if (ab != i_ab)  return;

   if (ab == 1)   const_cast<Function *>(fun)->set_monadic_threshold(threshold);
   else           const_cast<Function *>(fun)->set_dyadic_threshold(threshold);
}
//----------------------------------------------------------------------------

