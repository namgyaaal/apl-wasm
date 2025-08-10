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

#ifndef __IO_FILES_HH_DEFINED__
#define __IO_FILES_HH_DEFINED__

#include <stdio.h>
#include <iostream>
#include <fstream>

#include "Assert.hh"
#include "UTF8_string.hh"

using namespace std;

/*
 The classes below are used to combine normal user I/O and automatic
 testcase execution. It works like this:

       (cin)
         │
         │
         V
     ╔═══════╗             ╔═══════════════════╗
     ║ Input ║ <────────── ║ testcase files(s) ║
     ╚═══════╝             ╚═══════════════════╝
         │                           │
         │                           │
         V                           │
      ╔═════╗                        │
      ║ APL ║                        │
      ╚═════╝                        │
         │                           │
         │                           │
         ├───────────────────────┐   │
         │                       │   │
         │                       V   V
         │                    ╔═════════╗
         │                    ║ compare ║
         │                    ╚═════════╝
         │                         │
         │                         │
         V                         V
       (cout)                (test results)
 
 */

/** handling of Input files. IO_Files reads lines from files until they
    are all executed. The output of the APL interpreter is compared against
    the testcase files and mismatches are recorded.
 */
/// a sequence of files that are still to be executed
class IO_Files
{
   friend int main(int argc, const char *argv[]);
   friend struct UserPreferences;

public:
   /// count an error
   static void reset_errors()
      {
        apl_errors = 0;
        assert_errors = 0;
        diff_errors = 0;
        parse_errors = 0;
      }

   /// return the total number of errors
   static int error_count()
      { return apl_errors + assert_errors + diff_errors + parse_errors; }

   /// return \b true iff the interpreter shall exit after a file I/O error
   static bool exit_on_error()
      { return test_mode == TM_EXIT_AFTER_FILE_ERROR; }

   /// count and report a parse error
   static void syntax_error();

   /// reset APL errors, expecting cnt
   static void expect_apl_errors(const UCS_string & arg);

   /// count an APL error
   static void apl_error(const char * loc);

   /// count a failed assertion
   static void assert_error();

   /// count a output diff error
   static void diff_error();

   /// get one line from the current file, open the next file if necceessary
   static void get_file_line(UTF8_string & line, bool & eof);

   /// open the next test file 
   static void open_next_file();

   /// close current testcase file (prematurely) and opem mext one
   static void next_file();

   /// return the current test report
   static ofstream & get_current_testreport()
      { return current_testreport; }

   /// read one line from the current input file with CR and LF removed
   static void read_file_line(UTF8_string & file_line, bool & eof);

protected:
   /// dito (close files, print errors, summary etc).
   static bool end_of_current_file();

   /// how to handle test results
   static enum TestMode
      {
        /// exit() after the last testcase file was processed.
        TM_EXIT_AFTER_LAST_FILE       = 1 << 0,

        /// exit() after the last testcase file was processed and no error
        /// was detected.
        TM_EXIT_AFTER_LAST_FILE_IF_OK = 1 << 1,

        /// remain in APL interpreter after the last testcase
        /// file was processed.
        TM_STAY_AFTER_LAST_FILE       = 1 << 2,

        /// stop test execution after the first testcase file failed (but
        /// remain in the APL interpreter)
        TM_STOP_AFTER_FILE_ERROR      = 1 << 3,

        /// exit() after the first testcase file failed
        TM_EXIT_AFTER_FILE_ERROR      = 1 << 4,

        /// exit() after the first comparison (= testcase file line) failed
        TM_EXIT_AFTER_LINE_ERROR      = 1 << 5,

        /// stop test execution after the first testcase line failed (but
        TM_STOP_AFTER_LINE_ERROR      = 1 << 6,

        /// done if the first testcase line has failed
        TM_DONE_AFTER_LINE_ERROR = TM_STOP_AFTER_LINE_ERROR
                                 | TM_EXIT_AFTER_LINE_ERROR,
      } test_mode;   ///< the desired test mode as per --TM n

   /// write testcases summary file
   static void print_summary();

   /// true until total error count is printed.
   static bool need_total;

   /// the number of testcases provided
   static int testcase_count;

   /// the number of testcases executed
   static int testcases_done;

   /// the number of parse errors when executing test file
   static int parse_errors;

   /// the tital number of errors in all test files
   static int total_errors;

   /// the number of APL errors when executing in current file
   static int apl_errors;

   /// the source location where the last APL error was thrown
   static const char * last_apl_error_loc;

   /// the testcase file line that has triggered the last APL error
   static int last_apl_error_line;

   /// the number of output differences when executing test file
   static int diff_errors;

   /// the number of failed assertions when executing test file
   static int assert_errors;

   /// the current test report
   static ofstream current_testreport;

   /// name of the summary file (same directory as first testcase).
   static UTF8_string summary_path;

   /// when a .tc file was started
   static APL_time_us start_usecs;
};

#endif // __IO_FILES_HH_DEFINED__
