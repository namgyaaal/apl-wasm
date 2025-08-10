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

#include "Avec.hh"
#include "Common.hh"
#include "DiffOut.hh"
#include "InputFile.hh"
#include "IO_Files.hh"
#include "Output.hh"
#include "Performance.hh"
#include "UTF8_string.hh"

//----------------------------------------------------------------------------
int
DiffOut::overflow(int c)
{
   // process one output character c from APL.
   //
PERFORMANCE_START(cout_perf)
   Output::set_color_mode(errout ? Output::COLM_UERROR
                                 : Output::COLM_OUTPUT);

   // expand LF to CRLF if desired
   //
   if (expand_LF && c == '\n')   cout << "\r";

   cout << char(c);

   if (!InputFile::is_validating())
      {
        PERFORMANCE_END(fs_COUT_B, cout_perf, 1)
        return 0;
      }

   if (c != '\n')   // not end of line
      {
        aplout += c;
        PERFORMANCE_END(fs_COUT_B, cout_perf, 1)
        return 0;
      }

   // complete APL output line received. Compare it with the reference (= the
   // next input line) and write the comparison result into the current
   // .tc.log file.
   //
ofstream & report = IO_Files::get_current_testreport();
   Assert(report.is_open());

const char * apl = aplout.c_str();   // the APL output
UTF8_string ref;                     // the expected output (= the reference)
bool eof = false;
size_t diff_pos = 0;                 // the first mismatch
   IO_Files::read_file_line(ref, eof);
   if (eof)                // nothing in current_testfile
      {
        report << "extra: " << apl << endl;
      }
   else if (different(utf8P(apl), utf8P(ref.c_str()), diff_pos))
      {
        IO_Files::diff_error();
        report << "apl: ⋅⋅⋅" << apl << "⋅⋅⋅" << endl
            << "ref: ⋅⋅⋅" << ref.c_str() << "⋅⋅⋅" << endl
            << " ∆ : ⋅⋅⋅";
        loop(p, diff_pos)   report << " ";
        report << "^" << endl;
      }
   else                    // same
      {
        report << "== " << apl << endl;
      }

   aplout.clear();
   PERFORMANCE_END(fs_COUT_B, cout_perf, 1)
   cout << flush;
   return 0;
}
//----------------------------------------------------------------------------
bool
DiffOut::different(const UTF8 * apl, const UTF8 * ref, size_t & pos)
{
   for (pos = 0;; ++pos)   // compare position in ref
       {
         int len_apl;   // length of the UTF8 encoding
         int len_ref;   // length of the UTF8 encoding

         const Unicode a = UTF8_string::toUni(apl, len_apl, true);
         const Unicode r = UTF8_string::toUni(ref, len_ref, true);
         apl += len_apl;   // skip a in apl
         ref += len_ref;   // skip r in ref

         if (a == r)   // same char
            {
              if (a == 0)   return false;   // not different
              continue;
            }

         // different chars: try special matches (⁰, ¹, ², ³, ⁴, ⁵, ⁶, or ⁿ)...

         if (r == UNI_PAD_U0)   // ⁰: match one or more digits
            {
              if (!Avec::is_digit(a))   return true;   // different

              // skip trailing digits
              while (Avec::is_digit(Unicode(*apl)))   ++apl;
              continue;
            }

         if (r == UNI_PAD_U1)   // ¹: match zero or more spaces
            {
              if (a != UNI_SPACE)   return true;   // different

              // skip trailing digits
              while (*apl == ' ')   ++apl;
              continue;
            }

         if (r == UNI_PAD_U2)   // ²: match floating point number
            {
              if (!Avec::is_digit(a) &&
                  a != UNI_OVERBAR   &&
                  a != UNI_E   &&
                  a != UNI_J   &&
                  a != UNI_FULLSTOP)   return true;   // different

              // skip trailing digits
              //
              for (;;)
                  {
                    if (Avec::is_digit(Unicode(*apl)) ||
                        *apl == UNI_E           ||
                        *apl == UNI_J           ||
                        *apl == UNI_FULLSTOP)   ++apl;
                   else
                      {
                         int len;
                         if (UTF8_string::toUni(apl, len, true) == UNI_OVERBAR)
                            {
                              apl += len;
                              continue;
                            }
                         else break;
                      }
                  }

              continue;
            }

         if (r == UNI_PAD_U3)   // ³: match anything
            return false;   // not different

         if (r == UNI_PAD_U4)   // ⁴: match optional ¯
            {
              if (a != UNI_OVERBAR)   apl -= len_apl;   // restore apl
              continue;
            }

         if (r == UNI_PAD_U5)   // ⁵: match + or -
            {
              if (a != '+' && a != '-')   return true;   // different
              continue;
            }

         if (r == UNI_PAD_U6)   // ⁶: match 28 ⎕CR 42
            {
#ifdef cfg_RATIONAL_NUMBERS_WANTED
              if (a != '1')   return true;   // different
#else
              if (a != '0')   return true;   // different
#endif
              continue;
            }
         if (r == UNI_PAD_Un)   // ⁿ: optional unit multiplier
            {
              // ⁿ shall match an optional unit (milli, micro, or nano)
              // multiplier i.e. m, n, u, or μ
              //
              if (a == 'm')       continue;
              if (a == 'n')       continue;
              if (a == 'u')       continue;
              if (a == UNI_MUE)   continue;

              // no unit multiplier matches
              apl -= len_apl;
              continue;
            }

         return true;   // different
       }

   // not reached
   //
   return true;
}
//----------------------------------------------------------------------------
