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
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <sys/un.h>   // for sockaddr_un

#include "Bif_OPER2_INNER.hh"
#include "Bif_OPER2_OUTER.hh"
#include "Bif_OPER1_EACH.hh"
#include "Common.hh"
#include "FloatCell.hh"
#include "IntCell.hh"
#include "PointerCell.hh"
#include "Quad_FIO.hh"
#include "Performance.hh"
#include "Security.hh"
#include "StateIndicator.hh"
#include "Tokenizer.hh"
#include "Workspace.hh"

extern uint64_t top_of_memory();
uint64_t Quad_FIO::benchmark_cycles_from = 0;

std::vector<Quad_FIO::file_entry> Quad_FIO::open_files;

Quad_FIO  Quad_FIO::fun;

// A union holding a sockaddr and a sockaddr_in as to avoid casting
/// between sockaddr and a sockaddr_in
union SockAddr
{
  /// an arbitrary socket address
  sockaddr    addr;

  /// an AF_INET socket address
  sockaddr_in inet;

  ///  an AF_UNIX socket address
  sockaddr_un uNix;
};

   // CONVENTION: all functions must have an axis argument (like X
   // in A fun[X] B); the axis argument is a function number that selects
   // one of several functions provided by ⎕FIO...
   //
   // If the axis is missing, then a list of functions implemented by
   // ⎕FIO is displayed

Quad_FIO::_sub_fun Quad_FIO::sub_functions[] =
{
#define fiodef(N, name)   { N, #name },
#include "Quad_FIO.def"
};
//----------------------------------------------------------------------------
int
Quad_FIO::axis_compare(const void * key, const void * sf)
{
   return strcasecmp(reinterpret_cast<const char *>(key),
                     reinterpret_cast<const _sub_fun *>(sf)->key);
}
//----------------------------------------------------------------------------
int
Quad_FIO::function_name_to_int(const char * function_name)
{
  enum { SF_SIZE = sizeof(_sub_fun),
         SF_COUNT = sizeof(sub_functions)  / SF_SIZE };

#if 0
   // check that sub_functions are sorted by function name...
   //
   loop(f, SF_COUNT - 1)
       if (strcmp(sub_functions[f].key, sub_functions[f+1].key) >= 0)
          {
            // mismatch...
            Q1(sub_functions[f].key)
            Q1(sub_functions[f+1].key)
          }
#endif

  if (const void * vp = bsearch(function_name, sub_functions,
                                SF_COUNT, SF_SIZE, axis_compare))
      return reinterpret_cast<const _sub_fun *>(vp)->val;

  return -1;    // not found
}
//----------------------------------------------------------------------------
sAxis
Quad_FIO::subfun_to_axis(const UCS_string & name) const
{
UTF8_string name_utf(name);
   return function_name_to_int(name_utf.c_str());
}
//----------------------------------------------------------------------------
Quad_FIO::Quad_FIO()
   : QuadFunction(TOK_Quad_FIO)
{
   // init stdin, stdout, stderr, and maybe fd 3 
   //
file_entry f0(stdin, STDIN_FILENO);
   f0.path.append_ASCII("stdin");
   open_files.push_back(f0);
file_entry f1(stdout, STDOUT_FILENO);
   f1.path.append_ASCII("stdout");
   open_files.push_back(f1);
file_entry f2(stderr, STDERR_FILENO);
   f1.path.append_ASCII("stderr");
   open_files.push_back(f2);

   if (-1 != fcntl(3, F_GETFD))   // this process was forked from another APL
      {
        file_entry f3(0, 3);
        f3.path.append_ASCII("pipe-to_client");
        open_files.push_back(f3);
      }
}
//----------------------------------------------------------------------------
void
Quad_FIO::clear()
{
   // close open files, but leave stdin, stdout, and stderr open
   while(open_files.size() > 3)
      {
         file_entry & fe = open_files.back();
         if (fe.fe_FILE)   fclose(fe.fe_FILE);   // also closes fe.fe_fd
         else              close(fe.fe_fd);
         CERR << "WARNING: File " << fe.path << " still open - closing it"
              << endl;
        ::close(open_files.back().fe_fd);
        open_files.pop_back();
      }
}
//----------------------------------------------------------------------------
Quad_FIO::file_entry &
Quad_FIO::get_file_entry(int handle)
{
   loop(h, open_files.size())
      {
        if (open_files[h].fe_fd == handle)   return open_files[h];
      }

   MORE_ERROR() << handle
                << " is not an open file handle managed by ⎕FIO, see ⎕FIO 0.";
   DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
int
Quad_FIO::close_handle(int fd)
{
   loop(h, open_files.size())
      {
        file_entry & fe = open_files[h];
        if (fe.fe_fd != fd)   continue;

        // never close stdin, stdout, or stderr
        if (fe.fe_fd <= STDERR_FILENO)
           {
             MORE_ERROR() << "Attempt to close fd " << fe.fe_fd
                          << " (stdin, stdout, or stderr)";
             DOMAIN_ERROR;
           }

        if (fe.fe_FILE)   fclose(fe.fe_FILE);   // also closes fe.fe_fd
        else              close(fe.fe_fd);

        open_files.erase(open_files.begin() + h);
        return 0;   // OK
      }

   MORE_ERROR() << "Invalid ⎕FIO handle " << fd;
   return -EBADF;
}
//----------------------------------------------------------------------------
FILE *
Quad_FIO::get_FILE(int handle)
{
file_entry & fe = get_file_entry(handle);
   if (fe.fe_FILE == 0)
      {
        if (fe.fe_may_read && fe.fe_may_write)
           fe.fe_FILE = fdopen(fe.fe_fd, "a+");
        else if (fe.fe_may_read)
           fe.fe_FILE = fdopen(fe.fe_fd, "r");
        else if (fe.fe_may_write)
           fe.fe_FILE = fdopen(fe.fe_fd, "a");
        else
           {
             MORE_ERROR() << "bad (closed ?) file handle " << handle;
             DOMAIN_ERROR;   // internal error
           }
      }

   return fe.fe_FILE;
}
//----------------------------------------------------------------------------
Value_P
Quad_FIO::fds_to_val(fd_set * fds, int max_fd)
{
int fd_count = 0;
   if (fds)
      {
        for (int m = 0; m < max_fd; ++m)   if (FD_ISSET(m, fds))   ++fd_count;
      }

Value_P Z(ShapeItem(fd_count), LOC);
   if (fds)
      {
        for (int m = 0; m < max_fd; ++m)
            if (FD_ISSET(m, fds))   Z->next_ravel_Int(m);
      }

   return Z;
}
//----------------------------------------------------------------------------
Token
Quad_FIO::do_fprintf(FILE * outf, Value_P A)
{
   // A is expected to be a nested APL value. A[1] is the format string,
   // A[2...] are the values for each % field in A[1]. The result returned
   // is the number of characters (not bytes!) written to the file.
   //
UCS_string UZ;
const Value & A1 = *A->get_cfirst().get_pointer_value();
UCS_string A_format(A1);
   do_snprintf(UZ, A_format, A.get(), 1, "⎕FIO.fprintf B");
UTF8_string utf(UZ);
   fwrite(utf.c_str(), 1, utf.size(), outf);
   return Token(TOK_APL_VALUE1, IntScalar(UZ.size(), LOC));
}
//----------------------------------------------------------------------------
void
Quad_FIO::do_snprintf(UCS_string & UZ, const UCS_string & A_format,
                    const Value * B, int off_B, const char * funname)
{
   // A is the format string, B the nested APL values for each % field in A.
   // The result UZ is the formatted string. off_B is the starting point (1
   // for fprintf(A, ...) and 0 for snprintf(format, B ...).
   //
char numbuf[50];
const int arg_count_B = B->element_count() - off_B;
int conversion_count_A = 0;   // the number of conversions (in A_format)

#define COUNT_ARG if   (conversion_count_A++ >= arg_count_B)   goto missing_arg

   for (int fA = 0; fA < A_format.size(); /* no fA++ */ )
       {
         const Unicode uni = A_format[fA++];
         if (uni != UNI_PERCENT)   // not %
            {
              UZ.append(uni);
              continue;
            }

         // uni is % which is the start of a format field. Copy the format field
         // into to string fmt and printf() it with the next argument.
         // That is, for e.g. format = "...%42.42llf..." we want fmt to be
         // "%42.42llf"
         //
         char fmt[40];             // the naked format for one printf() item
         unsigned int fm = 0;      // an index into fmt;
         fmt[fm++] = '%';          // copy the % into fmt
         bool thousands = false;   // print thousands separator (',')
         for (;;)
             {
               if (fA >= A_format.size())
                  {
                    // After having seen a %, the end of the format string was
                    // reached without seeing the conversion specifier.
                    // Print the string and return. This happends only if
                    // A_format is somewhat mal-formed (e.g. "ab %555"),
                    // but the C/C++ printf() functions accept it.
                    //
                    UTF8_string utf(fmt);
                    UCS_string ufmt(utf);
                    UZ.append(ufmt);
                    return;
                  }

               if (fm >= sizeof(fmt))
                  {
                    // After seeing a %, no conversion specifier was seen
                    // within 40 characters. Most likely the user has
                    // forgotten it. In theory the format string could be
                    // proper, but we assumt it is mal-formed.
                    //
                    UTF8_string utf(fmt);
                    UCS_string ufmt(utf);
                    UZ.append(ufmt);
                    goto field_done;
                  }

               const Unicode uni_1 = A_format[fA++];
               switch(uni_1)
                  {
                     // flag chars and field width/precision
                     //
                     case '\'': thousands = true;
                                continue;

                     case '#':
                     case '0' ... '9':
                     case '-':
                     case ' ':
                     case '+':
                     case 'I':    // glibc
                     case '.':

                     // length modifiers
                     //
                     case 'h':
                     case 'l':
                     case 'L':
                     case 'q':
                     case 'j':
                     case 'z':
                     case 't': fmt[fm++] = uni_1;   fmt[fm] = 0;
                               continue;

                         // conversion specifiers
                         //
                     case 'd':   case 'i':   case 'o':
                     case 'u':   case 'x':   case 'X':   case 'p':
                          {
                            COUNT_ARG;
                            const Cell & cell = B->get_cravel(off_B++);
                            APL_Integer int_val;
                            if (cell.is_integer_cell())
                               {
                                 int_val = cell.get_int_value();
                               }
                            else
                               {
                                 const double fv = cell.get_real_value();
                                 if (fv < 0.0)   int_val = -int(-fv);
                                 else            int_val = int(fv);
                               }
                            fmt[fm++] = uni_1;   fmt[fm] = 0;
                            SPRINTF(numbuf, fmt, int_val);
                            if (thousands)  group_thousands(UZ, numbuf, false);
                            else            UZ.append_UTF8(numbuf);
                          }
                          goto field_done;

                     case 'e':   case 'E':   case 'f':   case 'F':
                     case 'g':   case 'G':   case 'a':   case 'A':
                          {
                            COUNT_ARG;
                            const APL_Float float_val =
                                  B->get_cravel(off_B++).get_real_value();
                            fmt[fm++] = uni_1;   fmt[fm] = 0;
                            SPRINTF(numbuf, fmt, float_val);
                            if (thousands)
                               {
                                 group_thousands(UZ, numbuf, true);
                               }
                            else if (char * const dot = strchr(numbuf, '.'))
                               {
                                 *dot = 0;
                                 UZ.append_ASCII(numbuf);
                                 UZ.append(Workspace::get_FC(0));
                                 UZ.append_ASCII(dot + 1);
                               }
                            else
                               {
                                 UZ.append_UTF8(numbuf);
                               }
                          }
                          goto field_done;

                     case 's':   // string or char
                          COUNT_ARG;
                          if (B->get_cravel(off_B).is_character_cell())
                             {
                               UZ.append(B->get_cravel(off_B++)
                                           .get_char_value());
                               goto field_done;
                             }
                          {
                            Value_P str =
                                    B->get_cravel(off_B++).get_pointer_value();
                            UCS_string ucs(*str.get());
                            UZ.append(ucs);
                          }
                          goto field_done;

                     case 'c':   // single char
                          COUNT_ARG;
                          UZ.append(B->get_cravel(off_B++).get_char_value());
                          goto field_done;

                     case 'm':
                          COUNT_ARG;
                          SPRINTF(numbuf, "%s", strerror(errno));
                          UZ.append_UTF8(numbuf);
                          goto field_done;

                     case '%':
                          if (fm == 0)   // %% is %
                             {
                               UZ.append(UNI_PERCENT);
                               goto field_done;
                             }
                          /* no break */

                     default:
                          MORE_ERROR() << "invalid format character " << uni_1
                                       << " in " << funname;
                          DOMAIN_ERROR;   // bad format char
                  }
             }
         field_done: ;
       }

   if (B->element_count() > off_B)
      {
        const int unused = B->element_count() - off_B;
        MORE_ERROR() << unused << " unused argument(s) in argument B of: "
                     << funname << "\n" << "    " << conversion_count_A
                     << " argument(s) were used, but " << arg_count_B
                     << " were provided.\n"
                        "    The format string was: '" << A_format << "'";
        LENGTH_ERROR;
      }
      return;

missing_arg:
   MORE_ERROR() << "Too few arguments in argument B of: " << funname << "\n"
                   "    Only " << arg_count_B
                <<   " argument(s) were provided in B.\n"
                   "    The format string was: '" << A_format << "'";
    LENGTH_ERROR;
}
//----------------------------------------------------------------------------
void
Quad_FIO::group_thousands(UCS_string & dest, char * buffer, bool flt)
{
   // buffer is the 0-terminated and ASCII-only output of some snprintf("%..."),
   // and the user has requested thousands' separators in the integer part of
   // buffer.

   if (flt)   // buffer may or may not have a fractional part
      {
        if (char * dot = strchr(buffer, '.'))   // buffer has a  fractional part
           {
             *dot = 0;
             group_thousands(dest, buffer, false);
             dest.append(Workspace::get_FC(0));
             dest.append_ASCII(dot + 1);
             return;
           }
      }

   // at this point, buffer is integer only.
   //
int digit_count = 0;
   for (const char * b = buffer; *b; ++b)
       if (*b >= '0' && *b <= '9')   ++digit_count;

   for (const char * b = buffer; *b;)
       {
         const char cc = *b++;
         dest.append(Unicode(cc));
         if (cc >= '0' && cc <= '9')   // digit
            {
              if (--digit_count && !(digit_count % 3))
                 dest.append(Workspace::get_FC(1));
            }
         }
}
//----------------------------------------------------------------------------
Value_P
Quad_FIO::get_random(APL_Integer mode, APL_Integer len)
{
const int device = open("/dev/urandom", O_RDONLY);
   if (device == -1)
      {
        MORE_ERROR() << "⎕FIO[60] failed when trying to open /dev/urandom: "
                     << strerror(errno);
        DOMAIN_ERROR;
      }

unsigned char buffer[32];   // caller has checked that len <= 32
const ssize_t bytes = read(device, buffer, len);
   close(device);

   if (bytes == -1)
      {
        MORE_ERROR() << "⎕FIO[60] failed when trying to read /dev/urandom: "
                     << strerror(errno);
        DOMAIN_ERROR;
      }

   if (mode == 0)        // single integer result (len ≤ 8)
      {
         APL_Integer result = 0;
         loop(l, len)   result = result << 8 | (buffer[l] & 0xFF);
         return IntScalar(result, LOC);
      }
   else if (mode == 1)   // byte vector result
      {
        Value_P Z(len, LOC);
        loop(l, len)   Z->next_ravel_Int(buffer[l] & 0xFF);
         return Z;
      }

   MORE_ERROR() << "Invalid mode A in A ⎕FIO[60] B";
   DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
Unicode
Quad_FIO::fget_utf8(FILE * file, ShapeItem & fget_count)
{
const int b0 = fgetc(file);
   if (b0 == EOF)   return UNI_EOF;

   ++fget_count;
   if (!(b0 & 0x80))   return Unicode(b0);   // ASCII

int len,bx;
   if      ((b0 & 0xE0) == 0xC0)   { len = 2;   bx = b0 & 0x1F; }
   else if ((b0 & 0xF0) == 0xE0)   { len = 3;   bx = b0 & 0x0F; }
   else if ((b0 & 0xF8) == 0xF0)   { len = 4;   bx = b0 & 0x0E; }
   else if ((b0 & 0xFC) == 0xF8)   { len = 5;   bx = b0 & 0x0E; }
   else if ((b0 & 0xFE) == 0xFC)   { len = 6;   bx = b0 & 0x0E; }
   else return UNI_EOF;

uint32_t uni = 0;
   loop(l, len - 1)
       {
         const int subc = fgetc(file);
         if (subc == EOF)   return UNI_EOF;
         bx  <<= 6;
         uni <<= 6;
         uni |= subc & 0x3F;
       }


   return Unicode(bx | uni);
}
//----------------------------------------------------------------------------
/// a Unicode source from either a file or a UCS_string
class File_or_String
{
public:
   /// constructor: from file
   File_or_String(FILE * f)
   : file(f),
     string(0),
     lookahead(Invalid_Unicode),
     unicodes_read(0)
   { offset = ftell(f);  }

   /// constructor: from string
   File_or_String(const UCS_string * u)
   : file(0),
     string(u),
     offset(0),
     lookahead(Invalid_Unicode),
     unicodes_read(0)
   {}

   /// return the number of characters
   ShapeItem get_count() const
      {
        if (file)    return ftell(file) - offset;
        return unicodes_read;
      }

   /// get the next character
   Unicode get_next()
      {
        if (lookahead != Invalid_Unicode)   // there is an ungotten char
           {
             const Unicode ret = lookahead;
             lookahead = Invalid_Unicode;
             // counted already, so don't count it twice
             return ret;
           }

         if (file)   // data comes from a UTF-8 encoded file
            {
              ShapeItem utf8_len = 0;
              const Unicode ret = Quad_FIO::fget_utf8(file, utf8_len);
              if (ret != UNI_EOF)   ++unicodes_read;
              return ret;
            }
         else        // data comes from a UCS_string
            {
              if (offset >= string->size())   return UNI_EOF;
              ++unicodes_read;
              return (*string)[offset++];
            }
      }

   /// unget uni. At most one Unicode (= lookahead char) can be ungotten.
   void unget(Unicode uni)
      {
        Assert(lookahead == Invalid_Unicode);
        lookahead = uni;
      }

   /// scan a long long in a string or in a file. Return either 0 ion error,
   /// or 1 on success (for /// the number of successful conversions).
   int scanf_long_long(const char conv, long long & value)
      {
         if (file)
            {
              // if we have read the next character from a file, then we
              // need to push it back so that fscanf() will get it.
              //
              if (lookahead != Invalid_Unicode)
                 {
                   ungetc(lookahead, file);
                   lookahead = Invalid_Unicode;
                 }
              const char fmt[] = { '%', 'l', 'l', conv, 0 };
              return fscanf(file, fmt, &value);
            }

         // string input...
         //
         char cc[40];
         unsigned int cidx = 0;
         if (lookahead == UNI_OVERBAR)
            {
              cc[cidx++] = '-';
              lookahead = Invalid_Unicode;
            }
         else if (lookahead != Invalid_Unicode)
            {
              cc[cidx++] = lookahead;
              lookahead = Invalid_Unicode;
            }

         while (cidx < (sizeof(cc) - 1) && offset < string->size())
            {
              const Unicode uni = get_next();
              if (Avec::is_digit(uni))       cc[cidx++] = uni;   // 0-9
              else if (uni == UNI_OVERBAR)   cc[cidx++] = '-';   // ¯
              else if (uni == UNI_MINUS)     cc[cidx++] = '-';   // -
              else { unget(uni);   break; }
            }

         cc[cidx] = 0;
         const int base =  (conv == 'X') || (conv == 'x') ? 16 : 10;
         errno = 0;
         value = strtoll(cc, 0, base);
         return errno ? 0 : 1;
      }

   /// scan a double in string or file. Return either 0 or 1 for
   /// the number of successful conversions.
   int scanf_double(const char conv, APL_Float & value)
      {
         if (file)
            {
              if (lookahead != Invalid_Unicode)
                 {
                   ungetc(lookahead, file);
                   lookahead = Invalid_Unicode;
                 }
              const char fmt[] = { '%', 'l', conv, 0 };
              return fscanf(file, fmt, &value);
            }

         // string input...
         //
         char cc[40];
         unsigned int cidx = 0;
         if (lookahead == UNI_OVERBAR)
            {
              cc[cidx++] = '-';
              lookahead = Invalid_Unicode;
            }
         else if (lookahead != Invalid_Unicode)
            {
              cc[cidx++] = lookahead;
              lookahead = Invalid_Unicode;
            }

         while (cidx < (sizeof(cc)) - 1 && offset < string->size())
            {
              const Unicode uni = get_next();
              if (uni == UNI_OVERBAR)                  cc[cidx++] = '-';   // ¯
              else if(strchr(".0123456789eE-", uni))   cc[cidx++] = uni;
              else { unget(uni);   break; }
            }

         cc[cidx] = 0;
         errno = 0;
         value = strtod(cc, 0);
         return errno ? 0 : 1;
      }

protected:
   /// UTF8-encoded source file (or 0 for string input)
   FILE * file;

   /// UCS_string source (or 0 for file source)
   const UCS_string * string;

   /// the next character in a string input
   ShapeItem offset;

   /// ungotten char
   Unicode lookahead;

   /// number of chars consumed thus far
   ShapeItem unicodes_read;
};
//----------------------------------------------------------------------------
Token
Quad_FIO::do_scanf(File_or_String & input, const UCS_string & format,
                   int function_number)
{
   if (format.size() == 0)   LENGTH_ERROR;

   // we take the total number of % as an upper bound for the number of items.
   // the real count may be lower due to %% and assugnment suppression. We fix
   // that after label out: below
   //
ShapeItem count = 0;
   loop(f, format.size() - 1)
      {
        if (format[f] == UNI_PERCENT)   ++count;
      }

Value_P Z(count, LOC);

Unicode lookahead = input.get_next();
   if (lookahead == UNI_EOF)   goto out;

   loop(f, format.size())
      {
        const Unicode fmt_ch = format[f];
        if (fmt_ch == UNI_SPACE)
           {
             // one space in the format matches 0 or more spaces in the input
             //
             while (lookahead == UNI_SPACE)
                {
                 lookahead = input.get_next();
                 if (lookahead == UNI_EOF)   goto out;
                }
             continue;
           }

        if (fmt_ch != UNI_PERCENT)
           {
             // normal character in format: match with input
             //
             match:
             if (fmt_ch != lookahead)   goto out;
             lookahead = input.get_next();
             if (lookahead == UNI_EOF)   goto out;
             continue;
           }

        ++f;   // skip (first) %
        const Unicode fmt_ch1 = format[f];
        if (fmt_ch1 == UNI_PERCENT)   goto match;   // double % is %

        Unicode conv = Unicode_0;   // no conversion specifier
        int conv_len = 0;
        bool suppress = false;
        for (;f < format.size(); ++f)
           {
             const Unicode cc = format[f];
             if (cc == UNI_ASTERISK ||      // *: assignment character
                 cc == UNI_STAR_OPERATOR)   // ⋆: assignment character
                {
                  suppress = true;
                  continue;
                }

             if (strchr("hjlLmqtz", cc))   // type modifier
                {
                  // we provide our own conversion modifiers and ignore
                  // the conversion modifiers given by the user (that only
                  // make sense in the C/C++ context).
                  continue;
                }

             if (strchr("0123456789", cc))   // field length
                {
                  conv_len = 10*conv_len + (cc - '0');
                  continue;
                }

             if (strchr("cdDfFeginousxX[", cc))
                {
                  conv = cc;
                  break; // conversion
                }
           }

        if (conv == Unicode_0)   // no conversion specifier
           {
             MORE_ERROR() << "expecting conversion character "
                             "%, c, d, f, i, n, o, u, s, or x after %";
             DOMAIN_ERROR;
           }

        if (strchr("dDiouxX", conv))  // integer conversion
           {
             input.unget(lookahead);   // so that scanf_long_long() can read it
             long long value = 0;
             const int count = input.scanf_long_long(conv, value);
             if (count == 1 && !suppress)   Z->next_ravel_Int(value);
             else                           goto out;

             lookahead = input.get_next();
             if (lookahead == UNI_EOF)   goto out;
           }
        else if (strchr("fFeg", conv))  // float conversion
           {
             input.unget(lookahead);   // let scanf_double() read it
             APL_Float value = 0;
             const int count = input.scanf_double(conv, value);
             if (count == 1 && !suppress)   Z->next_ravel_Float(value);
             else                           goto out;

             lookahead = input.get_next();
             if (lookahead == UNI_EOF)   goto out;
           }
        else if (conv == UNI_c)  // char(s)
           {
             if (conv_len == 0)   // default: single char
                {
                  if (!suppress)   Z->next_ravel_Char(lookahead);
                  lookahead = input.get_next();
                  if (lookahead == UNI_EOF)   goto out;
                }
             else
                {
                  UCS_string ucs;
                  loop(c, conv_len)
                     {
                       ucs.append(lookahead);
                       lookahead = input.get_next();
                       if (lookahead == UNI_EOF)   break;
                     }

                  if (!suppress)
                     {
                       Value_P ZZ(ucs, LOC);
                       Z->next_ravel_Pointer(ZZ.get());
                     }
                }
           }
        else if (conv == UNI_s)  // string
           {
             UCS_string ucs;
             while (lookahead > UNI_SPACE)
                 {
                   ucs.append(lookahead);
                   if (conv_len && ucs.size() >= conv_len)   break;
                   lookahead = input.get_next();
                   if (lookahead == UNI_EOF)   break;
                 }

             if (!suppress)
                {
                  Value_P ZZ(ucs, LOC);
                  Z->next_ravel_Pointer(ZZ.get());
                }
           }
        else if (conv == UNI_n)  // characters consumed thus far
           {
             if (!suppress)   Z->next_ravel_Int(input.get_count());
           }
        else if (conv == UNI_L_BRACK)   // character range
           {
             ++f;   // skip [

             // 1. read the pattern range [...]
             //

             // if the first char is ^ or ∧ then the characters in the range
             // shall be excluded rather than included. The range itself is
             // not affected, though.
             //
             if (f == format.size())   LENGTH_ERROR;
             const bool excluding = format[f] == UNI_CIRCUMFLEX   // ^
                                 || format[f] == UNI_AND;
            if (excluding)   ++f;   // skip ^ or ∧

             UCS_string range;   // the character range to be in- or excluded
             range.reserve(format.size());

             // if the next char is ] then it shall belong to the range,
             // otherwise it terminates the range than ending it.
             if (f == format.size())   LENGTH_ERROR;
             if (format[f] == UNI_R_BRACK)   range += format[f++];

            // the characters of the range, terminated by ]
            //
            for (;;)
                {
                 if (f == format.size())   // end of format string
                    {
                      MORE_ERROR() << "No ] in character range A of A ⎕FIO["
                                   << function_number << "] B";
                      DOMAIN_ERROR;
                    }

                  Unicode funi = format[f];
                  if ( funi == UNI_MINUS            // maybe a character range
                    && format[f+1] != UNI_R_BRACK   // otherwise its not
                    && range.size())                // otherwise its not
                     {
                           // the minus belongs to a range A-B. With Unicode
                           // the range could become ridiculously large and
                           // we throw a length error if that happens.
                           //
                           const Unicode from = range.back();
                           const Unicode to   = format[++f];
                           if (from == to)   {}   // 1-character range
                           else if (from > to)
                              {
                                MORE_ERROR() << "Invalid character range '"
                                             << from << "'-'" << to << "'\n";
                                DOMAIN_ERROR;
                              }

                           if ((to - from) >= 96)   // more than visible ASCII
                              {
                                MORE_ERROR() << "character range '" << from
                                             << "'-'" << to << "' too large.\n";
                                LENGTH_ERROR;
                              }

                       // at this point the A-B construct is valid. Insert
                       // the characters into range...
                       //
                       for (int u = from + 1; u <= to;)   range += Unicode(u++);
                     }
                  if (funi == UNI_R_BRACK)   break;   // end of range
                  ++f;
                  range += funi;
                }

             // 2. create the APL result
             //
             UCS_string ucs;
             ucs.reserve(format.size());
             for (;;)
                 {
                   if (lookahead == UNI_EOF)                 break;
                   if (conv_len && ucs.size() >= conv_len)   break;

                   if (range.contains(lookahead) == excluding)
                      {
                        // lookahead is the first character after the range
                        break;
                      }
                   ucs.append(lookahead);

                   lookahead = input.get_next();
                 }

             // ] shall only match a nonempty sequence of characters
             //
             if (ucs.size() == 0)   goto out;   // empty sequence

             if (!suppress)
                {
                  Value_P ZZ(ucs, LOC);
                  Z->next_ravel_Pointer(ZZ.get());
                }
           }
      }

out:
   // shrink Z to the actual number of converted items
   //
const Shape sh_Z(Z->get_valid_item_count());
   while (Z->more())   Z->next_ravel_0();
   Z->check_value(LOC);
   Z->set_shape(sh_Z);

   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Quad_FIO::list_functions(ostream & out, bool mapping)
{
   if (mapping)
      {
         // ⎕FIO "": print the number to name mappings like:
         //
         // ⎕FIO[20]  ←→  ⎕FIO['mkdir']        ←→  ⎕FIO.mkdir
         out <<
"      With a small performance penalty, ⎕FIO also accepts the following "
"strings\n      instead of function numbers as axis argument:\n\n";

         loop(f, sizeof(sub_functions)/sizeof(_sub_fun))
             {
               const int N = sub_functions[f].val;
               char NN[10];   SPRINTF(NN, "%2d", N);
               const char * name = sub_functions[f].key;
               out << "      ⎕FIO[" << NN
                   << "]  ←→  ⎕FIO['" << name << "']"
                   << UCS_string(13 - strlen(name), UNI_SPACE)
                   << "←→  ⎕FIO." << name << endl;
             }

         out << "\n      For a more detailed description of all functions:\n\n"
                "      ⎕FIO ⍬" << endl;
      }
   else
      {
       out <<
"   Functions provided by ⎕FIO...\n"
"\n"
"   Legend: a - address family, IPv4 address, port (or errno)\n"
"           b - byte(s) (Integer(s) in the range [0-255])\n"
"           d - table of dirent structs\n"
"           e - error code (integer as per errno.h)\n"
"           f - format string (printf(), scanf())\n"
"           h - file handle (small integer)\n"
"           i - integer\n"
"           n - names (nested vector of strings)\n"
"           p - path (filename string)\n"
"           s - string\n"
"           u - time divisor: 1       - second\n"
"                             1000    - milli second\n"
"                             1000000 - micro second\n"
"           y4 - seconds, wday, yday, dst (daylight saving time)\n"
"           y67- year, mon, day, hour, minute, second, [dst]\n"
"           y9 - year, mon, day, hour, minute, second, wday, yday, dst\n"
"           A1, A2, ...  nested vector with elements A1, A2, ...\n"
"\n"
"           ⎕FIO      ⍬    print this text on stderr\n"
"           ⎕FIO     ''    print function-number to -name mapping on stderr\n"
"           ⎕FIO      0    return a list of open file descriptors\n"
"        '' ⎕FIO      ⍬    print this text on stdout\n"
"           ⎕FIO     ''    print function-number to -name mapping on stdout\n"
"           ⎕FIO[ 0] ''    print this text on stderr\n"
"        '' ⎕FIO[ 0] ''    print this text on stdout\n"
"\n"
"   Zi ←    ⎕FIO[ 1] ''    errno (of last call)\n"
"   Zs ←    ⎕FIO[ 2] Be    strerror(Be)\n"
"   Zh ← As ⎕FIO[ 3] Bp    fopen(Bs, As) filename Bp mode As\n"
"   Zh ←    ⎕FIO[ 3] Bp    fopen(Bs, \"r\") filename Bp\n"
"\n"
"File I/O functions:\n"
"\n"
"   Ze ←    ⎕FIO[ 4] Bh    fclose(Bh)\n"
"   Ze ←    ⎕FIO[ 5] Bh    errno (of the last call using Bh)\n"
"   Zb ←    ⎕FIO[ 6] Bh    fread(Zi, 1, " << SMALL_BUF
                                          << ", Bh) 1 byte per Zb\n"
"   Zb ← Ai ⎕FIO[ 6] Bh    fread(Zi, 1, Ai, Bh) 1 byte per Zb\n"
"   Zi ← Ab ⎕FIO[ 7] Bh    fwrite(Ab, 1, ⍴Ai, Bh) 1 byte per Ai\n"
"   Zb ←    ⎕FIO[ 8] Bh    fgets(Zb, " << SMALL_BUF << ", Bh) 1 byte per Zb\n"
"   Zb ← Ai ⎕FIO[ 8] Bh    fgets(Zb, Ai, Bh) 1 byte per Zb\n"
"   Zb ←    ⎕FIO[ 9] Bh    fgetc(Zb, Bh) 1 byte\n"
"   Zi ←    ⎕FIO[10] Bh    feof(Bh)\n"
"   Ze ←    ⎕FIO[11] Bh    ferror(Bh)\n"
"   Zi ←    ⎕FIO[12] Bh    ftell(Bh)\n"
"   Zi ← Ai ⎕FIO[13] Bh    fseek(Bh, Ai, SEEK_SET)\n"
"   Zi ← Ai ⎕FIO[14] Bh    fseek(Bh, Ai, SEEK_CUR)\n"
"   Zi ← Ai ⎕FIO[15] Bh    fseek(Bh, Ai, SEEK_END)\n"
"   Zi ←    ⎕FIO[16] Bh    fflush(Bh)\n"
"   Zi ←    ⎕FIO[17] Bh    fsync(Bh)\n"
"   Zi ←    ⎕FIO[18] Bh    fstat(Bh)\n"
"   Zi ←    ⎕FIO[19] Bh    unlink(Bc)\n"
"   Zi ←    ⎕FIO[20] Bh    mkdir(Bc, 0777)\n"
"   Zi ← Ai ⎕FIO[20] Bh    mkdir(Bc, AI)\n"
"   Zi ←    ⎕FIO[21] Bh    rmdir(Bc)\n"
"   Zi ← A  ⎕FIO[22] 1     printf(         A1, A2...) format A1\n"
"   Zi ← A  ⎕FIO[22] 2     fprintf(stderr, A1, A2...) format A1\n"
"   Zi ← A  ⎕FIO[22] Bh    fprintf(Bh,     A1, A2...) format A1\n"
"   Zi ← Ac ⎕FIO[23] Bh    fwrite(Ac, 1, ⍴Ac, Bh) 1 Unicode per Ac, Output UTF8\n"
"   Zh ← As ⎕FIO[24] Bs    popen(Bs, As) command Bs mode As\n"
"   Zh ←    ⎕FIO[24] Bs    popen(Bs, \"r\") command Bs\n"
"   Ze ←    ⎕FIO[25] Bh    pclose(Bh)\n"
"   Zb ←    ⎕FIO[26] Bp    return entire file Bp as byte vector\n"
"   Zs ← Ap ⎕FIO[27] Bp    rename file Ap to Bp\n"
"   Zd ←    ⎕FIO[28] Bp    return content of directory Bp\n"
"   Zn ←    ⎕FIO[29] Bp    return file names in directory Bp\n"
"   Zs ←    ⎕FIO 30        getcwd()\n"
"   Zn ← As ⎕FIO[31] Bs    access(As, Bp) As ϵ 'RWXF'\n"
"   Zh ←    ⎕FIO[32] Bi    socket(Bi=AF_INET, SOCK_STREAM, 0)\n"
"   Ze ← Aa ⎕FIO[33] Bh    bind(Bh, Aa)\n"
"   Ze ←    ⎕FIO[34] Bh    listen(Bh, 10)\n"
"   Ze ← Ai ⎕FIO[34] Bh    listen(Bh, Ai)\n"
"   Za ←    ⎕FIO[35] Bh    accept(Bh)\n"
"   Ze ← Aa ⎕FIO[36] Bh    connect(Bh, Aa)\n"
"   Zb ←    ⎕FIO[37] Bh    recv(Bh, Zb, " << SMALL_BUF << ", 0) 1 byte per Zb\n"
"   Zb ← Ai ⎕FIO[37] Bh    recv(Bb, Zi, Ai, 0) 1 byte per Zb\n"
"   Zi ← Ab ⎕FIO[38] Bh    send(Bh, Ab, ⍴Ab, 0) 1 byte per Ab\n"
"   Zi ← Ac ⎕FIO[39] Bh    send(Bh, Ac, ⍴Ac, 0) 1 Unicode per Ac, Output UTF8\n"
"   Zi ←    ⎕FIO[40] B     select(B_read, B_write, B_exception, B_timeout)\n"
"   Zi ←    ⎕FIO[41] Bh    read(Bh, Zi, " << SMALL_BUF << ") 1 byte per Zi\n"
"   Zb ← Ai ⎕FIO[41] Bh    read(Bh, Zb, Ai) 1 byte per Zb\n"
"   Zi ← Ab ⎕FIO[42] Bh    write(Bh, Ab, ⍴Ab) 1 byte per Ab\n"
"   Zi ← Ac ⎕FIO[43] Bh    write(Bh, Ac, ⍴Ac) 1 Unicode per Ac, Output UTF8\n"
"   Za ←    ⎕FIO[44] Bh    getsockname(Bh)\n"
"   Za ←    ⎕FIO[45] Bh    getpeername(Bh)\n"
"   Zi ← Ai ⎕FIO[46] Bh    getsockopt(Bh, A_level, A_optname, Zi)\n"
"   Ze ← Ai ⎕FIO[47] Bh    setsockopt(Bh, A_level, A_optname, A_optval)\n"
"   Ze ← As ⎕FIO[48] Bh    fscanf(Bh, As)\n"
"   Zs ←    ⎕FIO[49] Bp    return entire file Bp as nested lines\n"
"   Zs ← LO ⎕FIO[49] Bp    ⎕FIO[49] Bp and pipe each line through LO.\n"
"   Zi ←    ⎕FIO[50] Bu    gettimeofday()\n"
"   Zy4←    ⎕FIO[51] By67  mktime(By67)  Note: Jan 2, 2017 is: 2017 1 2 ...\n"
"   Zy9←    ⎕FIO[52] Bi    localtime(Bi) Note: Jan 2, 2017 is: 2017 1 2 ...\n"
"   Zy9←    ⎕FIO[53] Bi    gmtime(Bi)    Note: Jan 2, 2017 is: 2017 1 2 ...\n"
"   Zi ←    ⎕FIO[54] Bs    chdir(Bs)\n"
"   Ze ← Af ⎕FIO[55] Bs    sscanf(Bs, Af) Af is the format string\n"
"   Zs ← As ⎕FIO[56] Bp    write nested lines As to file named Bp\n"
"   Zh ←    ⎕FIO[57] Bs    fork() and execve(Bs, { Bs, 0}, {0})\n"
"   Zs ← Af ⎕FIO[58] B     snprintf(Af, B...) As is the format string\n"
"   Zi ← Ai ⎕FIO[59] Bh    fcntl(Bh, Ai...) file control\n"
"   Zi ←    ⎕FIO[60] Bi    return a Bi-byte random integer (for setting ⎕RL)\n"
"   Zi ← 0  ⎕FIO[60] Bi    same as monadic ⎕FIO[60] Bi (random scalar, Bi≤8)\n"
"   Zb ← 1  ⎕FIO[60] Bi    return a vector of random bytes, (Bi ≥ ⍴Zi)\n"
"   Zi ←    ⎕FIO[61] Bv    seconds since 1/1/1970; Bv←YYYY [MM DD [HH MM SS]]\n"
"\n"
"Benchmarking functions:\n"
"\n"
"           ⎕FIO[200] Bi    clear statistics with ID Bi\n"
"   Zn ←    ⎕FIO[201] Bi    get statistics with ID Bi\n"
"           ⎕FIO[202] Bs    get monadic parallel threshold for primitive Bs\n"
"        Ai ⎕FIO[202] Bs    set monadic parallel threshold for primitive Bs\n"
"           ⎕FIO[203] Bs    get dyadic parallel threshold for primitive Bs\n"
"        Ai ⎕FIO[203] Bs    set dyadic parallel threshold for primitive Bs\n";
      }

   return Token(TOK_APL_VALUE1, Str0(LOC));
}
//----------------------------------------------------------------------------
Token
Quad_FIO::eval_B(Value_P B) const
{
   CHECK_SECURITY(disable_Quad_FIO);

   if (B->get_rank() > 1)   RANK_ERROR;
   if (B->get_rank() > 1)   RANK_ERROR;

   if (B->element_count() == 0)   // '' or ⍬
      {
        if (B->get_cfirst().is_character_cell())
           return list_functions(CERR, true);
        if (B->get_cfirst().is_integer_cell())
           return list_functions(CERR, false);
      }

const APL_Integer function_number = B->get_cfirst().get_int_value();
   switch(function_number)
      {
        // function_numbers < 0 refer to "hacker functions" that should not be
        // used by normal mortals.
        //
        case -17: // emulate Assert1()
                  Assert1(0 && "Simulated Assert1() (aka. ⎕FIO ¯17)");
                  return Token(TOK_APL_VALUE1, IntScalar(0, LOC));

        case -16: // emulate Assert()
                  Assert(0 && "Simulated Assert() (aka. ⎕FIO ¯16)");
                  return Token(TOK_APL_VALUE1, IntScalar(0, LOC));

        case -15: // print performance IDs and names...
             {
               const ShapeItem count = PFS_MAX3;
               Shape sh(count, 2);
               Value_P Z(sh, LOC);
#define perfo_4(id, b, name, thr) perfo_1(id, b, name, thr)
#define perfo_3(id, b, name, thr) perfo_1(id, b, name, thr)
#define perfo_2(id, b, name, thr) perfo_1(id, b, name, thr)
#define perfo_1(id, ab, name, _thr)         \
   { Z->next_ravel_Int(PFS_ ## id ## ab);   \
     UCS_string ucs(UTF8_string(#id #ab));  \
     Value_P uZ(ucs, LOC);                  \
     Z->next_ravel_Pointer(uZ.get());       }
#include "Performance.def"
               Z->check_value(LOC);
               return Token(TOK_APL_VALUE1, Z);
             }

        case -14: // print stacks. SI_top is the ⎕FIO call, so dont show it.
                  for (StateIndicator * si = Workspace::SI_top()->get_parent();
                       si; si = si->get_parent())
                      {
                        CERR << "[" << si->get_level() << "]: ";
                        si->get_prefix().print_stack(CERR, ".");
                      }
                   return Token(TOK_APL_VALUE1, IntScalar(0, LOC));

        case -13: // total number of UCS strings
             return Token(TOK_APL_VALUE1,
                          IntScalar(UCS_string::get_total_count(), LOC));
        case -12: // sbrk()
             return Token(TOK_APL_VALUE1, IntScalar(top_of_memory(), LOC));
        case -11: // fnew
             return Token(TOK_APL_VALUE1,
                          IntScalar(Value::fast_new_count, LOC));
        case -10: // slow new
             return Token(TOK_APL_VALUE1,
                          IntScalar(Value::slow_new_count, LOC));
        case -9: // screen height
             {
               struct winsize ws;
               ioctl(STDIN_FILENO, TIOCGWINSZ, &ws);
               return Token(TOK_APL_VALUE1, IntScalar(ws.ws_row, LOC));
             }
        case -8: // screen width
             {
               struct winsize ws;
               ioctl(STDIN_FILENO, TIOCGWINSZ, &ws);
               return Token(TOK_APL_VALUE1, IntScalar(ws.ws_col, LOC));
             }
        case -7: // throw a segfault
             {
               CERR << "NOTE: Triggering a segfault (keeping the current "
                       "SIGSEGV handler)..." << endl;

               const APL_Integer result = *reinterpret_cast<char *>(4343);
               CERR << "NOTE: Throwing a segfault failed." << endl;
               return Token(TOK_APL_VALUE1, IntScalar(result, LOC));
             }

        case -6: // throw a segfault
             {
               CERR << "NOTE: Resetting SIGSEGV handler and triggering "
                       "a segfault..." << endl;

               // reset the SSEGV handler
               //
               struct sigaction action;
               memset(&action, 0, sizeof(struct sigaction));
               action.sa_handler = 0;
               sigaction(SIGSEGV, &action, 0);
               const APL_Integer result = *reinterpret_cast<char *>(4343);
               CERR << "NOTE: Throwing a segfault failed." << endl;
               return Token(TOK_APL_VALUE1, IntScalar(result, LOC));
             }

        case -5: // return ⎕AV of IBM APL2
             {
               Value_P Z(256, LOC);
               const Unicode * ibm = Avec::IBM_quad_AV();
               loop(c, 256)   Z->next_ravel_Char(ibm[c]);
               Z->check_value(LOC);
               return Token(TOK_APL_VALUE1, Z);
             }

        case -4: // clear all probes (ignores B, returns 0)
             Probe::init_all();
             return Token(TOK_APL_VALUE1, IntScalar(0, LOC));

        case -3: // return the type of cycle_counter
#if HAVE_RDTSC
             return Token(TOK_APL_VALUE1, IntScalar(1, LOC));
#else
             return Token(TOK_APL_VALUE1, IntScalar(2, LOC));
#endif

        case -2: // return CPU frequency
             {
               timeval tv = { 0, 100000 }; // 100 ms
               const uint64_t from = cycle_counter();
               select(0, 0, 0, 0, &tv);
               const uint64_t to = cycle_counter();

               return Token(TOK_APL_VALUE1, IntScalar(10*(to - from), LOC));
             }

        case -1: // return CPU cycle counter
             {
               return Token(TOK_APL_VALUE1, IntScalar(cycle_counter(), LOC));
             }

        case 0:   // list of open file descriptors
             {
               errno = 0;
               Value_P Z(open_files.size(), LOC);
               loop(z, open_files.size())
                   {
                     Z->next_ravel_Int(open_files[z].fe_fd);
                   }
               Z->set_proto_Int();
               Z->check_value(LOC);
               return Token(TOK_APL_VALUE1, Z);
             }

        case 30:   // getcwd()
             {
               errno = 0;
               char buffer[APL_PATH_MAX + 1];
               char * success = getcwd(buffer, APL_PATH_MAX);
               if (!success)   goto out_errno;

               buffer[APL_PATH_MAX] = 0;   // just in case
               UTF8_string buffer_utf(buffer);
               UCS_string cwd(buffer_utf);

               Value_P Z(cwd, LOC);
               Z->set_proto_Spc();
               Z->check_value(LOC);
               return Token(TOK_APL_VALUE1, Z);
             }

        default: break;
      }

   return list_functions(CERR, false);

out_errno:
   MORE_ERROR() << "⎕FIO[" << function_number
              << "] B failed: " << strerror(errno);
   return Token(TOK_APL_VALUE1, IntScalar(-errno, LOC));
}
//----------------------------------------------------------------------------
APL_Integer
Quad_FIO::secs_epoch(const Value & B)
{
   // B is a time/date like YYYY [MM [DD [HH [MM [SS]]]]]
   if (B.get_rank() > 1)   RANK_ERROR;
const int len_B = B.element_count();
   if (len_B < 1 || len_B > 6)   LENGTH_ERROR;

const struct {
               const char * name;
               int min_val;
               int max_val;
             } range[6] =
             {
               { "year",   0, 2037 },
               { "month",  1,   11 },
               { "day",    1,   31 },
               { "hour",   0,   23 },
               { "minute", 0,   59 },
               { "second", 0,   60 },
             };

int Bv[6];  // values as provided in B
   loop(b, 6)
       {
         if (len_B <= b)   Bv[b] = range[b].min_val;
         else              Bv[b] = B.get_cravel(b).get_int_value();
         if (Bv[b] < range[b].min_val)
            {
              MORE_ERROR() << "In ⎕FX.secs_epoch : B["
                           << (b + Workspace::get_IO())
                           <<  "] (aka. " << range[b].name
                           << ") is too small (the valid range is "
                           << range[b].min_val << "-"
                           << range[b].max_val << ")";
              DOMAIN_ERROR;
            }
         else if (Bv[b] > range[b].max_val)
            {
              MORE_ERROR() << "In ⎕FX.secs_epoch : B["
                           << (b + Workspace::get_IO())
                           <<  "] (aka. " << range[b].name
                           << ") is too large (the valid range is "
                           << range[b].min_val << "-"
                           << range[b].max_val << ")";
              DOMAIN_ERROR;
            }
       }

   if (Bv[0] < 100)   Bv[0] += 2000;   // interpret year xx as 20xx

tm tm;

                                  //                   APL B[]    mktime
   tm.tm_sec  = Bv[5];            // Seconds           0-60       (0-60)
   tm.tm_min  = Bv[4];            // Minutes           0-59       (0-59)
   tm.tm_hour = Bv[3];            // Hours             0-23       (0-23)
   tm.tm_mday = Bv[2];            // Day of the month  1-31       (1-31)
   tm.tm_mon  = Bv[1] - 1;        // Month             1-12       (0-11)
   tm.tm_year = Bv[0] - 1900;     // Year              2000...    1900...

const time_t z = mktime(&tm);
   if (z != time_t(-1))   return APL_Integer(z);

   MORE_ERROR() << "⎕FIO B: bad B";
   DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
Token
Quad_FIO::eval_AB(Value_P A, Value_P B) const
{
   CHECK_SECURITY(disable_Quad_FIO);

   if (A->get_rank() > 1)   RANK_ERROR;
   if (B->get_rank() > 1)   RANK_ERROR;

   if (B->element_count() == 0)   // '' or ⍬
      {
        if (B->get_cfirst().is_character_cell())
           return list_functions(CERR, true);
        if (B->get_cfirst().is_integer_cell())
           return list_functions(CERR, false);
      }

const APL_Integer function_number = B->get_cfirst().get_int_value();
   switch(function_number)
      {
        case -3: // read probe A and clear it
             {
               APL_Integer probe = A->get_cfirst().get_int_value();

               // negative numbers mean take from end (like ↑ and ↓)
               // convention ins that 0, 1, 2, ... are a probe vector (such as
               // one entry per core) while probes at the end are individual
               // probes.
               //
               if (probe < 0)   probe = Probe::PROBE_COUNT + probe;
               if (probe >= Probe::PROBE_COUNT)   // too high
                  return Token(TOK_APL_VALUE1, IntScalar(-1, LOC));

               const int len = Probe::get_length(probe);
               if (len < 0)   return Token(TOK_APL_VALUE1, IntScalar(-1, LOC));

                Value_P Z(len, LOC);
                loop(m, len)
                    Z->next_ravel_Int(Probe::get_time(probe, m));

                Probe::init(probe);
                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
             }

        default: break;
      }

   return list_functions(COUT, false);
}
//----------------------------------------------------------------------------
Token
Quad_FIO::eval_ALXB(Value_P A, Token & LO, Value_P X, Value_P B) const
{
const ShapeItem function_number = X->get_cfirst().get_int_value();
   switch (function_number)
      {
        case -1:   // benchmark monadic LO with argument B
           {
#if ! HAVE_RDTSC
             MORE_ERROR() << "Platform has no 'rtdsc' instruction.";
             DOMAIN_ERROR;
#endif

             // doit...
             //
             cFunction_P fun = LO.get_function();
             Assert(fun);
             const uint64_t from = cycle_counter();
             Token result = fun->eval_AB(A, B);
             const uint64_t to = cycle_counter();
             if (result.get_tag() == TOK_SI_PUSHED)
                {
                  Workspace::SI_top()->                     // pretend ⎕ES
                             set_safe_execution_count();
                  benchmark_cycles_from = from;
                  return result;
                }

             // LO is a primitive
             //
             return Token(TOK_APL_VALUE1, IntScalar(to - from, LOC));
           }
      }

   MORE_ERROR() << "Bad function number (axis X) in operator A LO ⎕FIO[X] B";
   DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
Token
Quad_FIO::eval_LXB(Token & LO, Value_P X, Value_P B) const
{
   CHECK_SECURITY(disable_Quad_FIO);

   /* a common "mistake" is to suppress the printout of ⎕FIO by e.g.

      ⊣⎕FIO[X] B

      which lands here instead of calling eval_XB(). We "fix" that mistake...
    */
   if (LO.get_tag() == TOK_F2_LEFT)
      {
        Token result = eval_XB(X, B);
        if (result.get_tag() == TOK_APL_VALUE1)
           result.ChangeTag(TOK_APL_VALUE2);
         return result;
      }

const ShapeItem function_number = X->get_cfirst().get_int_value();
   switch (function_number)
      {
        case -1:   // benchmark monadic LO with argument B
           {
#if ! HAVE_RDTSC
             MORE_ERROR() << "Platform has no 'rtdsc' instruction.";
             DOMAIN_ERROR;
#endif

             // doit...
             //
             cFunction_P fun = LO.get_function();
             Assert(fun);
             const uint64_t from = cycle_counter();
             Workspace::SI_top()->set_safe_execution_count();   // pretend ⎕ES
             Token result = fun->eval_B(B);
             if (result.get_tag() == TOK_SI_PUSHED)
                {
                  benchmark_cycles_from = from | 1;
                  return result;
                }
             const uint64_t to = cycle_counter();
             Workspace::SI_top()->clear_safe_execution();
             return Token(TOK_APL_VALUE1, IntScalar(to - from, LOC));
           }
        case 49:
           {
             Token lines_B = eval_XB(X, B);
             return Bif_OPER1_EACH::do_eval_LB(LO, lines_B.get_apl_val());
           }
      }

   MORE_ERROR() <<
"Bad function number (axis X) " << function_number << " in LO ⎕FIO[X] B.\n"
"Chances are that you meant to use ⎕FIO[X] B and not LO ⎕FIO[X] B. In that\n"
"case use (⎕FIO[X]) or H←⎕FIO[X]";
   DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
Token
Quad_FIO::eval_XB(Value_P X, Value_P B) const
{
   CHECK_SECURITY(disable_Quad_FIO);

   if (B->get_rank() > 1)   RANK_ERROR;
   if (X->get_rank() > 1)   RANK_ERROR;

int function_number = -1;
   if (X->is_char_array())   // function name, e.g. "open"
      {
        UCS_string ucs_X(*X);
        UTF8_string utf_X(ucs_X);
        function_number = function_name_to_int(utf_X.c_str());
        if (function_number == -1)
           {
             MORE_ERROR() << "Bad function name X in ⎕FIO[X]B (X is '"
                          << ucs_X << "')";
             AXIS_ERROR;
           }
      }
   else
      {
        function_number = X->get_cfirst().get_near_int();
      }

   switch(function_number)
      {
         case 0:   // list functions
              return list_functions(CERR, false);

         case 1:   // return (last) errno
              goto out_errno;

         case 2:   // return strerror(B)
              {
                int b = B->get_cfirst().get_near_int();
                if (b < 0)   b = -b;
                const char * text = strerror(b);
                const int len = strlen(text);
                Value_P Z(len, LOC);
                loop(t, len)   Z->next_ravel_Char(Unicode(text[t]));

                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 3:   // fopen(Bs, "r") filename Bs
              {
                const UCS_string path_ucs(*B.get());
                const UTF8_string path(path_ucs);
                errno = 0;
                FILE * f = fopen(path.c_str(), "r");
                if (f == 0)   return Token(TOK_APL_VALUE1, IntScalar(-1, LOC));

                file_entry fe(f, fileno(f));
                fe.path = path;
                fe.fe_may_read = true;
                open_files.push_back(fe);
                return Token(TOK_APL_VALUE1, IntScalar(fe.fe_fd, LOC));
              }

         case 4:   // fclose(Bh)
              {
                errno = 0;
                file_entry & fe = get_file_entry(*B.get());

                // never close stdin, stdout, or stderr
                if (fe.fe_fd <= STDERR_FILENO)   DOMAIN_ERROR;

                if (fe.fe_FILE)   fclose(fe.fe_FILE);   // also closes fe.fe_fd
                else              close(fe.fe_fd);

                fe = open_files.back();       // move last file to fe
                open_files.pop_back();        // erase last file
                goto out_errno;
              }

         case 5:   // errno of Bh
              {
                errno = 0;
                file_entry & fe = get_file_entry(*B.get());
                return Token(TOK_APL_VALUE1, IntScalar(fe.fe_errno, LOC));
              }

         case 6:   // fread(Zi, 1, SMALL_BUF, Bh) 1 byte per Zi
              {
                errno = 0;
                FILE * file = get_FILE(*B.get());
                clearerr(file);

                char buffer[SMALL_BUF];

                const size_t len = fread(buffer, 1, SMALL_BUF, file);
                if (len == 0)   goto out_errno;

                Value_P Z(len, LOC);
                loop(z, len)   Z->next_ravel_Int(buffer[z] & 0xFF);
                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 8:   // fgets(Zi, SMALL_BUF, Bh) 1 byte per Zi
              {
                errno = 0;
                FILE * file = get_FILE(*B.get());
                clearerr(file);

                char buffer[SMALL_BUF];

                const char * s = fgets(buffer, SMALL_BUF, file);
                const int len = s ? strlen(s) : 0;
                Value_P Z(len, LOC);
                loop(z, len)   Z->next_ravel_Int(buffer[z] & 0xFF);
                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 9:   // fgetc(Bh)
              {
                FILE * file = get_FILE(*B.get());
                return Token(TOK_APL_VALUE1, IntScalar(fgetc(file), LOC));
              }

         case 10:   // feof(Bh)
              {
                FILE * file = get_FILE(*B.get());
                return Token(TOK_APL_VALUE1, IntScalar(feof(file), LOC));
              }

         case 11:   // ferror(Bh)
              {
                FILE * file = get_FILE(*B.get());
                return Token(TOK_APL_VALUE1, IntScalar(ferror(file),LOC));
              }

         case 12:   // ftell(Bh)
              {
                FILE * file = get_FILE(*B.get());
                return Token(TOK_APL_VALUE1, IntScalar(ftell(file),LOC));
              }

         case 16:   // fflush(Bh)
              {
                errno = 0;
                FILE * file = get_FILE(*B.get());
                fflush(file);
              }
              goto out_errno;

         case 17:   // fsync(Bh)
              {
                errno = 0;
                const int fd = get_fd(*B.get());
                fsync(fd);
              }
              goto out_errno;

         case 18:   // fstat(Bh)
              {
                errno = 0;
                const int fd = get_fd(*B.get());
                struct stat s;
                const int result = fstat(fd, &s);
                if (result)   goto out_errno;   // fstat failed

                Value_P Z(Value_P(13, LOC));
                Z->next_ravel_Int(s.st_dev);
                Z->next_ravel_Int(s.st_ino);
                Z->next_ravel_Int(s.st_mode);
                Z->next_ravel_Int(s.st_nlink);
                Z->next_ravel_Int(s.st_uid);
                Z->next_ravel_Int(s.st_gid);
                Z->next_ravel_Int(s.st_rdev);
                Z->next_ravel_Int(s.st_size);
                Z->next_ravel_Int(s.st_blksize);
                Z->next_ravel_Int(s.st_blocks);
                Z->next_ravel_Int(s.st_atime);
                Z->next_ravel_Int(s.st_mtime);
                Z->next_ravel_Int(s.st_ctime);
                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 19:   // unlink(Bc)
              {
                errno = 0;
                const UCS_string path_ucs(*B.get());
                const UTF8_string path(path_ucs);
                unlink(path.c_str());
              }
              goto out_errno;

         case 20:   // mkdir(Bc)
              {
                errno = 0;
                const UCS_string path_ucs(*B.get());
                const UTF8_string path(path_ucs);
                mkdir(path.c_str(), 0777);
                if (errno == EEXIST)   errno = 0;   // frequent non-error
              }
              goto out_errno;

         case 21:   // rmdir(Bc)
              {
                errno = 0;
                const UCS_string path_ucs(*B.get());
                const UTF8_string path(path_ucs);
                rmdir(path.c_str());
              }
              goto out_errno;

         case 24:   // popen(Bs, "r") command Bs
              {
                const UCS_string path_ucs(*B.get());
                const UTF8_string path(path_ucs);
                errno = 0;
                FILE * f = popen(path.c_str(), "r");
                if (f == 0)
                   {
                     if (errno)   goto out_errno;   // errno may be set or not
                     return Token(TOK_APL_VALUE1, IntScalar(-1, LOC));
                   }

                file_entry fe(f, fileno(f));
                fe.fe_may_read = true;
                open_files.push_back(fe);
                signal(SIGCHLD, SIG_DFL);
                return Token(TOK_APL_VALUE1, IntScalar(fe.fe_fd,LOC));
              }

         case 25:   // pclose(Bh)
              {
                errno = 0;
                file_entry & fe = get_file_entry(*B.get());
                int err = EBADF;   /* Bad file number */
                if (fe.fe_FILE)
                   {
                     err = pclose(fe.fe_FILE);
                     signal(SIGCHLD, SIG_IGN);
                   }

                fe = open_files.back();       // move last file to fe
                open_files.pop_back();        // erase last file

                if (err == -1)   goto out_errno;   // pclose() failed
                return Token(TOK_APL_VALUE1, IntScalar(err,LOC));
              }

         case 26:   // read entire file
              {
                errno = 0;
                const UCS_string path_ucs(*B.get());
                const UTF8_string path(path_ucs);
                int fd = open(path.c_str(), O_RDONLY);
                if (fd == -1)   goto out_errno;

                struct stat st;
                if (fstat(fd, &st))
                   {
                     close(fd);
                     goto out_errno;
                   }

                if (!S_ISREG(st.st_mode))
                   {
                     close(fd);
                     MORE_ERROR() << path << " is not a regular file";
                     DOMAIN_ERROR;
                   }

                const ShapeItem len = st.st_size;
                uint8_t * data = reinterpret_cast<uint8_t *>
                                 (mmap(0, len, PROT_READ, MAP_SHARED, fd, 0));
                close(fd);
                if (data == 0)   goto out_errno;

                Value_P Z(len, LOC);
                Z->set_proto_Spc();
                loop(z, len)   Z->next_ravel_Char(Unicode(data[z]));
                munmap(data, len);

                Z->set_proto_Spc();
                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 28:   // read directory Bs
         case 29:   // read file names in directory Bs
              {
                errno = 0;
                const UCS_string path_ucs(*B.get());
                const UTF8_string path(path_ucs);
                DIR * dir = opendir(path.c_str());
                if (dir == 0)   goto out_errno;

                vector<struct dirent> entries;
                for (;;)
                    {
                      dirent * entry = readdir(dir);
                      if (entry == 0)   break;   // directory done

                       // skip . and ..
                       //
                       const int entry_len = strlen(entry->d_name);
                       if (entry->d_name[0] == '.')
                          {
                            if (entry_len == 1 ||
                                (entry_len == 2 && entry->d_name[1] == '.'))
                               continue;
                          }

                      entries.push_back(*entry);
                    }
                closedir(dir);

                Shape shape_Z(entries.size());
                if (function_number == 28)   // 5 by N matrix
                   {
                     shape_Z.add_shape_item(5);
                   }

                Value_P Z(shape_Z, LOC);

                loop(e, entries.size())
                   {
                     const dirent & dent = entries[e];
                     if (function_number == 28)   // full dirent
                        {
                          // all platforms support inode number
                          //
                          Z->next_ravel_Int(dent.d_ino);


#ifdef _DIRENT_HAVE_D_OFF
                          Z->next_ravel_Int(dent.d_off);
#else
                          Z->next_ravel_1();
#endif


#ifdef _DIRENT_HAVE_D_RECLEN
                          Z->next_ravel_Int(dent.d_reclen);
#else
                          Z->next_ravel_1();
#endif


#ifdef _DIRENT_HAVE_D_TYPE
                         Z->next_ravel_Int(dent.d_type);
#else
                          Z->next_ravel_1();
#endif
                        }   // function_number == 28

                     UCS_string filename(UTF8_string(dent.d_name));
                     Value_P Z_name(filename, LOC);
                     Z->next_ravel_Pointer(Z_name.get());
                   }

                Z->set_proto_Spc();
                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 32:   // socket(Bi=AF_INET, SOCK_STREAM, 0)
              UNSAFE("socket", function_number);
              {
                errno = 0;
                APL_Integer domain = AF_INET;
                APL_Integer type = SOCK_STREAM;
                APL_Integer protocol = 0;
                if (B->element_count() > 0)
                   domain = B->get_cfirst().get_int_value();
                if (B->element_count() > 1)
                   type = B->get_cravel(1).get_int_value();
                if (B->element_count() > 2)
                   protocol = B->get_cravel(2).get_int_value();
                const int sock = socket(domain, type, protocol);
                if (sock == -1)   goto out_errno;

                file_entry fe(0, sock);
                fe.fe_may_read = true;
                fe.fe_may_write = true;
                open_files.push_back(fe);
                return Token(TOK_APL_VALUE1, IntScalar(fe.fe_fd, LOC));
              }

         case 34:   // listen(Bh, 10)
              {
                const int fd = get_fd(*B.get());
                errno = 0;
                listen(fd, 10);
                goto out_errno;
              }

         case 35:   // accept(Bh)
              {
                const int fd = get_fd(*B.get());
                errno = 0;
                SockAddr addr;
                socklen_t alen = sizeof(addr.inet);
                const int sock = accept(fd, &addr.addr, &alen);
                if (sock == -1)   goto out_errno;

                file_entry nfe (0, sock);
                open_files.push_back(nfe);

                Value_P Z(4, LOC);
                Z->next_ravel_Int(nfe.fe_fd);
                Z->next_ravel_Int(addr.inet.sin_family);
                Z->next_ravel_Int(ntohl(addr.inet.sin_addr.s_addr));
                Z->next_ravel_Int(ntohs(addr.inet.sin_port));
                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 37:   // recv(Bh, Zi, SMALL_BUF, 0) 1 byte per Zi
              {
                errno = 0;
                const int fd = get_fd(*B.get());

                char buffer[SMALL_BUF];

                const ssize_t len = recv(fd, buffer, sizeof(buffer), 0);
                if (len < 0)   goto out_errno;

                Value_P Z(len, LOC);
                loop(z, len)   Z->next_ravel_Int(buffer[z] & 0xFF);
                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 40:   // select(Br, Bw, Be, Bt)
              {
                fd_set readfds;     FD_ZERO(&readfds);
                fd_set writefds;    FD_ZERO(&writefds);
                fd_set exceptfds;   FD_ZERO(&exceptfds);
                timeval timeout = { 0, 0 };
                fd_set * rd = 0;
                fd_set * wr = 0;
                fd_set * ex = 0;
                timeval * to = 0;
                APL_Integer max_fd = -1;

                if (B->element_count() > 4)   LENGTH_ERROR;
                if (B->element_count() < 1)   LENGTH_ERROR;

                if (B->element_count() >= 4)
                   {
                     const APL_Integer milli = B->get_cravel(3).get_int_value();
                     if (milli < 0)   DOMAIN_ERROR;

                     timeout.tv_sec = milli / 1000;
                     timeout.tv_usec = (milli%1000) * 1000;
                   }

                if (B->element_count() >= 3)
                   {
                      Value_P vex = B->get_cravel(2).get_pointer_value();
                      loop(l, vex->element_count())
                          {
                            const int fd(vex->get_cravel(l).get_int_value());
                            if (fd < 0)                       DOMAIN_ERROR;
                            if (fd > 8*int(sizeof(fd_set)))   DOMAIN_ERROR;
                            FD_SET(fd, &exceptfds);
                            if (max_fd < fd)   max_fd = fd;
                            ex = &exceptfds;
                          }
                   }

                if (B->element_count() >= 2)
                   {
                      Value_P vwr = B->get_cravel(1).get_pointer_value();
                      loop(l, vwr->element_count())
                          {
                            const APL_Integer fd =
                                  vwr->get_cravel(l).get_int_value();
                            if (fd < 0)                       DOMAIN_ERROR;
                            if (fd > 8*int(sizeof(fd_set)))   DOMAIN_ERROR;
                            FD_SET(fd, &writefds);
                            if (max_fd < fd)   max_fd = fd;
                            wr = &writefds;
                          }
                   }

                if (B->element_count() >= 1)
                   {
                      Value_P vrd = B->get_cfirst().get_pointer_value();
                      loop(l, vrd->element_count())
                          {
                            const APL_Integer fd =
                                  vrd->get_cravel(l).get_int_value();
                            if (fd < 0)                         DOMAIN_ERROR;
                            if (fd > (8*int(sizeof(fd_set))))   DOMAIN_ERROR;
                            FD_SET(fd, &readfds);
                            if (max_fd < fd)   max_fd = fd;
                            rd = &readfds;
                          }
                   }

                const int count = select(max_fd + 1, rd, wr, ex, to);
                if (count < 0)   goto out_errno;

                Value_P Z(5, LOC);
                const APL_Integer milli_seconds = timeout.tv_sec*1000
                                                + timeout.tv_usec/1000;
                Z->next_ravel_Int(count);
                Z->next_ravel_Pointer(fds_to_val(rd, max_fd).get());
                Z->next_ravel_Pointer(fds_to_val(wr, max_fd).get());
                Z->next_ravel_Pointer(fds_to_val(ex, max_fd).get());
                Z->next_ravel_Int(milli_seconds);
                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 41:   // read(Bh, Zi, SMALL_BUF) 1 byte per Zi
              {
                errno = 0;
                const int fd = get_fd(*B.get());

                char buffer[SMALL_BUF];

                const ssize_t len = read(fd, buffer, sizeof(buffer));
                if (len < 0)   goto out_errno;

                Value_P Z(len, LOC);
                loop(z, len)   Z->next_ravel_Int(buffer[z] & 0xFF);
                Z->set_proto_Int();
                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 44:   // getsockname(Bh, Zi)
              {
                const int fd = get_fd(*B.get());
                errno = 0;
                SockAddr addr;
                socklen_t alen = sizeof(addr.inet);
                const int ret = getsockname(fd, &addr.addr, &alen);
                if (ret == -1)   goto out_errno;

                Value_P Z(3, LOC);
                Z->next_ravel_Int(addr.inet.sin_family);
                Z->next_ravel_Int(ntohl(addr.inet.sin_addr.s_addr));
                Z->next_ravel_Int(ntohs(addr.inet.sin_port));
                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 45:   // getpeername(Bh, Zi)
              {
                const int fd = get_fd(*B.get());
                errno = 0;
                SockAddr addr;
                socklen_t alen = sizeof(addr.inet);
                const int ret = getpeername(fd, &addr.addr, &alen);
                if (ret == -1)   goto out_errno;

                Value_P Z(3, LOC);
                Z->next_ravel_Int(addr.inet.sin_family);
                Z->next_ravel_Int(ntohl(addr.inet.sin_addr.s_addr));
                Z->next_ravel_Int(ntohs(addr.inet.sin_port));
                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 49:   // read entire file as nested lines
              {
                errno = 0;
                const UCS_string path_ucs(*B.get());
                const UTF8_string path(path_ucs);
                const int fd = open(path.c_str(), O_RDONLY);
                if (fd == -1)   goto out_errno;

                struct stat st;
                if (fstat(fd, &st))
                   {
                     close(fd);
                     goto out_errno;
                   }

                if (!S_ISREG(st.st_mode))
                   {
                     close(fd);
                     MORE_ERROR() << path << " is not a regular file";
                     DOMAIN_ERROR;
                   }

                const ShapeItem len = st.st_size;
                UTF8 * data = reinterpret_cast<UTF8 *>
                              (mmap(0, len, PROT_READ, MAP_SHARED, fd, 0));
                close(fd);
                if (data == 0)   goto out_errno;

                // count number of LFs in the file
                //
                ShapeItem line_count = 0;
                loop(l, len)
                    {
                      if (data[l] == '\n')   ++line_count;
                    }

                // if the last line does not end with \n then count it as well
                if (len && data[len - 1] != '\n')   ++line_count;

                Value_P Z(line_count, LOC);
                Z->set_proto_Spc();

                UTF8 * from = data;
                loop(l, len)
                    {
                      if (data[l] != '\n')   continue;

                      uint8_t * end = data + l;
                     // discard CR before LF
                      if (end > data && end[-1] == '\r')   --end;
                      UTF8_string utf(from, end - from);
                      UCS_string ucs(utf);
                      Value_P ZZ(ucs, LOC);
                      Z->next_ravel_Pointer(ZZ.get());
                      from = data + l + 1;
                    }

                if (len && data[len - 1] != '\n')   // incomplete final line
                   {
                      uint8_t * end = data + len;
                      if (end[-1] == '\r')   --end;   // discard trailing CR
                      UTF8_string utf(from, end - from);
                      UCS_string ucs(utf);
                      Value_P ZZ(ucs, LOC);
                      Z->next_ravel_Pointer(ZZ.get());
                   }

                munmap(data, len);

                Z->set_proto_Spc();
                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 50:   // gettimeofday
              {
                const APL_Integer unit = B->get_cfirst().get_near_int();
                timeval tv;
                gettimeofday(&tv, 0);
                int64_t usec = tv.tv_sec;
                usec *= 1000000;
                usec += tv.tv_usec;
                APL_Integer z = 0;
                if      (unit == 1)         z = usec/1000000;
                else if (unit == 1000)      z = usec/1000;
                else if (unit == 1000000)   z = usec;
                else
                   {
                     MORE_ERROR() <<
       "Invalid time unit (use 1 for seconds, 1000 for ms, or 1000000 for μs)";
                     DOMAIN_ERROR;
                   }
                return Token(TOK_APL_VALUE1, IntScalar(z, LOC));
              }

         case 51:   // mktime
              {
                if (B->element_count() < 6 || B->element_count() > 9)
                   LENGTH_ERROR;

                tm t;
                t.tm_year = B->get_cfirst().get_int_value() - 1900;
                t.tm_mon  = B->get_cravel(1).get_int_value() - 1;
                t.tm_mday = B->get_cravel(2).get_int_value();
                t.tm_hour = B->get_cravel(3).get_int_value();
                t.tm_min  = B->get_cravel(4).get_int_value();
                t.tm_sec  = B->get_cravel(5).get_int_value();
                if (B->element_count() > 6)   // dst provided
                   t.tm_isdst = B->get_cravel(6).get_int_value();
                else
                   t.tm_isdst = -1;

               const time_t seconds = mktime(&t);
               if (seconds == time_t(-1))   DOMAIN_ERROR;

                Value_P Z(4, LOC);
                Z->next_ravel_Int(seconds);
                Z->next_ravel_Int(t.tm_wday);
                Z->next_ravel_Int(t.tm_yday);
                Z->next_ravel_Int(t.tm_isdst);
                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 52:   // localtime
         case 53:   // gmtime
              {
                if (B->element_count() != 1)   LENGTH_ERROR;
                const time_t t = B->get_cfirst().get_int_value();
                const tm * tmp = (function_number == 52) ? localtime(&t)
                                                         : gmtime(&t);
                if (tmp == 0)   DOMAIN_ERROR;

                Value_P Z(9, LOC);
                Z->next_ravel_Int(tmp->tm_year + 1900);
                Z->next_ravel_Int(tmp->tm_mon + 1);
                Z->next_ravel_Int(tmp->tm_mday);
                Z->next_ravel_Int(tmp->tm_hour);
                Z->next_ravel_Int(tmp->tm_min);
                Z->next_ravel_Int(tmp->tm_sec);
                Z->next_ravel_Int(tmp->tm_wday);
                Z->next_ravel_Int(tmp->tm_yday);
                Z->next_ravel_Int(tmp->tm_isdst);
                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 54:    // chdir
              {
                const UCS_string path_ucs(*B.get());
                const UTF8_string path(path_ucs);
                errno = chdir(path.c_str());
                goto out_errno;
              }

         case 57:   // fork() + execve() in the child
              {
                errno = 0;
                const int fd = do_FIO_57(*B.get(), 0);
                if (fd == -1)   goto out_errno;
                return Token(TOK_APL_VALUE1, IntScalar(fd, LOC));
              }

         case 60:   // random value
              {
                 if (!B->is_scalar())   RANK_ERROR;
                 const APL_Integer len = B->get_cfirst().get_int_value();
                 if (len < 1)   LENGTH_ERROR;
                 if (len > 8)   LENGTH_ERROR;
                 Value_P Z = get_random(0, len);
                 return Token(TOK_APL_VALUE1, Z);
              }

         case 200:   // clear statistics Bi
         case 201:   // get statistics Bi
              {
                const Pfstat_ID b = Pfstat_ID(B->get_cfirst().get_int_value());
                Statistics * stat = Performance::get_statistics(b);
                if (stat == 0)   DOMAIN_ERROR;   // bad statistics ID

                if (function_number == 200)   // reset statistics
                   {
                     stat->reset();
                     return Token(TOK_APL_VALUE1, IntScalar(b, LOC));
                   }

                // get statistics
                //
                 const int t = Performance::get_statistics_type(b);
                 UCS_string stat_name(UTF8_string(stat->get_name()));
                 Value_P Zsub(stat_name, LOC);
                 if (t <= 2)   // cell function statistics
                    {
                      const Statistics_record * r1 = stat->get_first_record();
                      const Statistics_record * rN = stat->get_record();
                      Value_P Z(8, LOC);
                      Z->next_ravel_Int(t);
                      Z->next_ravel_Pointer(Zsub.get());
                      Z->next_ravel_Int(r1->get_count());
                      Z->next_ravel_Int(r1->get_sum());
                      Z->next_ravel_Float(r1->get_sum2());
                      Z->next_ravel_Int(rN->get_count());
                      Z->next_ravel_Int(rN->get_sum());
                      Z->next_ravel_Float(rN->get_sum2());
                      Z->check_value(LOC);
                      return Token(TOK_APL_VALUE1, Z);
                    }
                 else           // function statistics
                    {
                      const Statistics_record * r = stat->get_record();
                      Value_P Z(5, LOC);
                      Z->next_ravel_Int(t);
                      Z->next_ravel_Pointer(Zsub.get());
                      Z->next_ravel_Int(r->get_count());
                      Z->next_ravel_Int(r->get_sum());
                      Z->next_ravel_Float(r->get_sum2());
                      Z->check_value(LOC);
                      return Token(TOK_APL_VALUE1, Z);
                    }
              }

         case 202:   // get monadic parallel threshold
         case 203:   // get dyadic  parallel threshold
              {
                const Function * fun = 0;
                if (B->element_count() == 3)   // dyadic operator
                   {
                     const Unicode lfun = B->get_cfirst().get_char_value();
                     const Unicode oper = B->get_cravel(1).get_char_value();
                     if (oper == UNI_FULLSTOP)
                        {
                          if (lfun == UNI_RING_OPERATOR)   // ∘.g
                             fun = &Bif_OPER2_OUTER::fun;
                          else                             // f.g
                             fun = &Bif_OPER2_INNER::fun;
                        }
                   }
                else
                   {
                     const Unicode prim = B->get_cfirst().get_char_value();
                     const Token tok = Tokenizer::tokenize_function(prim);
                     if (!tok.is_function())   DOMAIN_ERROR;
                     fun = tok.get_function();
                   }
                if (fun == 0)   DOMAIN_ERROR;
                APL_Integer old_threshold;
                if (function_number == 202)
                   {
                     old_threshold = fun->get_monadic_threshold();
                   }
                else
                   {
                     old_threshold = fun->get_dyadic_threshold();
                   }

                 return Token(TOK_APL_VALUE1, IntScalar(old_threshold, LOC));
              }

         case 61:   // secs_epoch(Bh)
              return Token(TOK_APL_VALUE1, IntScalar(secs_epoch(*B), LOC));

        default: break;
      }

   MORE_ERROR() << "bad function number " << function_number <<
                   " in Quad_FIO::eval_XB()";

   CERR << "Bad eval_XB() function number: " << function_number << endl;
   VALENCE_ERROR;

out_errno:
   return Token(TOK_APL_VALUE1, IntScalar(-errno, LOC));
}
//----------------------------------------------------------------------------
int
Quad_FIO::do_FIO_57(const UCS_string & B, char * const * envp)
{
int spair[2];
   if (socketpair(AF_UNIX, SOCK_DGRAM, 0, spair))
      {
        MORE_ERROR() << "socketpair() failed: " << strerror(errno);
        DOMAIN_ERROR;
      }

const pid_t child = fork();
   if (child == -1)
      {
        MORE_ERROR() << "fork() failed: " << strerror(errno);
        DOMAIN_ERROR;
      }

   if (child)   // parent process: return handle
      {
        file_entry fe(0, spair[0]);
        fe.fe_may_read = true;
        fe.fe_may_write = true;
        open_files.push_back(fe);
        return fe.fe_fd;
      }

   // code executed in the forked child.,,
   // Close some fds and then execve(Bs)
   // No need to free any strings allocated here
   //
const int sock = spair[1];
   ::close(STDIN_FILENO);
   for (int j = STDERR_FILENO+2; j < 100; ++j)
       {
         if (j != sock)   ::close(j);
       }

   // make the communication socket file descriptor 3
   //  (== STDERR + 1) in the client
   //
   dup2(sock, 3);

const UCS_string path_ucs(B);
const UTF8_string path(path_ucs);
char * filename = strdup(path.c_str());
int argc = 1;
   for (const char * f = filename; *f; ++f)
       if (f[0] == ' ' && f[1] != ' ')   ++ argc;

char ** argv = new char *[argc + 2];
int ai = 0;
char * from = filename;
   for (char * f = filename; *f; ++f)
       {
         if (*f == ' ')   // end of argument
            {
              argv[ai++] = from;
              *f = 0;
              while (f[1] == ' ')   ++f;
              from = f + 1;
            }
       }

   if (*from)  argv[ai++] = from;
   argv[ai] = 0;

   execve(filename, argv, envp);   // no return on success

   // execve() failed
   //
   free(filename);
   ::close(3);

   usleep(100000);
   CERR << "*** execve() failed in 57 ⎕CR: " << strerror(errno);
   exit(-1);
}
//----------------------------------------------------------------------------
Token
Quad_FIO::eval_AXB(const Value_P A, const Value_P X, const Value_P B) const
{
   CHECK_SECURITY(disable_Quad_FIO);

   if (A->get_rank() > 1)   RANK_ERROR;
   if (B->get_rank() > 1)   RANK_ERROR;
   if (X->get_rank() > 1)   RANK_ERROR;

int function_number = -1;
   if (X->is_char_array())   // function name, e.g. "open"
      {
        UCS_string ucs_X(*X);
        UTF8_string utf_X(ucs_X);
        function_number = function_name_to_int(utf_X.c_str());
        if (function_number == -1)
           {
             MORE_ERROR() << "Bad function name X in ⎕FIO[X]B (X is '"
                          << ucs_X << "')";
             AXIS_ERROR;
           }
      }
   else
      {
        function_number = X->get_cfirst().get_near_int();
      }

   switch(function_number)
      {
         case 0:   // list functions
              return list_functions(COUT, false);

         case 3:   // fopen(Bs, As) filename Bs mode As
              {
                const UCS_string mode_ucs(*A.get());
                const UCS_string path_ucs(*B.get());
                UTF8_string mode(mode_ucs);
                const UTF8_string path(path_ucs);

                const char * m = mode.c_str();
                bool read = false;
                bool write = false;
                if      (!strncmp(m, "r+", 2))     read = write = true;
                else if (!strncmp(m, "r" , 1))     read         = true;
                else if (!strncmp(m, "w+", 2))     read = write = true;
                else if (!strncmp(m, "w" , 1))            write = true;
                else if (!strncmp(m, "a+", 2))     read = write = true;
                else if (!strncmp(m, "a" , 1))            write = true;
                else    DOMAIN_ERROR;

                errno = 0;
                FILE * f = fopen(path.c_str(), m);
                if (f == 0)   return Token(TOK_APL_VALUE1, IntScalar(-1, LOC));

                file_entry fe(f, fileno(f));
                fe.path = path;
                fe.fe_may_read = read;
                fe.fe_may_write = write;
                open_files.push_back(fe);
                return Token(TOK_APL_VALUE1, IntScalar(fe.fe_fd,LOC));
              }

         case 6:   // fread(Zi, 1, Ai, Bh) 1 byte per Zi
              {
                errno = 0;
                const size_t bytes = A->get_cfirst().get_near_int();
                FILE * file = get_FILE(*B.get());
                clearerr(file);

                char small_buffer[SMALL_BUF];
                char * buffer = small_buffer;
                char * del = 0;
                if (bytes > sizeof(small_buffer))
                   buffer = del = new char[bytes];

                const size_t len = fread(buffer, 1, bytes, file);
                if (len == 0)   goto out_errno;

                Value_P Z(len, LOC);
                loop(z, len)   Z->next_ravel_Int(buffer[z] & 0xFF);
                delete [] del;
                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 7:   // fwrite(Ai, 1, ⍴Ai, Bh) 1 byte per Zi
              {
                errno = 0;
                const size_t bytes = A->element_count();
                FILE * file = get_FILE(*B.get());

                char small_buffer[SMALL_BUF];
                char * buffer = small_buffer;
                char * del = 0;
                if (bytes > sizeof(small_buffer))
                   buffer = del = new char[bytes];

                loop(z, bytes)   buffer[z] = A->get_cravel(z).get_near_int();

                const size_t len = fwrite(buffer, 1, bytes, file);
                delete [] del;

                return Token(TOK_APL_VALUE1, IntScalar(len, LOC));
              }

         case 8:   // fgets(Zi, Ai, Bh) 1 byte per Zi
              {
                errno = 0;
                const size_t bytes = A->get_cfirst().get_near_int();
                FILE * file = get_FILE(*B.get());
                clearerr(file);

                char small_buffer[SMALL_BUF];
                char * buffer = small_buffer;
                char * del = 0;
                if (bytes > sizeof(buffer))
                   buffer = del = new char[bytes + 1];

                const char * s = fgets(buffer, bytes, file);
                const int len = s ? strlen(s) : 0;
                Value_P Z(len, LOC);
                loop(z, len)   Z->next_ravel_Int(buffer[z] & 0xFF);
                delete [] del;
                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 13:   // fseek(Bh, Ai, SEEK_SET)
              {
                errno = 0;
                FILE * file = get_FILE(*B.get());
                const APL_Integer pos = A->get_cfirst().get_near_int();
                fseek(file, pos, SEEK_SET);
              }
              goto out_errno;

         case 14:   // fseek(Bh, Ai, SEEK_CUR)
              {
                errno = 0;
                FILE * file = get_FILE(*B.get());
                const APL_Integer pos = A->get_cfirst().get_near_int();
                fseek(file, pos, SEEK_CUR);
              }
              goto out_errno;

         case 15:   // fseek(Bh, Ai, SEEK_END)
              {
                errno = 0;
                FILE * file = get_FILE(*B.get());
                const APL_Integer pos = A->get_cfirst().get_near_int();
                fseek(file, pos, SEEK_END);
              }
              goto out_errno;

         case 20:   // mkdir(Bc, Ai)
              {
                errno = 0;
                const int mask = A->get_cfirst().get_near_int();
                const UCS_string path_ucs(*B.get());
                const UTF8_string path(path_ucs);
                mkdir(path.c_str(), mask);
                if (errno == EEXIST)   errno = 0;   // frequent non-error
              }
              goto out_errno;

         case 22:   // fprintf(Bh, A) ←→
                    // fprintf(FILE *stream, const char *format, ...)
              {
                errno = 0;
                UCS_string UZ;
                FILE * file = get_FILE(*B.get());
                return do_fprintf(file, A);
              }

         case 23:   // fwrite(Ac, 1, ⍴Ac, Bh) Unicode Ac Output UTF-8
              {
                errno = 0;
                FILE * file = get_FILE(*B.get());
                UCS_string text(*A.get());
                UTF8_string utf(text);
                const size_t len = fwrite(utf.c_str(), 1, utf.size(), file); 
                return Token(TOK_APL_VALUE1, IntScalar(len, LOC));
              }

         case 24:   // popen(Bs, As) command Bs mode As
              {
                const UCS_string mode_ucs(*A.get());
                const UCS_string path_ucs(*B.get());
                const UTF8_string mode(mode_ucs);
                const UTF8_string path(path_ucs);
                const char * m = mode.c_str();
                bool read = false;
                bool write = false;
                if      (!strncmp(m, "er", 2))     read = true;
                else if (!strncmp(m, "r" , 1))     read = true;
                else if (!strncmp(m, "ew", 2))     write = true;
                else if (!strncmp(m, "w" , 1))     write = true;
                else    DOMAIN_ERROR;

                errno = 0;
                FILE * f = popen(path.c_str(), m);
                if (f == 0)
                   {
                     if (errno)   goto out_errno;   // errno may be set or not
                     return Token(TOK_APL_VALUE1, IntScalar(-1, LOC));
                   }

                signal(SIGCHLD, SIG_DFL);
                file_entry fe(f, fileno(f));
                fe.fe_may_read = read;
                fe.fe_may_write = write;
                open_files.push_back(fe);
                return Token(TOK_APL_VALUE1, IntScalar(fe.fe_fd, LOC));
              }

         case 27:   // rename(As, Bs)
              {
                const UCS_string old_name_ucs(*A.get());
                const UCS_string new_name_ucs(*B.get());
                UTF8_string old_name(old_name_ucs);
                UTF8_string new_name(new_name_ucs);
                errno = 0;
                const int result = rename(old_name.c_str(), new_name.c_str());
                if (result && errno)   goto out_errno;   // errno should be set

               return Token(TOK_APL_VALUE1, IntScalar(result, LOC));
              }

         case 31:   // access
              {
                const UCS_string mode_ucs(*A.get());
                const UCS_string path_ucs(*B.get());
                UTF8_string permissions(mode_ucs);
                const UTF8_string path(path_ucs);
                int perms = 0;
                loop(a, permissions.size())
                    {
                      int p = permissions[a] & 0xFF;
                      if      (p == 'R')   perms |= R_OK;
                      else if (p == 'r')   perms |= R_OK;
                      else if (p == 'W')   perms |= W_OK;
                      else if (p == 'w')   perms |= W_OK;
                      else if (p == 'X')   perms |= X_OK;
                      else if (p == 'x')   perms |= X_OK;
                      else if (p == 'F')   perms |= F_OK;
                      else if (p == 'f')   perms |= F_OK;
                      else DOMAIN_ERROR;
                    }

                 errno = 0;
                 const int not_ok = access(path.c_str(), perms);
                 if (not_ok)   goto out_errno;
                 return Token(TOK_APL_VALUE1, IntScalar(0, LOC));
              }

         case 33:   // bind(Bh, Aa)
              {
                const int fd = get_fd(*B.get());
                SockAddr addr;
                memset(&addr, 0, sizeof(addr.inet));
                addr.inet.sin_family = A->get_cfirst().get_int_value();
                addr.inet.sin_addr.s_addr = htonl(A->get_cravel(1)
                                                    .get_int_value());
                addr.inet.sin_port = htons(A->get_cravel(2).get_int_value());
                errno = 0;
                bind(fd, &addr.addr, sizeof(addr.inet));
                goto out_errno;
              }

         case 34:   // listen(Bh, Ai)
              {
                const int fd = get_fd(*B.get());
                APL_Integer backlog = 10;
                if (A->element_count() > 0)
                   backlog = A->get_cfirst().get_int_value();

                errno = 0;
                listen(fd, backlog);
                goto out_errno;
              }

         case 36:   // connect(Bh, Aa)
              {
                const int fd = get_fd(*B.get());
                errno = 0;
                SockAddr addr;
                memset(&addr, 0, sizeof(addr.inet));
                addr.inet.sin_family = A->get_cfirst().get_int_value();
                addr.inet.sin_addr.s_addr = htonl(A->get_cravel(1)
                                                    .get_int_value());
                addr.inet.sin_port = htons(A->get_cravel(2).get_int_value());
                errno = 0;
                connect(fd, &addr.addr, sizeof(addr));
                goto out_errno;
              }

         case 37:   // recv(Bh, Zi, Ai, 0) 1 byte per Zi
              {
                const size_t bytes = A->get_cfirst().get_near_int();
                const int fd = get_fd(*B.get());

                char small_buffer[SMALL_BUF];
                char * buffer = small_buffer;
                char * del = 0;
                if (bytes > sizeof(small_buffer))
                   buffer = del = new char[bytes];

                errno = 0;
                const ssize_t len = recv(fd, buffer, bytes, 0);
                if (len < 0)   goto out_errno;

                Value_P Z(len, LOC);
                loop(z, len)   Z->next_ravel_Int(buffer[z] & 0xFF);
                delete [] del;
                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 38:   // send(Bh, Ai, ⍴Ai, 0) 1 byte per Zi
              {
                errno = 0;
                const size_t bytes = A->element_count();
                const int fd = get_fd(*B.get());

                char small_buffer[SMALL_BUF];
                char * buffer = small_buffer;
                char * del = 0;
                if (bytes > sizeof(small_buffer))
                   buffer = del = new char[bytes];

                loop(z, bytes)   buffer[z] = A->get_cravel(z).get_near_int();

                const ssize_t len = send(fd, buffer, bytes, 0);
                if (len < 0)   goto out_errno;
                delete [] del;

                return Token(TOK_APL_VALUE1, IntScalar(len, LOC));
              }

         case 39:   // send(Bh, Ac, ⍴Ac, 0) Unicode Ac Output UTF-8
              {
                UCS_string text(*A.get());
                UTF8_string utf(text);
                const int fd = get_fd(*B.get());
                errno = 0;
                const ssize_t len = send(fd, utf.c_str(), utf.size(), 0); 
                if (len < 0)   goto out_errno;

                return Token(TOK_APL_VALUE1, IntScalar(len, LOC));
              }

         case 41:   // read(Bh, Zi, Ai) 1 byte per Zi
              {
                const size_t bytes = A->get_cfirst().get_near_int();
                const int fd = get_fd(*B.get());

                char small_buffer[SMALL_BUF];
                char * buffer = small_buffer;
                char * del = 0;
                if (bytes > sizeof(small_buffer))
                   buffer = del = new char[bytes];

                errno = 0;
                const ssize_t len = read(fd, buffer, bytes);
                if (len < 0)   goto out_errno;

                Value_P Z(len, LOC);
                loop(z, len)   Z->next_ravel_Int(buffer[z] & 0xFF);
                delete [] del;
                Z->check_value(LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 42:   // write(Bh, Ai, ⍴Ai) 1 byte per Zi
              {
                const size_t bytes = A->element_count();
                const int fd = get_fd(*B.get());

                char small_buffer[SMALL_BUF];
                char * buffer = small_buffer;
                char * del = 0;
                if (bytes > sizeof(small_buffer))
                   buffer = del = new char[bytes];

                loop(z, bytes)   buffer[z] = A->get_cravel(z).get_byte_value();

                errno = 0;
                const ssize_t len = write(fd, buffer, bytes);
                delete [] del;
                if (len < 0)   goto out_errno;

                return Token(TOK_APL_VALUE1, IntScalar(len, LOC));
              }

         case 43:   // write(Bh, Ac, ⍴Ac) Unicode Ac Output UTF-8
              {
                const int fd = get_fd(*B.get());
                UCS_string text(*A.get());
                UTF8_string utf(text);
                errno = 0;
                const ssize_t len = write(fd, utf.c_str(), utf.size()); 
                if (len < 0)   goto out_errno;
                return Token(TOK_APL_VALUE1, IntScalar(len, LOC));
              }

         case 46:   // getsockopt(Bh, A_level, A_optname, Zi)
              {
                const APL_Integer level = A->get_cfirst().get_int_value();
                const APL_Integer optname = A->get_cravel(1).get_int_value();
                const int fd = get_fd(*B.get());
                int optval = 0;
                socklen_t olen = sizeof(optval);
                errno = 0;
                const int ret = getsockopt(fd, level, optname, &optval, &olen);
                if (ret < 0)   goto out_errno;
                return Token(TOK_APL_VALUE1, IntScalar(optval, LOC));
              }

         case 47:   // setsockopt(Bh, A_level, A_optname, A_optval)
              {
                const APL_Integer level = A->get_cfirst().get_int_value();
                const APL_Integer optname = A->get_cravel(1).get_int_value();
                const int optval =  A->get_cravel(2).get_int_value();
                const int fd = get_fd(*B.get());
                errno = 0;
                setsockopt(fd, level, optname, &optval, sizeof(optval));
                goto out_errno;
              }

         case 48:   // fscanf(Bh, A_format)
              {
                FILE * file = get_FILE(*B.get());
                const UCS_string format(*A.get());
                File_or_String fos(file);
                errno = 0;
                return do_scanf(fos, format, function_number);
              }

         case 55:   // sscanf(Bh, A_format)
              {
                const UCS_string format(*A.get());
                const UCS_string data(*B.get());
                File_or_String fos(&data);
                errno = 0;
                return do_scanf(fos, format, function_number);
              }

         case 56:   // write nested lines As to file Bs
              {
                // 1. before opening the output file, check that A is valid.
                //
                errno = 0;
                size_t items_written = 0;
                const UCS_string path_ucs(*B.get());
                const UTF8_string path(path_ucs);
                if (A->get_rank() > 1)   RANK_ERROR;
                const ShapeItem line_count = A->element_count();
                loop(a, line_count)
                    {
                      const Cell & cA = A->get_cravel(a);
                      if (!cA.is_pointer_cell())
                         {
                            MORE_ERROR() <<
"The left argument of A ⎕FIO[56] B is not a nested vector of strings. The\n"
"first non-nested element is A[⎕IO+" << a << "].";
                            DOMAIN_ERROR;
                         }

                      Value_P Ai = cA.get_pointer_value();
                      if (!Ai->is_char_vector())
                         {
                            MORE_ERROR() <<
"The left argument of A ⎕FIO[56] B is not a nested vector of strings. The\n"
"first non-string element is A[⎕IO+" << a << "].";
                            DOMAIN_ERROR;
                         }
                    }

                // 2. at this point As is OK. Write it to file Bs.
                FILE * f = fopen(path.c_str(), "w");
                if (f == 0)   goto out_errno;

                loop(a, line_count)
                    {
                      const Cell & cA = A->get_cravel(a);
                      const Value & Ai = *cA.get_pointer_value();
                      UCS_string line_ucs(Ai);
                      UTF8_string line_utf(line_ucs);
                      line_utf += '\n';
                      const size_t len = line_utf.size();
                      size_t written = fwrite(line_utf.c_str(), 1, len, f);
                      if (len != written)   goto out_errno;
                      items_written += len;
                    }
                fclose(f);
                return Token(TOK_APL_VALUE1, IntScalar(items_written, LOC));
              }

         case 58:   // snprintf(Af, B...)
              {
                const UCS_string A_format(*A);
                UCS_string UZ;
                do_snprintf(UZ, A_format, B.get(), 0, "A ⎕FIO.snprintf B");
                Value_P Z(UZ, LOC);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 59:   // fcntl(Bh, Ai...)
              {
                const int fd = get_fd(*B.get());
                errno = 0;
                int result = -1;
                const Cell * cA = &A->get_cfirst();
                switch(A->element_count())
                   {
                      case 1: result = fcntl(fd, cA[0].get_int_value());
                              break;

                      case 2: result = fcntl(fd, cA[0].get_int_value(),
                                                 cA[1].get_int_value());
                              break;

                      default: LENGTH_ERROR;
                   }

                if (result == -1 && errno)   goto out_errno;
                return Token(TOK_APL_VALUE1, IntScalar(result, LOC));
              }

         case 60:   // random value(s)
              {
                errno = 0;
                if (!A->is_scalar())   RANK_ERROR;
                if (!B->is_scalar())   RANK_ERROR;
                const APL_Integer mode = A->get_cfirst().get_int_value();
                const APL_Integer len  = B->get_cfirst().get_int_value();
                if (len < 1)    LENGTH_ERROR;
                if (len > 32)   LENGTH_ERROR;
                Value_P Z = get_random(mode, len);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 202:   // set monadic parallel threshold
         case 203:   // set dyadicadic parallel threshold
              {
                const APL_Integer threshold = A->get_cfirst().get_int_value();
                cFunction_P fun = 0;
                if (B->element_count() == 3)   // dyadic operator
                   {
                     const Unicode oper = B->get_cravel(1).get_char_value();
                     if (oper != UNI_FULLSTOP)   DOMAIN_ERROR;
                     fun = &Bif_OPER2_INNER::fun;
                   }
                else
                   {
                     const Unicode prim = B->get_cfirst().get_char_value();
                     const Token tok = Tokenizer::tokenize_function(prim);
                     if (!tok.is_function())   DOMAIN_ERROR;
                     fun = tok.get_function();
                   }
                if (fun == 0)   DOMAIN_ERROR;
                APL_Integer old_threshold;
                if (function_number == 202)
                   {
                     old_threshold = fun->get_monadic_threshold();
                     const_cast<Function *>(fun)
                                            ->set_monadic_threshold(threshold);
                   }
                else
                   {
                     old_threshold = fun->get_dyadic_threshold();
                     const_cast<Function *>(fun)
                                            ->set_dyadic_threshold(threshold);
                   }

                 return Token(TOK_APL_VALUE1, IntScalar(old_threshold, LOC));
              }

        default: break;
      }

   MORE_ERROR() << "bad function number " << function_number <<
                   " in Quad_FIO::eval_AXB(). The monadic ⎕FIO[X] B is valid.";

   CERR << "eval_AXB() function number: " << function_number << endl;
   VALENCE_ERROR;

out_errno:
   MORE_ERROR() << "A ⎕FIO[" << function_number
              << "] B failed: " << strerror(errno);
   return Token(TOK_APL_VALUE1, IntScalar(-errno, LOC));
}
//----------------------------------------------------------------------------

