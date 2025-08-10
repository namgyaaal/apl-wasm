/*
    This file is part of GNU APL, a free implementation of the
    ISO/IEC Standard 13751, "Programming Language APL, Extended"

    Copyright (C) 2015  Dr. Dirk Laurie

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


#ifndef __LIBAPL_H_DEFINED__
#define __LIBAPL_H_DEFINED__

#include <stdio.h>
#include <stdint.h>

/* NOTE: you may replace the following typedef for APL_Float by your own
   class. In that case, please provide a complete class declaration here,

   'complete' means that all functions used by GNU APL from external libraries
   (such as libc or libm) that have arguments and/or return results of type
   'double' values must either be:

   a.   declared here (and linked later), or (even better)
   b.   implemented here as (inline) functions.

   See file APL_Float_as_class.h for an example which contains a list of all
   functions needed.
 */
typedef double APL_Float;

enum C_CellType
{
   CCT_CHAR    = 0x02,
   CCT_POINTER = 0x04,
   CCT_INT     = 0x10,
   CCT_FLOAT   = 0x20,
   CCT_COMPLEX = 0x40,
   CCT_NUMERIC = CCT_INT | CCT_FLOAT | CCT_COMPLEX,
};

// errors codes
enum LIBAPL_error
{
#define err_def(c, _txt, major, minor)   LAE_ ## c = (major << 16 | minor),
#include "Error.def"
};

#ifdef __cplusplus   // libapl used from C++

class Value;
typedef Value * APL_value;

class Function;
typedef Function const * APL_function;
extern "C" {

#else                // libapl used from C

struct Value;
typedef struct Value * APL_value;
typedef enum LIBAPL_error LIBAPL_error;

struct Function;
typedef struct Function const * APL_function;

#endif               // C++ vs. C

/* Application Program Interface for GNU APL...
   See

   info libapl

   for details.
 */

/// LOC is a macro that expands to the source filename and line number as 
/// a C string literal. It should (but does not have to) be used as the
/// 'const char * loc' argument of the functions below. That simplifies
/// the troubleshooting of value-related problems.
///
/// Stringify x.
#define STR(x) #x
/// The current location in the source file.
#define LOC Loc(__FILE__, __LINE__)
/// The line l in file f.
#define Loc(f, l) f ":" STR(l)


/// Initialize libapl. Call this function first with argv[0] and log_startup
/// as needed
extern void init_libapl(const char * progname, int log_startup);

/// expand output LF to CR/LF, return current value
extern int expand_LF_to_CRLF(int on);

/// Pass `line` to the interpreter for immediate execution as APL code.
extern LIBAPL_error apl_exec(const char * line_utf8);

/// Pass `line` to the interpreter for immediate execution as APL code.
/// line_ucs is a 0-terminated string of unicode integers
extern LIBAPL_error apl_exec_ucs(const unsigned int * line_ucs);

/// Pass `command` to the command processor and return its output. The result
/// was allocated with strndup(), therefore the caller must free() it.
extern const char * apl_command(const char * command_utf8);

/** perform one step (line) of a REPL loop. Return the file sequence
    number > 0 from where the line was read, or 0 if all files in
    InputFile::files_todo were read. Called with:

    input_buffer:  a buffer allocated by the caller, or NULL if the
                   APL input is not of interest
    input_bufsize" size of the \b input_buffer
    output_buffer: a buffer allocated by the caller, or NULL if the
                    APL output is not of interest.
    output_bufsize" size of the \b output_buffer

   On return:

    input_buffer:  APL input passed to APL. Always 0-terminated, but possibly
                   truncted to fit the input_bufsize
    input_bufsize: size of the \b input_buffer (before being truncated)
    output_buffer: APL output from APL. Always 0-terminated, but possibly
                   truncted to fit the output_bufsize
    output_bufsize: size of the \b input_buffer (before being truncated)
    error:         an error (typically IN/OUT_BUFFER_OVERFLOW).

 **/
extern long repl(char * input_buffer, int * input_bufsize,
                 char * output_buffer, int * output_bufsize,
                 LIBAPL_error * error);

/// call repl() until all scripts were processed.
#define SYNC_APL_SCRIPTS   { do ; while(repl(0, 0, 0, 0, 0)); }

/// Pass `command` to the command processor and return its output. line_ucs
/// is a 0-terminated string of unicode integers (and so is the result)
/// caller shall free() the returned unsigned int *.
extern const unsigned int * apl_command_ucs(const unsigned int * command_ucs);

/// define an APL function with header function_lines[0] and body
/// function_lines[1...N]. function_lines must be 0-terminated, i.e.
/// function_lines[N+1] == NULL. fix_function() is a front-end for
/// fix_function_NL() which is ofter more convenient to use.
extern LIBAPL_error fix_function(const char ** function_lines_utf8);

/// define an APL function with \b function_lines_utf8 being the header and
/// function lines, separated by UNI_LF (= 0x0A).
extern LIBAPL_error fix_function_NL(const char * function_lines_utf8);

/// print \b string_ucs to \b out (for debugging purposes)
extern void print_ucs(FILE * out, const unsigned int * string_ucs);

/** a function called with the final value of an APL statement.
    int \b committed says if the value was committed (aka. assigned to a
    variable).

    The int returned by the callback tells if the result shall be printed (0)
    or not (non-0).

    The APL_value result MUST NOT be released with release_value() below.
 **/
typedef int (*result_callback)(const APL_value result, int committed);

extern result_callback res_callback;

/******************************************************************************
   1. APL value constructor functions. The APL_value returned must be released
      with release_value() below at some point in time (unless it is 0)
 */

/// the current value of a variable, 0 if the variable has no value
extern APL_value get_var_value(const char * var_name_utf8, const char * loc);  

/// A new integer scalar.
extern APL_value int_scalar(int64_t val, const char * loc);

/// A new floating point scalar.
extern APL_value double_scalar(APL_Float val, const char * loc);

/// A new complex scalar.
extern APL_value complex_scalar(APL_Float real, APL_Float imag,
                                const char * loc);

/// A new character scalar.
extern APL_value char_scalar(int unicode, const char * loc);

/// A new APL value with given rank and shape. All ravel elements are
/// initialized to integer 0.
extern APL_value apl_value(int rank, const int64_t * shape, const char * loc);

/// a new scalar with ravel 0
inline APL_value apl_scalar(const char * loc)
   { return apl_value(0, 0, loc); }

/// a new vector with a ravel of length len and ravel elements initialized to 0
inline APL_value apl_vector(int64_t len, const char * loc)
   { return apl_value(1, &len, loc); }

/// a new matrix. all ravel elements are initialized to integer 0
inline APL_value apl_matrix(int64_t rows, int64_t cols, const char * loc)
   { const int64_t sh[] = { rows, cols };   return apl_value(2, sh, loc); }

/// a new 3-dimensional value. all ravel elements are initialized to integer 0
inline APL_value apl_cube(int64_t blocks, int64_t rows, int64_t cols,
                          const char *loc)
   { const int64_t sh[] = { blocks, rows, cols };
     return apl_value(3, sh, loc); }

extern APL_value char_vector(const char * str, const char * loc);

/******************************************************************************
   2. APL value destructor function. All non-0 APL_values must be released
      at some point in time (even const ones). release_value(0) is not needed
      but accepted.
 */
extern void release_value(const APL_value val, const char * loc);

/******************************************************************************
   3. read access to APL values. All ravel indices count from ⎕IO←0.
 */

/// return ⍴⍴val
extern int get_rank(const APL_value val);

/// return (⍴val)[axis]
extern int64_t get_axis(const APL_value val, unsigned int axis);

/// return ×/⍴val
extern uint64_t get_element_count(const APL_value val);

/// return the type of (,val)[idx]
extern int get_type(const APL_value val, uint64_t idx);

/// return non-0 if val[idx] is a character
inline int is_char(const APL_value val, uint64_t idx)
   { return get_type(val, idx) == CCT_CHAR; }

/// return non-0 if val[idx] is a character for every valid idx
extern int is_string(APL_value val);

/// return non-0 if val[idx] is an integer
inline int is_int(const APL_value val, uint64_t idx)
   { return get_type(val, idx) == CCT_INT; }

/// return non-0 if val[idx] is a double
inline int is_double(const APL_value val, uint64_t idx)
   { return get_type(val, idx) == CCT_FLOAT; }

/// return non-0 if val[idx] is integer, real, or complex. The get_real() and
/// get_imag() functions may be called for all numeric ravel items

inline int is_complex(const APL_value val, uint64_t idx)
   { return get_type(val, idx) == CCT_COMPLEX; }

/// return non-0 if val[idx] is a complex
inline int is_numeric(const APL_value val, uint64_t idx)
   { return get_type(val, idx) & CCT_NUMERIC; }

/// return non-0 if val[idx] is a (nested) value
inline int is_value(const APL_value val, uint64_t idx)
   { return get_type(val, idx) == CCT_POINTER; }

/// return the character val[idx] (after having checked is_char())
extern int get_char(const APL_value val, uint64_t idx);

/// return the integer val[idx] (after having checked is_int())
extern int64_t get_int(const APL_value val, uint64_t idx);

/// return the double val[idx] (after having checked is_double())
extern APL_Float get_real(const APL_value val, uint64_t idx);

/// return the complex val[idx] (after having checked is_complex())
extern APL_Float get_imag(const APL_value val, uint64_t idx);

/// return the (nested) value val[idx] (after having checked is_value()).
/// The APL_value returned must be released with release_value() later on.
///
extern APL_value get_value(const APL_value val, uint64_t idx);

/******************************************************************************
   4. write access to APL values. All ravel indices count from ⎕IO←0.
 */
/// var_name←shape⍴0
extern APL_value assign_var(const unsigned int * var_name,
                            int rank,  uint64_t * shape);

/// val[idx]←unicode
extern void set_char(int unicode, APL_value val, uint64_t idx);

/// val[idx]←new_int
extern void set_int(int64_t new_double, APL_value val, uint64_t idx);

/// val[idx]←new_double
extern void set_double(APL_Float new_real, APL_value val, uint64_t idx);

/// val[idx]←new_real J new_imag
extern void set_complex(APL_Float new_real, APL_Float new_imag,
                        APL_value val, uint64_t idx);

/// val[idx]←new_value
extern void set_value(APL_value new_value, APL_value val, uint64_t idx);


/******************************************************************************
   5. other
 */

/// var_name_utf8 ← new_value. If new_value is 0 then no assignment is made but
/// var_name_utf8 is checked. Return non-0 on error.
extern int set_var_value(const char * var_name_utf8, const APL_value new_value,
                  const char * loc);

/// print value
extern void print_value(const APL_value value, FILE * out);

/// print value into a string
extern char * print_value_to_string(const APL_value value);

/// UTF8 string to one Unicode. If len is non-0 then the number of characters
/// used in utf is returned in * length.
extern int UTF8_to_Unicode(const char * utf, int * length);

/// One Unicode to UTF8. Provide at least 7 bytes for dest. dest will be
/// 0-teminated
extern void Unicode_to_UTF8(int unicode, char * dest, int * length);

/// a callback function when APL requests input. Return 0 for EOF. APL will not
/// touch the return value (and you must not touch the prompt). USE WITH CARE!
typedef const char * get_line_from_user_cb(
   int /* Immediate execution = 0, ⍞ = 1, ⎕ = 2, ⎕INP = 3, ∇ = 4 */ mode,
   const char * /* don't free() ! */ prompt);

/// install new callback (old one will be returned)
extern get_line_from_user_cb *
 install_get_line_from_user_cb(get_line_from_user_cb * new_callback);

/// return the number of owners of value val. An owner count of 1 means that
/// the next \b release_value(val) will delete the value val. If the APL
/// command )CHECK reports "stale values" then they might be caused by
/// missing calls of \b release_value(); in this case get_owner_count() can
/// be used to trouble-shoot the cause for stale values.
extern int get_owner_count(APL_value val);

/// return APL primitive or user defined function or operator \b name
/// and maybe its left and right function arguments L and R (if non-0)
extern APL_function
get_function_ucs(const unsigned int * name, APL_function * L, APL_function * R);

/// evaluate niladic function f
extern APL_value eval__fun(APL_function fun);

/// dyadic function fun with arguments A and B
extern APL_value eval__A_fun_B(APL_value A, APL_function fun, APL_value B);

/// monadic operator oper with function L arguments A and B
extern APL_value eval__A_L_oper_B(APL_value A, APL_function L, APL_function fun,
                                  APL_value B);

/// dyadic function fun with axis X and arguments A and B
extern APL_value eval__A_fun_X_B(APL_value A, APL_function fun, APL_value X,
                                 APL_value B);

/// dyadic operator oper with functions L and R and arguments A and B
extern APL_value eval__A_L_oper_R_B(APL_value A, APL_function L,
                                    APL_function fun, APL_function R,
                                    APL_value B);

/// monadic operator oper with functions L, axis X, and arguments A and B
extern APL_value eval__A_L_oper_X_B(APL_value A, APL_function L,
                                    APL_function fun, APL_value X, APL_value B);

/// dyadic operator oper with functions L and R, axis X, and arguments A and B
extern APL_value eval__A_L_oper_R_X_B(APL_value A, APL_function L,
                                      APL_function fun, APL_function R,
                                      APL_value X, APL_value B);

/// monadic function fun with argument B
extern APL_value eval__fun_B(APL_function fun, APL_value B);

/// monadic operator oper with function L argument B
extern APL_value eval__L_oper_B(APL_function L, APL_function fun, APL_value B);

/// monadic function fun with axis X and argument B
extern APL_value eval__fun_X_B(APL_function fun, APL_value X, APL_value B);

/// dyadic operator oper with functions L and R and argument B
extern APL_value eval__L_oper_R_B(APL_function L, APL_function fun,
                                  APL_function R, APL_value B);

/// monadic operator oper with function L, axis X, and argument B
extern APL_value eval__L_oper_X_B(APL_function L, APL_function fun,
                                  APL_value X, APL_value B);

/// dyadic operator oper with functions L and R, axis X, and argument B
extern APL_value eval__L_oper_R_X_B(APL_function L, APL_function fun,
                                    APL_function R, APL_value X, APL_value B);

/// lib APL is, by default, started in safe_mode (which disables some
/// potentially dangerous functions and commands. disable_safe_mode() undoes
/// that.
extern void disable_safe_mode();

#ifdef __cplusplus
}

/// print value
#include <ostream>
extern std::ostream & print_value(const APL_value value, std::ostream & out);

#endif

#endif // __LIBAPL_H_DEFINED__
