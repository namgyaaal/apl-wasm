/*
    This file is part of GNU APL, a free implementation of the
    ISO/IEC Standard 13751, "Programming Language APL, Extended"

    Copyright © 2018-2019  Dr. Jürgen Sauermann

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

#ifndef __Quad_PNG_DEFINED__
#define __Quad_PNG_DEFINED__

#include <pthread.h>
#include <semaphore.h>

#include "QuadFunction.hh"

/// The class implementing ⎕PNG
class Quad_PNG : public QuadFunction
{
public:
   /// Constructor.
   Quad_PNG();

   static Quad_PNG  fun;          ///< Built-in function.

   /// a semaphore blocking until gtk_widget_show_all() was called
   static sem_t * PNG_window_sema;

   /// a semaphore waiting from pthread_create() until the thread has started.
   static sem_t * PNG_threads_sema;

   /// the type and compressed length of a PNG filter
   struct filter_info
      {
        const int y;             ///< scan line
        int filter_type;         ///< PNG filter type
        int compressed_length;   ///< compressed length
      };

protected:
   /// Destructor.
   ~Quad_PNG();

   /// valid type/depth combinations for PNG files (not yet used, though)
   enum Type_Depth
      {
        GRAY_1   = 0x0001,   ///< 1-bit grayscale
        GRAY_2   = 0x0002,   ///< 2-bit grayscale
        GRAY_4   = 0x0004,   ///< 4-bit grayscale
        GRAY_8   = 0x0008,   ///< 8-bit grayscale
        GRAY_16  = 0x0010,   ///< 16-bit grayscale
        RGB_8    = 0x0208,   ///< 8-bit R/G/B
        RGB_16   = 0x0210,   ///< 16-bit R/G/B
        PALE_1   = 0x0301,   ///< 1-bit palette
        PALE_2   = 0x0302,   ///< 2-bit palette
        PALE_4   = 0x0304,   ///< 4-bit palette
        PALE_8   = 0x0308,   ///< 8-bit palette
        GRAY_8A  = 0x0408,   ///< 8-bit grayscale with ⍺
        GRAY_16A = 0x0410,   ///< 16-bit grayscale with ⍺
        RGB_8A   = 0x0608,   ///< 8-bit R/G/B with ⍺
        RGB_16A  = 0x0610,   ///< 16-bit R/G/B with ⍺
      };

   /// overloaded Function::eval_AB()
   Token eval_AB(Value_P A, Value_P B) const;

   /// overloaded Function::eval_B()
   Token eval_B(Value_P B) const;

   /// control logging etc. of ⎕PNG
   Value_P window_control(APL_Integer B) const;

   /// read PNG file \b filename and return its pixels as 4×N×M matrix
   static Value_P read_PNG_file(const UTF8_string & filename);

   /// display an RGB matrix
   static APL_Integer display_PNG(Value & B);

   /// display an RGB matrix (interpreter code)
   static APL_Integer display_PNG_main(Value_P B);

   /// return true iff the combination of color_type and bit_depth is
   /// valid in the PNG standard AND supported by ⎕PNG.
   static bool valid_type_and_bits(int color_type, int bit_depth);

   /// write APL pixel matrix \b B PNG file \b filename
   static void write_PNG_file(const char * filename, int bit_depth,
                              const Value & B);
};

#endif // __Quad_PNG_DEFINED__
