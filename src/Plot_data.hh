/*
    This file is part of GNU APL, a free implementation of the
    ISO/IEC Standard 13751, "Programming Language APL, Extended"

    Copyright © 2018-2022  Dr. Jürgen Sauermann

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

#ifndef __PLOT_DATA_HH_DEFINED__
#define __PLOT_DATA_HH_DEFINED__

#include <stdint.h>
#include <string>

#include "Common.hh"

typedef int16_t Pixel_X;
typedef int16_t Pixel_Y;
typedef int16_t Pixel_Z;   // offset in the Z direction
typedef uint32_t Color;

typedef std::string String;

//============================================================================

/// a window coordinate in pixels
struct Pixel_XY
{
   /// constructor
   Pixel_XY(Pixel_X px, Pixel_Y py)
   : x(px),
     y(py)
   {}

   /// the square of the distance between \b this and other
   const double distance2(const Pixel_XY other) const
      { const double dx = x - other.x;
        const double dy = y - other.y;
        return dx*dx + dy*dy;
      }


   /// the distance between \b this and other
   const double distance(const Pixel_XY other) const
      { return sqrt(distance2(other)); }

   Pixel_X x;   ///< horizontal pixels
   Pixel_Y y;   ///< vertical pixels
};
//============================================================================
/// data for one plot line, aka. data row. The plot line consists of N
/// data points, each with coordinates X, Y, and Z.
class Plot_data_row
{
public:
   /** constructor. Always called from Quad_PLOT::setup_data() which allocates
       it with std::new(void *, size_t). For efficiency, all data points in all
       rows are put into the same double * and our ~Plot_data_row() destructor
       is responsible for deleting it (when row 0 is destructed).
    **/
   Plot_data_row(const double * pX, const double * pY, const double * pZ,
                 uint32_t idx, uint32_t len)
   : X(pX), Y(pY), Z(pZ), row_num(idx), N(len)
   {
     min_X = max_X = get_X(0);
     min_Y = max_Y = get_Y(0);
     min_Z = max_Z = get_Z(0);

     for (size_t n = 1; n < N; ++n)
         {
             const double Xn = get_X(n);
            if (min_X > Xn)   min_X = Xn;
            if (max_X < Xn)   max_X = Xn;

            const double Yn = get_Y(n);
            if (min_Y > Yn)   min_Y = Yn;
            if (max_Y < Yn)   max_Y = Yn;

            const double Zn = get_Z(n);
            if (min_Z > Zn)   min_Z = Zn;
            if (max_Z < Zn)   max_Z = Zn;
         }
   }

   /// destructor
   ~Plot_data_row()
   {
     // all rows belong to the same double *, so we only delete row 0
     //
     if (row_num == 0)   delete[] X;
   }

   /// return the number of data points
   uint32_t get_N() const
      { return N; }

   /// return the n-th X coordinate
   double get_X(uint32_t n) const
      { return X ? X[n] : n + 1; }

   /// return the n-th Y coordinate
   double get_Y(uint32_t n) const
      { return Y[n]; }

   /// return the n-th Z coordinate
   double get_Z(uint32_t n) const
      { return Z ? Z[n] : row_num + 1; }

   /// return the smallest value in X
   double get_min_X() const
      { return min_X; }

   /// return the largest value in X
   double get_max_X() const
      { return max_X; }

   /// return the smallest value in Y
   double get_min_Y() const
      { return min_Y; }

   /// return the largest value in Y
   double get_max_Y() const
      { return max_Y; }

   /// return the smallest value in Z
   double get_min_Z() const
      { return min_Z; }

   /// return the largest value in Y
   double get_max_Z() const
      { return max_Z; }

protected:
   /// the X coordinates (optional, 0 if none)
   const double * X;

   /// the Y coordinates
   const double * Y;

   /// the Z coordinates (optional, 0 if none)
   const double * Z;

   /// the row number
   const uint32_t row_num;

   /// the row length
   const uint32_t N;

   /// the smallest value in X
   double min_X;

   /// the largest value in X
   double max_X;

   /// the smallest value in Y
   double min_Y;

   /// the largest value in Y
   double max_Y;

   /// the smallest value in Z
   double min_Z;

   /// the largest value in Z
   double max_Z;
};
//============================================================================
/// data for all plot lines

struct level_color
{
   int      level;   ///< level 0-100
   uint32_t rgb;     ///< color as RGB integer
};

/// the data to be plotted
class Plot_data
{
public:
   /// constructor
   Plot_data(uint32_t rows)
   : surface(false),
     row_count(rows),
     idx(0)
     {
        data_rows = new const Plot_data_row *[row_count];
      }

   /// destructor
   ~Plot_data()
   {
     loop(r, idx)   delete data_rows[r];
     delete data_rows;
   }

   /// return the number of plot lines
   uint32_t get_row_count() const
      { return row_count; }

   /// return the X and Y values of the data matrix row/col
   void get_XY(double & X, double & Y, uint32_t row, uint32_t col) const
       {
          Assert(row < idx);
          X = data_rows[row]->get_X(col);
          Y = data_rows[row]->get_Y(col);
        }

   /// return the smallest X value in the data matrix
   double get_min_X() const
      { return min_X; }

   /// return the largest X value in the data matrix
   double get_max_X() const
      { return max_X; }

   /// return the smallest Y value in the data matrix
   double get_min_Y() const
      { return min_Y; }

   /// return the largest Y value in the data matrix
   double get_max_Y() const
      { return max_Y; }

   /// return the smallest Z value in the data matrix
   double get_min_Z() const
      { return min_Z; }

   /// return the largest Z value in the data matrix
   double get_max_Z() const
      { return max_Z; }

   /// return the X coordinate at row and col
   double get_X(uint32_t row, uint32_t col) const
      { return data_rows[row]->get_X(col); }

   /// return the Y coordinate at row and col
   double get_Y(uint32_t row, uint32_t col) const
      { return data_rows[row]->get_Y(col); }

   /// return the Z coordinate at row and col
   double get_Z(uint32_t row, uint32_t col) const
      { return data_rows[row]->get_Z(col); }

   /// return true for surface plots
   bool is_surface_plot() const       { return surface; }

   /// add a row to the data matrix
   void add_row(const Plot_data_row * row)
      {
        Assert(idx < row_count);
        data_rows[idx++] = row;
        if (idx == 1)   // first row
           {
             min_X = row->get_min_X();
             max_X = row->get_max_X();
             min_Y = row->get_min_Y();
             max_Y = row->get_max_Y();
             min_Z = row->get_min_Y();
             max_Z = row->get_max_Y();
           }
        else
           {
             if (min_X > row->get_min_X())   min_X = row->get_min_X();
             if (max_X < row->get_max_X())   max_X = row->get_max_X();
             if (min_Y > row->get_min_Y())   min_Y = row->get_min_Y();
             if (max_Y < row->get_max_Y())   max_Y = row->get_max_Y();
             if (min_Z > row->get_min_Z())   min_Z = row->get_min_Z();
             if (max_Z < row->get_max_Z())   max_Z = row->get_max_Z();
           }
      }

   /// convert integer to a string
   static const char * uint32_t_to_str(uint32_t val)
      {
        static char ret[40];
        snprintf(ret, sizeof(ret), "%u", val);
        ret[sizeof(ret) - 1] = 0;
        return ret;
      }

   /// val as string with unit 'pixel'
   static const char * Pixel_X_to_str(Pixel_X val)
      {
        static char ret[40];
        snprintf(ret, sizeof(ret), "%u pixel", val);
        ret[sizeof(ret) - 1] = 0;
        return ret;
      }

   /// val as string with unit 'pixel'
   static const char * Pixel_Y_to_str(Pixel_Y val)
      {
        static char ret[40];
        snprintf(ret, sizeof(ret), "%u pixel", val);
        ret[sizeof(ret) - 1] = 0;
        return ret;
      }

   /// convert a double to a string
   static const char * double_to_str(double val)
      {
        static char ret[40];
        snprintf(ret, sizeof(ret), "%.1lf", val);
        ret[sizeof(ret) - 1] = 0;
        return ret;
      }

   /// convert a Color to a string
   static const char * Color_to_str(Color val)
      {
        static char ret[40];
        snprintf(ret, sizeof(ret), "#%2.2X%2.2X%2.2X",
                 val >> 16 & 0xFF, val >>  8 & 0xFF, val       & 0xFF);
         ret[sizeof(ret) - 1] = 0;
         return ret;
      }

   /// convert a String to a string
   static String String_to_str(String val)
      {
        return val;
      }

   /// return the idx'th row
   const Plot_data_row & operator[](ShapeItem idx) const
      { Assert(idx < row_count);   return *data_rows[idx]; }

   /// convert a string to a Pixel
   static Pixel_X Pixel_X_from_str(const char * str, const char * & error)
      { error = 0;   return strtoll(str, 0, 10); }

   /// convert a string to a Pixel
   static Pixel_Y Pixel_Y_from_str(const char * str, const char * & error)
      { error = 0;   return strtoll(str, 0, 10); }

   /// convert a string to a double
   static double double_from_str(const char * str, const char * & error)
      { error = 0;   return strtod(str, 0); }

   /// convert a string like "#RGB" or "#RRGGB" to a color
   static Color Color_from_str(const char * str, const char * & error);

  /// convert a string to a uint32_t
   static uint32_t uint32_t_from_str(const char * str, const char * & error)
      { error = 0;   return strtoll(str, 0, 10); }

   /// convert a string to a String
   static String String_from_str(const char * str, const char * & error)
      { error = 0;   return str; }

   /// true for surface plots
   bool surface;

protected:
   /// the rows of the data matrix (0-terminated)
   const Plot_data_row ** data_rows;

   /// the number of rows in the data matrix
   const uint32_t row_count;

   /// the current number of columns in the data matrix
   uint32_t idx;

   /// the smallest value in X
   double min_X;

   /// the largest value in X
   double max_X;

   /// the smallest value in Y
   double min_Y;

   /// the largest value in Y
   double max_Y;

   /// the smallest value in Z
   double min_Z;

   /// the largest value in Z
   double max_Z;
};
//============================================================================

#endif // __PLOT_DATA_HH_DEFINED__

