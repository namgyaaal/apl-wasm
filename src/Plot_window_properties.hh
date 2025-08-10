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

#ifndef __PLOT_WINDOW_PROPERTIES_HH_DEFINED__
#define __PLOT_WINDOW_PROPERTIES_HH_DEFINED__
#include "Plot_data.hh"

class Plot_line_properties;

//============================================================================
/// properties of the entire plot window (from A in A ⎕PLOT B)
class Plot_window_properties
{
public:
   /// the kind of range
   enum Plot_Range_type
      {
        NO_RANGE           = 0,   ///< no plot range provided
        PLOT_RANGE_MIN     = 1,   ///< min value provided
        PLOT_RANGE_MAX     = 2,   ///< max value provided
        PLOT_RANGE_MIN_MAX = PLOT_RANGE_MIN | PLOT_RANGE_MAX ///< min. and max.
      };

   /// constructor
   Plot_window_properties(const Plot_data * data, int verbosity);

   /// destructor
   ~Plot_window_properties();

   /// update derived properties after changing primary ones, return \b true
   /// on error
   bool update(int verbosity);

   /// handle window resize event
   void set_window_size(Pixel_X width, Pixel_Y height);

   /// return true iff the user has dictated a window position
   bool get_user_pw_pos() const   { return user_pw_pos; }

   /// return true iff the user has dictated a window caption
   bool get_user_caption() const   { return user_caption; }

   // get_XXX() and set_XXX() functions for weindow properties
# define gdef(ty,  na,  _val, _descr)                                     \
  /** return the value of na **/                                         \
  ty get_ ## na() const   { return na; }                                 \
  /** set the value of na **/                                            \
  void set_ ## na(ty val)                                                \
     { na = val;                                                         \
       if (!strcmp(#na, "rangeX_min"))                                   \
          rangeX_type = Plot_Range_type(rangeX_type | PLOT_RANGE_MIN);   \
       else if (!strcmp(#na, "rangeX_max"))                              \
          rangeX_type = Plot_Range_type(rangeX_type | PLOT_RANGE_MAX);   \
       else if (!strcmp(#na, "rangeY_min"))                              \
          rangeY_type = Plot_Range_type(rangeY_type | PLOT_RANGE_MIN);   \
       else if (!strcmp(#na, "rangeY_max"))                              \
          rangeY_type = Plot_Range_type(rangeY_type | PLOT_RANGE_MAX);   \
       else if (!strcmp(#na, "rangeZ_min"))                              \
          rangeZ_type = Plot_Range_type(rangeZ_type | PLOT_RANGE_MIN);   \
       else if (!strcmp(#na, "rangeY_max"))                              \
          rangeZ_type = Plot_Range_type(rangeZ_type | PLOT_RANGE_MAX);   \
     }

# define ldef(ty,  na, _val, _descr)                              \
   /** set the  value of na in all lines**/                        \
   void set_all_ ## na(ty v, int propnum) { loop(ln, line_count)   \
       if (can_be_set(ln, propnum))   line_properties[ln]->set_ ## na(v); }

# include "Quad_PLOT.def"

   /// return true iff a rangeX_min property was specified
   bool rangeX_min_valid() const
      { return !!(rangeX_type & PLOT_RANGE_MIN); }

   /// return true iff a rangeX_max property was specified
   bool rangeX_max_valid() const
      { return !!(rangeX_type & PLOT_RANGE_MAX); }

   /// return true iff a rangeY_min property was specified
   bool rangeY_min_valid() const
      { return !!(rangeY_type & PLOT_RANGE_MIN); }

   /// return true iff a rangeY_max property was specified
   bool rangeY_max_valid() const
      { return !!(rangeY_type & PLOT_RANGE_MAX); }

   /// return true iff a rangeZ_min property was specified
   bool rangeZ_min_valid() const
      { return !!(rangeZ_type & PLOT_RANGE_MIN); }

   /// return true iff a rangeZ_max property was specified
   bool rangeZ_max_valid() const
      { return !!(rangeZ_type & PLOT_RANGE_MAX); }

   /// an array of plot line properties
   Plot_line_properties * const * const get_line_properties() const
      { return line_properties; }

   /// convert \n val_X to the X coordinate of the pixel in the plot area
   Pixel_X valX2pixel(double val_X) const
      { return pa_border_L + val_X * scale_X; }

   /// convert \n val_Y to the Y coordinate of the pixel in the plot area
   Pixel_Y valY2pixel(double val_Y) const
      { return pa_border_T + pa_height - val_Y * scale_Y; }

   /// convert \n val_Z to the relative Z vector (q. quadrant)
   Pixel_Z valZ2pixel(double val_Z) const
      { return  val_Z * scale_Z; }

   /// convert values \b val_X,  \b val_Y,  and \b val_Z, to the X and Y
   /// coordinate of the pixel below the Z=0 plane of the plot area
   Pixel_XY valXYZ2pixelXY(double val_X, double val_Y, double val_Z) const;

   /// return the pixel position of the point where the X, Y, and possibly
   /// Z-axis cross.
   Pixel_XY get_origin(bool surface) const
      {
        if (surface)   return valXYZ2pixelXY(get_min_X(),
                                             get_min_Y(),
                                             get_min_Z());
        return Pixel_XY(valX2pixel(0), valY2pixel(0));   // 2D
      }

   /// print the properties (for debugging purposes)
   int print(ostream & out) const;

   /// for e.g. att_and_val = "pa_width: 600" set pa_width to 600.
   /// Return error string on error.
   const char * set_attribute(const char * att_and_val);

   /// for e.g. A.pa_width ←  600" set pa_width to 600.
   /// Return error string on error.
   const char * set_attribute(const UCS_string & att, const Cell & val);

   /// return the width of the plot window
   Pixel_X  get_window_width() const    { return window_width; }

  /// return the height of the plot window
   Pixel_Y  get_window_height() const   { return window_height; }

   /// return the smallest X value
   double get_min_X() const           { return min_X; }

   /// return the largest X value
   double get_max_X() const           { return max_X; }

   /// return the smallest Y value
   double get_min_Y() const           { return min_Y; }

   /// return the largest Y value
   double get_max_Y() const           { return max_Y; }

   /// return the smallest Z value
   double get_min_Z() const           { return min_Z; }

   /// return the largest Z value
   double get_max_Z() const           { return max_Z; }

   /// return the X value → X pixels scaling factor
   double get_scale_X() const         { return scale_X; }

   /// return the Y value → Y pixels scaling factor
   double get_scale_Y() const         { return scale_Y; }

   /// return the Z value → Z pixels scaling factor
   double get_scale_Z() const        { return scale_Z; }

   /// return the X-value difference for one tile (= between two X grid lines)
   double get_value_per_tile_X() const          { return tile_X; }

   /// return the Y-value difference for one tile (= between two Y grid lines)
   double get_value_per_tile_Y() const          { return tile_Y; }

   /// return the Z-value difference for one tile (= between two Z grid lines)
   double get_value_per_tile_Z() const          { return tile_Z; }

   /// return the last index of the X grid
   Pixel_X get_gridX_last() const      { return gridX_last; }

   /// return the last index of the Y grid
   Pixel_Y get_gridY_last() const      { return gridY_last; }

   /// return the last index of the Y grid
   Pixel_Y get_gridZ_last() const      { return gridZ_last; }

   /// return the verbosity when plotting
   int get_verbosity() const       { return verbosity; }

   /// set the verbosity when plotting
   void set_verbosity(int verb)      { verbosity = verb; }

   /// return the date to be plotted
   const Plot_data & get_plot_data() const   { return plot_data; }

   /// return the 3D color gradient
   const vector<level_color> & get_gradient() const
      { return gradient; }

   /// for level 0.0 <= alpha <= 1.0: return the color for alpha according
   /// to \b gradient
   uint32_t get_color(double alpha) const;

   /// return the number of plot lines
   uint32_t get_line_count() const
      { return line_count; }

   bool get_show_legend() const
      { return show_legend; }

   /// move the legend rectangle by dx:dy
   void move_legend(Pixel_X dx, Pixel_Y dy)
      {
        legend_X += dx;
        legend_Y += dy;
      }

protected:
   /// the number of plot lines
   const int line_count;

   /// the date to be plotted
   const Plot_data & plot_data;

# define gdef(ty, na, _val, descr) /** descr **/ ty na;
# include "Quad_PLOT.def"

   /// whether the window position was dictated by the user
   bool user_pw_pos;

   /// whether the window caption was dictated by the user
   bool user_caption;

   /// the width of the plot window
   Pixel_X window_width;
  /// the height of the plot window
   Pixel_Y window_height;

   /// the smallest X value
   double min_X;

   /// the largest X value
   double max_X;

   /// the smallest Y value
   double min_Y;

   /// the largest Y value
   double max_Y;

   /// the smallest Z value
   double min_Z;

  /// the largest Z value
   double max_Z;

   /// the X value → X pixels scaling factor
   double scale_X;

   /// the Y value → Y pixels scaling factor
   double scale_Y;

   /// the Z value → Z pixels scaling factor
   double scale_Z;

   /// the number of pixels between X grid lines
   double tile_X;

   /// the number of pixels between Y grid lines
   double tile_Y;

   /// the number of pixels between Z grid lines
   double tile_Z;

  /// the last index of the X grid
   int gridX_last;

   /// the last index of the Y grid
   int gridY_last;

   /// the last index of the Y grid
   int gridZ_last;

   /// the verbosity when plotting
   int verbosity;

   /// the kind of X range
   Plot_Range_type rangeX_type;

   /// the kind of Y range
   Plot_Range_type rangeY_type;

   /// the kind of Z range
   Plot_Range_type rangeZ_type;

  /// array containing the data for the plot lines
   Plot_line_properties * * line_properties;

   /// some levels with colors (for surface plots)
   vector<level_color> gradient;

   /// round val up to the next higher 1/2/5×10^N
   static double round_up_1_2_5(double val);

   /// round val (< 24) up to the next higher divisor of 24
   static int round_up_24(int val);

   /// round val (<= 60) up to the next higher divisor of 60
   static int round_up_60(int val);

   /// round a time_t up or down to the next minute, hour, day, month, or year.
   /// positive diff round up, negative round down.
   static double round_up_seconds(double val);

   /// return true unless property \b propnum was already set in plot
   /// line \b line
   bool can_be_set(uint16_t line, uint16_t propnum);

   /// true if the plot window shall have a legend
   const bool show_legend;

   /// a list of properties that have been set alreay (to protect them from
   /// from being overwridden in set_all_XXX() functions. Every 16-bit item
   /// of \b properties_set is the line (upper 8 bits) and the property number
   /// (lower 8 bits).
   basic_string<uint16_t> properties_set;
};
//============================================================================

#endif // __PLOT_WINDOW_PROPERTIES_HH_DEFINED__

