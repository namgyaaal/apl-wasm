/*
    This file is part of GNU APL, a free implementation of the
    ISO/IEC Standard 13751, "Programming Language APL, Extended"

    Copyright © 2018-2020  Dr. Jürgen Sauermann

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

#include "Quad_PLOT.hh"
#include "Plot_data.hh"
#include "Plot_line_properties.hh"
#include "Plot_window_properties.hh"

//----------------------------------------------------------------------------
Plot_window_properties::Plot_window_properties(const Plot_data * data,
                                               int verbosity)
   : line_count(data->get_row_count()),
     plot_data(*data),
# define gdef(_ty,  na,  val, _descr) na(val),
# include "Quad_PLOT.def"
     user_pw_pos(false),
     user_caption(false),
     window_width(pa_border_L  + origin_X + pa_width  + pa_border_R),
     window_height(pa_border_T + origin_Y + pa_height + pa_border_B),
     min_X(data->get_min_X()),
     max_X(data->get_max_X()),
     min_Y(data->get_min_Y()),
     max_Y(data->get_max_Y()),
     min_Z(data->get_min_Z()),
     max_Z(data->get_max_Z()),
     scale_X(0),
     scale_Y(0),
     scale_Z(0),
     tile_X(0),
     tile_Y(0),
     tile_Z(0),
     verbosity(0),
     rangeX_type(NO_RANGE),
     rangeY_type(NO_RANGE),
     rangeZ_type(NO_RANGE),
     show_legend(legend_lX != 0)
{
   if (verbosity & SHOW_DRAW)
      CERR << setw(20) << "min_X: " << min_X << endl
           << setw(20) << "max_X: " << max_X << endl
           << setw(20) << "min_Y: " << min_Y << endl
           << setw(20) << "max_Y: " << max_Y << endl
           << setw(20) << "min_Z: " << min_Z << endl
           << setw(20) << "max_Z: " << max_Z << endl;

   line_properties = new Plot_line_properties *[line_count + 1];
   loop (l, line_count)   line_properties[l] = new Plot_line_properties(l);
   line_properties[line_count] = 0;

   // set the origin_X and origin_Y properties back to 0 so that they
   // affect only surface plots.
   if (!data->surface)
      {
         set_origin_X(0);
         set_origin_Y(0);
      }

   update(verbosity);
}
//----------------------------------------------------------------------------
Plot_window_properties::~Plot_window_properties()

{
   Log(LOG_Quad_PLOT)
      CERR << "~Plot_window_properties(): deleting plot_data" << endl;

   delete &plot_data;
}
//----------------------------------------------------------------------------
void
Plot_window_properties::set_window_size(Pixel_X width, Pixel_Y height)
{
   // pa_width and pa_height are the size of the plot area (without borders).
   //
   pa_width  = width  - pa_border_L - origin_X - pa_border_R;
   pa_height = height - pa_border_T - origin_Y - pa_border_B;
   update(0);
}
//----------------------------------------------------------------------------

bool
Plot_window_properties::update(int verbosity)
{
   // this function is called for 2 reasons:
   //
   // 1. when constructor Plot_window_properties::Plot_window_properties()
   //    is called in plot_main(), i.e. when the the plot window is created·
   //    for the first time, and
   //
   // 2. when the user resizes the plot window.
   //
   // In both cases the caller has most likely changed pa_width or pa_height,
   // and this function recomputes all variables that depend on them.
   //
   // Some variables, e.g. min/max_X/Y/Z have not changed in case 2, but
   // it is simpler to recompute them rather than usng different functions
   // for the two cases.
   //
   window_width  = pa_border_L + origin_X + pa_width  + pa_border_R;
   window_height = pa_border_T + origin_Y + pa_height + pa_border_B;

   min_X = plot_data.get_min_X();
   max_X = plot_data.get_max_X();
   min_Y = plot_data.get_min_Y();
   max_Y = plot_data.get_max_Y();
   min_Z = plot_data.get_min_Z();
   max_Z = plot_data.get_max_Z();

   // at this point the plot variables were computed from only the plot data
   // (aka. auto-scaled). Now override them with user preferences from the plot
   // attributes A (of A ⎕PLOT B). The XXX_valid() functions tell us whether
   // or not an attribute was provided in A.
   //
#define overwrite_if_provided(src, dest)  if (src ## _valid()) dest = src;

   overwrite_if_provided(rangeX_min, min_X);
   overwrite_if_provided(rangeX_max, max_X);
   overwrite_if_provided(rangeY_min, min_Y);
   overwrite_if_provided(rangeY_max, max_Y);
   overwrite_if_provided(rangeZ_min, min_Z);
   overwrite_if_provided(rangeZ_max, max_Z);

   if (min_X >= max_X)
      {
        MORE_ERROR() << "empty X range [" << min_X << ":" << max_X
                     << "] in data B of A ⎕PLOT B";
        return true;   // error
      }

   if (min_Y >= max_Y)
      {
        MORE_ERROR() << "empty Y range [" << min_Y << ":" << max_Y
                     << "] in data B of A ⎕PLOT B";
        return true;   // error
      }

   if (min_Z >= max_Z)
      {
        MORE_ERROR() << "empty Z range [" << min_Z << ":" << max_Z
                     << "] in data B of A ⎕PLOT B";
        return true;   // error
      }

double delta_X = max_X - min_X;
double delta_Y = max_Y - min_Y;
double delta_Z = max_Z - min_Z;
const double orig_len = sqrt(origin_X*origin_X + origin_Y*origin_Y);

   // first approximation for value → pixel factor
   //
   scale_X = pa_width  / delta_X;   // pixels per X-value
   scale_Y = pa_height / delta_Y;   // pixels per Y-value
   scale_Z = orig_len ? orig_len / delta_Z : 1.0;    // pixels per Z-value

   if (verbosity & SHOW_DRAW)
      CERR << setw(20) << "delta_X (1): " << delta_X << " (Xmax-Xmin)" << endl
           << setw(20) << "delta_Y (1): " << delta_Y << " (Ymax-Ymin)" << endl
           << setw(20) << "delta_Z (1): " << delta_Z << " (Zmax-Zmin)" << endl
           << setw(20) << "scale_X (1): " << scale_X << " pixels/X" << endl
           << setw(20) << "scale_Y (1): " << scale_Y << " pixels/Y" << endl
           << setw(20) << "scale_Z (1): " << scale_Z << " pixels/Z" << endl;

const double tile_X_raw = gridX_pixels / scale_X;
const double tile_Y_raw = gridY_pixels / scale_Y;
const double tile_Z_raw = gridZ_pixels / scale_Z;

  if (verbosity & SHOW_DRAW)
      CERR << setw(20) << "tile_X_raw: " << tile_X_raw
                                         << "(∆X between grid lines)" << endl
           << setw(20) << "tile_Y_raw: " << tile_Y_raw
                                         << "(∆Y between grid lines)" << endl
           << setw(20) << "tile_Z_raw: " << tile_Z_raw
                                         << "(∆Z between grid lines)" << endl;

   tile_X = round_up_1_2_5(tile_X_raw);   // assume neither time nor var. grid
   if (gridX_variable)  // maybe there is a time item in format_X
      {
        for (const char * p = format_X.c_str(); (p = strchr(p, '%')); ++p)
            {
              if (strchr("SIHhDdMmQqYy", p[1]))   // see strchr() in Plot_gtk.cc
                 {
                   tile_X = round_up_seconds(tile_X_raw);
                 }
            }
      }

   tile_Y = round_up_1_2_5(tile_Y_raw);
   tile_Z = round_up_1_2_5(tile_Z_raw);

const int min_Xi = floor(min_X / tile_X);
const int min_Yi = floor(min_Y / tile_Y);
const int min_Zi = floor(min_Z / tile_Z);
const int max_Xi = ceil(max_X / tile_X);
const int max_Yi = ceil(max_Y / tile_Y);
const int max_Zi = ceil(max_Z / tile_Z);
   gridX_last = 0.5 + max_Xi - min_Xi;
   gridY_last = 0.5 + max_Yi - min_Yi;
   gridZ_last = 0.5 + max_Zi - min_Zi;
   min_X = tile_X * floor(min_X / tile_X);
   min_Y = tile_Y * floor(min_Y / tile_Y);
   min_Z = tile_Z * floor(min_Z / tile_Z);
   max_X = tile_X * ceil(max_X  / tile_X);
   max_Y = tile_Y * ceil(max_Y  / tile_Y);
   max_Z = tile_Z * ceil(max_Z  / tile_Z);

   delta_X = max_X - min_X;
   delta_Y = max_Y - min_Y;
   delta_Z = max_Z - min_Z;

   // final value → pixel factor
   //
   scale_X = pa_width  / delta_X;
   scale_Y = pa_height / delta_Y;
   scale_Z = orig_len ? orig_len  / delta_Z : 1.0;

   if (verbosity & SHOW_DRAW)
      CERR << setw(20) << "min_X (2): " << min_X << endl
           << setw(20) << "max_X (2): " << max_X << endl
           << setw(20) << "min_Y (2): " << min_Y << endl
           << setw(20) << "max_Y (2): " << max_Y << endl
           << setw(20) << "min_Z (2): " << min_Z << endl
           << setw(20) << "max_Z (2): " << max_Z << endl
           << setw(20) << "delta_X (2): " << delta_X << " (Xmax-Xmin)" << endl
           << setw(20) << "delta_Y (2): " << delta_Y << " (Ymax-Ymin)" << endl
           << setw(20) << "delta_Z (2): " << delta_Z << " (Zmax-Zmin)" << endl
           << setw(20) << "scale_X (2): " << scale_X << " pixels/X" << endl
           << setw(20) << "scale_Y (2): " << scale_Y << " pixels/Y" << endl
           << setw(20) << "scale_Z (2): " << scale_Z << " pixels/Z" << endl
           << setw(20) << "tile_X  (2): " << tile_X
                                          << "(∆X between grid lines)" << endl
           << setw(20) << "tile_Y  (2): " << tile_Y
                                          << "(∆Y between grid lines)" << endl
           << setw(20) << "tile_Z  (2): " << tile_Z
                                          << "(∆Z between grid lines)" << endl;

   return false;   // OK
}
//----------------------------------------------------------------------------
int
Plot_window_properties::print(ostream & out) const
{
# define gdef(ty,  na,  _val, _descr) \
   out << setw(20) << #na ":  " << Plot_data::ty ## _to_str(na) << endl;
# include "Quad_PLOT.def"

   loop(g, gradient.size())
       {
         char cc[30];
         SPRINTF(cc, "color_level-%u: ", gradient[g].level);
         out << setw(20) << cc
             << Plot_data::Color_to_str(gradient[g].rgb) << endl;
       }

   for (Plot_line_properties ** lp = line_properties; *lp; ++lp)
       {
         out << endl;
         (*lp)->print(out);
       }

   return 0;
}
//----------------------------------------------------------------------------
const char *
Plot_window_properties::set_attribute(const char * att_and_val)
{
   /*
       called with leading whitespaces in att_and_val removed.

       att_and_val is a string specifying one of:

       1. a window attribute, e.g.:   origin_X: 100                 , or
       2. a line attribute,   e.g.:   point_color-1:  #000000       , or
       3. a color gradient,   e.g.:   color_level-50: #00FF00

       The value (right of ':') can be

       an integer value, like:     100
       a color, like:              #000000 or #000
       a string, like:             "Window Title"

      return 0 on success or else an appropriate error string.
    */

   // remember if certain properties were set by the users
   //
   if      (!strncmp(att_and_val, "pw_pos_", 7))   user_pw_pos  = true;
   else if (!strncmp(att_and_val, "caption", 7))   user_caption = true;

   // find ':' which separates the attribute name and the attribute value.
   //
const char * colon = strchr(att_and_val, ':');
   if (colon == 0)   return "Expecting 'attribute: value' but no : found";

   // skip leading whitespace before the attribute value
   //
const char * value = colon + 1;
   while (*value && (*value <= ' '))   ++value;

   // check for optional line number resp. color level
   //
int propnum = 0;
const char * minus = strchr(att_and_val, '-');
int line_number = -1;

   if (minus && minus < colon)   // line attribute or color gradient
      {
        // it is not unlikely that the user wants to use the same attributes
        // for different plots that may differ in the number of lines. We
        // therefore silently ignore such over-specified attribute rather
        // than returning an error string (which then raises a DOMAIN error).
        //
        if (!strncmp(att_and_val, "color_level-", 12))
           {
             const int level = strtoll(minus + 1, 0, 10);
             if (level < 0)     return "color gradient level < 0%";
             if (level > 100)   return "color gradient level > 100%";
             const char * error = 0;
              const Color color = Plot_data::Color_from_str(value, error);
              if (error)   return error;

              level_color grad = { level, color };

              // insert grad into gradient, but keeping gradient sorted
              // by increasing level
              //
              loop(g, gradient.size())
                  {
                    if (gradient[g].level == grad.level)
                       return "duplicate gradient level";

                    if (gradient[g].level > grad.level)
                       {
                         gradient.insert(gradient.begin() + g, grad);
                         return 0;
                       }
                  }
              gradient.push_back(grad);
              return 0;   // OK
           }

        // figure the line number in the attribute name, for example in
        // line_color-2. No line number means all lines.
        //
        const int line = strtoll(minus + 1, 0, 10) - Workspace::get_IO();
        if (line < 0)             return "line number ≤ ⎕IO";
        if (line >= line_count)   return 0;   // silently ignore
        line_number = line;
      }

# define ldef(ty,  na,  val, _descr)                                       \
         ++propnum;                                                        \
         if (Plot_line_properties::is_line_property(att_and_val, #na))     \
            { const char * error = 0;                                      \
              if (line_number == -1)                                       \
                 set_all_ ## na(Plot_data::ty ## _from_str(value, error),  \
                                propnum); \
              else                                                         \
                {  line_properties[line_number]->set_ ## na(Plot_data::ty  \
                                             ## _from_str(value, error));  \
                   properties_set.push_back(line_number << 8 | propnum); } \
              return error;                                                \
            }

# define gdef(ty,  na,  _val, _descr) \
         if (!strncmp(#na, att_and_val, colon - att_and_val))         \
            { const char * error = 0;                                 \
              set_ ## na(Plot_data::ty ## _from_str(value, error));   \
              return error;                                           \
            }
# include "Quad_PLOT.def"

   return "Bad or unknown window attribute.";
}
//----------------------------------------------------------------------------
bool
Plot_window_properties::can_be_set(uint16_t line, uint16_t propnum)
{
   loop(ps, properties_set.size())
       {
         if (properties_set[ps] == (line << 8 | propnum))   return false;
       }

   return true;
}
//----------------------------------------------------------------------------
const char *
Plot_window_properties::set_attribute(const UCS_string & att, const Cell & val)
{
   // replace trailing _NNN in att with -NNN.
   //
UCS_string att1 = att;
bool trailing_NNN = false;
   rev_loop(a, att1.size())
       {
         const Unicode uni = att1[a];
         if (!Avec::is_digit(uni))
            {
               trailing_NNN = uni == UNI_UNDERSCORE;
               if (trailing_NNN)   att1[a] = UNI_MINUS;
               break;
            }
       }

   if (att1.size() > 40)   // most are < 20
      {
        return "attribute too long";
      }

char att_and_value[100];
UTF8_string attname_utf(att1);
const char * attname_cp = attname_utf.c_str();
   if (val.is_integer_cell())   // integer attribute value
      {
        const int ival = val.get_int_value();
        SPRINTF(att_and_value, "%s: %d", attname_cp, ival);
        return set_attribute(att_and_value);
      }
   else if (val.is_pointer_cell())   // string value
      {
        UCS_string val_ucs = val.get_pointer_value()->get_UCS_ravel();
        UTF8_string attval_utf(val_ucs);
        const char * attval_cp = attval_utf.c_str();
        SPRINTF(att_and_value, "%s: %s", attname_cp, attval_cp);
        return set_attribute(att_and_value);
      }
   else
      {
        return "Bad attribute value type";
      }

}
//----------------------------------------------------------------------------
double
Plot_window_properties::round_up_1_2_5(double val)
{
int expo = 0;
   while (val >= 10)    { val /= 10;   ++expo; }
   while (val <  1)     { val *= 10;   --expo; }

   // at this point 1 ≤ val < 10.
   //
   if (expo > -18 && expo < 18)   // use integer arithmetic for 10^expo
      {
        const int abs_expo = expo < 0 ? -expo : expo;
        uint64_t expo_val = 1;
        loop(a, abs_expo)   expo_val *= 10;

        if (val <= 2.0)   return  expo < 0 ? 2.0/expo_val : 2.0*expo_val;
        if (val <= 5.0)   return  expo < 0 ? 5.0/expo_val : 5.0*expo_val;
        return  expo < 0 ? 10.0/expo_val : 10.0*expo_val;
      }
   else                           // use float arithmetic for 10^expo
      {
        const double expo_val = pow(10, expo);
        if (val <= 2.0)   return  expo < 0 ? 2.0/expo_val : 2.0*expo_val;
        if (val <= 5.0)   return  expo < 0 ? 5.0/expo_val : 5.0*expo_val;
        return  expo < 0 ? 10.0/expo_val : 10.0*expo_val;
      }
}
//----------------------------------------------------------------------------
int
Plot_window_properties::round_up_24(int val)
{
   if (val <  5)   return val;   //  1..4 evenly divide 24
   if (val <  6)   return 6;     //  6 evenly divides 24
   if (val <  8)   return 8;     //  8 evenly divides 24
   if (val < 12)   return 12;    // 12 evenly divides 24
   return 24;
}
//----------------------------------------------------------------------------
int
Plot_window_properties::round_up_60(int val)
{
   if (val <  7)   return val;   //  1..6 evenly divide 60
   if (val < 10)   return 10;    // 10 evenly divides 60
   if (val < 12)   return 12;    // 12 evenly divides 60
   if (val < 15)   return 15;    // 15 evenly divides 60
   if (val < 20)   return 20;    // 20 evenly divides 60
   if (val < 30)   return 30;    // 30 evenly divides 60
   return 60;
}
//----------------------------------------------------------------------------
double
Plot_window_properties::round_up_seconds(double val)
{
const time_t seconds(val);
const tm * tile = localtime(&seconds);
   if (tile == 0)   return round_up_1_2_5(val);   // localtime() failed

   // call broken-down 'tm tile' odd if more than one of its primary units
   // (year/month/day/hour/minute/second) is set
   //
int nz = 0;
   if (tile->tm_year)   ++nz;
   if (tile->tm_mon)    ++nz;
   if (tile->tm_mday)   ++nz;
   if (tile->tm_hour)   ++nz;
   if (tile->tm_min)    ++nz;
   if (tile->tm_sec)    ++nz;
const int odd = nz > 1 ? 1 : 0;

tm rounded;   // tile rounded up
   memset(&rounded, 0, sizeof(rounded));

   /* NOTE (man mktime):

       The mktime() function modifies the fields of the tm structure  as  fol‐
       lows:  tm_wday  and  tm_yday are set to values determined from the con‐
       tents of the other fields; if structure members are outside their valid
       interval,  they will be normalized (so that, for example, 40 October is
       changed into 9 November); tm_isdst is set (regardless  of  its  initial

      We therefore need not care for overflows caused by + inc.
    */

   if (int year = tile->tm_year)         // round up to full year
      {
        rounded.tm_year = round_up_1_2_5(year + odd);
      }
   else if (int month = tile->tm_mon)     // round up to full month
      {
        rounded.tm_mon  = month + odd;
      }
   else if (int mday = tile->tm_mday)     // round up to full day
      {
        rounded.tm_mday = mday + odd;
      }
   else if (int hour = tile->tm_hour)     // round up to full hour
      {
        rounded.tm_hour = round_up_24(hour + odd);
      }
   else if (int minute = tile->tm_min)   // round up to full minute
      {
        rounded.tm_min = round_up_60(minute + odd);
      }

   return mktime(&rounded);   // back to seconds
}
//----------------------------------------------------------------------------
uint32_t
Plot_window_properties::get_color(double alpha) const
{
   Assert(alpha >= 0.0);
   Assert(alpha <= 1.0);

   if (gradient.size() == 0)   // no gradient specified: use gray-scale
      return uint32_t(255*alpha) << 16
           | uint32_t(255*alpha) << 8
           | uint32_t(255*alpha);

   if (gradient.size() == 1)   // only one gradient specified: use it
      return gradient[0].rgb;

   loop(g, gradient.size())
       {
         const double level_g = 0.01*gradient[g].level;
         if (alpha == level_g)   return gradient[g].rgb;
         if (alpha < level_g)   // level_g is the first level above alpha
            {
               if (g == 0)   return gradient[0].rgb;   // use first gradient

               // interpolate
               //
               const double l0 = 0.01*gradient[g-1].level;   // previous level
               const double beta1 = (alpha - l0) / (level_g - l0);
               Assert(beta1 >= 0.0);
               Assert(beta1 <= 1.0);
               const double beta0 = 1.0 - beta1;
               const uint32_t rgb0 = gradient[g-1].rgb;
               const uint32_t rgb1 = gradient[g].rgb;
               const uint32_t red = beta1*(rgb1 >> 16 & 0xFF)
                                  + beta0*(rgb0 >> 16 & 0xFF);
               const uint32_t grn = beta1*(rgb1 >>  8 & 0xFF)
                                  + beta0*(rgb0 >>  8 & 0xFF);
               const uint32_t blu = beta1*(rgb1       & 0xFF)
                                  + beta0*(rgb0       & 0xFF);
               return red << 16 | grn << 8 | blu;
            }
       }

   // alpha is above last gradient: use last gradient
   //
   return gradient.back().rgb;
}
//----------------------------------------------------------------------------
Pixel_XY
Plot_window_properties::valXYZ2pixelXY(double X, double Y, double Z) const
{
const double phi = atan2(get_origin_Y(), get_origin_X());
const Pixel_Z pz = valZ2pixel(Z - get_min_Z());
const Pixel_X px = valX2pixel(X - get_min_X()) + get_origin_X() - pz*cos(phi);
const Pixel_X py = valY2pixel(Y - get_min_Y())                  + pz*sin(phi);

   return Pixel_XY(px, py);
}
//----------------------------------------------------------------------------

