
/*
    This file is part of GNU APL, a free implementation of the
    ISO/IEC Standard 13751, "Programming Language APL, Extended"

    Copyright © 2018-2024  Dr. Jürgen Sauermann

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

#include "Common.hh"
#if HAVE_LOCALE_H
#include <locale.h>
#endif

#if apl_GTK3 && apl_X11

#include <X11/Xlib.h>
#include <gtk/gtk.h>

# include "Plot_data.hh"
# include "Plot_line_properties.hh"
# include "Plot_window_properties.hh"

# include "ComplexCell.hh"
# include "FloatCell.hh"
# include "Quad_PLOT.hh"
# include "Workspace.hh"

/// the font to be used for texts
const char * FONT_NAME = "sans-serif";

/// the size of the font to be used for texts
enum {  FONT_SIZE = 10 };

// ===========================================================================
/** a structure that aggregates:

     plot data and plot attributes (Plot_window_properties) and
     GTK widgets (window, drawing_area).
 **/
class GTK_context : public Quad_PLOT::PLOT_context
{
public:
   /// constructor
   GTK_context(Plot_window_properties & pwp, Quad_PLOT::Handle handle)
   : PLOT_context(handle),
     w_props(pwp),
     window(0),
     drawing_area(0),
     surface(0),
     configured(false),
     legend_drag(false),
     legend_drag_X(0),
     legend_drag_Y(0),
     legend_X0(0),
     legend_X1(0),
     legend_Y0(0),
     legend_Y1(0)
   {}

   // destructor
   ~GTK_context()
      {
        delete &w_props;
      }

   /// overloaded PLOT_context::plot_stop()
   virtual void plot_stop()
      {
        gtk_window_close(GTK_WINDOW(window));
      }

   /// return the required width of the entire plot area
   int get_total_width() const
       {
         return w_props.get_pa_width()
              + w_props.get_pa_border_L()
              + w_props.get_origin_X()
              + w_props.get_pa_border_R();
       }

   /// return the required height of the entire plot area
   int get_total_height() const
       {
         return w_props.get_pa_height()
              + w_props.get_pa_border_T()
              + w_props.get_origin_Y()
              + w_props.get_pa_border_B();
       }

   /// if file name: write the plot window, including its borders. Must be
   /// called late (after the window manager has displayed it)
   void save_to_file_with_border();

   /// if file name: write the plot window, without its borders. May
   // be called early (in Draw), before the window is displayed,
   void save_to_file_no_border(cairo_surface_t * surface);

   /// the window properties (as choosen by the user)
   Plot_window_properties & w_props;

   /// the window of this GTK_context
   GtkWidget * window;

   /// the drawing area in the window of this GTK_context
   GtkWidget * drawing_area;

   /// the cairo surface on which the plat is drawn
   cairo_surface_t * surface;

   /// true after the first configure events
   bool configured;

   /// true if the legend is being dragged
   bool legend_drag;

   /// the starting point of a legend drag
   int legend_drag_X, legend_drag_Y;

   Pixel_X get_legend_X0(const char * loc) const   { return legend_X0; }
   Pixel_X get_legend_X1(const char * loc) const   { return legend_X1; }
   Pixel_Y get_legend_Y0(const char * loc) const   { return legend_Y0; }
   Pixel_Y get_legend_Y1(const char * loc) const   { return legend_Y1; }

   void set_legend_X0(Pixel_X x0, const char * loc)   { legend_X0 = x0; }
   void set_legend_X1(Pixel_X x1, const char * loc)   { legend_X1 = x1; }
   void set_legend_Y0(Pixel_Y y0, const char * loc)   { legend_Y0 = y0; }
   void set_legend_Y1(Pixel_Y y1, const char * loc)   { legend_Y1 = y1; }

protected:
   /// the left and right edges of the legend rectangle
   Pixel_X legend_X0, legend_X1;

   /// the top and bottom edges of the legend rectangle
   Pixel_Y legend_Y0, legend_Y1;
};
//----------------------------------------------------------------------------
/// same as standard cairo_set_RGB_source() but with Color instead of double
/// red, green, and blue values.
static inline void
cairo_set_RGB_source(cairo_t * cr, Color color)
{
   // cairo-RGB runs from 0.0 to 1.0
   //
   cairo_set_source_rgba(cr, (color >> 16 & 0xFF) / 255.0,
                             (color >>  8 & 0xFF) / 255.0,
                             (color       & 0xFF) / 255.0, 1.0);   // opaque
}
//----------------------------------------------------------------------------
/// draw a line from P0 to P1
static void
draw_line(cairo_t * cr, Color line_color, int line_style, int line_width,
          const Pixel_XY & P0, const Pixel_XY & P1)
{
double dashes_1[] = {  };              // ────────
double dashes_2[] = {  5.0 };          // ╴╴╴╴╴╴╴╴
double dashes_3[] = { 10.0, 5.0 };     // ─╴─╴─╴─╴

   enum
      {
        num_dashes_1 = sizeof(dashes_1) / sizeof(double),
        num_dashes_2 = sizeof(dashes_2) / sizeof(double),
        num_dashes_3 = sizeof(dashes_3) / sizeof(double),
      };

   cairo_set_RGB_source(cr, line_color);
   if      (line_style == 3)   cairo_set_dash(cr, dashes_3, num_dashes_3, 0);
   else if (line_style == 2)   cairo_set_dash(cr, dashes_2, num_dashes_2, 0);
   else                        cairo_set_dash(cr, dashes_1, num_dashes_1, 0);

   cairo_set_line_width(cr, 1.0*line_width);   // 1 pixel = 2 cairo
   cairo_move_to(cr, P0.x, P0.y);
   cairo_line_to(cr, P1.x, P1.y);
   cairo_stroke(cr);

   if (line_style != 1)   cairo_set_dash(cr, dashes_1, num_dashes_1, 0);
}
//----------------------------------------------------------------------------
/// draw a circle with diameter \b size around P0
static void
draw_circle(cairo_t * cr, Pixel_XY P0, Color color, int size)
{
   cairo_set_RGB_source(cr, color);
   cairo_arc(cr, P0.x, P0.y, 0.5*size, 0.0, 2*M_PI);
   cairo_fill(cr);
}
//----------------------------------------------------------------------------
/// draw a ▲ or ▼ with given \b size around P0
static void
draw_delta(cairo_t * cr, Pixel_XY P0, bool up, Color color, int size)
{
const double l = 0.51*size;       // ∆ center to top vertex
const double m = 0.866025404*l;   // ∆ center to base
const double s = 0.5*l;           // ∆ base middle to left/right vertex

   cairo_set_RGB_source(cr, color);

   if (up)   // ▲
      {
        cairo_move_to(cr, P0.x,     P0.y + l);   // top vertex
        cairo_line_to(cr, P0.x + m, P0.y - s);   // right vertex
        cairo_line_to(cr, P0.x - m, P0.y - s);   // left vertex
        cairo_close_path(cr);                    // back to top
      }
   else      // ▼
      {
        cairo_move_to(cr, P0.x,     P0.y - l);   // bottom vertex
        cairo_line_to(cr, P0.x + m, P0.y + s);   // right vertex
        cairo_line_to(cr, P0.x - m, P0.y + s);   // left vertex
        cairo_close_path(cr);                    // back to top
      }

   cairo_fill(cr);
}
//----------------------------------------------------------------------------
/// draw a ■ or ◆ with given \b size around P0
static void
draw_quad(cairo_t * cr, Pixel_XY P0, bool caro, Color color, int size)
{
   cairo_set_RGB_source(cr, color);

   if (caro)   // ◆
      {
        const double dlta = 0.5 * size;
        cairo_move_to(cr, P0.x,        P0.y + dlta);   // top vertex
        cairo_line_to(cr, P0.x + dlta, P0.y);          // left vertex
        cairo_line_to(cr, P0.x,        P0.y - dlta);   // top vertex
        cairo_line_to(cr, P0.x - dlta, P0.y);          // left vertex
        cairo_close_path(cr);                          // back to top
      }
   else        // ■
      {
        const double dlta = 0.35*size;
        cairo_rectangle(cr, P0.x - dlta,  P0.y - dlta, 2*dlta, 2*dlta);
      }

   cairo_fill(cr);
}
//----------------------------------------------------------------------------
/// draw a 🞤 or 🞫 with given \b size around P0
static void
draw_cross(cairo_t * cr, Pixel_XY P0, bool plus, Color color,
           double size, int size2)
{
   size *= 0.98;
   cairo_set_RGB_source(cr, color);

const double half = 0.5*size;
   if (size2 == 0)   size2 = 2;   // default line thickness
   cairo_set_line_width(cr, 1.0*size2);   // 1 pixel = 2 cairo
   if (plus)   // 🞤
      {
        cairo_move_to(cr, P0.x - half, P0.y);
        cairo_line_to(cr, P0.x + half, P0.y);
        cairo_move_to(cr, P0.x, P0.y + half);
        cairo_line_to(cr, P0.x, P0.y - half);
      }
   else        // 🞫
      {
        const double dlta = 0.707106781*half;
        cairo_move_to(cr, P0.x - dlta, P0.y + dlta);
        cairo_line_to(cr, P0.x + dlta, P0.y - dlta);
        cairo_move_to(cr, P0.x + dlta, P0.y + dlta);
        cairo_line_to(cr, P0.x - dlta, P0.y - dlta);
      }

   cairo_stroke(cr);
}
//----------------------------------------------------------------------------
/// draw a point with given \b point_style and \b size around P
static void
draw_point(cairo_t * cr, Pixel_XY P, int point_style,
           const Color outer_color, int outer_dia,
           const Color inner_color, int inner_dia)
{
const bool es = ! (point_style & 1);   // even style
   switch(point_style)
      {
        case 0:                                                   return;
        case 1: draw_circle(cr, P,     outer_color, outer_dia);   break;   // ●
        case 2:                                                            // ▲
        case 3: draw_delta( cr, P, es, outer_color, outer_dia);   break;   // ▼
        case 4:                                                            // ◆
        case 5: draw_quad(  cr, P, es, outer_color, outer_dia);   break;   // ■
        case 6:                                                            // 🞤
        case 7: draw_cross( cr, P, es, outer_color, outer_dia,
                                                    inner_dia);   return; // 🞫
        default: cerr << "⎕PLOT: Invalid point style: "
                      << point_style << endl;                     return;
      }

   // at this point, point_style is one of those that requires (though may not
   // yet honor) the inner_dia.
   //
   if (inner_dia)   switch(point_style)
      {
        case 1: draw_circle(cr, P,     inner_color, inner_dia);   return;   // ●
        case 2:                                                             // ▲
        case 3: draw_delta( cr, P, es, inner_color, inner_dia);   return;   // ▼
        case 4:                                                             // ◆
        case 5: draw_quad(  cr, P, es, inner_color, inner_dia);   return;   // ■
      }
}
//----------------------------------------------------------------------------
/// draw a red marker for debugging purposes (to show where pixel P0 is)
inline void
draw_marker(cairo_t * cr, Pixel_XY P0)
{
   draw_point(cr, P0, 1, 0xFF0000, 10, 0, 0);
}
//----------------------------------------------------------------------------
/// draw an arrow from O to A
static void
draw_arrow(cairo_t * cr, Pixel_XY O, Pixel_XY A, const Color color)
{
   // draw an arrow for an axis that starts at origin O and ends at
   // A (where the arrow starts).

   // 1. create a unit vector U of length 1 parallel to the axis O-A
   //
const int dx = A.x - O.x;
const int dy = A.y - O.y;
const double len = sqrt(dx*dx + dy*dy);
const double Ux = dx/len;
const double Uy = dy/len;

   // arrow dimensions (pixel)
   //
   enum { SHAFT = 20, WING  = 5, TIP = 15 };

   // draw the shaft
   //
const Pixel_XY S(A.x + SHAFT*Ux, A.y + SHAFT*Uy);
   cairo_move_to(cr, A.x, A.y);
   cairo_line_to(cr, S.x, S.y);
   cairo_stroke(cr);

const Pixel_XY W(S.x - WING*Ux, S.y - WING*Uy);
const Pixel_XY W1(W.x - WING*Uy, W.y + WING*Ux);
const Pixel_XY W2(W.x + WING*Uy, W.y - WING*Ux);
const Pixel_XY T(S.x + TIP*Ux, S.y + TIP*Uy);
   cairo_move_to(cr, S.x, S.y);
   cairo_line_to(cr, W1.x, W1.y);
   cairo_line_to(cr, T.x, T.y);
   cairo_line_to(cr, W2.x, W2.y);
   cairo_close_path(cr);
   cairo_fill(cr);
}
//----------------------------------------------------------------------------
/// format \b val according to \b format (with \b percent pointing to % in
/// format, e.g. %d or %f like in printf()).
const char *
format_user_tick(double val, int tidx, const char * format,
                 const char * percent)
{
static char cc[100] = "";   // should suffice

   if (percent[1] == 's')   // %s: inline tick texts separated by %
      {
        // static tick texts, e.g. '%sTick0%Tick1%...TickN'. 
        // They depend on tidx (which selects one of them)
        //
        const char * tick_text = percent + 2;   // e.g. Tick0
        for (int idx = tidx; idx > 0; --idx)   // skip texts < idx
            {
              const char * tick_end = strchr(tick_text + 1, '%');
              if (tick_end == 0)   // no more '%'
                 {
                   CERR << "⎕PLOT: too few tick texts in " << percent
                                << " (at tick " << tidx << ")";
                   SPRINTF(cc, "Tick-%d", tidx);
                   return cc;
                 }
              tick_text = tick_end + 1;
            }

        size_t tick_len = 0;
        if (const char * tick_end = strchr(tick_text, '%'))
           tick_len = tick_end - tick_text;
        else
           tick_len = strlen(tick_text);

        if (tick_len >= (sizeof(cc) - 1))
           {
             CERR << "⎕PLOT: tick text too long";
             SPRINTF(cc, "Tick-%d", tidx);
             return cc;
           }

        strncpy(cc, tick_text, tick_len);
        return cc;
      }
   else if (strchr("gvSIHhDdMmQqYy", percent[1]))   // some kind of date
      {
        // dynamic tick text (a subset of those in "man date")
        //
        const time_t when = time_t(val);
        const tm * tm = localtime(&when);
        if (tm == 0)   // error in localtime()
           {
             cerr << "bad value in formatted ⎕PLOT tick";
             return format;
           }
        const int year    = tm->tm_year + 1900;   // tm 0-100 → APL
        const int month   = tm->tm_mon  + 1;      // tm 0-11  → APL
        const int quarter = tm->tm_mon / 3;       // tm 0-11  → APL

        int cc_pos = 0;
        enum { cc_max = sizeof(cc) - 1 };
        while (const char chr = *format++)
           {
             if (chr != '%')
                {
                  if (cc_pos < cc_max)   cc[cc_pos++] = chr;
                  continue;   // next char in format
                }

             // chr is % (start of field).
             //
             char item[20];
             switch (*format++)
                {
                  case 'S': SPRINTF(item, "%2.2d", tm->tm_sec);    break;
                  case 'G': SPRINTF(item, "%d", tidx + 1);         break;
                  case 'g': SPRINTF(item, "%d", tidx);             break;
                  case 'v': SPRINTF(item, "%d", int(val));         break;
                  case 'I': SPRINTF(item, "%2.2d", tm->tm_min);    break;
                  case 'H': SPRINTF(item, "%2.2d", tm->tm_hour);   break;
                  case 'h': SPRINTF(item, "%d", tm->tm_hour);      break;
                  case 'D': SPRINTF(item, "%2.2d", tm->tm_mday);   break;
                  case 'd': SPRINTF(item, "%d", tm->tm_mday);      break;
                  case 'M': SPRINTF(item, "%2.2d", month);         break;
                  case 'm': SPRINTF(item, "%d", month);            break;
                  case 'Q': SPRINTF(item, "%d", quarter + 1);      break;
                  case 'q': SPRINTF(item, "%d", quarter);          break;
                  case 'Y': SPRINTF(item, "%4.4d", year);          break;
                  case 'y': SPRINTF(item, "%2.2d", year%100);      break;

                   default: break;   // invalid/unsupported; copy cc1
                }

             // copy item
             //
             item[sizeof(item) - 1] = 0;
             for (const char * i = item; *i; ++i)
                 if (cc_pos < cc_max)   cc[cc_pos++] = *i;
           }
        cc[cc_pos] = 0;   // 0-terminator fpr cc.
        return cc;
      }

bool round = false;
   while (*percent)
      {
        const char cc = *percent++;
        if (cc == 'd')  { round = true;   break; }   // %d
        if (cc == 'i')  { round = true;   break; }   // %i
        if (cc == 'u')  { round = true;   break; }   // %u
      }

   if (!round)         SPRINTF(cc, format, val)
   else if (val > 0)   SPRINTF(cc, format, int(rint(val) + 0.5))
   else                SPRINTF(cc, format, int(rint(val) - 0.5))

   return cc;
}
//----------------------------------------------------------------------------
/// format the text of tick \b idx (with value \b val) according to \b format.
const char *
format_tick(double val, double dV, int idx, const char * format)
{
   if (*format)   // user-defined format string
      {
        if (const char * percent = strchr(format, '%'))
           return format_user_tick(val, idx, format, percent);

        cerr << "⎕PLOT: no % in user-defined format string" << endl;
        return format;
      }

   if (val == 0)   return "0";

static char cc[40];   // should suffice

   /* Avoid duplicate axis strings when dV is small. For that we need so many
      fractional digits that dV makes a difference.

      if dV ≥ 1    then adding dV will increment the integer part of val and
                        no  fractional digits are needed.
      if dV ≥ 0.1  then one fractional digit is needed to make a difference
      if dV ≥ 0.01 then two fractional digit is needed
      ...

      dV is { 1, 2, 5 }×10ⁿ and threrfore it is better to compare with 0.9
      instead of 1.0 as to avoid rounding problems with 1.0 < 1.0.
    */
char fmt[10];
short digits = 0;
   while (dV < 0.9)   { ++digits;   dV *= 10.0; }
   SPRINTF(fmt, "%%.%uf", digits);   // e.g. %.2f
   SPRINTF(cc,  fmt, val);

   // skip leading 0 in 0.xxx
   if (cc[1] == '.' && cc[0] == '0')   return cc + 1;

   // skip leading 0 in -0.xxx
   if (cc[2] == '.' && cc[1] == '0' && cc[0] == '-')
      {
      cc[1] = '-';
       return cc + 1;
      }

   // skip leading 0 in -0.xxx
   return cc;
}
//----------------------------------------------------------------------------
/// return the size of single_line string \b text when printed with font \b
/// font_name of /// size \b font_size
void
cairo_string_size(double & lx, double & ly, cairo_t * cr, const char * text,
                  const char * font_name, double font_size)
{
  cairo_select_font_face(cr, font_name, CAIRO_FONT_SLANT_NORMAL,
                                        CAIRO_FONT_WEIGHT_NORMAL);

  cairo_set_font_size(cr, font_size);

cairo_text_extents_t extends;
   extends.width = 0;
   extends.height = 0;

  cairo_text_extents(cr, text, &extends);
  lx = extends.width;
  ly = extends.height;

  if (Quad_PLOT::get_verbosity() & SHOW_DRAW)
     {
        CERR << "    string '" << text << "' : "
             << int(strlen(text) - UTF8_string::bytes_chars(text))
             << " Unicode point(s), "
             << strlen(text) << " UTF-8 byte(s), "
             <<  lx << " pixels" << endl;
     }
}
//----------------------------------------------------------------------------
/// return the size len_x:len_y of the multi_line string \b lines when
/// printed with font \b font_name of size \b font_size
void
cairo_multiline_size(double & len_x, double & len_y, cairo_t * cr,
                     const char * lines, const char * font_name,
                     double font_size)
{
  len_x = 0;
  len_y = 0;
char line[strlen(lines) + 10];

   // split lines into single strings line and cumulate the sizes
   for (;;)
       {
         double lx = 0;
         double ly = 0;
         if (const char * nl = strchr(lines, '\n'))   // more lines coming
            {
              const size_t len = nl - lines;
              memcpy(line, lines, len);
              line[len] = 0;
              lines = nl + 1;

              cairo_string_size(lx, ly, cr, line, font_name, font_size);
              if (len_x < lx)   len_x = lx;   // horizontal: take maximum
              if (len_y)   len_y += 2;        // space between lines
              len_y += ly;                    // vertical: add height
            }
         else                      // final line
            {
              cairo_string_size(lx, ly, cr, lines, font_name, font_size);
              if (len_x < lx)   len_x = lx;   // horizontal: take maximum
              if (len_y)   len_y += 2;        // space between lines
              len_y += ly;                    // vertical: add height
              break;   // for (;;)
            }
       }
}
//----------------------------------------------------------------------------
/// draw single-line string \b text at position xy
void
draw_text(cairo_t * cr, const char * text, const Pixel_XY & xy)
{
  cairo_select_font_face(cr, FONT_NAME,
                             CAIRO_FONT_SLANT_NORMAL,
                             CAIRO_FONT_WEIGHT_NORMAL);
  cairo_set_font_size(cr, FONT_SIZE);

  cairo_move_to(cr, xy.x, xy.y);

  ///  cairo_show_text(cr, text);
  cairo_text_path(cr, text);
  cairo_fill(cr);
}
//----------------------------------------------------------------------------
/// draw multi-line string \b lines at position xy
void
draw_multiline(cairo_t * cr, const char * lines, Pixel_XY xy, double width)
{
const char * font_name = FONT_NAME;
const int font_size = FONT_SIZE;
  cairo_select_font_face(cr, font_name,
                             CAIRO_FONT_SLANT_NORMAL,
                             CAIRO_FONT_WEIGHT_NORMAL);

  cairo_set_font_size(cr, font_size);

char line[strlen(lines) + 10];

   for (;;)
       {
         if (const char * nl = strchr(lines, '\n'))   // more lines coming
            {
              const size_t len = nl - lines;
              memcpy(line, lines, len);
              line[len] = 0;
              lines = nl + 1;

              double line_w = 0, line_h = 0;   // size of line
              cairo_string_size(line_w, line_h, cr, line, font_name, font_size);
              const double indent = 0.5*(width - line_w);

              cairo_move_to(cr, xy.x + indent, xy.y);
              cairo_text_path(cr, line);
              xy.y += 2 + FONT_SIZE;
              cairo_fill(cr);
            }
         else   // final line
            {
              cairo_move_to(cr, xy.x, xy.y);
              cairo_text_path(cr, lines);
              cairo_fill(cr);
              break;
            }
       }
}
//----------------------------------------------------------------------------
double
longest_legend_string(cairo_t * cr, int line_count, 
                      Plot_line_properties const * const * l_props)
{
double ly = FONT_SIZE;
double longest_len = 0.0;
   loop(l, line_count)
       {
         double lx = 0.0;
         const String & line_name = l_props[l]->get_legend_name();
         cairo_string_size(lx, ly, cr, line_name.c_str(), FONT_NAME, FONT_SIZE);
         if (longest_len < lx)   longest_len = lx;
       }
   return longest_len;
}
//----------------------------------------------------------------------------
/// draw a legend for the different plot lines
void
draw_legend(cairo_t * cr, GTK_context & pctx, bool surface_plot)
{
const Plot_window_properties & w_props = pctx.w_props;
   if (!w_props.get_show_legend())   return;   // no legend

Plot_line_properties const * const * l_props = w_props.get_line_properties();

const int line_count = surface_plot ? 1 : w_props.get_line_count();

   // compute the length of the longest legend string
   //
  cairo_select_font_face(cr, FONT_NAME, CAIRO_FONT_SLANT_NORMAL,
                                        CAIRO_FONT_WEIGHT_NORMAL);
  cairo_set_font_size(cr, FONT_SIZE);

const double longest_len = longest_legend_string(cr, line_count, l_props);

const Color canvas_color = w_props.get_canvas_color();
const Color legend_color = w_props.get_legend_color();

  /*
                             ┌────────────────────────┐
   every legend looks like:  │  ───o───  legend_name  │──── y0
                             └────────────────────────┘
                                │  │  │  │
                               x0 x1 x2 xt
                                │  │  │  │
                                │  │  │  └──── start of legend text
                                │  │  └─────── end of legend line
                                │  └────────── point of legend line
                                └───────────── start of legend line
  */

const Pixel_XY origin = w_props.get_origin(surface_plot);
const Pixel_X x0 = origin.x + w_props.get_legend_X();
const Pixel_Y y0 = origin.y + w_props.get_legend_Y() - w_props.get_pa_height();

const Pixel_X x1 = x0 + 30;                              // point o in --o--
const Pixel_X x2 = x1 + 30;                              // end of     --o--
const Pixel_X xt = x2 + 10;                              // text after --o--
const Pixel_X xe = xt + longest_len;                     // end of legend_name

const Pixel_Y dy = w_props.get_legend_dY();

   // draw the legend background
   {
     // clear the background and a 10 px BORDER around the items. The BORDER
     // is between the legend reactangle and the line entries
     //
     const double ly2 = 0.5*FONT_SIZE;

     enum { BORDER = 10 };   // border around legend block
     const double X0 = x0                             - BORDER;
     const double X1 = xe                             + BORDER;
     const double Y0 = y0 - ly2                       - BORDER;
     const double Y1 = y0 + ly2 + dy*(line_count - 1) + BORDER;

     // Remember the lest legend corners in in pctx. These corners are only
     // used in Button_press_event() to quickly determine if the next mouse
     // button press is inside or outside the currently visibble legend
     // rectangle (drawn ny this function). Don't confuse
     // pctx.set_legend_[XY][01]() with w_props.get_legend_[XY]() which is
     // the initial legend position chosen by the user and which is 
     // updated in Motion_notify_event() as the legend is being dragged.
     //
     pctx.set_legend_X0(X0, LOC);
     pctx.set_legend_X1(X1, LOC);
     pctx.set_legend_Y0(Y0, LOC);
     pctx.set_legend_Y1(Y1, LOC);

     if (Quad_PLOT::get_verbosity() & SHOW_DRAW)
        CERR << "draw_legend(): X0=" << X0 << " Y0=" << Y0
             << " W=" << (X1 - X0) << " H=" << (Y1 - Y0)
             << " Background: 0:0 to " << pctx.get_total_width()
             << ":" << pctx.get_total_height() << endl;
     cairo_set_RGB_source(cr, legend_color);
     cairo_rectangle(cr, X0, Y0, X1 - X0, Y1 - Y0);

     cairo_fill_preserve(cr);              // fill the rectangle but keep it
     cairo_set_RGB_source(cr, 0x000000);   // black brim around the background
     cairo_set_line_width(cr, 2);
     cairo_stroke(cr);
   }
   for (int l = 0; l < line_count; ++l)
       {
         const Plot_line_properties & lp = *l_props[l];
         const Color line_color  = lp.get_line_color();
         const int line_style    = lp.get_line_style();
         const int line_width    = lp.get_line_width();
         const int point_style   = lp.get_point_style();
         const Color point_color = lp.get_point_color();
         const int point_size    = lp.get_point_size();
         const int point_size2   = lp.get_point_size2();

         const Pixel_Y y1 = y0 + l*dy;

         draw_line(cr, line_color, line_style, line_width,
                   Pixel_XY(x0, y1), Pixel_XY(x2, y1));
         draw_point(cr, Pixel_XY(x1, y1), point_style, point_color,
                    point_size, canvas_color, point_size2);
         draw_text(cr, lp.get_legend_name().c_str(), Pixel_XY(xt, y1 + 5));
       }
}
//----------------------------------------------------------------------------
/// swap pixels P0 with value Y0, and P1 with value Y1
static inline void
pv_swap(Pixel_XY & P0, double & Y0, Pixel_XY & P1, double & Y1)
{
const double   Y = Y0;   Y0 = Y1;   Y1 = Y;
const Pixel_XY P = P0;   P0 = P1;   P1 = P;
}
//----------------------------------------------------------------------------
/// draw a 3D triangle where one point P0 is at visual height H0 and the
/// other two points P1 and P2 are the same at visual height H12.
void
draw_triangle(cairo_t * cr, const GTK_context & pctx, int verbosity,
              Pixel_XY P0, double H0, Pixel_XY P1, Pixel_XY P2, double H12)
{
   // draw a triangle with P1 and P2 at the same level H12
   //
   Assert(H0 >= 0.0);    Assert(H0 <= 1.0);
   Assert(H12 >= 0.0);   Assert(H12 <= 1.0);

   if (verbosity & SHOW_DRAW)
      CERR <<   " ∆2: P0(" << P0.x << ":" << P0.y << ") @H0=" << H0
           << "     P1(" << P1.x << ":" << P1.y << ") @H12=" << H12
           << "     P2(" << P2.x << ":" << P2.y << ") @H12=" << H12 << endl;

   // every line is ~1 pixel, so the max y should suffice for steps
   //
int steps = P0.y;
   if (steps < P1.y)   steps = P1.y;
   if (steps < P2.y)   steps = P2.y;
const double dH = (H12 - H0) / steps;

   loop(s, steps + 1)
       {
         const double alpha = H0 + s*dH;
         const double beta  = (1.0 * s)/steps;   // line lenght
         const Color line_color = pctx.w_props.get_color(alpha);
         const Pixel_XY P0_P1(P0.x + beta * (P1.x - P0.x),
                              P0.y + beta * (P1.y - P0.y));
         const Pixel_XY P0_P2(P0.x + beta * (P2.x - P0.x),
                              P0.y + beta * (P2.y - P0.y));
         draw_line(cr, line_color, 1, 1, P0_P1, P0_P2);
       }
}
//----------------------------------------------------------------------------
/// draw a 3D triangle where the points P0, P1, and P2 are at visual
/// heights H0, H1, and H2 respectively. This is done by splitting the
/// triangle into two triangles that each have 2 points at the same height.
void
draw_triangle(cairo_t * cr, const GTK_context & pctx, int verbosity,
              Pixel_XY P0, double H0, Pixel_XY P1, double H1,
              Pixel_XY P2, double H2)
{
   // draw a triangle with P0, P1 and P2 at levels H0, H1, and H2

   Assert(H0 >= 0);   Assert(H0 <= 1.0);
   Assert(H1 >= 0);   Assert(H1 <= 1.0);
   Assert(H2 >= 0);   Assert(H2 <= 1.0);

   if (verbosity & SHOW_DRAW)
      CERR << "\n∆1: P0(" << P0.x << ":" << P0.y << ")@H=" << H0
           << "      P1(" << P1.x << ":" << P1.y << ")@H=" << H1
           << "      P2(" << P2.x << ":" << P2.y << ")@H=" << H2 << endl;

   if (H0 < H1)   pv_swap(P0, H0, P1, H1);   // then H0 >= H1
   if (H0 < H2)   pv_swap(P0, H0, P2, H2);   // then H0 >= H2
   if (H1 < H2)   pv_swap(P1, H1, P2, H2);   // then H1 >= H2

   // here H0 >= H1 >= H2
   //
   Assert(H0 >= H1);
   Assert(H1 >= H2);

const Plot_window_properties & w_props = pctx.w_props;
const vector<level_color> & color_steps = w_props.get_gradient();
   if (color_steps.size() == 0)
      {
        CERR << "*** no color_steps" << endl;
        return;
      }

   if (H0 == H1)   // P0 and P1 have the same height
      {
        draw_triangle(cr, pctx, verbosity, P2, H2, P0, P1, H0);
      }
   else if (H1 == H2)   // P1 and P2 have the same height
      {
        draw_triangle(cr, pctx, verbosity, P0, H0, P1, P2, H1);
      }
   else            // P2 lies below P1
      {
        const double alpha = (H0 - H1) / (H0 - H2);   // alpha → 1 as P1 → P2
        Assert(alpha >= 0.0);
        Assert(alpha <= 1.0);

        // compute the point P on P0-P2 that has the heigth H1 (of P1)
        const Pixel_XY P(P0.x + alpha*(P2.x - P0.x),
                         P0.y + alpha*(P2.y - P0.y));
        draw_triangle(cr, pctx, verbosity, P0, H0, P1, P, H1);
        draw_triangle(cr, pctx, verbosity, P2, H2, P1, P, H1);
      }
}
//----------------------------------------------------------------------------
/// draw the (vertical) X grid-lines of the plot (starting from the Y axis)
/// and proceeding with equal distances w_props.get_value_per_tile_X().
void
draw_X_grid(cairo_t * cr, const GTK_context & pctx, bool surface_plot)
{
const Plot_window_properties & w_props = pctx.w_props;
const int line_width = w_props.get_gridX_line_width();
const Color grid_color = w_props.get_gridX_color();

const Pixel_Y py0 = w_props.valY2pixel(0);
const double dv = w_props.get_max_Y() - w_props.get_min_Y();
const Pixel_Y py1 = w_props.valY2pixel(dv);
const int grid_style = w_props.get_gridX_style();

   for (int ix = 0; ix <= w_props.get_gridX_last(); ++ix)
       {
         const double dV = w_props.get_value_per_tile_X();
         const double v = w_props.get_min_X() + ix*dV;
         const int px0 = w_props.valX2pixel(v - w_props.get_min_X())
                       + w_props.get_origin_X();

         // draw the first and last grid line solid, the others with
         // the desired line style.
         //
         if (ix == 0 || ix == w_props.get_gridX_last())
            {
              draw_line(cr, grid_color, /* solid */ 1, line_width,
                        Pixel_XY(px0, py0 + 5), Pixel_XY(px0, py1));
            }
         else
            {
              draw_line(cr, grid_color, grid_style, line_width,
                        Pixel_XY(px0, py0 + 5), Pixel_XY(px0, py1));
            }

         string format = w_props.get_format_X();
         const char * cc = format_tick(v, dV, ix, format.c_str());
         double cc_width, cc_height;
         cairo_multiline_size(cc_width, cc_height, cr, cc,
                              FONT_NAME, FONT_SIZE);

         Pixel_XY cc_pos(px0 - 0.5*cc_width, py0 + cc_height + 8);
         if (surface_plot)
            {
              cc_pos.x -= w_props.get_origin_X();
              cc_pos.y += w_props.get_origin_Y();
            }
         draw_multiline(cr, cc, cc_pos, cc_width);
       }

   if (w_props.get_axisX_arrow())
      {
        const Pixel_XY origin = w_props.get_origin(surface_plot);
        const Pixel_X px = w_props.valX2pixel(dv) + w_props.get_origin_X();

        Pixel_XY P(px, origin.y);
        draw_arrow(cr, origin, P, grid_color);

        const string arrow_label = w_props.get_axisX_label();
        if (arrow_label.size())
           {
             draw_text(cr, arrow_label.c_str(), Pixel_XY(P.x + 40, P.y + 5));
           }
      }
}
//----------------------------------------------------------------------------
/// draw (vertical) X grid-lines where plot points exist.
void
draw_X_vargrid(cairo_t * cr, const GTK_context & pctx, bool surface_plot)
{
const Plot_window_properties & w_props = pctx.w_props;
const int line_width = w_props.get_gridX_line_width();
const Color grid_color = w_props.get_gridX_color();

const Pixel_Y py0 = w_props.valY2pixel(0);
const double dv = w_props.get_max_Y() - w_props.get_min_Y();
const Pixel_Y py1 = w_props.valY2pixel(dv);
const int grid_style = w_props.get_gridX_style();

   // draw the first and last grid line solid
   {
     const double dV = w_props.get_value_per_tile_X();
     double v = w_props.get_min_X();
     int px0 = w_props.valX2pixel(v - w_props.get_min_X())
                       + w_props.get_origin_X();
     draw_line(cr, grid_color, /* solid */ 1, line_width,
               Pixel_XY(px0, py0 + 5), Pixel_XY(px0, py1));

     v = w_props.get_min_X() + w_props.get_gridX_last()*dV;
     px0 = w_props.valX2pixel(v - w_props.get_min_X())
                       + w_props.get_origin_X();
     draw_line(cr, grid_color, /* solid */ 1, line_width,
               Pixel_XY(px0, py0 + 5), Pixel_XY(px0, py1));
   }

basic_string<double> xvals;
const Plot_data & data = w_props.get_plot_data();
   loop(r, data.get_row_count())
      {
        const Plot_data_row & row = data[r];
        loop(col, row.get_N())
            {
              const double val = row.get_X(col);
              xvals.push_back(val);
            }
      }

   loop(ix, xvals.size())
       {
         const double dV = w_props.get_value_per_tile_X();
         const double v = xvals[ix];
         const int px0 = w_props.valX2pixel(v - w_props.get_min_X())
                       + w_props.get_origin_X();

         // draw the first other lines with the desired line style.
         //
         draw_line(cr, grid_color, grid_style, line_width,
                        Pixel_XY(px0, py0 + 5), Pixel_XY(px0, py1));

         string format = w_props.get_format_X();
         const char * cc = format_tick(v, dV, ix, format.c_str());
         double cc_width, cc_height;
         cairo_multiline_size(cc_width, cc_height, cr, cc,
                              FONT_NAME, FONT_SIZE);

         Pixel_XY cc_pos(px0 - 0.5*cc_width, py0 + cc_height + 8);
         if (surface_plot)
            {
              cc_pos.x -= w_props.get_origin_X();
              cc_pos.y += w_props.get_origin_Y();
            }
         draw_multiline(cr, cc, cc_pos, cc_width);
       }

   if (w_props.get_axisX_arrow())
      {
        const Pixel_XY origin = w_props.get_origin(surface_plot);
        const Pixel_X px = w_props.valX2pixel(dv) + w_props.get_origin_X();

        Pixel_XY P(px, origin.y);
        draw_arrow(cr, origin, P, grid_color);

        const string arrow_label = w_props.get_axisX_label();
        if (arrow_label.size())
           {
             draw_text(cr, arrow_label.c_str(), Pixel_XY(P.x + 40, P.y + 5));
           }
      }
}
//----------------------------------------------------------------------------
/// draw the (horizontal) Y grid-lines of the plot (starting at the Y axis)
void
draw_Y_grid(cairo_t * cr, const GTK_context & pctx, bool surface_plot)
{
const Plot_window_properties & w_props = pctx.w_props;
const int line_width = w_props.get_gridY_line_width();
const Color grid_color = w_props.get_gridY_color();

const Pixel_X px0 = w_props.valX2pixel(0) + w_props.get_origin_X();
const double dv = w_props.get_max_X() - w_props.get_min_X();
const Pixel_X px1 = w_props.valX2pixel(dv) + w_props.get_origin_X();
const int grid_style = w_props.get_gridY_style();

   for (int iy = 0; iy <= w_props.get_gridY_last(); ++iy)
       {
         const double dV = w_props.get_value_per_tile_Y();
         const double v = w_props.get_min_Y() + iy*dV;
         const Pixel_Y py0 = w_props.valY2pixel(v - w_props.get_min_Y());

         // draw the first and last grid line solid, the others with
         // the desired line style.
         //
         if (iy == 0 || iy == w_props.get_gridY_last())   // first or last line
            {
              draw_line(cr, grid_color, /* solid */ 1, line_width,
                        Pixel_XY(px0 - 5, py0), Pixel_XY(px1 + 1, py0));
            }
         else
            {

              draw_line(cr, grid_color, grid_style, line_width,
                        Pixel_XY(px0 - 5, py0), Pixel_XY(px1 + 1, py0));
            }
         string format = w_props.get_format_Y();
         const char * cc = format_tick(v, dV, iy, format.c_str());
         double cc_width, cc_height;
         cairo_multiline_size(cc_width, cc_height, cr, cc,
                              FONT_NAME, FONT_SIZE);

         Pixel_XY cc_pos(px0 - cc_width - 8, py0 + 0.5 * cc_height - 1);
         if (surface_plot)
            {
              cc_pos.x -= w_props.get_origin_X();
              cc_pos.y += w_props.get_origin_Y();
            }

           if (int16_t(cc_pos.x) < 0)
              {
                CERR << "⎕PLOT warning: pa_border_L="
                     << int(w_props.get_pa_border_L())
                     << " is too small to print all Y-axis ticks (add "
                     << -int16_t(cc_pos.x) << ").";
                COUT << endl;
              }

           draw_multiline(cr, cc, cc_pos, cc_width);
       }

   if (w_props.get_axisY_arrow())
      {
        const Pixel_XY origin = w_props.get_origin(surface_plot);
        Pixel_Y Ay;
        if (surface_plot)
           {
            Ay = w_props.valXYZ2pixelXY(w_props.get_min_X(),
                                        w_props.get_max_Y(),
                                        w_props.get_min_Z()).y;
           }
        else
           {
            Ay = w_props.valY2pixel(dv);
           }

        Pixel_XY P(origin.x, Ay);
        draw_arrow(cr, origin, P, grid_color);

        const string arrow_label = w_props.get_axisY_label();
        if (arrow_label.size())
           {
             double cc_width, cc_height;
             cairo_string_size(cc_width, cc_height, cr, arrow_label.c_str(),
                               FONT_NAME, FONT_SIZE);
             draw_text(cr, arrow_label.c_str(),
                       Pixel_XY(P.x - 0.5*cc_width, P.y - 40));
           }
      }
}
//----------------------------------------------------------------------------
/// draw the (slanted) Z grid-lines of the plot
void
draw_Z_grid(cairo_t * cr, const GTK_context & pctx)
{
const Plot_window_properties & w_props = pctx.w_props;
const int line_width = w_props.get_gridZ_line_width();
const Color grid_color = w_props.get_gridZ_color();

const int ix_max = w_props.get_gridX_last();
const int iy_max = w_props.get_gridY_last();
const int iz_max = w_props.get_gridZ_last();
const Pixel_X len_Zx = w_props.get_origin_X();
const Pixel_Y len_Zy = w_props.get_origin_Y();
const Pixel_XY orig = w_props.valXYZ2pixelXY(w_props.get_min_X(),
                                             w_props.get_min_Y(),
                                             w_props.get_min_Z());
const Pixel_X len_X = w_props.valX2pixel(w_props.get_max_X())
                    - w_props.valX2pixel(w_props.get_min_X());

   /* NOTE: in cairo (other than in xcb) the Y-coordinates increase when moving
      down the screen. Therefore larger Y values correspond to smsller Y
      coordinates and we must subtract w_props.valY2pixel(w_props.get_max_Y())
      from w_props.valY2pixel(w_props.get_min_Y()) and not the other way
      around.
    */
const Pixel_Y len_Y = w_props.valY2pixel(w_props.get_min_Y())
                    - w_props.valY2pixel(w_props.get_max_Y());

   // lines starting on the Z-axis...
   //
int grid_style = w_props.get_gridZ_style();
   for (int iz = 1; iz <= iz_max; ++iz)
       {
         const Pixel_X px0 = orig.x - iz * len_Zx / iz_max;
         const Pixel_Y py0 = orig.y + iz * len_Zy / iz_max;
         const Pixel_X px1 = px0 + len_X;
         const Pixel_Y py1 = py0 - len_Y;
         const  Pixel_XY PZ(px0, py0);   // point on the Z axis
         const  Pixel_XY PX(px1, py0);   // along the X axis
         const  Pixel_XY PY(px0, py1);   // along the Y axis

         // draw the first and last grid line solid, the others with
         // the desired line style.
         //
         if (iz == iz_max)   // full line
            {
              draw_line(cr, grid_color, /* solid */ 1, line_width, PZ, PX);
              draw_line(cr, grid_color, /* solid */ 1, line_width, PZ, PY);
            }
         else
            {
              draw_line(cr, grid_color, grid_style, line_width, PZ, PX);
              draw_line(cr, grid_color, grid_style, line_width, PZ, PY);
            }

         const double dV = w_props.get_value_per_tile_Z();
         const double v = w_props.get_min_Z() + iz*dV;
         string format = w_props.get_format_Z();
         const char * cc = format_tick(v, dV, iz, format.c_str());
         double cc_width, cc_height;
         cairo_multiline_size(cc_width, cc_height, cr, cc,
                              FONT_NAME, FONT_SIZE);
         const Pixel_XY cc_pos(px1 + 10, py0 + 0.5*cc_height - 1);
         draw_multiline(cr, cc, cc_pos, cc_width);
       }

   // lines starting on the X-axis...
   //
   grid_style = w_props.get_gridX_style();
   for (int ix = 0; ix <= ix_max; ++ix)
       {
         const Pixel_X px0 = orig.x + ix * len_X / ix_max;
         const Pixel_Y py0 = orig.y;
         const Pixel_X px1 = px0 - len_Zx;
         const Pixel_Y py1 = py0 + len_Zy;
         const  Pixel_XY P0(px0, py0);   // point on the X axis
         const  Pixel_XY P1(px1, py1);   // point along the Z axis

         if (ix == 0 || ix == ix_max)   // full line
            draw_line(cr, grid_color, 1, line_width, P0, P1);
         else
            draw_line(cr, grid_color, grid_style, line_width, P0, P1);
       }

   // lines starting on the Y-axis...
   //
   grid_style = w_props.get_gridY_style();
   for (int iy = 1; iy <= iy_max; ++iy)
       {
         const Pixel_X px0 = orig.x;
         const Pixel_Y py0 = orig.y - iy * len_Y / iy_max;
         const Pixel_X px1 = px0 - len_Zx;
         const Pixel_Y py1 = py0 + len_Zy;
         const  Pixel_XY P0(px0, py0);   // point on the Y axis
         const  Pixel_XY P1(px1, py1);   // point along the Z axis

         if (iy == iy_max)   // full line
            draw_line(cr, grid_color, 1, line_width, P0, P1);
         else
            draw_line(cr, grid_color, grid_style, line_width, P0, P1);
       }

   if (w_props.get_axisZ_arrow())
      {
        const Pixel_XY origin = w_props.get_origin(true);
        const Pixel_XY P(orig.x - len_Zx, orig.y + len_Zy);
        draw_arrow(cr, origin, P, grid_color);

        const string arrow_label = w_props.get_axisZ_label();
        if (arrow_label.size())
           {
             double cc_width, cc_height;
             cairo_string_size(cc_width, cc_height, cr, arrow_label.c_str(),
                               FONT_NAME, FONT_SIZE);
             draw_text(cr, arrow_label.c_str(),
                       Pixel_XY(P.x - 30 - 0.5*cc_width, P.y + 35));
           }
      }
}
//----------------------------------------------------------------------------
/// draw normal (2D) data lines
void
draw_plot_lines(cairo_t * cr, const GTK_context & pctx)
{
const Plot_window_properties & w_props = pctx.w_props;
const Plot_data & data = w_props.get_plot_data();
Plot_line_properties const * const * l_props = w_props.get_line_properties();

   loop(l, data.get_row_count())
       {
         const Plot_line_properties & lp = *l_props[l];
         const Color line_color = lp.get_line_color();
         const int line_style   = lp.get_line_style();
         const int line_width   = lp.get_line_width();

         // draw lines between points
         //
         Pixel_XY last(0, 0);
         loop(n, data[l].get_N())
             {
               double vx, vy;   data.get_XY(vx, vy, l, n);

               const Pixel_XY P(w_props.valX2pixel(vx - w_props.get_min_X())
                                + w_props.get_origin_X(),
                                w_props.valY2pixel(vy - w_props.get_min_Y()));
               if (n)   // unless starting point
                  draw_line(cr, line_color, line_style, line_width, last, P);
               last = P;
             }

         // draw points...
         //
         loop(n, data[l].get_N())
             {
               double vx, vy;   data.get_XY(vx, vy, l, n);
               const Pixel_X px = w_props.valX2pixel(vx - w_props.get_min_X())
                                + w_props.get_origin_X();
               const Pixel_Y py = w_props.valY2pixel(vy - w_props.get_min_Y());
               draw_point(cr, Pixel_XY(px, py),
                              lp.get_point_style(), lp.get_point_color(),
                              lp.get_point_size(), w_props.get_canvas_color(),
                              lp.get_point_size2());
             }
       }
}
//----------------------------------------------------------------------------
/// draw surface (3D) data lines
void
draw_surface_lines(cairo_t * cr, const GTK_context & pctx)
{
const Plot_window_properties & w_props = pctx.w_props;
const Plot_data & data = w_props.get_plot_data();
Plot_line_properties const * const * l_props = w_props.get_line_properties();
const Plot_line_properties & lp0 = *l_props[0];

const Color canvas_color = w_props.get_canvas_color();
const Color point_color  = lp0.get_point_color();
const Color line_color   = lp0.get_line_color();
const int line_width     = lp0.get_line_width();

   // 1. draw areas between plot lines...
   //
const double Hmin = w_props.get_min_Y();
const double dH = w_props.get_max_Y() - Hmin;   // 100%
const int verbosity = w_props.get_verbosity();

   loop(row, data.get_row_count() - 1)
   loop(col, data[row].get_N() - 1)
       {
         if (verbosity & SHOW_DATA)
            CERR << "B[" << row << ";" << col << "]"
                 << " X=" << data.get_X(row, col)
                 << " Y=" << data.get_Y(row, col)
                 << " Z=" << data.get_Z(row, col) << endl;

         if (!w_props.get_gradient().size())   continue; // no gradient

    //   if (row != 2)   continue;   // show only given row
    //   if (col != 2)   continue;   // show only given column

         const double   X0 = data.get_X(row, col);
         const double   Y0 = data.get_Y(row, col);
         const double   Z0 = data.get_Z(row, col);
         const double   H0 = (Y0 - Hmin)/dH;
         const Pixel_XY P0 = w_props.valXYZ2pixelXY(X0, Y0, Z0);

         const double   X1 = data.get_X(row, col + 1);
         const double   Y1 = data.get_Y(row, col + 1);
         const double   Z1 = data.get_Z(row, col + 1);
         const double   H1 = (Y1 - Hmin)/dH;
         const Pixel_XY P1 = w_props.valXYZ2pixelXY(X1, Y1, Z1);

         const double   X2 = data.get_X(row + 1, col);
         const double   Y2 = data.get_Y(row + 1, col);
         const double   Z2 = data.get_Z(row + 1, col);
         const double   H2 = (Y2 - Hmin)/dH;
         const Pixel_XY P2 = w_props.valXYZ2pixelXY(X2, Y2, Z2);

         const double   X3 = data.get_X(row + 1, col + 1);
         const double   Y3 = data.get_Y(row + 1, col + 1);
         const double   Z3 = data.get_Z(row + 1, col + 1);
         const double   H3 = (Y3 - Hmin)/dH;
         const Pixel_XY P3 = w_props.valXYZ2pixelXY(X3, Y3, Z3);

         /* the surface has 4 points P0, P1, P2, and P3, but they are not
            necessarily coplanar. We could draw the surface in 2 ways:

            A. find the intersection PM of P0-P3 and P1-P2 on the bottom
               plane Y=0, interpolate the respective heights, and draw
               4 triangles from PM to each of the four edges of the square

            B. fold the surface at its shorter edge into 2 triangles that
               have the shorter edge in common. (triangles are always coplanar).

            Option B seems to look better.
         */

#if 0    /* Option A...

            interpolate middle point PM with height HM and split the
            square P0-P1-P2-P3 into 4 triangles with common point PM
            and edges P0-P1, P1-P3, P3-P2, and P2-P0 respectively.
          */
         const double beta = intersection(X0,Z0, X1,Z1, X2,Z2, X3,Z3);
         const double   XM = X1 + beta*(X2 - X1);
         const double   YM = Y1 + beta*(Y2 - Y1);
         const double   ZM = Z1 + beta*(Z2 - Z1);
         const double   HM = (YM - Hmin)/dH;
         const Pixel_XY PM = w_props.valXYZ2pixelXY(XM, YM, ZM);

         draw_triangle(pctx, verbosity, PM, HM, P0, H0, P1, H1);
         draw_triangle(pctx, verbosity, PM, HM, P1, H1, P3, H3);
         draw_triangle(pctx, verbosity, PM, HM, P3, H3, P2, H2);
         draw_triangle(pctx, verbosity, PM, HM, P2, H2, P0, H0);

#else    /* Option B...

            fold surface-square along its shorter edge
          */

         if (P0.distance2(P3) < P1.distance2(P2))   // P0-P3 is shorter
            {
              draw_triangle(cr, pctx, verbosity, P1, H1, P0, H0, P3, H3);
              draw_triangle(cr, pctx, verbosity, P2, H2, P0, H0, P3, H3);
            }
         else                                       // P1-P2 is shorter
            {
              draw_triangle(cr, pctx, verbosity, P0, H0, P1, H1, P2, H2);
              draw_triangle(cr, pctx, verbosity, P3, H3, P1, H1, P2, H2);
            }
#endif
       }

   // 2. draw lines between plot points
   //
   loop(row, data.get_row_count() - 1)
   loop(col, data[row].get_N() - 1)
       {
         if (verbosity & SHOW_DATA)
            CERR << "data[" << row << "," << col << "]"
                 << " X=" << data.get_X(row, col)
                 << " Y=" << data.get_Y(row, col)
                 << " Z=" << data.get_Z(row, col) << endl;

         const double X0 = data.get_X(row, col);
         const double Y0 = data.get_Y(row, col);
         const double Z0 = data.get_Z(row, col);
         const Pixel_XY P0 = w_props.valXYZ2pixelXY(X0, Y0, Z0);

         const double X1 = data.get_X(row, col + 1);
         const double Y1 = data.get_Y(row, col + 1);
         const double Z1 = data.get_Z(row, col + 1);
         const Pixel_XY P1 = w_props.valXYZ2pixelXY(X1, Y1, Z1);
         const double X2 = data.get_X(row + 1, col);
         const double Y2 = data.get_Y(row + 1, col);
         const double Z2 = data.get_Z(row + 1, col);
         const Pixel_XY P2 = w_props.valXYZ2pixelXY(X2, Y2, Z2);

         const double X3 = data.get_X(row + 1, col + 1);
         const double Y3 = data.get_Y(row + 1, col + 1);
         const double Z3 = data.get_Z(row + 1, col + 1);
         const Pixel_XY P3 = w_props.valXYZ2pixelXY(X3, Y3, Z3);

#if 0
         // debug: plot points as vertical lines
         //
         const Pixel_XY xy0 = w_props.valXYZ2pixelXY(data.get_X(row, col),
                                                     w_props.get_min_Y(),
                                                     data.get_Z(row, col));
         draw_line(pctx, pctx.line, xy0, P0);
         continue;
#endif
         draw_line(cr, line_color, 1, line_width, P0, P1);
         if (row == (data.get_row_count() - 2))   // last row
            draw_line(cr, line_color, 1, line_width, P2, P3);

         draw_line(cr, line_color, 1, line_width, P0, P2);
         if (col == (data[row].get_N() - 2))   // last column
            draw_line(cr, line_color, 1, line_width, P1, P3);
       }

  // 3. draw plot points
  //
const int point_style  = lp0.get_point_style();

  loop(row, data.get_row_count())
  loop(col, data[row].get_N())
      {
        const Pixel_XY P0 = w_props.valXYZ2pixelXY(data.get_X(row, col),
                                                   data.get_Y(row, col),
                                                   data.get_Z(row, col));
        draw_point(cr, P0, point_style, point_color, 2, canvas_color, 0);
      }
}
//----------------------------------------------------------------------------
/// plot the data
static void
do_plot(GtkWidget * drawing_area, GTK_context & pctx, cairo_t * cr)
{
const Plot_window_properties & w_props = pctx.w_props;
const Color canvas_color = w_props.get_canvas_color();
  cairo_set_RGB_source(cr, canvas_color);

  cairo_rectangle(cr, 0, 0, pctx.get_total_width(), pctx.get_total_height());
  cairo_fill(cr);

  cairo_select_font_face(cr, "sans-serif",
                             CAIRO_FONT_SLANT_NORMAL,
                             CAIRO_FONT_WEIGHT_NORMAL);
  cairo_set_font_size(cr, 12);

   // draw grid lines...
   //
const bool surface_plot = w_props.get_plot_data().is_surface_plot();

   if (w_props.get_gridX_variable())   draw_X_vargrid(cr, pctx, surface_plot);
   else                                draw_X_grid(cr, pctx, surface_plot);
  draw_Y_grid(cr, pctx, surface_plot);
  if (surface_plot)    draw_Z_grid(cr, pctx);

  if  (surface_plot)   draw_surface_lines(cr, pctx);
  else                 draw_plot_lines(cr, pctx);

  // draw legend...
  //
  draw_legend(cr, pctx, surface_plot);
}
//----------------------------------------------------------------------------
// event handlers (callbacks). They should be declared extern "C" as to be
// linked properly... In our case every event handler has a trailing
// GTK_context * pctc argument

#define Event_handler(name, ...)                                \
   extern "C" gboolean name(__VA_ARGS__, GTK_context * pctx);   \
   extern "C" gboolean name(__VA_ARGS__, GTK_context * pctx)

//----------------------------------------------------------------------------
/// callback when the mouse button is pressed. This may or may not start
/// the dragging of the legend.
Event_handler(Button_press_event, GtkWidget * widget, GdkEventButton * ev)
{
   Assert1(pctx && pctx->window == widget);

const int x = ev->x;
const int y = ev->y;

   if (x <  pctx->get_legend_X0(LOC))   return TRUE;   // left of the legend
   if (x >= pctx->get_legend_X1(LOC))   return TRUE;   // right of the legend
   if (y <  pctx->get_legend_Y0(LOC))   return TRUE;   // above the legend
   if (y >= pctx->get_legend_Y1(LOC))   return TRUE;   // below the legend

   // at this point a mouse button was pressed inside the legend. We take
   // that as the start of dragging the legend. Subsequent moves of the mouse
   // (with the button held down) will move the legend with it and update
   // legend_drag_X/Y after every move.
   //
   // cerr << "legend drag started at " << x << " : " << y << endl;

   pctx->legend_drag = true;
   pctx->legend_drag_X = x;
   pctx->legend_drag_Y = y;
   return TRUE;   // event handled, do not propagate it further
}
//----------------------------------------------------------------------------
/// the mouse button was released. Stop any ongoing dragging of the legend
Event_handler(Button_release_event, GtkWidget * widget, GdkEventButton *)
{
   Assert1(pctx && pctx->window == widget);

   pctx->legend_drag = false;
   pctx->legend_drag_X = 0;
   pctx->legend_drag_Y = 0;

   return TRUE;   // event handled, do not propagate it further
}
//----------------------------------------------------------------------------
/// the mouse was moved.
Event_handler(Motion_notify_event, GtkWidget * widget, GdkEventMotion * ev)
{
   Assert1(pctx && pctx->window == widget);

   if (!pctx->legend_drag)   return TRUE;

   // the legend is being dragged. Compute how much since the first
   // (start_legend_drag) or previous (move_legend_drag) event.
   //
const int x = ev->x;
const int y = ev->y;
const int dx = x - pctx->legend_drag_X;
const int dy = y - pctx->legend_drag_Y;

   // cerr << "MOVE " << dx << " : " << dy << endl;

   if (dx || dy)
      {
        pctx->w_props.move_legend(dx, dy);
        GdkWindow * gdk_win = gtk_widget_get_window(widget);
        const gint wx = gdk_window_get_width(gdk_win);
        const gint hy = gdk_window_get_height(gdk_win);
        const GdkRectangle rect = { 0, 0, wx, hy };
        gdk_window_invalidate_rect(gdk_win, &rect, FALSE);
      }

   // let the next move start at x:y
   pctx->legend_drag_X = x;
   pctx->legend_drag_Y = y;

   return TRUE;   // event handled, do not propagate it further
}
//----------------------------------------------------------------------------
// the plot window is closed
Event_handler(Hide, GtkWidget * widget)
{
   Assert1(pctx && pctx->window == widget);

   if (pctx->w_props.get_with_border())   pctx->save_to_file_with_border();
   return FALSE;   // propagate this event further further
}
//----------------------------------------------------------------------------
// the plot window is destroyed
Event_handler(Destroy, GtkWidget * widget)
{
   Assert1(pctx && pctx->window == widget);

   if (Quad_PLOT::get_verbosity() & SHOW_EVENTS)
      CERR << "PLOT DESTROYED" << endl;

   Quad_PLOT::PLOT_context::remove_handle(pctx->handle);
   delete pctx;

   return TRUE;   // event handled, do not propagate it further
}
//----------------------------------------------------------------------------
/// callback when the plot window shall be (re-)drawn
Event_handler(Draw, GtkWidget * widget, cairo_t * cr)
{
   Assert1(pctx && pctx->drawing_area == widget);

const int new_width  = gtk_widget_get_allocated_width(widget);
const int new_height = gtk_widget_get_allocated_height(widget);
   if (Quad_PLOT::get_verbosity() & SHOW_EVENTS)
      CERR << "Draw(drawing_area = " << widget << ")  "
           << "width: " << new_width << ", height: " << new_height << endl;

   pctx->w_props.set_window_size(new_width, new_height);

cairo_surface_t * surface = gdk_window_create_similar_surface(
                                gtk_widget_get_window(widget),
                                CAIRO_CONTENT_COLOR, new_width, new_height);

   {
     cairo_t * cr1 = cairo_create(surface);
     do_plot(widget, *pctx, cr1);   // plot the data
     cairo_destroy(cr1);
   }

   // copy pctx->surface to the cr of the caller
   //
   cairo_set_source_surface(cr, surface, 0, 0);
   cairo_paint(cr);

   if (pctx->w_props.get_with_border())
      {
        // the user wants the window borders to be printed around the plot
        // window. At this point in time, however, the plot window content
        // has been printed to surface, but the window manager has not yet
        // added the border to the plot window and will only do so after we
        // have returned.
        //
        // We fix this problem by adding a timout after which the window
        // and its borders will be copied from the parent (= the entire
        // screen) to the output file. In the mean time, i.e. between
        // returning from this event handler and the expiration of the
        // timeout, the window manager has hopefully enough time to draw
        // te window borders.
        //
        return TRUE;
      }

   pctx->save_to_file_no_border(surface);

   cairo_surface_destroy(surface);

   if (Quad_PLOT::get_verbosity() & SHOW_EVENTS)
      CERR << "Draw() done." << endl;
   return TRUE;   // event handled, do not propagate it further
}
//----------------------------------------------------------------------------
void
GTK_context::save_to_file_no_border(cairo_surface_t * surface)
{
UTF8_string fname(w_props.get_output_filename().c_str());
   if (fname.size() == 0)   return;   // no output_filename: don't save

   // append .png unless already there
   //
   if (!fname.ends_with(".png"))   fname.append_ASCII(".png");

   // save the plot window without its borders
   errno = 0;

cairo_status_t stat = cairo_surface_write_to_png(surface, fname.c_str());
   if (stat == CAIRO_STATUS_SUCCESS)
      {
        Quad_PLOT::get_verbosity() && CERR << "wrote output file: "
                                           << fname << endl;
       //    if (w_props.get_auto_close() == 1)   gtk_main_quit();
      }
   else   // error writing file
      {
        CERR << "*** writing output file: '" << fname << "' failed: "
             << strerror(errno) << endl;
       //    if (w_props.get_auto_close() == 2)   gtk_main_quit();
      }
}
//----------------------------------------------------------------------------
void
GTK_context::save_to_file_with_border()
{
   /* to add the borders of our top-level (but border-less) plot window,
      we go to its parent (i.e. the entire screen) and cut out the rectangle
      that contains our window and its borders.

      It appears as if the window sizes returned by gtk_window_get_size()
      and gdk_window_get_geometry() below are identical, while the window
      positions returned by gtk_window_get_position() change as the window
      is moved, while the position in gdk_window_get_geometry() remains the
      same (and we assume that it is the N/W border.
    */

UTF8_string fname(w_props.get_output_filename().c_str());
   if (fname.size() == 0)   return;   // no output_filename: don't save

   // append .png unless already there
   //
   if (!fname.ends_with(".png"))   fname.append_ASCII(".png");

GdkWindow * gdk_win = gtk_widget_get_window(window);
GdkWindow * parent = gdk_window_get_parent(gdk_win);
   Assert(gdk_win);

int gtk_x, gtk_y;   // position of the plot window on the screen
   gtk_window_get_position(GTK_WINDOW(window), &gtk_x, &gtk_y);

int gtk_w, gtk_h;   // size of the plot window
   gtk_window_get_size(GTK_WINDOW(window), &gtk_w, &gtk_h);

int gdk_x, gdk_y, gdk_w, gdk_h;
gdk_window_get_geometry(gdk_win, &gdk_x, &gdk_y, &gdk_w, &gdk_h);

   // create a GdkPixbuf. GDK windows cannot access their borders,
   // we therefore copy from the parent of the (top-level) gdk_win.
   //

   // create the result surface by copying parts of the parent window
   // of the inner window, For a top-level window like ours, its
   // parent is the entire screen.
   //
GdkPixbuf * pixbuf = gdk_pixbuf_get_from_window(parent,
                          gtk_x, gtk_y, gtk_w, gtk_h + gdk_y - gdk_x);

cairo_surface_t *
      surface = gdk_cairo_surface_create_from_pixbuf(pixbuf, 1, gdk_win);
   errno = 0;
   if (surface)
      {
         cairo_surface_write_to_png(surface, fname.c_str());
         cairo_surface_destroy(surface);
      }
   else
      {
        CERR << "*** writing output file: '" << fname << "' failed: "
             << strerror(errno) << endl;
      }
}
//----------------------------------------------------------------------------
/** gtk_main_wrapper() makes gtk_main() suitable for pthread_create()
    and warn if it returns (which is not expected to happen).

    gtk_main_wrapper() is the top-level main() functions of a thread
    created in plot_main_GTK() which is called from ⎕PLOT::do_plot_data():

   ⎕PLOT → eval_AB(), or eval_B()
             → do_plot_data()
                 → plot_main_GTK()
                   ↓
                   ↓ → → pthread_create(gtk_main_wrapper) → return immediately
                   ↓                            ↓
            ┌─────────────┐           ┌────── thread ──────┐
            │ GTK_context │           │ gtk_main_wrapper() │
            └─────────────┘           │ → gtk_main()       │
                   ↓                  └────────────────────┘
                   ↓                            ↓
                   ↓ → → gtk_window_new() → → event
                   ↓                            ↓
           all_PLOT_windows → GTK_context → → window

   NOTE that gtk_main_wrapper() is called only once (and not per window).
   Therefore the functions called from it must not throw any APL ERRORs.
 */

static void *
gtk_main_wrapper(void *)
{
   gtk_main();

   // supposedly not reached

   CERR << "*** gtk_main() thread done." << endl;

   return 0;
}
//----------------------------------------------------------------------------

void
Quad_PLOT::plot_main_GTK(void * vp_props, Handle handle)
{
// CERR << "plot_main(" << vp_props << ")" << endl;

   if (getenv("DISPLAY") == 0)   // DISPLAY not set
      setenv("DISPLAY", ":0", true);

Plot_window_properties & w_props =
      *reinterpret_cast<Plot_window_properties *>(vp_props);
   verbosity = w_props.get_verbosity();

   XInitThreads();

   if (!gtk_init_done)
      {
        int argc = 0;
        char ** argv = { 0 };
        gtk_init(&argc, &argv);   setlocale(LC_ALL, "C");
        gtk_init_done = true;

        pthread_t thread = 0;
        pthread_create(&thread, 0, gtk_main_wrapper, 0);

#if HAVE_PTHREAD_SETNAME_NP
         // show with e.g.   ps H -o 'pid tid cmd comm'
         pthread_setname_np(thread, "apl/GTK");
#endif
      }

GTK_context * pctx = new GTK_context(w_props, handle);
   Assert(pctx);
   Quad_PLOT::all_PLOT_windows.push_back(pctx);

   pctx->window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
   Assert(pctx->window);
   if (w_props.get_user_caption())   // the user has dictated the caption
      {
        gtk_window_set_title(GTK_WINDOW(pctx->window),
                             w_props.get_caption().c_str());
      }
   else   // default caption ("⎕PLOT": add the handle
     {
       char cc[50];
       SPRINTF(cc, "%s %d", w_props.get_caption().c_str(), pctx->handle);
       gtk_window_set_title(GTK_WINDOW(pctx->window), cc);
     }

   gtk_window_set_resizable(GTK_WINDOW(pctx->window), true);

   pctx->drawing_area = gtk_drawing_area_new();
   gtk_container_add(GTK_CONTAINER(pctx->window), pctx->drawing_area);

   // resize the drawing_area before showing it so that draw_callback()
   // will not be called twice.
   //
   gtk_widget_set_size_request(pctx->drawing_area,
                               pctx->get_total_width(),
                               pctx->get_total_height());

   if (w_props.get_user_pw_pos())   // the user has dictated pw_pos
      {
        gtk_window_move(GTK_WINDOW(pctx->window),
                        w_props.get_pw_pos_X(),
                        w_props.get_pw_pos_Y());
      }
   else                             // the default pw_pos
      {
        // display the windows at different positions so that the user
        // can see their captions (and their close buttons).
        gtk_window_move(GTK_WINDOW(pctx->window),
                        w_props.get_pw_pos_X() + 50*(pctx->handle - 1),
                        w_props.get_pw_pos_Y() + 50*(pctx->handle - 1));
      }

   // do not steal the keyboard focus (whicj annoys when working interactively)
   gtk_window_set_focus_on_map(GTK_WINDOW(pctx->window), false);

   // but stay on top even though not focused
   gtk_window_set_keep_above(GTK_WINDOW(pctx->window), true);

   gtk_widget_show_all(pctx->window);

#define Connect_signal(instance, signal_name, callback)               \
   g_signal_connect(instance, signal_name, G_CALLBACK(callback), pctx);

   Connect_signal(pctx->window,                 "hide", Hide)
   Connect_signal(pctx->window,              "destroy", Destroy)
   Connect_signal(pctx->window,   "button-press-event", Button_press_event)
   Connect_signal(pctx->window, "button-release-event", Button_release_event)
   Connect_signal(pctx->window,  "motion-notify-event", Motion_notify_event)
   Connect_signal(pctx->drawing_area, "draw",           Draw)

   sem_post(Quad_PLOT::expose_sema);   // unleash the APL interpreter
}
//----------------------------------------------------------------------------
#endif // apl_GTK3 && apl_X11
