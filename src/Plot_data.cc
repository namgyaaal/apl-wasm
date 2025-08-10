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

#include "Plot_data.hh"

//============================================================================
Color
Plot_data::Color_from_str(const char * str, const char * & error)
{
   error = 0;   // assume no error

uint32_t r, g, b;
   if (3 == sscanf(str, " %u %u %u", &r, &b, &g))
      return (r & 0xFF) << 16 | (g & 0xFF) << 8 | (b & 0xFF);

   if (const char * h = strchr(str, '#'))
      {
        if (strlen(h) == 4)   // #RGB
           {
             const int v = strtoll(str + 1, 0, 16);
             return (0x11*(v >> 8 & 0x0F)) << 16
                  | (0x11*(v >> 4 & 0x0F)) << 8
                  | (0x11*(v      & 0x0F)  << 0);
           }
        else if (strlen(h) == 7)   // #RRGGBB
           {
             return strtoll(str + 1, 0, 16);
           }
        else if (strlen(h) == 9)   // #xxRRGGBB
           {
             return strtoll(str + 1, 0, 16);
           }
      }

   error = "Bad color format";
   return 0;
}
//============================================================================

