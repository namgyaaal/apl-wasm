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

#ifndef __IDENUMS_HH_DEFINED__
#define __IDENUMS_HH_DEFINED__

//----------------------------------------------------------------------------
/// an ID. Every internal object known to APL (primitive, ⎕xx, ...) has one
enum Id
{
#define pp(i, _u, v) ID_ ##   i v,
#define qf(i, _u, v) ID_Quad_ ## i v,
#define qv(i, _u, v) ID_Quad_ ## i v,
#define sf(i, _u, v) ID_ ##   i v,
#define st(i, _u, v) ID_ ##   i v,

#include "Id.def"
};
//----------------------------------------------------------------------------

#endif // __IDENUMS_HH_DEFINED__

