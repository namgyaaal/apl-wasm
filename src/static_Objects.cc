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

#include <stdio.h>
#include <iostream>

#include "Common.hh"
#include "DiffOut.hh"
#include "DynamicObject.hh"
#include "Logging.hh"
#include "Macro.hh"
#include "StateIndicator.hh"
#include "static_Objects.hh"
#include "Workspace.hh"

/*
   See 3.6.2 of "ISO standard Programming Languages — C++"
*/

bool static_Objects::show_constructors = false;
bool static_Objects::show_destructors  = false;

//----------------------------------------------------------------------------
static_Objects::static_Objects(const char * l, const char * w)
   : what(w),
     loc(l)
{
   if (show_constructors)   cerr << "++ constructing " << what << endl;
}
//----------------------------------------------------------------------------
static_Objects::~static_Objects()
{
   if (show_destructors)   cerr << "-- destructing " << what << endl;
}
//----------------------------------------------------------------------------

#define INFO(m, l) DO_INFO(#m, l)
#define DO_INFO(m, l)   extern static_Objects info_ ## l; \
                        static_Objects info_ ## l  (LOC, m);

INFO(ErrOut::used, __LINE__)
bool ErrOut::used = false;

// prerequisites for Workspace::the_workspace...

INFO(DynamicObject::all_values, __LINE__)
DynamicObject DynamicObject::all_values(LOC);

INFO(DynamicObject::all_index_exprs, __LINE__)
DynamicObject DynamicObject::all_index_exprs(LOC);

INFO(Workspace::the_workspace, __LINE__)
Workspace Workspace::the_workspace;

INFO(StateIndicator::top_level_error, __LINE__)
Error StateIndicator::top_level_error(E_NO_ERROR, LOC);

INFO(Quad_CR::fun, __LINE__)
Quad_CR Quad_CR   ::fun;

INFO(Quad_EC::fun, __LINE__)
Quad_EC Quad_EC   ::fun;

INFO(Quad_ES::fun, __LINE__)
Quad_ES Quad_ES   ::fun;

INFO(CPU_pool::the_CPUs, __LINE__);
std::vector<CPU_Number> CPU_pool::the_CPUs;

INFO(Macro::all_macros, __LINE__)
#define mac_def(name, txt) Macro Macro::name(MAC_ ## name, UTF8_string(txt));
#include "Macro.def"

