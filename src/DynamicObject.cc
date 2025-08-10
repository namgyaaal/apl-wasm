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


#include "Common.hh"
#include "DynamicObject.hh"
#include "IndexExpr.hh"
#include "PrintOperator.hh"
#include "Value.hh"

//----------------------------------------------------------------------------
ostream &
operator << (ostream & out, const DynamicObject & dob)
{
   dob.print(out);
   return out;
}
//----------------------------------------------------------------------------
void
DynamicObject::print_chain(ostream & out) const
{
int pos = 0;
   for (const DynamicObject * p = this; ;)
       {
         out << "    Chain[" << setw(2) << pos++ << "]  " 
             << voidP(p->prev) << " --> "
             << voidP(p)       << " --> "
             << voidP(p->next) << "    "
             << p->where_allocated() << endl;

          p = p->next;
          if (p == this)   break;
       }
}
//----------------------------------------------------------------------------
void
DynamicObject::print_new(ostream & out, const char * loc) const
{
   out << "new    " << voidP(this) << " at " << loc << endl;
}
//----------------------------------------------------------------------------
void
DynamicObject::print(ostream & out) const
{
   out << "DynamicObject: " << voidP(this)
       << " (Value: " << voidP(this) << ") :"       << endl
       << "    prev:      " << voidP(prev)          << endl
       << "    next:      " << voidP(next)          << endl
       << "    allocated: " << where_allocated()    << endl;
}
//----------------------------------------------------------------------------
/// cast DynamicObject to derived class Value.
/// This only works properly after including Value.hh !
Value &
DynamicObject::rValue()
{
   /* static_cast<> is inherently unsafe, but we cannot used dynamic_cast<>()
      because DynamicObject is non-virtual. The caller must therefore be
      careful and we allow only class Value to use rValue().
    */
   return *static_cast<Value *>(this);
}
//----------------------------------------------------------------------------
/// cast DynamicObject to derived class Value.
/// This only works properly after including Value.hh !
const Value &
DynamicObject::rValue() const
{
   /* static_cast<> is inherently unsafe, but we cannot used dynamic_cast<>()
      because DynamicObject is non-virtual. The caller must therefore be
      careful and we allow only class Value to use rValue().
    */
  return *static_cast<const Value *>(this);
}
//----------------------------------------------------------------------------
/// cast DynamicObject to derived class IndexExpr.
// This only works properly after #include IndexExpr.hh !
const IndexExpr *
DynamicObject::pIndexExpr() const
{
  return static_cast<const IndexExpr *>(this);
}
//----------------------------------------------------------------------------
/// cast DynamicObject to derived class Value.
// This only works properly after #include IndexExpr.hh !
IndexExpr *
DynamicObject::pIndexExpr()
{
  return static_cast<IndexExpr *>(this);
}
//----------------------------------------------------------------------------

