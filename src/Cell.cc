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

#include "CharCell.hh"
#include "ComplexCell.hh"
#include "Error.hh"
#include "FloatCell.hh"
#include "IntCell.hh"
#include "LvalCell.hh"
#include "Output.hh"
#include "PointerCell.hh"
#include "PrintOperator.hh"
#include "Value.hh"
#include "SystemLimits.hh"
#include "Workspace.hh"

#include "Cell.icc"

//----------------------------------------------------------------------------
void *
Cell::operator new(std::size_t s, void * pos)
{
   return pos;
}
//----------------------------------------------------------------------------
void
Cell::init_from_value(Value * value, Value & cell_owner, const char * loc)
{
   if (value->is_simple_scalar())
      {
        value->get_cfirst().init_other(this, cell_owner, loc);
      }
   else
      {
        new (this) PointerCell(value, cell_owner);
      }
}
//----------------------------------------------------------------------------
Value_P
Cell::to_value(const char * loc) const
{
   if (is_pointer_cell())
      {
        Value_P Z = get_pointer_value();   //->clone(LOC);
        return Z;
      }
   else
      {
        Value_P Z(loc);
        Z->set_ravel_Cell(0, *this);
        Z->check_value(LOC);
        return Z;
      }
}
//----------------------------------------------------------------------------
void
Cell::init_type(const Cell & other, Value & cell_owner, const char * loc)
{
   if (other.is_pointer_cell())
      {
        // Somewhat tricky! We must 'Value_P proto' so that proto owns
        // the cloned value until PointerCell() takes over ownership of it.
        //
        Value_P proto = other.get_pointer_value()->clone(loc);
        Assert(!proto->is_simple_scalar());
        proto->to_type(false);
        new (this) PointerCell(proto.get(), cell_owner);
      }
   else if (other.is_lval_cell())      new (this) LvalCell(0, 0);
   else if (other.is_character_cell()) new (this) CharCell(UNI_SPACE);
   else                                new (this) IntCell(0);
}
//----------------------------------------------------------------------------
void
Cell::copy(Value & val, const Cell * & src, ShapeItem count)
{
   loop(c, count)
      {
        Assert1(val.more());
        val.next_ravel_Cell(*src++);
      }
}
//----------------------------------------------------------------------------
bool
Cell::greater(const Cell & other) const
{
   MORE_ERROR() << "Cell::greater() : Objects of class " << get_classname()
                << " cannot be compared with objects of class"
                << other.get_classname();
   DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
bool
Cell::equal(const Cell & other, double qct) const
{
   MORE_ERROR() << "Cell::equal() : Objects of class " << get_classname()
                << " cannot be compared with objects of class"
                << other.get_classname();
   DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
bool
Cell::A_greater_B(const Cell * const & A, const Cell * const & B,
                  const void * /* comp_arg not used */)
{
   return A->greater(*B);
}
//----------------------------------------------------------------------------
bool
Cell::compare_stable(const Cell * const & A, const Cell * const & B,
                  const void * unused_comp_arg)
{
   if (const Comp_result cr = A->compare(*B))   return cr == COMP_GT;
   return A > B;
}
//----------------------------------------------------------------------------
bool
Cell::compare_ptr(const Cell * const & A, const Cell * const & B,
                  const void * unused_comp_arg)
{
   return A > B;
}
//----------------------------------------------------------------------------
Unicode
Cell::get_char_value() const
{
   MORE_ERROR() << "Bad cell type " << int(get_cell_type())
                << " aka. " << get_cell_type_name(get_cell_type())
                << " when expecting a CHARACTER cell";
   DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
Value_P
Cell::get_pointer_value() const
{
   MORE_ERROR() << "Bad cell type " << int(get_cell_type())
                << " aka. " << get_cell_type_name(get_cell_type())
                << " when expecting a nested cell";
   DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
bool
Cell::is_near_int(APL_Float value)
{
   // all large values are considered int because their fractional part
   // has been rounded off. However, they may not fit into an int64_t,
   // and if that is the concern then use is_near_int64_t() below.

   if (value > LARGE_INT)   return true;
   if (value < SMALL_INT)   return true;

const APL_Float result = nearbyint(value);
const APL_Float diff = value - result;
   if (diff >= INTEGER_TOLERANCE)    return false;
   if (diff <= -INTEGER_TOLERANCE)   return false;

   return true;
}
//----------------------------------------------------------------------------
bool
Cell::is_near_int64_t(APL_Float value)
{
   if (value > LARGE_INT)   return false;
   if (value < SMALL_INT)   return false;

const APL_Float result = nearbyint(value);
const APL_Float diff = value - result;
   if (diff >=  INTEGER_TOLERANCE)   return false;
   if (diff <= -INTEGER_TOLERANCE)   return false;

   return true;
}
//----------------------------------------------------------------------------
APL_Integer
Cell::near_int(APL_Float value)
{
   if (value >= LARGE_INT)   DOMAIN_ERROR;
   if (value <= SMALL_INT)   DOMAIN_ERROR;

const APL_Float result = nearbyint(value);
const APL_Float diff = value - result;
   if (diff >  INTEGER_TOLERANCE)   DOMAIN_ERROR;
   if (diff < -INTEGER_TOLERANCE)   DOMAIN_ERROR;

   if (result > 0.0)   return   APL_Integer(0.3 + result);
   else                return - APL_Integer(0.3 - result);
}
//----------------------------------------------------------------------------
bool
Cell::greater_cp(const ShapeItem &  A, const ShapeItem & B, const void * ctx)
{
const ravel_comp_len * rcl = reinterpret_cast<const ravel_comp_len *>(ctx);
const Cell * cells = rcl->ravel;
const ShapeItem comp_len = rcl->comp_len;
const Cell * cell_A = cells + A * comp_len;
const Cell * cell_B = cells + B * comp_len;

   loop(l, comp_len)
       {
         if (const Comp_result cr = cell_A++->compare(*cell_B++))
            return cr == COMP_GT;
       }

   // at this point all cells were equal
   //
   return A > B;
}
//----------------------------------------------------------------------------
bool
Cell::smaller_cp(const ShapeItem &  A, const ShapeItem & B, const void * ctx)
{
const ravel_comp_len * rcl = reinterpret_cast<const ravel_comp_len *>(ctx);
const Cell * cells = rcl->ravel;
const ShapeItem comp_len = rcl->comp_len;
const Cell * cell_A = cells + A * comp_len;
const Cell * cell_B = cells + B * comp_len;

   loop(l, comp_len)
       {
         if (const Comp_result cr = cell_A++->compare(*cell_B++))
            return cr == COMP_LT;
       }

   // at this point all cells were equal
   //
   return A > B;
}
//----------------------------------------------------------------------------
ostream &
operator <<(ostream & out, const Cell & cell)
{
PrintBuffer pb = cell.character_representation(PR_BOXED_GRAPHIC);
UCS_string ucs(pb, 0, Workspace::get_PW());
   return out << ucs << ' ';
}
//----------------------------------------------------------------------------
ErrorCode
Cell::bif_equal(Cell * Z, const Cell * A) const
{
   // incompatible types ?
   //
   if (is_character_cell() != A->is_character_cell())   return IntCell::z0(Z);

   return IntCell::zI(Z, equal(*A, Workspace::get_CT()));
}
//----------------------------------------------------------------------------
ErrorCode
Cell::bif_not_equal(Cell * Z, const Cell * A) const
{
   // incompatible types ?
   //
   if (is_character_cell() != A->is_character_cell())   return IntCell::z1(Z);

   return IntCell::zI(Z, !equal(*A, Workspace::get_CT()));
}
//----------------------------------------------------------------------------
ErrorCode
Cell::bif_greater_than(Cell * Z, const Cell * A) const
{
   return IntCell::zI(Z, (A->compare(*this) == COMP_GT) ? 1 : 0);
}
//----------------------------------------------------------------------------
ErrorCode
Cell::bif_less_eq(Cell * Z, const Cell * A) const
{
   return IntCell::zI(Z, (A->compare(*this) != COMP_GT) ? 1 : 0);
}
//----------------------------------------------------------------------------
ErrorCode
Cell::bif_less_than(Cell * Z, const Cell * A) const
{
   return IntCell::zI(Z, (A->compare(*this) == COMP_LT) ? 1 : 0);
}
//----------------------------------------------------------------------------
ErrorCode
Cell::bif_greater_eq(Cell * Z, const Cell * A) const
{
   return IntCell::zI(Z, (A->compare(*this) != COMP_LT) ? 1 : 0);
}
//----------------------------------------------------------------------------
ErrorCode
Cell::sorted_indices(vector<ShapeItem> & indices, const Value & value,
                     Sort_order order, ShapeItem comp_len)
{
   Assert(indices.size() == 0);   // initially empty, filled by this function

const ShapeItem rows = value.get_shape_item(0);
   indices.reserve(rows);

   // initialize indices with 0, 1, ... length-1
   //
   try
     {
       loop(r, rows)   indices.push_back(r);
     }
   catch (...)
     {
       return E_WS_FULL;
     }
     

const ravel_comp_len ctx = { &value.get_cfirst(), comp_len};
   if (order == SORT_ASCENDING)
      Heapsort<ShapeItem>::sort(indices.data(), rows, &ctx, &Cell::greater_cp);
   else
      Heapsort<ShapeItem>::sort(indices.data(), rows, &ctx, &Cell::smaller_cp);
   return E_NO_ERROR;
}
//----------------------------------------------------------------------------
const char *
Cell::get_cell_type_name(CellType ct)
{
   switch(ct & CT_MASK)
      {
        case CT_NONE:    return "NONE";
        case CT_BASE:    return "BASE";
        case CT_CHAR:    return "CHARACTER";
        case CT_POINTER: return "NESTED";
        case CT_CELLREF: return "LEFTVAL";
        case CT_INT:     return "INTEGER";
        case CT_FLOAT:   return "FLOAT";
        case CT_COMPLEX: return "COMPLEX";
      }

   return "UNKNOWN";
}
//----------------------------------------------------------------------------
