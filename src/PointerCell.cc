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

#include "Error.hh"
#include "PointerCell.hh"
#include "PrintOperator.hh"
#include "Value.hh"
#include "Workspace.hh"

//----------------------------------------------------------------------------
PointerCell::PointerCell(Value * sub_val, Value & cell_owner)
{
   Assert(!sub_val->is_simple_scalar());

   new (&value.pval.valp) Value_P(sub_val, LOC);
   value.pval.owner = &cell_owner;

   Assert(value.pval.owner != sub_val);   // typical cut-and-paste error
   cell_owner.increment_pointer_cell_count();
   cell_owner.add_subcount(sub_val->nz_element_count());
}
//----------------------------------------------------------------------------
PointerCell::PointerCell(Value * sub_val, Value & cell_owner, uint32_t magic)
{
   // DO NOT: Assert(!sub_val->is_simple_scalar()); This is a special
   // constructor only used in ScalarFunction.cc to create an un-initialized
   // PointerCell

   Assert(magic == 0x6B616769);

   new (&value.pval.valp) Value_P(sub_val, LOC);
   value.pval.owner = &cell_owner;

   Assert(value.pval.owner != sub_val);   // typical cut-and-paste error
   cell_owner.increment_pointer_cell_count();
   cell_owner.add_subcount(sub_val->nz_element_count());
}
//----------------------------------------------------------------------------
void
PointerCell::init_other(void * other, Value & other_owner,
                            const char * loc) const
{
   Assert(other);   // the new PointerCell to be created

Value_P sub;   // instantiate beforehand so that sub is 0 if clone() fails

   sub = CLONE_P(get_pointer_value(), loc);
   Assert(+sub);
   new (other) PointerCell(sub.get(), other_owner);
}
//----------------------------------------------------------------------------
void
PointerCell::release(const char * loc)
{
   // 1. update the PointerCell related counters in the owner of this
   //    PointerCell (since this Cell is no longer a PointerCell).
   //
   value.pval.owner->decrement_pointer_cell_count();
   value.pval.owner->add_subcount(-get_pointer_value()->nz_element_count());

   // 2. decrement the owner_count of our sub-value via reset() (since this
   //    PointerCell releases the ownership of the sub-value.
   //
   value.pval.valp.reset();

   // 3. not needed but to be on the safe side: make this Cell an IntCell
   //    (in case someone still points to it).
   //
   IntCell::z0(this);
}
//----------------------------------------------------------------------------
bool
PointerCell::equal(const Cell & other, double qct) const
{
   if (!other.is_pointer_cell())   return false;

Value_P A = get_pointer_value();
Value_P B = other.get_pointer_value();

   if (!A->same_shape(*B))                 return false;

const ShapeItem count = A->nz_element_count();
   loop(c, count)
       if (!A->get_cravel(c).equal(B->get_cravel(c), qct))   return false;

   return true;
}
//----------------------------------------------------------------------------
bool
PointerCell::greater(const Cell & other) const
{
   if (compare(other) == COMP_GT)   return true;
   if (compare(other) == COMP_LT)   return false;
   return this > &other;
}
//----------------------------------------------------------------------------
Comp_result
PointerCell::compare(const Cell & other) const
{
   if (other.get_cell_type() & CT_SIMPLE)   // nested > numeric > char
      return COMP_GT;

   if (other.get_cell_type() != CT_POINTER)   DOMAIN_ERROR;

   // at this point both cells are pointer cells.
   //
Value_P v1 = get_pointer_value();
Value_P v2 = other.get_pointer_value();

   // compare ranks
   //
   if (v1->get_rank() != v2->get_rank())   // ranks differ
      return v1->get_rank() > v2->get_rank() ? COMP_GT : COMP_LT;

   // same rank, compare shapes
   //
   loop(r, v1->get_rank())
      {
        const ShapeItem axis1 = v1->get_shape_item(r);
        const ShapeItem axis2 = v2->get_shape_item(r);
        if (axis1 != axis2)   // axis r differs
           return axis1 > axis2 ? COMP_GT : COMP_LT;
      }

   // same rank and shape, compare ravel
   //
const Cell * C1 = &v1->get_cfirst();
const Cell * C2 = &v2->get_cfirst();
   loop(e, v1->nz_element_count())
      {
        if (const Comp_result comp = C1++->compare(*C2++))   return  comp;
      }

   // everthing equal
   //
   return COMP_EQ;
}
//----------------------------------------------------------------------------
Value_P
PointerCell::get_pointer_value() const
{
Value * vp = const_cast<Value *>(value.pval.valp.get());
Value_P ret(vp, LOC);   // Value_P constructor increments owner_count
   return ret;
}
//----------------------------------------------------------------------------
bool
PointerCell::is_member_anchor() const
{
   return value.pval.valp.value_p &&
          value.pval.valp.value_p->is_member();
}
//----------------------------------------------------------------------------
CellType
PointerCell::deep_cell_types() const
{
   return CellType(CT_POINTER | get_pointer_value()->deep_cell_types());
}
//----------------------------------------------------------------------------
CellType
PointerCell::deep_cell_subtypes() const
{
   return CellType(CT_POINTER | get_pointer_value()->deep_cell_subtypes());
}
//----------------------------------------------------------------------------
PrintBuffer
PointerCell::character_representation(const PrintContext & pctx) const
{
Value_P val = get_pointer_value();
   Assert(+val);

   if (pctx.get_style() & PST_QUOTE_CHARS)
      {
        if (val->is_char_vector())   return PrintBuffer(*val, pctx, 0);
      }

   if (pctx.get_style() == PR_APL_FUN)   // APL function display
      {
        const ShapeItem ec = val->element_count();
        UCS_string ucs;

        if (val->is_char_vector())
           {
             ucs.append(UNI_SINGLE_QUOTE);
             loop(e, ec)
                {
                  const Unicode uni = val->get_cravel(e).get_char_value();
                  ucs.append(uni);
                  if (uni == UNI_SINGLE_QUOTE)   ucs.append(uni);   // ' -> ''
                }
             ucs.append(UNI_SINGLE_QUOTE);
           }
        else
           {
             ucs.append(UNI_L_PARENT);
             loop(e, ec)
                {
                  PrintBuffer pb = val->get_cravel(e).
                        character_representation(pctx);
                  ucs.append(UCS_string(pb, 0, Workspace::get_PW()));

                  if (e < ec - 1)   ucs.append(UNI_SPACE);
                }
             ucs.append(UNI_R_PARENT);
           }

        ColInfo ci;
        PrintBuffer ret(ucs, ci);
        ret.get_info().int_len  = ret.get_column_count();
        ret.get_info().real_len = ret.get_column_count();
        return ret;
      }

PrintBuffer ret(*val, pctx, 0);
   ret.get_info().flags &= ~CT_MASK;
   ret.get_info().flags |= CT_POINTER;

   if (pctx.get_style() & PST_CS_INNER)
      {
        const ShapeItem ec = val->element_count();
        if (ec == 0)   // empty value
           {
             int style = pctx.get_style();
             loop(r, val->get_rank())
                {
                  if (val->get_shape_item(r) == 0)   // an empty axis
                     {
                       if (r == val->get_rank() - 1)   style |= PST_EMPTY_LAST;
                       else                            style |= PST_EMPTY_NLAST;
                     }
                }

             // do A←(1⌈⍴A)⍴⊂↑A
             Value_P proto = val->prototype(LOC);

             // compute a shape like ⍴ val but 0 replaced with 1.
             //
             Shape sh(val->get_shape());
             loop(r, sh.get_rank())
                 {
                   if (sh.get_shape_item(r) == 0)   sh.set_shape_item(r, 1);
                 }

             Value_P proto_reshaped(sh, LOC);
             const ShapeItem len = proto_reshaped->nz_element_count();

             // store proto in the first ravel item, and copies of proto in
             // the subsequent ravel items
             //
             loop(rv, len)   proto_reshaped->next_ravel_Value(proto.get());

             proto_reshaped->check_value(LOC);
             ret = PrintBuffer(*proto_reshaped, pctx, 0);
             ret.add_frame(PrintStyle(style), proto_reshaped->get_shape(),
                           proto_reshaped->compute_depth());
           }
       else
           {
             if (!(pctx.get_style() & PST_QUOTE_CHARS && val->is_char_array()))
                {
                  ret.add_frame(pctx.get_style(), val->get_shape(),
                                val->compute_depth());
                }
           }
      }

   ret.get_info().int_len  = ret.get_column_count();
   ret.get_info().real_len = ret.get_column_count();
   return ret;
}
//----------------------------------------------------------------------------

