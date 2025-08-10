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

#include <sys/types.h>

#include "Bif_F12_PARTITION_PICK.hh"
#include "CDR_string.hh"
#include "CharCell.hh"
#include "ComplexCell.hh"
#include "Common.hh"
#include "Error.hh"
#include "FloatCell.hh"
#include "IndexExpr.hh"
#include "IndexIterator.hh"
#include "IntCell.hh"
#include "IO_Files.hh"
#include "LvalCell.hh"
#include "Macro.hh"
#include "Output.hh"
#include "PointerCell.hh"
#include "Parallel.hh"
#include "PrintOperator.hh"
#include "Quad_XML.hh"
#include "StateIndicator.hh"
#include "SystemVariable.hh"
#include "UCS_string.hh"
#include "UserFunction.hh"
#include "Value.hh"
#include "ValueHistory.hh"
#include "Workspace.hh"

#include "Workspace.icc"

extern uint64_t top_of_memory();

uint64_t Value::value_count = 0;
uint64_t Value::total_ravel_count = 0;

// most static members of class Value are defined in StaticObjects.cc

_deleted_value * Value::deleted_values = 0;

int Value::deleted_values_count = 0;

uint64_t Value::fast_new_count = 0;

uint64_t Value::slow_new_count = 0;

uint64_t Value::alloc_size = 0;

//----------------------------------------------------------------------------
void
Value::init_ravel()
{
   owner_count = 0;
   fetcher = &cell_fetcher;
   pointer_cell_count = 0;
   nz_subcell_count = 0;
   check_ptr = 0;
   IntCell::z0(short_value);
   ravel = short_value;

   ++value_count;
   if (Quad_SYL::value_count_limit &&
       Quad_SYL::value_count_limit < APL_Integer(value_count))
      {
        MORE_ERROR() <<
"the system limit on the APL value count (as set in ⎕SYL["
<< (Quad_SYL::SYL_VALUE_COUNT_LIMIT + Workspace::get_IO())
<< ";2]) was reached\n"
"(and to avoid lock-up, this system limit in ⎕SYL was automatically cleared).";

        // reset the limit so that we don't get stuck here.
        //
        Quad_SYL::value_count_limit = 0;
        set_attention_raised(LOC);
        set_interrupt_raised(LOC);
      }

const ShapeItem length = shape.get_volume();

   // small values always succeed...
   //
   if (length <= cfg_SHORT_VALUE_LENGTH_WANTED)
      {
        check_ptr = charP(this) + 7;
        return;
      }

   // large Value. If the compiler uses 4-byte pointers, then do not allow APL
   // values that are too large. The reason is that new[] may return
   // a non-0 pointer (thus pretending everything is OK), but a subsequent
   // attempt to initialize the value might then throw a segfault.
   //
   enum { MAX_LEN = 2000000000 / sizeof(Cell) };
   if (length > MAX_LEN && sizeof(void *) < 6)
      throw_apl_error(E_WS_FULL, alloc_loc);

   if (Quad_SYL::ravel_count_limit &&
       Quad_SYL::ravel_count_limit < APL_Integer(total_ravel_count + length))
      {
CERR << "*** Quad_SYL::ravel_count_limit hit ***" << endl;
        // make sure that the value is properly initialized
        //
        new (&shape) Shape();
        ravel = short_value;
        IntCell::zI(ravel, 42);

        MORE_ERROR() <<
"the system limit on the total ravel size (as set in ⎕SYL["
<< (Quad_SYL::SYL_RAVEL_BYTES_LIMIT + Workspace::get_IO())
<< ";2]) was reached\n"
"(and to avoid lock-up, this system limit in ⎕SYL was automatically cleared).";
        // reset the limit so that we don't get stuck here.
        //
        Quad_SYL::ravel_count_limit = 0;
        set_attention_raised(LOC);
        set_interrupt_raised(LOC);
      }

   alloc_size = length * sizeof(Cell);
   ravel = reinterpret_cast<Cell *>(new char[alloc_size]);

/*
   ravel = 0;   // assume new() fails
// try
//    {
        ravel = reinterpret_cast<Cell *>(new char[length * sizeof(Cell)]);
        if (ravel == 0)   // new failed (without trowing an exception)
           {
              Log(LOG_Value_alloc)
                 CERR << "new char[" << (length * sizeof(Cell))
                      << "] (aka. long ravel allocation) returned 0 at " LOC
                      << endl;

              MORE_ERROR() << "The instatiation of a Value object succeeded, "
                              "but allocation of its (large) ravel failed.";
              new (&shape) Shape();
              ravel = short_value;
              throw_apl_error(E_WS_FULL, alloc_loc);
           }
      }
   catch (...)
      {
        // for some unknown reason, this object gets cleared after
        // throwing the E_WS_FULL below (which destroys the prev and
        // next pointers. We therefore unlink() the object here (where
        // prev and next pointers are still intact).
        //
        unlink();

        Log(LOG_Value_alloc)
           {
             CERR << "new char[" << (length * sizeof(Cell))
                  << "] (aka. long ravel allocation) threw an exception at " LOC
                  << endl;
           }

        MORE_ERROR() << "The instatiation of a Value object succeeded, "
                        "but allocation of its (large) ravel failed.";
        new (&shape) Shape();
        ravel = short_value;
        throw_apl_error(E_WS_FULL, alloc_loc);
      }
*/

   // init the first ravel element to (prototype) 0 so that we can avoid
   // many empty checks all over the place
   //
   IntCell::z0(&get_wproto());
   check_ptr = charP(this) + 7;
   total_ravel_count += length;
}
//----------------------------------------------------------------------------
bool
Value::check_WS_FULL(const char * args, ShapeItem requested_cell_count,
                     const char * loc)
{
const int64_t used_memory
               = (total_ravel_count + requested_cell_count) * sizeof(Cell)
               + (value_count + 1) * sizeof(Value)
               + Workspace::SI_entry_count() * sizeof(StateIndicator);

   if (int64_t((Quad_WA::total_memory*Quad_WA::WA_scale/100)) >
       (used_memory + Quad_WA::WA_margin))   return false;   // OK

   Log(LOG_Value_alloc) CERR
   << "    value_count:       " << value_count             << endl
   << "    total_ravel_count: " << total_ravel_count       << " cells" << endl
   << "    new cell_count:    " << requested_cell_count    << " cells" << endl
   << "    total_memory:      " << Quad_WA::total_memory   << " bytes" << endl
   << "    used_memory:       " << used_memory             << " bytes" << endl
   << "    ⎕WA margin:        " << Quad_WA::WA_margin      << " bytes" << endl
   << "    ⎕WA scale:         " << Quad_WA::WA_scale       << "%" << endl

           << " at " << LOC << endl;
   return true;
}
//----------------------------------------------------------------------------
void
Value::catch_Error(const Error & error, const char * args, const char * loc)
{
   Log(LOG_Value_alloc)   CERR << "Ravel allocation failed" << endl;
   MORE_ERROR() << "new Value(" << args
                << ") failed (APL error in ravel allocation)";
   throw error;   // rethrow
}
//----------------------------------------------------------------------------
void
Value::catch_exception(const exception & ex, const char * args,
                      const char * caller,  const char * loc)
{
const int64_t used_memory
               = total_ravel_count * sizeof(Cell)
               + value_count * sizeof(Value)
               + Workspace::SI_entry_count() * sizeof(StateIndicator);

   Log(LOG_Value_alloc)
      CERR << "Value_P::Value_P(" << args << ") failed at " << loc
           << " (caller: "        << caller << ")" << endl
           << " what: "           << ex.what() << endl
           << " initial sbrk(): 0x" << hex << Quad_WA::initial_sbrk << endl
           << " current sbrk(): 0x" << top_of_memory() << endl
           << " alloc_size:     0x" << alloc_size << dec << " ("
                                    << alloc_size << ")" << endl
           << " used memory:    0x" << hex  << used_memory << dec
                                    << " (" << used_memory << ")" << endl;

   MORE_ERROR() << "new Value(" << args << ") failed (" << ex.what() << ")";
   WS_FULL;
}
//----------------------------------------------------------------------------
void
Value::catch_ANY(const char * args, const char * caller, const char * loc)
{
   Log(LOG_Value_alloc)
      CERR << "Value_P::Value_P(Shape " << args << " failed at " << loc
           << " (caller: " << caller << ")" << endl;
   MORE_ERROR() << "new Value(" << args << ") failed (ANY)";
   WS_FULL;
}
//----------------------------------------------------------------------------
Value::Value(const char * loc)
   : DynamicObject(loc, &all_values),
     flags(VF_NONE),
     valid_ravel_items(0)
{
   ADD_EVENT(this, VHE_Create, 0, loc);
   init_ravel();
}
//----------------------------------------------------------------------------
Value::Value(const Cell & cell, const char * loc)
   : DynamicObject(loc, &all_values),
     flags(VF_NONE),
     valid_ravel_items(0)
{
   ADD_EVENT(this, VHE_Create, 0, loc);
   init_ravel();

   set_ravel_Cell(0, cell);
   check_value(LOC);
}
//----------------------------------------------------------------------------
Value::Value(ShapeItem sh, const char * loc)
   : DynamicObject(loc, &all_values),
     shape(sh),
     flags(VF_NONE),
     valid_ravel_items(0)
{
   ADD_EVENT(this, VHE_Create, 0, loc);
   init_ravel();
}
//----------------------------------------------------------------------------
Value::Value(const Shape & sh, const char * loc)
   : DynamicObject(loc, &all_values),
     shape(sh),
     flags(VF_NONE),
     valid_ravel_items(0)
{
   ADD_EVENT(this, VHE_Create, 0, loc);
   init_ravel();
}
//----------------------------------------------------------------------------
Value::Value(const Shape & sh, uint64_t * bits, const char * loc)
   : DynamicObject(loc, &all_values),
     shape(sh),
     fetcher(&packed_fetcher),
     owner_count(0),
     pointer_cell_count(0),
     flags(VF_packed),
     valid_ravel_items(sh.get_nz_volume()),
     nz_subcell_count(0),
     ravel(reinterpret_cast<Cell *>(bits))
{
   ADD_EVENT(this, VHE_Create, 0, loc);
   check_ptr = charP(this) + 7;

   if (ravel)   return;   // caller has allocated 

   // round the size up to the next 64 bit boundary.
const size_t uint64_count = (sh.get_nz_volume() + 63) >> 6;
   bits = new uint64_t[uint64_count];
   loop(u, uint64_count)   bits[u] = 0;
   ravel = reinterpret_cast<Cell *>(bits);
   set_complete();
}
//----------------------------------------------------------------------------
Value::Value(const UCS_string & ucs, const char * loc)
   : DynamicObject(loc, &all_values),
     shape(ucs.size()),
     flags(VF_NONE),
     valid_ravel_items(0)
{
   ADD_EVENT(this, VHE_Create, 0, loc);
   init_ravel();

   set_proto_Spc();                          // prototype
   loop(l, ucs.size())   next_ravel_Char(ucs[l]);
   set_complete();
}
//----------------------------------------------------------------------------
Value::Value(const UTF8_string & utf, const char * loc)
   : DynamicObject(loc, &all_values),
     shape(utf.size()),
     flags(VF_NONE),
     valid_ravel_items(0)
{
   ADD_EVENT(this, VHE_Create, 0, loc);
   init_ravel();

   set_proto_Spc();                          // prototype
   loop(l, utf.size())   next_ravel_Char(Unicode(utf[l] & 0xFF));
   set_complete();
}
//----------------------------------------------------------------------------
Value::Value(const CDR_string & ui8, const char * loc)
   : DynamicObject(loc, &all_values),
     shape(ui8.size()),
     flags(VF_NONE),
     valid_ravel_items(0)
{
   ADD_EVENT(this, VHE_Create, 0, loc);
   init_ravel();

   set_proto_Spc();                          // prototype
   loop(l, ui8.size())   next_ravel_Char(Unicode(ui8[l]));
   set_complete();
}
//----------------------------------------------------------------------------
Value::Value(const PrintBuffer & pb, const char * loc)
   : DynamicObject(loc, &all_values),
     shape(pb.get_row_count(), pb.get_column_count()),
     flags(VF_NONE),
     valid_ravel_items(0)
{
   ADD_EVENT(this, VHE_Create, 0, loc);
   init_ravel();

   set_proto_Spc();                          // prototype

const ShapeItem height = pb.get_row_count();
const ShapeItem width = pb.get_column_count();

   loop(y, height)
   loop(x, width)   next_ravel_Char(pb.get_char(x, y));

   set_complete();
}
//----------------------------------------------------------------------------
Value::Value(const char * loc, const Shape * sh)
   : DynamicObject(loc, &all_values),
     shape(ShapeItem(sh->get_rank())),
     flags(VF_NONE),
     valid_ravel_items(0)
{
   ADD_EVENT(this, VHE_Create, 0, loc);
   init_ravel();

   IntCell::z0(&get_wproto());   // prototype

   loop(r, sh->get_rank())   next_ravel_Int(sh->get_shape_item(r));

   set_complete();
}
//----------------------------------------------------------------------------
Value::~Value()
{
   ADD_EVENT(this, VHE_Destruct, 0, LOC);
   unlink();

   if (flags & VF_packed)
      {
        uint8_t * bits = reinterpret_cast<uint8_t *>(ravel);
        delete[] bits;
        ravel = 0;
        Assert(check_ptr == charP(this) + 7);
        check_ptr = 0;
        return;
      }

const ShapeItem length = nz_element_count();

#if APL_Float_is_class

   //  APL_Float is a class, therefore we need to release all Cells

#else

   // APL_Float is NOT a class, Therefore release only PointerCells
   if (get_pointer_cell_count() > 0)

#endif
      {
        Cell * cZ = &get_wfirst();
        loop(c, length)
            {
              if (get_pointer_cell_count() == 0)   break;
              cZ++->release(LOC);
            }
      }

   Assert1(get_pointer_cell_count() == 0);

   --value_count;

   if (ravel == 0)   return;   // new() failed

   if (ravel != short_value)   // long value
      {
        total_ravel_count -= length;
        delete [] ravel;
      }

   Assert(check_ptr == charP(this) + 7);
   check_ptr = 0;
}
//----------------------------------------------------------------------------
Value_P
Value::get_cellrefs(const char * loc)
{
   /* Create a non-nested (!) (left-) value left_Z from this (right-)value.
      left_Z has the same shape and consists entirely of LvalCells that point
      to the corresponding cells of this value.

      NOTE that any PointerCell in this are NOT handled differently than
      simple cells (and get_cellrefs() will simply point to them)
      rather than recursing into its sub-value. For that reason:

      1. the result left_Z below is flat (not nested), and
      2. assign_cellrefs() below must (at its later point in time),call
         get_cellrefs() when it encounters an LvalCell whose target is a
         PointerCell.

      The reason for delaying the recursion into sub-values is that the
      subsequent processing of left_Z may erase those sub-values and therefore
      doing it now may render useless later.
   */

Value_P left_Z(get_shape(), loc);

   // next_ravel_Cell() is 0 for empty values.
   // Therefore empty values (with element_count == 0) must use
   // set_ravel_Cell() instead of next_ravel_Cell() !
   //
   if (const ShapeItem ec = element_count())
      {
        loop(e, ec)
            {
              Cell & right_cell = get_wravel(e);
              const LvalCell left_cell(&right_cell, this);
              left_Z->next_ravel_Cell(left_cell);
            }
      }
   else   // prototype
      {
        left_Z->set_ravel_Cell(0, LvalCell(&get_wproto(), this));
      }

   left_Z->check_value(LOC);
   return left_Z;
}
//----------------------------------------------------------------------------
void
Value::assign_cellrefs(Value_P new_value)
{
   // assign new_value to this left value

const ShapeItem value_count = new_value->nz_element_count();
const ShapeItem dest_count  = element_count();
   if (dest_count == 0)   return;   // nothing to assign

const int dest_incr = (dest_count == 1)                    ? 0 : 1;
const int src_incr  = (new_value->nz_element_count() == 1) ? 0 : 1;

   if (src_incr  &&   // this is not a acalar or 1-element value, and
       dest_incr &&   // new_value  is not a acalar or 1-element value, and
       !this->conforms_to(*new_value))   // non-trivial shape mismatch
      {
        // NOTE:  M←3 2ρι6 ◊ (1 0/M) ← 'abc' succeeds in IBM APL2, even
        //        though the shapes of 1 0/M and 'abc' differ. We therefore
        //
        MORE_ERROR() << "in (f Z)←B: length(X⊃F Z) is " << dest_count
                     << ", but length(X⊃V) is " << value_count
                     << " (for some X)";
        LENGTH_ERROR;
      }

   /* this: a value containing LvalCells and possibly PointerCells.

      test with:

      C←"001011010010010110010100100001000010001101" ◊ ((C='0')/C)←' ' ◊ C
      K←'RED' 'WHITE' 'BLUE'   ◊ (↑K)←'YELLOW' ◊ K
      V←'v' 'vw' 'vwx' 'vwxyz' ◊ (2↑¨V)←5      ◊ 4 ⎕CR V

    */

   // consistency check: all items are LvalCells
   //
   check_lval_consistency();

   if (is_scalar() && !new_value->is_scalar())
      {
        const Cell * C0 = &get_cscalar();
        if (!C0->is_lval_cell())   LEFT_SYNTAX_ERROR;
        const LvalCell * LVC0 = reinterpret_cast<const LvalCell *>(C0);
        if (Cell * target = LVC0->get_lval_value())   // valid right Cell
           {
             Value * owner = LVC0->get_cell_owner();
             target->release(LOC);   // free sub-values etc (if any)
             new (target)   PointerCell(new_value.get(), *owner);
           }
        return;
      }

const Cell * src = &new_value->get_cfirst();
   loop(d, dest_count)
      {
        Cell & left_C = get_wravel(d);
        if (left_C.is_pointer_cell())
           {
             /*
                NOTE: At this point left_C is one of the left cells in a
                      selective assignment, say:

                      (f1 f2 ... fn VAR) ← new_value.

                The initial cellrefs (i.e. of VAR) contains no PointerCells
                (though possibly Lval cells pointing to PointerCells).

                Therefore left_C must have been created by one of the fi and
                then all sub-values of left_C must also be Lval Cells.
              */
              Value * sub = left_C.get_pointer_value().get();   // right-value
              loop(s, sub->nz_element_count())
                  {
                    Cell * Csub = &sub->get_wravel(s);
                    if (!Csub->is_lval_cell())   LEFT_SYNTAX_ERROR;

                    const LvalCell * LVC =
                                     reinterpret_cast<const LvalCell *>(Csub);
                    if (Cell * target = LVC->get_lval_value())   // can be 0!
                       {
                         target->release(LOC);   // free sub-values etc (if any)

                         // erase the pointee when overriding a pointer-cell.
                         //
                         Value * owner = LVC->get_cell_owner();
                         target->init(*src, *owner, LOC);
                       }
                  }
           }
        else if (left_C.is_lval_cell())
           {
             const LvalCell & LVC = reinterpret_cast<const LvalCell &>(left_C);
             if (Cell * target = left_C.get_lval_value())   // target can be 0!
                {
                  target->release(LOC);   // free sub-values etc (if any)

                  // erase the pointee when overriding a pointer-cell.
                  //
                  Value * owner = LVC.get_cell_owner();
                  target->init(*src, *owner, LOC);
                }
           }
        else   LEFT_SYNTAX_ERROR;
        src += src_incr;
      }
}
//----------------------------------------------------------------------------
void
Value::check_lval_consistency() const
{
const ShapeItem ec = nz_element_count();

   // left value rules:
   //
   // 1. every item is either an LvalCell, or a PointerCell
   // 2. for every LvalCells:
   // 2a. either cell is 0 and owner is 0,
   // 2c, or cell is ≠0 and owner is ≠0 and cell lies in the ravel of owner
   //
   loop(e, ec)
       {
         const Cell * cell = &get_cravel(e);   // rarely
         if (cell->is_pointer_cell())
            {
              cell->get_pointer_value()->check_lval_consistency();
            }
         else if (cell->is_lval_cell())
            {
              const LvalCell * LVC = reinterpret_cast<const LvalCell *>(cell);
              LVC->check_consistency();
            }
         else       // error, e.g. 3{⍺+2←⍵}4
            {
              MORE_ERROR() << "mal-formed selective specification";
              SYNTAX_ERROR;
            }
       }
}
//----------------------------------------------------------------------------
Cell *
Value::get_member(const basic_string<const UCS_string *> & members,
                  Value * & owner, bool create_if_needed, bool throw_error)
{
   owner = this;

   // members[members.size() - 1] == this
   //
   for (int m = members.size() - 2; m >= 0; --m)
       {
         if (!owner->is_member())
            {
              UCS_string & more = MORE_ERROR()
                  << "member access: non-structured variable "
                  << *members.back() << " has no member ";
              more.append_members(members, 0);
              if (throw_error)   DOMAIN_ERROR;
              else               return 0;
            }

         const UCS_string & member_ucs = *members[m];
         if (owner->get_rank() != 2)
            {
              if (!throw_error)   return 0;
              UCS_string & more = MORE_ERROR() << "member access: the rank of ";
              more.append_members(members, m);
              more << " is not 2.\n"
                      "Expecting an N×2 matrix of member,value pairs.";
              RANK_ERROR;
            }

         if (owner->get_cols() != 2)
            {
              if (!throw_error)   return 0;
              UCS_string & more = MORE_ERROR()
                 << "member access: the number of columns of ";
              more.append_members(members, m);
              more << " is not 2.\n"
                      "Expecting an N×2 matrix of member,value pairs.";
              LENGTH_ERROR;
            }

         Cell * member_cell = owner->get_member_data(member_ucs);
         if (member_cell)   // existing member
            {
              if (m == 0)   return member_cell; // final member

              // more members coming. Then member_cell should point to a
              // structured sub-member
              //
              if (!member_cell->is_pointer_cell() ||
                  !member_cell->get_pointer_value()->is_member())
                 {
                   if (!throw_error)   return 0;
                   UCS_string & more = MORE_ERROR()
                                << "member access: member " << member_ucs
                                << " exists in ";
                   more.append_members(members, m);
                   more << "but its (internal) value is not nested";
                   DOMAIN_ERROR;
                 }

              // next member
              //
              owner = member_cell->get_pointer_value().get();
            }
         else   // member does not exist
            {
              if (!create_if_needed)   // give up
                 {
                   if (!throw_error)   return 0;   // silent

                   UCS_string & more = MORE_ERROR()
                                       << "member access: structure ";
                   more.append_members(members, m + 1);
                   more << " has no member '" << member_ucs << "'.";
                   VALUE_ERROR;
                 }

              // create new member row...

              // find an unused slot in owner. get_new_member() is guaranteed
              // to find one (by possibly increasing the ravel of owner.
              //
              Cell * member_data = owner->get_new_member(member_ucs);
              if (m == 0)   return member_data;

              // more members coming
              //
              Value_P member_sub = EmptyStruct(LOC);
              new (member_data)   PointerCell(member_sub.get(), *owner);
              owner = member_sub.get();
            }   // if (member_cell == 0)
       }

   FIXME;   // not reached
}
//----------------------------------------------------------------------------
ShapeItem
Value::get_all_members_count() const
{
   Assert(is_structured());

ShapeItem count = 0;
const ShapeItem rows = get_rows();
   loop(r, rows)
       {
         const Cell & member_name_cell = get_cravel(2*r);
         if (!member_name_cell.is_pointer_cell())   continue;  // unused row
            {
              ++count;
              const Cell & member_data_cell = get_cravel(2*r + 1);
              if (member_data_cell.is_pointer_cell())   // nested value
                 {
                   const Value & subval = *member_data_cell.get_pointer_value();
                   if (!subval.is_structured())   continue;
                   count += subval.get_all_members_count();
                 }
            }
       }

   return count;
}
//----------------------------------------------------------------------------
void
Value::used_members(std::basic_string<ShapeItem> & result, bool sorted) const
{
   Assert(is_structured());

   // 1. colllect the used columns...
   //
const ShapeItem rows = get_rows();
   result.reserve(rows);
   loop(r, rows)
       {
         const Cell & member_name_cell = get_cravel(2*r);
         if (!member_name_cell.is_pointer_cell())   continue;   // unused

         result.push_back(r);
       }

   if (!sorted)   return;

   // make result[m] ≤ result[m+k] for all k > 0 and all m ≥ 0
   //
   loop(m, result.size())   // for all m
       {
         ShapeItem small = m;   // assume that m is already the smallest
         for (size_t j = m + 1; j < result.size(); ++j)   // for all j = m + k
             {
               if (get_cravel(2*result[small]).greater(get_cravel(2*result[j])))
                  small = j;
             }

         if (small != m)   // there was an even smaller item above m
            {
              const ShapeItem tmp = result[m];
              result[m] = result[small];
              result[small] = tmp;
            }
       }

}
//----------------------------------------------------------------------------
void
Value::sorted_members(std::basic_string<ShapeItem> & result,
                      const Unicode * filters) const
{
   // we take advantage of the fact that the positions in the member
   // prefixes are limited, so we can sort them in linear time.
   //
   // CAUTION: This function works only for members with a _NNN prefix
   // indication the sorting order (as produced by ⎕XML) @@@
   //
   Assert(is_structured());

const Unicode filters_all[] = { UNI_DELTA_UNDERBAR, UNI_DELTA,
                                UNI_UNDERSCORE,     Unicode_0 };

   if (filters == 0)   filters = filters_all;

const ShapeItem rows = get_rows();
const ShapeItem max_member_count = get_all_members_count() + 1;   // ⎕IO←0 or 1
const ShapeItem alloc = 2*max_member_count; // one for ⍙, one for ∆ and _
ShapeItem * sorted = new ShapeItem[alloc];
   if (sorted == 0)   WS_FULL;

ShapeItem * sorted_attributes = sorted;      // ⍙
ShapeItem * sorted_nodes = sorted + max_member_count;   // ∆ and _

   loop(a, alloc)   sorted[a] = -1;

   loop(r, rows)
       {
         const Cell & member_name_cell = get_cravel(2*r);
         if (!member_name_cell.is_pointer_cell())   continue;   // unused

         const Value & val = *member_name_cell.get_pointer_value();
         ShapeItem member_pos;     // the position in the XML file
         Unicode category;
         UCS_string name;
         Quad_XML::split_name(&category, &member_pos, &name, val);

         Assert(member_pos <= max_member_count);
         loop(f, 4)   // at most filters ⍙, ∆, and _
             {
               const Unicode filter = filters[f];
               if (filter == Unicode_0)   break;
               if (filter == category)
                  {
                    if (category == UNI_DELTA_UNDERBAR)
                       sorted_attributes[member_pos] = r;
                    else
                       sorted_nodes[member_pos] = r;
                    break;
                  }
             }
       }

   loop(a, alloc)
       {
         if (sorted[a] != -1)   result.push_back(sorted[a]);
       }

   delete [] sorted;
}
//----------------------------------------------------------------------------
const Cell *
Value::get_member_data(const UCS_string & member) const
{
const ShapeItem rows = get_rows();

ShapeItem row = member.FNV_hash() % rows;
   loop(r, rows)   // loop over member rows
       {
         if (++row >= rows)   row = 0;
         const Cell & name_cell = get_cravel(2*row);

         if (name_cell.is_pointer_cell() &&
             name_cell.get_pointer_value()->equal_string(member))
            return &get_cravel(2*row + 1);
       }

   // member row not found
   //
   return 0;
}
//----------------------------------------------------------------------------
Cell *
Value::get_member_data(const UCS_string & member_name)
{
const ShapeItem rows = get_rows();

ShapeItem row = member_name.FNV_hash() % rows;

   // loop over the member rows. Since we hash the start position this will
   // typically return quickly and the slow case where all rows are tested
   // will throw a VALUE ERROR right after returning 0.
   //
   loop(r, rows)
       {
         if (++row >= rows)   row = 0;
         const Cell & name_cell = get_cravel(2*row);

         if (name_cell.is_pointer_cell() &&
             name_cell.get_pointer_value()->equal_string(member_name))
            return &get_wravel(2*row + 1);
       }

   // member row not found
   //
   return 0;
}
//----------------------------------------------------------------------------
ShapeItem
Value::get_member_count() const
{
   // this function shall only be called for values that have the same
   // layout as structured variables.
   //
   // it checks if the first column of this value looks OK and returns
   // the number of used rows.
   //
   Assert(get_cols() == 2);

const ShapeItem rows = get_rows();
ShapeItem valid_rows = 0;

   loop(r, rows)
      {
        const Cell & name_cell = get_cravel(2*r);

        /*
           name_cell must  be:

           i.   integer 0       for unused rows, or
           ii.  any character   for 1-character member names, or else
           iii. any string      for other member names

         */
        if (name_cell.is_integer_cell())   // maybe i.
           {
             if (name_cell.get_int_value() != 0)   // invalid unused
                {
                   MORE_ERROR() << "invalid member name in row "
                                << r << " (integer)";
                   DOMAIN_ERROR;
                }
           }
        else
           {
             ++valid_rows;    // assume ii. or iii.

             if (name_cell.is_pointer_cell())   // maybe iii ?
                {
                  if (!name_cell.get_pointer_value()->is_char_string())   // no.
                     {
                       MORE_ERROR() << "invalid member name in row "
                                    << r << " (nested non-string)";
                      DOMAIN_ERROR;
                     }
                }
             else if (!name_cell.is_character_cell())   // unexpected type
                     {
                       MORE_ERROR() << "invalid member name in row "
                                    << r << " (type)";
                       DOMAIN_ERROR;
                     }
           }
      }

   return valid_rows;
}
//----------------------------------------------------------------------------
Cell *
Value::get_new_member(const UCS_string & new_member_name)
{
   // find an unused slot in owner. Return its data cell (= its name cell + 1)
   //
   for (;;)   // until an unused row was found
       {
         const ShapeItem rows = get_rows();   // changed by double_ravel() !
         ShapeItem row = new_member_name.FNV_hash() % rows;
         loop(r, 14)
             {
               if (++row >= rows)   row = 0;
               Cell * cell = &get_wravel(2*row);
               if (cell->is_integer_cell() &&
                   cell->get_int_value() == 0)   // unused row
                  {
                    Value_P name_val(new_member_name, LOC);
                    new (cell) PointerCell(name_val.get(), *this);
                    return cell + 1;
                  }
             }

          /* at this point a cluster of 14 consecutive used rows was hit.
             Common wisdom (Knuth) has it, that a hash table should maintain
             fill level (i.e. used rows ÷ all rows) below 50%.

             For a hash table with N rows and an unknown fill level (like this
             one) we could either:

             A1. first determine the fill level (an O(N) operation), and
             A2. double the table size (another O(n) operation) if the fill
                 level is > 50 %,

                 or:

             B.   double the table size regardless of the fill level (and
                  an 1:16384 chance that the doubling was premature).


             We choose B. because it is simpler and because prematurely
             doubling the table could very well amortize itself later.
          */

         double_ravel(LOC);
       }
}
//----------------------------------------------------------------------------
void
Value::double_ravel(const char * loc)
{
Cell * const old_ravel = ravel;

   Assert(is_member());
const char * del = 0;
   if (ravel != short_value)   del = reinterpret_cast<char *>(ravel);

   Assert(get_rank() == 2);
   Assert(get_cols() == 2);

const ShapeItem old_rows  = get_rows();
const ShapeItem new_rows  = 2*old_rows;
const ShapeItem new_cells = 2*new_rows;

Cell * doubled = new Cell[new_cells];
   loop(n, new_cells)   IntCell::z0(doubled + n);
   valid_ravel_items = new_cells;
   shape.set_shape_item(0, new_rows);
   ravel = doubled;

   loop(r, old_rows)
       {
         // transfer the member name. The old member_name_cell must be released
         // since get_new_member() below creates a new one in the new ravel.
         //
         Cell & member_name_cell = old_ravel[2*r];
         if (!member_name_cell.is_pointer_cell())   continue;   // unused
         Assert(member_name_cell.is_pointer_cell());
         UCS_string member_name(*member_name_cell.get_pointer_value());
         member_name_cell.release(LOC);

         // transfer the member value. memcpy() should work because ownership
         // remains the same
         //
         void * dest = get_new_member(member_name);
         void * old_member_data_cell = old_ravel + 2*r + 1;
         memcpy(dest, old_member_data_cell, sizeof(Cell));
       }

   delete [] del;   // del is char *, so no cell destructor is called
}
//----------------------------------------------------------------------------
Value *
Value::get_lval_cellowner() const
{
   /* this Value may or may not be a left value. If it is, then its
      top-level (!) ravel is a mix of:

      1.  PointerCells pointing to (not yet get_cellrefs()ed) right Values,
      2a. valid LvalCells (from get_cellrefs()), or
      2b. invalid LvalCells(0, 0) (e.g. from ↑ overtake).

      The first case 1. or 2a. (if any) determines the cellowner
    */
   loop(e, nz_element_count())
      {
        const Cell * cell = &get_cravel(e);
        if (cell->is_pointer_cell())   // case 1.
           {
             return  cell->get_pointer_value()->get_lval_cellowner();
           }

        if (cell->is_lval_cell())      // case 2a. or 2b.
           {
             const LvalCell * lval = reinterpret_cast<const LvalCell *>(cell);
             if (lval->get_cell_owner())   return lval->get_cell_owner();
           }
      }

   return 0;   // not found (this is most likely not a left value)
}
//----------------------------------------------------------------------------
bool
Value::is_or_contains(const Value * value, const Value * sub)
{
   if (value == 0)     return false;   // value is not a valid value
   if (value == sub)   return true;    // value is sub

   for (ConstRavel_P v(*value, true); +v; ++v)
      {
        if (v->is_pointer_cell() &&
            is_or_contains(v->get_pointer_value().get(), sub))   return true;
      }

   return false;
}
//----------------------------------------------------------------------------
void
Value::flag_info(const char * loc, ValueFlags flag, const char * flag_name,
                 bool set) const
{
const char * sc = set ? " SET " : " CLEAR ";
const int new_flags = set ? flags | flag : flags & ~flag;
const char * chg = flags == new_flags ? " (no change)" : " (changed)";

   CERR << "Value " << voidP(this)
        << sc << flag_name << " (" << HEX(flag) << ")"
        << " at " << loc << " now = " << HEX(new_flags)
        << chg << endl;
}
//----------------------------------------------------------------------------
void
Value::init()
{
   Log(LOG_startup)
      CERR << "Max. Rank            is " << MAX_RANK << endl;
};
//----------------------------------------------------------------------------
bool
Value::is_structured() const
{
   if (!is_member())      return false;
   if (get_rank() != 2)   return false;
   if (get_cols() != 2)   return false;

const ShapeItem rows = get_rows();
   loop(r, rows)
       {
         const Cell & key = get_cravel(2*r);
         if (key.is_integer_cell() &&
             key.get_int_value() == 0)                    ;   // OK: unused row
         else if (key.is_pointer_cell() &&
             key.get_pointer_value()->is_char_vector())   ;   // OK: key
         else return false;
       }

   return true;    // OK: all rows were OK
}
//----------------------------------------------------------------------------
void
Value::add_member(const UCS_string & member_name, Value * member_value)
{
   if (!is_structured())
      {
        MORE_ERROR() << "attempt to add member (" << member_name
                     << "to a non-structured value";
        DOMAIN_ERROR;
      }

basic_string<const UCS_string *> members;
   members.push_back(&member_name);
   members.push_back(&member_name);   // ignored

Value * member_owner = 0;
Cell * data = get_member(members, member_owner, true, false);
   Assert(member_owner);
   Assert(member_owner == this);
   new (data) PointerCell(member_value, *this);
}
//----------------------------------------------------------------------------
void
Value::mark_all_dynamic_values()
{
   for (DynamicObject * dob = DynamicObject::all_values.get_prev();
        dob != &all_values; dob = dob->get_prev())
       {
         dob->rValue().set_marked();
       }
}
//----------------------------------------------------------------------------
void
Value::unmark() const
{
   clear_marked();
   if (is_packed())   return;

const ShapeItem ec = nz_element_count();
const Cell * C = &get_cfirst();
   loop(e, ec)
      {
        if (C->is_pointer_cell())   C->get_pointer_value()->unmark();
        ++C;
      }
}
//----------------------------------------------------------------------------
void
Value::rollback(ShapeItem items, const char * loc)
{
   ADD_EVENT(this, VHE_Unroll, 0, loc);

   // this value has only items < nz_element_count() valid items.
   // init the rest...
   //
   while (more())   next_ravel_0();

   const_cast<Value *>(this)->alloc_loc = loc;
}
//----------------------------------------------------------------------------
void
Value::erase_all(ostream & out)
{
   for (const DynamicObject * dob = DynamicObject::all_values.get_next();
        dob != &DynamicObject::all_values; dob = dob->get_next())
       {
         const Value & value = dob->rValue();
         out << "erase_all() sees Value:" << endl
             << "  Allocated by " << value.where_allocated() << endl
             << "  ";
         value.list_one(CERR, false);
       }
}
//----------------------------------------------------------------------------
int
Value::erase_stale(const char * loc)
{
int count = 0;

   Log(LOG_Value__erase_stale)
      CERR << endl << endl << "erase_stale() called from " << loc << endl;

   for (DynamicObject * dob = all_values.get_next();
        dob != &all_values; dob = dob->get_next())
       {
         Value & v = dob->rValue();
         if (dob == dob->get_next())   // a loop
            {
              CERR << "A loop in DynamicObject::all_values (detected in "
                      "function erase_stale() at object "
                   << voidP(dob) << "): " << endl;
              all_values.print_chain(CERR);
              CERR << endl;

              // use separate CERR << in case this crashes
              //
              CERR << " DynamicObject: " << dob << endl;
              CERR << " Value:         " << v   << endl;
              CERR << v                         << endl;
            }

         Assert(dob != dob->get_next());
         if (v.owner_count)   continue;

         ADD_EVENT(&v, VHE_Stale, v.owner_count, loc);

         Log(LOG_Value__erase_stale)
            {
              CERR << "Erasing stale Value "
                   << voidP(dob) << ":" << endl
                   << "  Allocated by " << v.where_allocated() << endl
                   << "  ";
              v.list_one(CERR, false);
            }

         // count it unless we know it is dirty
         //
         ++count;

         dob->unlink();

         // v->erase(loc) could mess up the chain, so we start over
         // rather than continuing
         //
         dob = &all_values;
       }

   return count;
}
//----------------------------------------------------------------------------
ostream &
Value::list_one(ostream & out, bool show_owners) const
{
   if (flags)
      {
        out << "   Flags =";
        char sep = ' ';
        if (is_member())     { out << sep << "MEMBER";     sep = '+'; }
        if (is_complete())   { out << sep << "COMPLETE";   sep = '+'; }
        if (is_marked())     { out << sep << "MARKED";     sep = '+'; }
        if (is_packed())     { out << sep << "PACKED";     sep = '+'; }
      }
   else
      {
        out << "   Flags = NONE";
      }

   out << ", ⍴" << get_shape() << " ≡" << compute_depth() << ":" << endl;
   print(out);
   out << endl;

   if (!show_owners)   return out;

   // print owners...
   //
   out << "Owners of " << voidP(this) << ":" << endl;

   Workspace::show_owners(out, *this);

   out << "---------------------------" << endl << endl;
   return out;
}
//----------------------------------------------------------------------------
ostream &
Value::list_all(ostream & out, bool show_owners)
{
int num = 0;
   for (const DynamicObject * dob = all_values.get_prev();
        dob != &all_values; dob = dob->get_prev())
       {
         out << "Value #" << num++ << ":";
         dob->rValue().list_one(out, show_owners);
       }

   return out << endl;
}
//----------------------------------------------------------------------------
ShapeItem
Value::get_enlist_count() const
{
const ShapeItem ec = element_count();
ShapeItem count = ec;

   loop(c, ec)
       {
         const Cell & cell = get_cravel(c);
         if (cell.is_pointer_cell())
            {
               count--;   // the pointer cell
               count += cell.get_pointer_value()->get_enlist_count();
            }
         else if (cell.is_lval_cell())
            {
              Cell * cp = cell.get_lval_value();
              if (cp && cp->is_pointer_cell())
                 {
                   count--;
                   count += cp->get_pointer_value()->get_enlist_count();
                 }
            }
       }

   return count;
}
//----------------------------------------------------------------------------
void
Value::enlist_right(Value & Z) const
{
   // Z is a new (un-initialized) Value that shall be (recursively)
   // initialized with the (non-pointer) items of this (right-) value
   //
   //
   loop(c, element_count())
       {
         const Cell & cell = get_cravel(c);
         if (cell.is_pointer_cell())
            {
              cell.get_pointer_value()->enlist_right(Z);
            }
         else if (!cell.is_lval_cell())
            {
              Z.next_ravel_Cell(cell);
            }
         else   FIXME;
       }
}
//----------------------------------------------------------------------------
void
Value::enlist_left(Value & Z) const
{
   // Z is a new (un-initialized) Value that shall be (recursively)
   // initialized with the (non-pointer) items of this (left-) value
   //
   loop(c, element_count())
       {
         const Cell & cell = get_cravel(c);
         if (cell.is_pointer_cell())
            {
              cell.get_pointer_value()->enlist_left(Z);
            }
         else if (cell.is_lval_cell())
            {
              Cell * target = cell.get_lval_value();
              if (target == 0)
                 {
                   CERR << "0-pointer at " LOC << endl;
                 }
              else if (target->is_pointer_cell())
                 {
                   reinterpret_cast<PointerCell *>(target)->isolate(LOC);
                   target->get_pointer_value()->enlist_left(Z);
                 }
              else 
                 {
                   Z.next_ravel_Cell(cell);
                 }
            }
         else   // neither PointerCell nor LvalCell
            {
              /* cell is a right hand cell in a left expression.
                 This happens when assign_cellrefs() is flat (i.e. always)
                 and this left-value contains pointer-cells (supposedly
                 created AFTER get_celrefs() was called).

                 test with:

                 A←'ZIPPITY' 'DOO' 'DAH' ◊ (ϵA[2])←0 ◊ A
                 A←(10 20 30) 'AB'       ◊ (ϵA)←⍳5   ◊ A
                 A←''                    ◊ (↑A)←33   ◊ ↑A


              */

              // cannot use Z->next_ravel_Cell() since then the cell owner
              // would be Z and not this!
              //
              LvalCell z(const_cast<Cell *>(&cell),
                         const_cast<Value *>(this));
              Z.next_ravel_Cell(z);
            }
       }
}
//----------------------------------------------------------------------------
bool
Value::is_apl_char_vector() const
{
   if (get_rank() != 1)   return false;

   loop(c, get_shape_item(0))
      {
        if (!get_cravel(c).is_character_cell())   return false;

        const Unicode uni = get_cravel(c).get_char_value();
        if (Avec::find_char(uni) == Avec::Invalid_CHT)   // not in ⎕AV
           return false;
      }

   return true;
}
//----------------------------------------------------------------------------
bool
Value::is_char_array() const
{
const Cell * C = &get_cfirst();
   loop(c, nz_element_count())   // also check prototype
      if (!C++->is_character_cell())   return false;   // not char
   return true;
}
//----------------------------------------------------------------------------
bool
Value::NOTCHAR() const
{
   // always test element 0.
   if (!get_cfirst().is_character_cell())   return true;

const ShapeItem ec = element_count();
   for (ShapeItem e = 1; e < ec; ++e)
       if (!get_cravel(e).is_character_cell())   return true;

   // all ravel items are (single) characters.
   return false;
}
//----------------------------------------------------------------------------
bool
Value::is_int_array() const
{
const ShapeItem ec = nz_element_count();
   loop(c, ec)
       {
         if (!get_cravel(c).is_near_int())   return false;
       }

   return true;   // all ravel items are near-int
}
//----------------------------------------------------------------------------
bool
Value::is_complex(bool check_numeric) const
{
const ShapeItem ec = nz_element_count();

   loop(e, ec)
      {
        const Cell & cell = get_cravel(e);
        if (!cell.is_numeric())
           {
             if (check_numeric)    DOMAIN_ERROR;
             else                  continue;
           }
        if (!cell.is_near_real())   return true;
      }

   return false;   // all cells numeric and not complex
}
//----------------------------------------------------------------------------
bool
Value::can_be_compared() const
{
const ShapeItem count = nz_element_count();
   loop(c, count)
      {
       const Cell & cell = get_cravel(c);
       const CellType ctype = cell.get_cell_type();
       if (ctype & (CT_CHAR | CT_INT | CT_FLOAT))   continue;
       if (cell.is_near_real())                     continue;
       if (cell.is_pointer_cell() &&
           cell.get_pointer_value()->can_be_compared())   continue;
       return false;
      }

   return true;
}
//----------------------------------------------------------------------------
bool
Value::equal_string(const UCS_string & ucs) const
{
   if (get_rank() == 1)        // most likely: char vector
      {
        const ShapeItem len = element_count();
        if (ucs.size() != len)   return false;   // wrong length
        loop(u, len)
            if (ucs[u] != get_cravel(u).get_char_value())   // mismatch
               return false;
        return true;
      }
   else if (get_rank() == 0)   // rarely: char scalar
      {
        if (ucs.size() != 1)   return false;   // wrong length
        return ucs[0] == get_cfirst().get_char_value();
      }

   RANK_ERROR;            // never
}
//----------------------------------------------------------------------------
bool
Value::is_simple() const
{
   if (flags & VF_packed)   return true;

const ShapeItem count = element_count();
const Cell * C = &get_cfirst();

   loop(c, count)
       {
         if (C->is_pointer_cell())   return false;
         if (C->is_lval_cell())      return false;
         ++C;
       }

   return true;
}
//----------------------------------------------------------------------------
bool
Value::is_one_dimensional() const
{
   // lrm would not return false if the value itself is, e.g. a matrix.
   // That is wrong, however
   //
   if (get_rank() > 1)   return false;

const ShapeItem count = nz_element_count();
const Cell * C = &get_cfirst();

   loop(c, count)
       {
         if (C->is_pointer_cell())
            {
             Value_P sub_val = C->get_pointer_value();
             if (sub_val->get_rank() > 1)                return false;
             if (!sub_val->is_one_dimensional())         return false;
            }
         ++C;
       }

   return true;   // all items are scalars or vectors
}
//----------------------------------------------------------------------------
APL_types::Depth
Value::compute_depth() const
{
   if (is_scalar())
      {
        if (get_cfirst().is_pointer_cell())
           {
             APL_types::Depth d = get_cfirst().get_pointer_value()
                                     ->compute_depth();
             return 1 + d;
           }

        return 0;
      }

const ShapeItem count = nz_element_count();

APL_types::Depth sub_depth = 0;
   loop(c, count)
       {
         APL_types::Depth d = 0;
         if (get_cravel(c).is_pointer_cell())
            {
              d = get_cravel(c).get_pointer_value()->compute_depth();
            }
         if (sub_depth < d)   sub_depth = d;
       }

   return sub_depth + 1;
}
//----------------------------------------------------------------------------
CellType
Value::flat_cell_types() const
{
int32_t ctypes = 0;

const ShapeItem count = nz_element_count();
   loop(c, count)   ctypes |= get_cravel(c).get_cell_type();

   return CellType(ctypes);
}
//----------------------------------------------------------------------------
CellType
Value::flat_cell_subtypes() const
{
int32_t ctypes = 0;

const ShapeItem count = nz_element_count();
   loop(c, count)   ctypes |= get_cravel(c).get_cell_subtype();

   return CellType(ctypes);
}
//----------------------------------------------------------------------------
CellType
Value::deep_cell_types() const
{
int32_t ctypes = 0;

const ShapeItem count = nz_element_count();
   loop(c, count)
      {
       const Cell & cell = get_cravel(c);
        ctypes |= cell.get_cell_type();
        if (cell.is_pointer_cell())
            ctypes |= cell.get_pointer_value()->deep_cell_types();
      }

   return CellType(ctypes);
}
//----------------------------------------------------------------------------
CellType
Value::deep_cell_subtypes() const
{
int32_t ctypes = 0;

const ShapeItem count = nz_element_count();
   loop(c, count)
      {
       const Cell & cell = get_cravel(c);
        ctypes |= cell.get_cell_subtype();
        if (cell.is_pointer_cell())
           ctypes |= cell.get_pointer_value()->deep_cell_subtypes();
      }

   return CellType(ctypes);
}
//----------------------------------------------------------------------------
Value_P
Value::index(const IndexExpr & IX) const
{
   // if IX.value_count() were 1 (as in A[X] as opposed to A[X1;...]) then
   // it should have been parsed as an axis and Value::index(Value_P X) should
   // have been called instead of this one. This is an internal error.

   if (is_member())
      {
         // for a structured variable VAR, we only allow VAR[1;] to obtain
         // the valid member names...
         //
         if (IX.get_rank() != 2)   RANK_ERROR;
         if (+IX.values[1])   // not elided
            {
              MORE_ERROR() << "member access: second index is not elided";
              LENGTH_ERROR;   // not elided
            }

         if (IX.values[0]->element_count() != 1)
            {
              MORE_ERROR() << "member access: first index too long";
              LENGTH_ERROR;   // not 1 columns
            }

         const APL_Integer col = IX.values[0]->get_cfirst().get_int_value();
         if (col != Workspace::get_IO())           INDEX_ERROR;

         // count the number of member names.
         //
         ShapeItem member_count = 0;
         const ShapeItem rows = get_rows();
         loop(row, rows)
             {
               if (get_cravel(2*row).is_pointer_cell())   ++member_count;
             }

         Value_P Z(member_count, LOC);
         loop(row, rows)
             {
               if (get_cravel(2*row).is_pointer_cell())
                  Z->next_ravel_Cell(get_cravel(2*row));
             }

         if (member_count == 0)   // no valid members (last member )ERASEd)
            new (&Z->get_wfirst()) PointerCell(Idx0(LOC).get(), *Z);

         Z->check_value(LOC);
         return Z;
      }

   Assert(!IX.is_axis());   // should have called index(Value_P X)

   if (get_rank() != IX.get_rank())   RANK_ERROR;   // ISO p. 158

   // Notes:
   //
   // 1.  IX is parsed from right to left:    B[I2;I1;I0]  --> I0 I1 I2
   //     the shapes of this and IX are then related as follows:
   //
   //     this     IX
   //     ---------------
   //     0        rank-1   (rank = IX->value_count())
   //     1        rank-2
   //     ...      ...
   //     rank-2   1
   //     rank-1   0
   //     ---------------
   //
   // 2.  shape Z is the concatenation of all shapes in IX
   // 3.  rank Z is the sum of all ranks in IX

   // construct result rank_Z and shape_Z.
   // We go from higher indices of IX to lower indices (Note 1.)
   //
Shape shape_Z;
   loop(this_r, get_rank())
       {
         const ShapeItem idx_r = get_rank() - this_r - 1;

         Value_P I = IX.values[idx_r];
         if (!I)
            {
              shape_Z.add_shape_item(this->get_shape_item(this_r));
            }
         else
            {
              loop(s, I->get_rank())
                shape_Z.add_shape_item(I->get_shape_item(s));
            }
       }

   // check that all indices are valid
   //
   IX.check_index_range(get_shape());

MultiIndexIterator mult(get_shape(), IX);

Value_P Z(shape_Z, LOC);
const ShapeItem ec_z = Z->element_count();

   if (ec_z == 0)   // empty result
      {
        Z->set_default(*this, LOC);
        Z->check_value(LOC);
        return Z;
      }

   // construct iterators.
   // We go from lower indices to higher indices in IX, which
   // means from higher indices to lower indices in this and Z
   //
   loop(z, ec_z)
       Z->next_ravel_Cell(get_cravel(mult++));

   Assert(!mult.has_more());
   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
Value::index(const Value * X) const
{
   if (!X)   // A[] (1-item elided index)
      {
        return CLONE(const_cast<Value *>(this), LOC);
      }

   if (is_member())
      {

        const UCS_string name(*X);

        if (const Cell * data = get_member_data(name))   // member exists
           {
             if (data->is_pointer_cell())
                {
                  return CLONE_P(data->get_pointer_value(), LOC);
                }
             Value_P Z(LOC);
             Z->next_ravel_Cell(*data);
             Z->check_value(LOC);
             return Z;
           }

        // construct a )MORE error...
        //
        UCS_string & more = MORE_ERROR();
        more << "member access: member " << name
             << " not found. The valid members are:";
        const ShapeItem rows = get_rows();
        loop(r, rows)
            {
              const Cell & cell_r = get_cravel(2*r);
              if (cell_r.is_pointer_cell())
                 {
                   more << "\n      "
                        << UCS_string(*cell_r.get_pointer_value());
                 }
            }
        INDEX_ERROR;
      }

const ShapeItem max_idx = element_count();
const APL_Integer qio = Workspace::get_IO();

   // important (since frequent) special case: A[X] with scalar X and vector A
   //
   if (get_rank() == 1 && X->is_scalar())
      {
        const APL_Integer idx0 = X->get_cscalar().get_near_int() - qio;
        if (idx0 >= 0 && idx0 < max_idx)
           {
             Value_P Z(LOC);
             Z->next_ravel_Cell(get_cravel(idx0));
             Z->check_value(LOC);
             return Z;
           }

        // fall through re-using the verbose INDEX_ERROR below
      }

   if (get_rank() != 1)   RANK_ERROR;

   // ⍴A[X] = ⍴X
   //
Value_P Z(X->get_shape(), LOC);

const Cell * cI = &X->get_cfirst();

   while (Z->more())
      {
         const ShapeItem idx0 = cI++->get_near_int() - qio;
         if (idx0 < 0 || idx0 >= max_idx)
            {
              MORE_ERROR() << "min index=⎕IO (=" << qio
                           <<  "), offending index=" << (idx0 + qio)
                           << ", max index=⎕IO+" << (max_idx - 1)
                           << " (=" << (max_idx + qio - 1) << ")";
              Z->rollback(Z->valid_ravel_items, LOC);
              INDEX_ERROR;
            }

         Z->next_ravel_Cell(get_cravel(idx0));
      }

   Z->set_default(*this, LOC);
   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------
sRank
Value::get_single_axis(const Value * val, sRank max_axis)
{
   if (val == 0)   AXIS_ERROR;

   if (!val->is_scalar_or_len1_vector())     AXIS_ERROR;

   if (!val->get_cfirst().is_near_int())   AXIS_ERROR;

   // if axis becomes (signed) negative then it will be (unsigned) too big.
   // Therefore we need not test for < 0.
   //
const int axis = val->get_cfirst().get_near_int() - Workspace::get_IO();
   if (axis >= max_axis)   AXIS_ERROR;

   return axis;
}
//----------------------------------------------------------------------------
Shape
Value::to_shape(const Value * val)
{
   if (val == 0)
      {
        MORE_ERROR() << "illegal elided index [].";
        INDEX_ERROR;   // elided index ?
      }

const ShapeItem xlen = val->element_count();
const APL_Integer qio = Workspace::get_IO();

Shape shape;
     loop(x, xlen)
        shape.add_shape_item(val->get_cravel(x).get_near_int() - qio);

   return shape;
}
//----------------------------------------------------------------------------
AxesBitmap
Value::to_bitmap(const char * where, uRank rank_B) const
{
const APL_Integer qio = Workspace::get_IO();
AxesBitmap ret = 0;

   if (get_rank() > 1)
      {
         MORE_ERROR() << "In " << where
                      << ": invalid ⍴⍴X ( = " << get_rank() << ")";
         AXIS_ERROR;
      }

   // note: length error on X will either produce a range error or a
   // duplicte error below.
   //
   loop(e, element_count())
       {
         const Cell & cX = get_cravel(e);
         if (!cX.is_near_int())
            {
              MORE_ERROR() << "In " << where << ": X[" << (e + qio)
                           << "] is not integral.";
              AXIS_ERROR;
            }

         const APL_Integer axis = cX.get_near_int() - qio;
         if (axis < 0)
            {
              MORE_ERROR() << "In " << where << " : X[" << (e + qio)
                           << "] = " << (axis + qio)
                           << " is too small (note: ⎕IO is " << qio << ").";
              AXIS_ERROR;
            }

         if (axis >= rank_B)
            {
              MORE_ERROR() << "In " << where << " : X[" << (e + qio)
                           << "] = " << (axis + qio)
                           << " is too large (note: ⍴⍴B is " << rank_B << ").";
              AXIS_ERROR;
            }

         if (ret & 1 << axis)   // aready set
            {
              MORE_ERROR() << "In " << where << " : duplicate axis X["
                           << (e + qio) << "] = " << (axis + qio);
              AXIS_ERROR;
            }

         ret |= 1 << axis;
       }

   return ret;
}
//----------------------------------------------------------------------------
void
Value::glue(Token & result, const Token & token_A, const Token & token_B, const char * loc)
{
Value_P A = token_A.get_apl_val();
Value_P B = token_B.get_apl_val();

const bool strand_A = token_A.get_tag() == TOK_APL_VALUE3;
const bool strand_B = token_B.get_tag() == TOK_APL_VALUE3;

   if (strand_A)
      {
        if (strand_B)   glue_strand_strand(result, A, B, loc);
        else            glue_strand_closed(result, A, B, loc);
      }
   else
      {
        if (strand_B)   glue_closed_strand(result, A, B, loc);
        else            glue_closed_closed(result, A, B, loc);
      }
}
//----------------------------------------------------------------------------
void
Value::glue_strand_strand(Token & result, Value_P A, Value_P B,
                          const char * loc)
{
   // glue two strands A and B
   //
const ShapeItem len_A = A->element_count();
const ShapeItem len_B = B->element_count();

   Log(LOG_glue)
      {
        CERR << "gluing strands " << endl << *A
             << "with shape " << A->get_shape() << endl
             << " and " << endl << *B << endl
             << "with shape " << B->get_shape() << endl;
      }

   Assert(A->is_scalar_or_vector());
   Assert(B->is_scalar_or_vector());

Value_P Z(len_A + len_B, LOC);

   loop(a, len_A)   Z->next_ravel_Cell(A->get_cravel(a));
   loop(b, len_B)   Z->next_ravel_Cell(B->get_cravel(b));

   Z->check_value(LOC);
   new (&result) Token(TOK_APL_VALUE3, Z);
}
//----------------------------------------------------------------------------
void
Value::glue_strand_closed(Token & result, Value_P A, Value_P B,
                          const char * loc)
{
   // glue a strand A to new item B
   //
   Log(LOG_glue)
      {
        CERR << "gluing strand " << endl << *A
             << " to non-strand " << endl << *B << endl;
      }

   Assert(A->is_scalar_or_vector());

const ShapeItem len_A = A->element_count();
Value_P Z(len_A + 1, LOC);

   loop(a, len_A)   Z->next_ravel_Cell(A->get_cravel(a));

   Z->next_ravel_Value(B.get());
   Z->check_value(LOC);
   new (&result) Token(TOK_APL_VALUE3, Z);
}
//----------------------------------------------------------------------------
void
Value::glue_closed_strand(Token & result, Value_P A, Value_P B,
                          const char * loc)
{
   // glue a new item A to the strand B
   //
const ShapeItem len_A = A->element_count();
const ShapeItem len_B = B->element_count();
   Log(LOG_glue)
      {
        CERR << "gluing non-strand[" << len_A << "] " << endl << *A
             << " to strand[" << len_B << "] " << endl << *B << endl;
      }

   Assert(B->is_scalar_or_vector());

Value_P Z(len_B + 1, LOC);
   Z->next_ravel_Value(A.get());

   loop(b, len_B)   Z->next_ravel_Cell(B->get_cravel(b));

   Z->check_value(LOC);
   new (&result) Token(TOK_APL_VALUE3, Z);
}
//----------------------------------------------------------------------------
void
Value::glue_closed_closed(Token & result, Value_P A, Value_P B,
                          const char * loc)
{
   // glue two non-strands together, starting a strand
   //
   Log(LOG_glue)
      {
        CERR << "gluing two non-strands " << endl << *A
             << " and " << endl << *B << endl;
      }

Value_P Z(2, LOC);
   Z->next_ravel_Value(A.get());
   Z->next_ravel_Value(B.get());

   Z->check_value(LOC);
   new (&result) Token(TOK_APL_VALUE3, Z);
}
//----------------------------------------------------------------------------
void
Value::check_value(const char * loc)
{
#ifdef cfg_VALUE_CHECK_WANTED

   // if value was initialized by means of a next_ravel_XXX() mechanism,
   // then all cells are supposed to be OK.
   //
   if (valid_ravel_items && valid_ravel_items >= element_count())
      {
        set_complete();
        return;
      }

uint32_t error_count = 0;
const Cell * C = &get_cfirst();

const ShapeItem ec = nz_element_count();
    loop(c, ec)
       {
         const CellType ctype = C->get_cell_type();
         switch(ctype)
            {
              case CT_CHAR:
              case CT_POINTER:
              case CT_CELLREF:
              case CT_INT:
              case CT_FLOAT:
              case CT_COMPLEX:   break;   // OK

              default:
                 CERR << endl
                      << "*** check_value(" << loc << ") detects:" << endl
                      << "   bad ravel[" << c << "] (CellType "
                      << ctype << ")" << endl;

                 ++error_count;
            }

         if (error_count >= 10)
            {
              CERR << endl << "..." << endl;
              break;
            }

         ++C;
       }

   if (error_count)
      {
        CERR << "Shape: " << get_shape() << endl;
        print(CERR) << endl
           << "************************************************"
           << endl;
        Assert(0 && "corrupt ravel ");
      }
#endif

   set_complete();
}
//----------------------------------------------------------------------------
int
Value::total_CDR_size_netto(CDR_type cdr_type) const
{
   if (cdr_type != 7)   // not nested
      return 16 + 4*get_rank() + CDR_data_size(cdr_type);

   // nested: header + offset-array + sub-values.
   //
const ShapeItem ec = nz_element_count();
int size = 16 + 4*get_rank() + 4*ec;   // top_level size
   size = (size + 15) & ~15;           // rounded up to 16 bytes

   loop(e, ec)
      {
        const Cell & cell = get_cravel(e);
        if (cell.is_simple_cell())
           {
             // a non-pointer sub value consisting of its own 16 byte header,
             // and 1-16 data bytes, padded up to 16 bytes,
             //
             size += 32;
           }
        else if (cell.is_pointer_cell())
           {
             Value_P sub_val = cell.get_pointer_value();
             const CDR_type sub_type = sub_val->get_CDR_type();
             size += sub_val->total_CDR_size_brutto(sub_type);
           }
         else
           DOMAIN_ERROR;
      }

   return size;
}
//----------------------------------------------------------------------------
int
Value::CDR_data_size(CDR_type cdr_type) const
{
const ShapeItem ec = nz_element_count();
   
   switch (cdr_type) 
      {
        case 0: return (ec + 7) / 8;   // 1/8 byte bit, rounded up
        case 1: return 4*ec;           //   4 byte integer
        case 2: return 8*ec;           //   8 byte float
        case 3: return 16*ec;          // two 8 byte floats
        case 4: return ec;             // 1 byte char
        case 5: return 4*ec;           // 4 byte Unicode char
        case 7: break;                 // nested: continue below.
             
        default: FIXME;
      }      
             
   // compute size of a nested CDR.
   // The top level consists of structural offsets that do not count as data.
   // We therefore simly add up the data sizes for the sub-values.
   //
int size = 0;
   loop(e, ec)
      {
        const Cell & cell = get_cravel(e);
        if (cell.is_simple_cell())
           {
             size += cell.CDR_size();
           }
        else if (cell.is_pointer_cell())
           {
             Value_P sub_val = cell.get_pointer_value();
             const CDR_type sub_type = sub_val->get_CDR_type();
             size += sub_val->CDR_data_size(sub_type);
           }
         else
           DOMAIN_ERROR;
      }

   return size;
}
//----------------------------------------------------------------------------
CDR_type
Value::get_CDR_type() const
{
const ShapeItem ec = nz_element_count();
const Cell & cell_0 = get_cfirst();

   // if all cells are characters (8 or 32 bit), then return 4 or 5.
   // if all cells are numeric (1, 32, 64, or 128 bit, then return  0 ... 3
   // otherwise return 7 (nested) 

   if (cell_0.is_character_cell())   // 8 or 32 bit characters.
      {
        bool has_big = false;   // assume 8-bit char
        loop(e, ec)
           {
             const Cell & cell = get_cravel(e);
             if (!cell.is_character_cell())   return CDR_NEST32;
             const Unicode uni = cell.get_char_value();
             if (uni < 0)      has_big = true;
             if (uni >= 256)   has_big = true;
           }

        return has_big ? CDR_CHAR32 : CDR_CHAR8;   // 8-bit or 32-bit char
      }

   if (cell_0.is_numeric())
      {
        bool has_int     = false;
        bool has_float   = false;
        bool has_complex = false;

        loop(e, ec)
           {
             const Cell & cell = get_cravel(e);
             if (cell.is_integer_cell())
                {
                  const APL_Integer i = cell.get_int_value();
                  if (i == 0)                   ;
                  else if (i == 1)              ;
                  else if (i >  0x7FFFFFFFLL)   has_float = true;
                  else if (i < -0x80000000LL)   has_float = true;
                  else                          has_int   = true;
                }
             else if (cell.is_float_cell())
                {
                  has_float  = true;
                }
             else if (cell.is_complex_cell())
                {
                  has_complex  = true;
                }
             else return CDR_NEST32;   // mixed: return 7
           }

        if (has_complex)   return CDR_CPLX128;
        if (has_float)     return CDR_FLT64;
        if (has_int)       return CDR_INT32;
        return CDR_BOOL1;
      }

   return CDR_NEST32;
}
//----------------------------------------------------------------------------
ostream &
Value::print(ostream & out) const
{
   if (is_member())   return print_member(out, UCS_string());

PrintContext pctx = Workspace::get_PrintContext(PR_APL);
   if (get_rank() == 0)   // scalar
      {
        pctx.set_style(PR_APL_MIN);
      }
   else if (get_rank() == 1)   // vector
      {
        if (element_count() == 0 &&   // empty vector
            (get_cfirst().is_simple_cell()))
           {
             return out << endl;
           }

        pctx.set_style(PR_APL_MIN);
      }
   else                  // matrix or higher
      {
        pctx.set_style(PrintStyle(pctx.get_style() | PST_NO_FRACT_0));
      }

PrintBuffer pb(*this, pctx, &out);   // constructor prints it
   return out;
}
//----------------------------------------------------------------------------
ostream &
Value::print_member(ostream & out, UCS_string member_prefix) const
{
const ShapeItem rows = get_rows();

   // figure the longest member name...
   //
ShapeItem longest_name = 0;
   loop(r, rows)
      {
        const Cell & cell = get_cravel(2*r);   // (nested) member-name or 0
        if (cell.is_pointer_cell())
           {
              Value_P member_name = cell.get_pointer_value();
              const ShapeItem name_len = member_name->element_count();
              if (longest_name < name_len)  longest_name = name_len;
           }
      }

const size_t indent = member_prefix.size() + longest_name + 3;

   loop(r, rows)
       {
         const Cell * cell = &get_cravel(2*r);   // (nested) member-name or 0
         if (!cell->is_pointer_cell())       continue;

         Value_P cell_sub = cell->get_pointer_value();
         Assert(cell_sub->is_char_string());

         // print the member name
         //
         const UCS_string member_name = cell_sub->get_UCS_ravel();
         const size_t pad = longest_name - member_name.size();
         UCS_string member = member_prefix;   // ancestor.ancestor ...
         member += UNI_FULLSTOP;
         member.append(member_name);
         out << member << ": ";
         out << UCS_string(pad, UNI_SPACE);

         // print the member value
         //
         ++cell;   // member value
         if (cell->is_pointer_cell())   // sub-member or leaf
            {
              bool printed = false;
              Value_P sub = cell->get_pointer_value();
              if (sub->is_member())
                 {
                   out << "□" << endl;
                   sub->print_member(out, member);
                   printed = true;
                 }
              else if (sub->is_char_vector())   // maybe multi-line with \n
                 {
                   Value_P sub1 = Quad_CR::do_CR35(sub.get());
                   Value_P sub2 = Bif_F12_PICK::disclose(sub1, false);
                   if (sub2->get_rows() > 1)
                      {
                        sub2->print_boxed(out, indent);
                        printed = true;
                      }

                   // for obscure reasons we need to release the lines in sub1
                   //
                   loop(c, sub1->nz_element_count())
                       {
                         Cell & cell = sub1->get_wravel(c);
                         cell.release(LOC);
                         IntCell::z0(&cell);
                       }
                 }

              if (!printed)
                 {
                   sub->print_boxed(out, indent);
                 }
            }
         else                           // simple member value
            {
              Value_P sub(*cell, LOC);
              sub->print_boxed(out, indent);
            }
       }

   return out;
}
//----------------------------------------------------------------------------
ostream &
Value::print1(ostream & out, PrintContext pctx) const
{
int style = pctx.get_style();
   if (get_rank() < 2)   // scalar or vector
      {
        style = PR_APL_MIN;
      }
   else                  // matrix or higher
      {
        style |= PST_NO_FRACT_0;
      }

   pctx.set_style(PrintStyle(style));

PrintBuffer pb(*this, pctx, &out);
   return out;
}
//----------------------------------------------------------------------------
ostream &
Value::print_properties(ostream & out, int indent, bool help) const
{
UCS_string ind(indent, UNI_SPACE);
   if (help)
      {
        out << ind << "Rank:  " << get_rank()  << endl
            << ind << "Shape:";
        loop(r, get_rank())   out << " " << get_shape_item(r);
        out << endl
            << ind << "Depth: " << compute_depth()   << endl
            << ind << "Type:  ";

       const CellType types = deep_cell_types();
       if (!(types & CT_CHAR))           // no chars in this value
          out << "numeric";
       else if (!(types & CT_NUMERIC))   // no numbers in this value
          out << "character";
       else                              // chars and numbers in this value
          out << "mixed";
      }
   else
      {
        out << ind << "Addr:    " << voidP(this) << endl
            << ind << "Rank:    " << get_rank()  << endl
            << ind << "Shape:   " << get_shape() << endl
            << ind << "Flags:   " << get_flags();
        if (is_complete())    out << " VF_complete";
        if (is_marked())      out << " VF_marked";
        if (is_member())      out << " VF_member";
        if (is_packed())      out << " VF_packed";
        out << endl
             << ind << "First:   " << get_cfirst()  << endl
             << ind << "Dynamic: ";

        DynamicObject::print(out);
      }
   return out;
}
//----------------------------------------------------------------------------
void
Value::debug(const char * info) const
{
const PrintContext pctx = Workspace::get_PrintContext(PR_APL);
PrintBuffer pb(*this, pctx, 0);
   pb.debug(CERR, info);
}
//----------------------------------------------------------------------------
ostream &
Value::print_boxed(ostream & out, int indent) const
{
const PrintContext pctx(PST_NONE);

Value_P Z = Quad_CR::do_CR(8, this, pctx);
   Assert(Z->get_rank() == 2);
const ShapeItem rows = Z->get_rows();
const ShapeItem cols = Z->get_cols();
   loop(r, rows)
       {
         if (indent && r)   out << UCS_string(indent, UNI_SPACE);
         loop(c, cols)   out << Z->get_cravel(c + r*cols).get_char_value();
         out << endl;
       }
   out << endl;
   return out;
}
//----------------------------------------------------------------------------
UCS_string
Value::get_UCS_ravel() const
{
UCS_string ucs;

const ShapeItem ec = element_count();
   loop(e, ec)   ucs.append(get_cravel(e).get_char_value());

   return ucs;
}
//----------------------------------------------------------------------------
void
Value::to_type(bool force_numeric)
{
   loop(e, nz_element_count())
      {
        Cell & cell = get_wravel(e);
        if (cell.is_pointer_cell())
           {
             reinterpret_cast<PointerCell &>(cell).isolate(LOC);
             cell.get_pointer_value()->to_type(false);
           }
        else if (cell.is_character_cell())
           {
             if (force_numeric)   set_ravel_Int(e, 0);
             else                 set_ravel_Char(e, UNI_SPACE);
           }
        else                      set_ravel_Int(e, 0);
      }
}
//----------------------------------------------------------------------------
void
Value::print_structure(ostream & out, int indent, ShapeItem idx) const
{
   loop(i, indent)   out << "    ";
   if (indent)   out << "[" << idx << "] ";
   out << "addr=" << voidP(this)
       << " ≡" << compute_depth()
       << " ⍴" << get_shape()
       << " flags: " << HEX4(get_flags()) << "   "
       << get_flags()
       << " " << where_allocated()
       << endl;

const ShapeItem ec = nz_element_count();
const Cell * c = &get_cfirst();
   loop(e, ec)
      {
        if (c->is_pointer_cell())
           c->get_pointer_value()->print_structure(out, indent + 1, e);
        ++c;
      }
}
//----------------------------------------------------------------------------
Value_P
Value::clone(const char * loc) const
{
#ifdef cfg_PERFORMANCE_COUNTERS_WANTED
const uint64_t start_1 = cycle_counter();
#endif

Value_P Z(get_shape(), loc);
   Z->flags |= flags & VF_member;   // propagate member flag

   // NOTE: if something fails in the allocation of Z (⎕SYL etc.) then
   // Z->get_shape() may differ from this->get_shape(). We therefore use
   // the (in that case smaller) Z->element_count() here.
   //
   if (const ShapeItem count = Z->element_count())   // non-empty
      {
        loop(c, count)   Z->next_ravel_Cell(get_cravel(c));
      }
   else                                              // empty
      {
        get_cproto().init_other(&Z->get_wproto(), *Z, LOC);
      }

   Z->check_value(LOC);

#ifdef cfg_PERFORMANCE_COUNTERS_WANTED
const uint64_t end_1 = cycle_counter();
const uint64_t count1 = nz_element_count();
   Performance::fs_clone_B.add_sample(end_1 - start_1, count1);
#endif

   return Z;
}
//----------------------------------------------------------------------------
Value_P
Value::prototype(const char * loc) const
{
   /** lrm p.46:

       The type of an array is an array with the same structure, but with
      all numbers replaced with 0 and all characters replaced with ' '.

      The prototype of an array is the type of the first element of the array.

      type B        ←→   ↑0⍴⊂B
      prototype B   ←→   ↑0⍴⊂↑B

      Since ↑B is a scalar the prototype of B is also a scalar.
    **/

const Cell & first = get_cfirst();
   if (first.is_integer_cell())     return IntScalar(0, LOC);
   if (first.is_character_cell())   return CharScalar(UNI_SPACE, LOC);
   if (first.is_pointer_cell())
      {
        Value_P B0 = first.get_pointer_value();
        Value_P Z(B0->get_shape(), loc);

        loop(z, Z->element_count())   Z->next_ravel_Proto(B0->get_cravel(z));
        Z->check_value(LOC);
        return Z;
      }

   DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
/// lrp p.138: S←⍴⍴A + NOTCHAR (per column)
int32_t
Value::get_col_spacing(bool & not_char, ShapeItem col, bool framed) const
{
int32_t max_spacing = 0;
   not_char = false;

const ShapeItem ec = element_count();
const ShapeItem cols = get_last_shape_item();
const ShapeItem rows = ec/cols;
   loop(row, rows)
      {
        // compute spacing, which is the spacing required by this item.
        //
        int32_t spacing = 1;   // assume simple numeric
        const Cell & cell = get_cravel(col + row*cols);

        if (cell.is_pointer_cell())   // nested 
           {
             if (framed)
                {
                  not_char = true;
                  spacing = 1;
                }
             else
                {
                  Value_P v =  cell.get_pointer_value();
                  spacing = v->get_rank();
                  if (v->NOTCHAR())
                     {
                       not_char = true;
                       ++spacing;
                     }
                }
           }
        else if (cell.is_character_cell())   // simple char
           {
             spacing = 0;
           }
        else                                 // simple numeric
           {
             not_char = true;
           }

        if (max_spacing < spacing)   max_spacing = spacing;
      }

   return max_spacing;
}
//----------------------------------------------------------------------------
int
Value::print_incomplete(ostream & out)
{
std::basic_string<const Value *> incomplete;
bool goon = true;

   for (const DynamicObject * dob = all_values.get_prev();
        goon && (dob != &all_values); dob = dob->get_prev())
       {
         const Value & value = dob->rValue();
         goon = (dob != dob->get_prev());

         if (value.is_complete())   continue;

         out << "incomplete value at " << voidP(&value) << endl;
         incomplete.push_back(&value);

         if (!goon)
            {
              out << "Value::print_incomplete() : endless loop in "
                     "Value::all_values; stopping display." << endl;
            }
       }

   // then print more info...
   //
int count = 0;
   loop(s, incomplete.size())
      {
        incomplete[s]->print_stale_info(out, incomplete[s]);
        if (++count > 20)   // its getting boring
           {
             CERR << endl << " ... ( " << (incomplete.size() - count) 
                  << " more incomplete values)..." << endl;
             break;
           }
      }

   return incomplete.size();
}
//----------------------------------------------------------------------------
int
Value::print_stale(ostream & out)
{
std::basic_string<const Value *> stale_vals;
std::basic_string<const DynamicObject *> stale_dobs;
bool goon = true;
int count = 0;

   // first print addresses and remember stale values
   //
   for (const DynamicObject * dob = all_values.get_prev();
        goon && (dob != &all_values); dob = dob->get_prev())
       {
         const Value & value = dob->rValue();
         goon = (dob == dob->get_prev());

         if (value.owner_count)   continue;

         out << "stale value at " << voidP(&value) << endl;
         stale_vals.push_back(&value);
         stale_dobs.push_back(dob);

         if (!goon)
            {
              out << "Value::print_stale() : endless loop in "
                     "Value::all_values; stopping display." << endl;
            }
       }

   // then print more info...
   //
   loop(s, stale_vals.size())
      {
        const DynamicObject * dob = stale_dobs[s];
        const Value * val = stale_vals[s];
        val->print_stale_info(out, dob);
        if (++count > 20)   // its getting boring
           {
             CERR << endl << " ... ( " << (stale_vals.size() - count) 
                  << " more stale values)..." << endl;
             break;
           }
       }

   // mark all dynamic values, and then unmark those known in the workspace
   //
   mark_all_dynamic_values();
   Workspace::unmark_all_values();
   Macro::unmark_all_macros();

   // print all values that are still marked
   //
   for (const DynamicObject * dob = all_values.get_prev();
        dob != &all_values; dob = dob->get_prev())
       {
         const Value & value = dob->rValue();

         // don't print values found in the previous round.
         //
         bool known_stale = false;
         loop(s, stale_vals.size())
            {
              if (&value == stale_vals[s])
                 {
                   known_stale = true;
                   break;
                 }
            }
         if (known_stale)   continue;

         if (value.is_marked())
            {
              ++count;
              if (count < 20)   value.print_stale_info(out, dob);
              else if (count == 20)
              CERR << endl << " ... (more stale values)..." << endl;
              value.unmark();
            }
       }

   return count;
}
//----------------------------------------------------------------------------
void
Value::print_stale_info(ostream & out, const DynamicObject * dob) const
{
   out << "print_stale_info():   alloc(" << dob->where_allocated()
       << ") flags(" << get_flags() << ")" << endl;

   VH_entry::print_history(out, dob->rValue(), LOC);

   try
      {
        print_structure(out, 0, 0);
        const PrintContext pctx(PST_NONE);
        Value_P Z = Quad_CR::do_CR(7, this, pctx);
        Z->print(out);
        out << endl;
      }
   catch (...)   { out << " *** corrupt stale ***"; }

   out << endl;
}
//----------------------------------------------------------------------------
int
Value::check_all_Cells(ostream & out)
{
int errors = 0;

   for (const DynamicObject * dob = all_values.get_prev();
        dob != &all_values; dob = dob->get_prev())
       {
         const Value & value = dob->rValue();
         if (value.check_Cells(out))   ++errors;
       }

   return errors;
}
//----------------------------------------------------------------------------
int
Value::check_Cells(ostream & out) const
{
size_t errors = 0;
ShapeItem pointer_count = 0;
ShapeItem lval_count = 0;

   loop(c, nz_element_count())
       {
         const Cell & cell = get_cravel(c);
         if (cell.is_pointer_cell())     ++pointer_count;
         else if (cell.is_lval_cell())   ++lval_count;
       }

   if (lval_count)
      {
        out << "*** Warning: value " << voidP(this)
            << " has " << lval_count << " Lval Cells" << endl;
        VH_entry::print_history(cerr, *this, LOC);

        ++errors;
      }

   if (pointer_count != get_pointer_cell_count())
      {
        out << "*** Error: value " << voidP(this)
            << " has pointer_cell_count=" << pointer_cell_count
            << " but " << pointer_count << " PointerCells" << endl;
        ++errors;
      }

   return errors;
}
//----------------------------------------------------------------------------
void
Value::explode()
{
   Assert(is_packed());
const uint8_t * bits = reinterpret_cast<const uint8_t *>(ravel);

IntCell * new_ravel = 0;
   try           { ravel = new IntCell[element_count()]; }
   catch (...)   { WS_FULL }

   loop(b, element_count())
      if (bits[b >> 3] & 1 << (b & 7))   new_ravel[b].set_int_value(1);

   delete [] bits;
   ravel = new_ravel;
   clear_packed();
   fetcher = &cell_fetcher;
}
//----------------------------------------------------------------------------
const char *
Value::try_implode()
{
   if (is_packed())          return "value already packed";
   if (pointer_cell_count)   return "value not simple";

   // at this point we are optimistic that all B-items are boolean.
   //
const ShapeItem Z_len = (nz_element_count() + 63) >> 6;
uint64_t * bits = new uint64_t[Z_len];
const char * error_reason = 0;

   if (bits == 0)   return "WS FULL";

   // first set all bits to 0
   //
   loop(z, Z_len)   bits[z] = 0;

   // then set some bits to 1
   //
uint64_t zchunk = 0;
uint64_t zbit  = 1;

ShapeItem z = 0;
   for (ConstRavel_P b(*this, true); +b; ++b)
       {
         if (!b->is_near_bool())
            {
              error_reason = "value has non-boolean items";
              break;
            }

         // OK, bit is 0 or 1. Already done if bit is 0.
         if (b->get_near_bool())   zchunk |= zbit;   // bit in B is set

         zbit += zbit;                   // next bit
         if (zbit)   continue;           // more bits in same chunk
         bits[z++] = zchunk;
         zchunk = 0;
       }

   if (zchunk)   bits[Z_len - 1] = zchunk;   // rest bits

   if (error_reason)
      {
        delete[] bits;
      }
   else
      {
        if (ravel != short_value)   delete[] ravel;
        ravel = reinterpret_cast<Cell *>(bits);
        flags |= VF_packed;
      }

   return error_reason;
}
//----------------------------------------------------------------------------
ostream &
operator <<(ostream & out, const Value & v)
{
   v.print(out);
   return out;
}
//----------------------------------------------------------------------------
Value_P
IntScalar(APL_Integer val, const char * loc)
{
Value_P Z(loc);
   Z->next_ravel_Int(val);
   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
FloatScalar(APL_Float val, const char * loc)
{
Value_P Z(loc);
   Z->next_ravel_Float(val);
   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
ComplexScalar(APL_Complex val, const char * loc)
{
Value_P Z(loc);
   Z->next_ravel_Complex(val);
   Z->check_value(loc);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
ComplexScalar(APL_Float real, APL_Float imag, const char * loc)
{
Value_P Z(loc);
   Z->next_ravel_Complex(real, imag);
   Z->check_value(loc);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
CharScalar(Unicode uni, const char * loc)
{
Value_P Z(loc);
   Z->next_ravel_Char(uni);
   Z->check_value(loc);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
Idx0(const char * loc)
{
Value_P Z(ShapeItem(0), loc);
   // default proto is 0, so no need to set it here
   Z->check_value(loc);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
Str0(const char * loc)
{
Value_P Z(ShapeItem(0), loc);
   Z->set_proto_Spc();
   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
Str0_0(const char * loc)
{
Shape sh(ShapeItem(0), ShapeItem(0));
Value_P Z(sh, loc);
   Z->set_proto_Spc();
   Z->check_value(loc);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
Idx0_0(const char * loc)
{
Shape sh(ShapeItem(0), ShapeItem(0));
Value_P Z(sh, loc);
   Z->check_value(loc);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
EmptyStruct(const char * loc)
{
   enum {
          rows = 8,
          cols = 2,
          items = rows * cols
        };

const Shape shape_Z(rows, cols);
Value_P Z(shape_Z, loc);
   loop(z, items)  Z->next_ravel_0();
   Z->check_value(LOC);
   Z->set_member();
   return Z;
}
//----------------------------------------------------------------------------
/// declared in PrintOperator.hh
ostream &
operator << (ostream & out, const AP_num3 & ap3)
{
   return out << ap3.proc << "." << ap3.parent << "." << ap3.grand;
}
//----------------------------------------------------------------------------

