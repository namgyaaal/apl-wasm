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

#include <stdlib.h>

#include "Quad_RL.hh"
#include "Value.hh"
#include "IntCell.hh"

uint64_t Quad_RL::state = 0;

//============================================================================
Quad_RL::Quad_RL()
   : SystemVariable(ID_Quad_RL)
{
Value_P Z(LOC);

const unsigned int seed = reset_seed();

   Z->next_ravel_Int(seed);
   Z->check_value(LOC);

   Symbol::assign(Z, false, LOC);
}
//----------------------------------------------------------------------------
void
Quad_RL::assign(Value_P B, bool /* clone */, const char * loc)
{
   if (!B->is_scalar_or_len1_vector())
      {
        if (B->get_rank() > 1)   RANK_ERROR;
        else                     LENGTH_ERROR;
      }

const Cell & cell = B->get_cscalar();
const APL_Integer val = cell.get_near_int();

   state = val;
   Symbol::assign(IntScalar(state, loc), false, LOC);
}
//----------------------------------------------------------------------------
uint64_t
Quad_RL::get_random()
{
   Assert(value_stack.size());   // by Quad_RL::assign()
   if (value_stack.back().get_NC() != NC_VARIABLE)   VALUE_ERROR;  // localized

   state *= Knuth_a;
   state += Knuth_c;

   value_stack.back().get_val_wptr()->set_ravel_Int(0, state);
   return state;
}
//----------------------------------------------------------------------------
void
Quad_RL::push()
{
   // clone the current state
   //
   Symbol::push_value(IntScalar(state, LOC));
}
//----------------------------------------------------------------------------
void
Quad_RL::pop()
{
   Symbol::pop();
   state = value_stack.back().get_val_cptr()->get_cfirst().get_near_int();
}
//============================================================================
