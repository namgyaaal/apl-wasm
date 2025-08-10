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

#include "Logging.hh"
#include "PointerCell.hh"
#include "Quad_DLX.hh"
#include "PrintOperator.hh"
#include "Token.hh"
#include "Workspace.hh"

Quad_DLX  Quad_DLX::fun;

//============================================================================
/// one '1' in a sparse constraint matrix
class DLX_Node
{
public:
   /// default constructor for node allocation
   DLX_Node(bool hdr)
   : row(-1),
     col(-1),
     up(this),
     down(this),
     left(this),
     right(this),
     is_header(hdr)
   {}

   /// normal constructor
   DLX_Node(bool hdr, ShapeItem R, ShapeItem C,
            DLX_Node * u, DLX_Node * d, DLX_Node * l, DLX_Node * r)
   : row(R),
     col(C),
     up(u),
     down(d),
     left(l),
      right(r),
     is_header(hdr)
   { insert_lr();   insert_ud(); }

   /// assignment
   void operator = (const DLX_Node & other)
      { new (this) DLX_Node(other); }

   /// (re-)insert \b this node horizontally (between left and right)
   void insert_lr()   { left->right = this;  right->left = this; }

   /// (re-)insert \b this node vertically (between up and down)
   void insert_ud()   { up->down    = this;  down->up    = this; }

   /// remove(unlink) \b this node horizontally
   void remove_lr()   { left->right = right; right->left = left; }

   /// remove(unlink) \b this node vertically
   void remove_ud()   { up->down    = down;  down->up    = up; }

   /// check the consistency of the \b up, \b down, \b left, and \b right links
   void check() const
      {
        Assert(up);    Assert(up->col    == col);
        Assert(down);  Assert(down->col  == col);
        Assert(left);  Assert(left->row  == row);
        Assert(right); Assert(right->row == row);
      }

   /// print the row and columns of \b this node
   ostream & print_RC(ostream & out) const
     {
       return out << std::right << setw(2) << row << ":"
                  << std::left  << setw(2) << col << std::right;
     }

   /// print \b this node
   void print(ostream & out) const
      {
        print_RC(out);
        up->print_RC(out << " ↑");
        down->print_RC(out << " ↓");
        left->print_RC(out << " ←");
        right->print_RC(out << " →");
        out << endl;
        check();
      }

   /// the type of column
   enum Col_Type
      {
        Col_INVALID   = -1,      ///< invalid (in input APL value)
        Col_NULL      = 0,       ///< no 1 or 2 seen (yet)
        Col_PRIMARY   = 1,       ///< primary column
        Col_SECONDARY = 2,       ///< secondary column
        Col_UNKNOWN = Col_NULL   ///< invalid (for default constructor)
      };

   /// return  the type of column
   static Col_Type get_col_type(const Cell & cell);

   /// the row of this node
   const ShapeItem row;

   /// the column of this node
   const ShapeItem col;

   /// link to the next node above
   DLX_Node * up;

   /// link to the next node below
   DLX_Node * down;

   /// link to the next node on the left
   DLX_Node * left;

   /// link to the next node on the right
   DLX_Node * right;

   /// true if ths node is a header node (containing no data)
   const bool is_header;
};
//============================================================================
/// A (column-) header node
class DLX_Header_Node : public DLX_Node
{
public:
   /// constructor for empty header array
   DLX_Header_Node()
   : DLX_Node(true)
   {}

   /// constructor for memory allocation
   DLX_Header_Node(ShapeItem C, DLX_Node * l, DLX_Node * r)
   : DLX_Node(true, -1, C, this, this, l, r),
     count(0),
     col_type(Col_UNKNOWN),
     item_r(0)
   {}

   /// the number of '1's remaining in this column
   ShapeItem count;

   /// the type of column
   Col_Type col_type;

   /// the items r and j in the double-loop around cover()/uncover()
   DLX_Node * item_r;
};
//----------------------------------------------------------------------------
DLX_Node::Col_Type
DLX_Node::get_col_type(const Cell & cell)
{
   if (cell.is_character_cell())
      {
        // blank and '0' shall mean 0, '1' means 1, and '2' means 2
        //
        switch(cell.get_char_value())
           {
             case ' ':
             case '0':   return Col_NULL;
             case '1':   return Col_PRIMARY;
             case '2':   return Col_SECONDARY;
             default:    return Col_INVALID;
           }
      }
   if (cell.is_numeric())
      {
        switch(cell.get_int_value())
           {
             case 0:   return Col_NULL;
             case 1:   return Col_PRIMARY;
             case 2:   return Col_SECONDARY;
             default:  return Col_INVALID;
           }
      }

   return Col_INVALID;
}
//============================================================================
/// The root node of the constraints matrix
class DLX_Root_Node : public DLX_Node
{
public:
   /// constructor
   DLX_Root_Node(ShapeItem rows, ShapeItem cols, ShapeItem max_sol,
                 const Value & B);

   /// check of all nodes
   void deep_check() const;

   /// indent for current level
   ostream & indent(ostream & out)
      {
        loop(s, level)   out << "  ";
        return out;
      }

   /// cover columns j (see Knuth)
  void cover(ShapeItem j)
     {
       ++cover_count;

       DLX_Header_Node & c = headers[j];
       Assert(c.row == -1);
       Assert(c.col == j);
       c.remove_lr();
       if (c.col_type == Col_PRIMARY)   --primary_count;

       for (DLX_Node * i = c.down; i != &c; i = i->down)
           {
             Assert(i->col == j);
             for (DLX_Node * j = i->right; j != i; j = j->right)
                 {
                   j->remove_ud();
                   --headers[j->col].count;
                 }
           }
     }

   /// uncover columns j (see Knuth)
   void uncover(ShapeItem j)
     {
       DLX_Header_Node & c = headers[j];
       Assert(c.row == -1);
       Assert(c.col == j);
       for (DLX_Node * i = c.up; i != &c; i = i->up)
           {
             Assert(i->col == j);
             for (DLX_Node * j = i->left; j != i; j = j->left)
                 {
                   j->insert_ud();
                   headers[j->col].count++;
                 }
           }
       c.insert_lr();
       if (c.col_type == Col_PRIMARY)   ++primary_count;
       c.check();
     }

   /// return the (decreasing) number of columns
   ShapeItem get_column_count() const;

   /// display the matrix
   void display(ostream & out) const;

   /// solve the constraints matrix
   void solve();

   /// do some dance steps with the constraints matrix
   Token preset(const Cell * steps, ShapeItem count, const Cell * cB);

   /// return the number of solutions found
   ShapeItem get_solution_count() const   { return solution_count; }

   /// return the number of items picked
   ShapeItem get_pick_count() const       { return pick_count; }

   /// return the number of columns covered
   ShapeItem get_cover_count() const      { return cover_count; }

   /// all solutions as len rows... len rows ...
   std::basic_string<ShapeItem> all_solutions;

protected:
   /// the max. number of solutions to produce, 0 = all
   const ShapeItem max_solutions;

   /// the number of rows
   const ShapeItem rows;

   /// the number of columns
   const ShapeItem cols;

   /// the column headers
   std::vector<DLX_Header_Node> headers;

   /// the '1's and '2's in the (sparse) matrix
   std::vector<DLX_Node> nodes;

   /// the number of primary columns
   ShapeItem primary_count;

   /// the number of primary columns
   ShapeItem secondary_count;

   /// the number of solutions found so far
   ShapeItem solution_count;

   /// the number of rows in the current partial solution (aka recursion depth)
   ShapeItem level;

   /// the total number of pick'ed elements
   ShapeItem pick_count;

   /// the total number of covr operations
   ShapeItem cover_count;

   /// the first 0 in the matrix (for figuring the format)
   const Cell * first_0;
};
//----------------------------------------------------------------------------
DLX_Root_Node::DLX_Root_Node(ShapeItem rs, ShapeItem cs, ShapeItem max_sol,
                             const Value & B)
   : DLX_Node(true, -1, -1, this, this, this, this),
     max_solutions(max_sol),
     rows(rs),
     cols(cs),
     primary_count(0),
     secondary_count(0),
     solution_count(0),
     level(0),
     pick_count(0),
     cover_count(0),
     first_0(0)
{
   // count the number of non-zero elements in the matrix
   //
const ShapeItem ec_B = B.element_count();
const int qio = Workspace::get_IO();
ShapeItem ones = 0;
const Cell * b = &B.get_cfirst();
   loop(e, ec_B)
      {
        const Cell & cell = *b++;
        const Col_Type ct = get_col_type(cell);
        if (ct == Col_INVALID)
           {
             const ShapeItem row = qio + e/B.get_cols();
             const ShapeItem col = qio + e%B.get_cols();
             UCS_string & more = MORE_ERROR() << "Bad B[" << row << ";"
                                              << col << "]: ";
             if (cell.is_integer_cell())
                more << cell.get_int_value();
             else if (cell.is_character_cell())
                more << "'" << cell.get_char_value() << "'";
             else
                more << "neither integer nor character";
             more << " in ⎕DLX B";
             DOMAIN_ERROR;
           }
        if (ct != Col_UNKNOWN)   ++ones;
        else if (!first_0)   first_0 = &cell;
      }

   Log(LOG_Quad_DLX)   CERR << "Matrix has " << ones << " ones" << endl;

   // set up column headers. std::vector.push_back() may move the headers so
   // we first append all headers and initialize then
   //
   headers.reserve(cols);
   loop (c, cols)   headers.push_back(DLX_Header_Node());
   loop (c, cols)
      {
        if (c) new (&headers[c]) DLX_Header_Node(c, &headers[c - 1], this);
        else   new (&headers[c]) DLX_Header_Node(c, this,            this);
      }

   // set up non-header nodes. std::vector.push_back() may move the headers so
   // we first append all nodes and initialize then.
   //
   b = &B.get_cfirst();
   nodes.reserve(ones);
   loop(o, ones)   nodes.push_back(DLX_Node(false));

   DLX_Node * n = &nodes[0];
   loop (r, rows)
      {
        DLX_Node * const lm = n;   // leftmost item in this row
        DLX_Node * rm = n;         // rightmost item in this row
        loop (c, cols)
           {
             DLX_Header_Node & hdr = headers[c];
             const Col_Type ct = get_col_type(*b++);
             if (ct < Col_PRIMARY)   continue;   // most likely: not '1' or '2'

             if (hdr.col_type == Col_UNKNOWN)   // first '1' or '2' in this col
                {
                  hdr.col_type = ct;
                  if (ct == Col_PRIMARY)   ++primary_count;
                  else                     ++secondary_count;
                }
             else if (ct != hdr.col_type)       // col type mismatch
                {
                  MORE_ERROR() <<
                  "the columns of B in ⎕DLX B must either be primary ("
                  "containing 1s) or secondary (containing  2s), but not both";
                  DOMAIN_ERROR;
                }


             if (rm == n)
                rm = new (n++)   DLX_Node(false, r, c, hdr.up, &hdr, rm, lm);
             else
                rm = new (n++)   DLX_Node(false, r, c, hdr.up, &hdr, rm, lm);
             ++headers[c].count;
           }
      }

   // check that all columns have a 1 or 2
   //
   loop (c, cols)
      {
        if (headers[c].col_type == Col_UNKNOWN)
           {
             MORE_ERROR() << "⎕DLX B with empty column B[;"
                          << (c + qio) << "]";
             DOMAIN_ERROR;
           }
      }

   Log(LOG_Quad_DLX)   { loop(n, ones)   nodes[n].print(CERR); }
}
//----------------------------------------------------------------------------
void
DLX_Root_Node::display(ostream & out) const
{
int w = 1;
   if (cols >= 10)   w = 2;
   if (cols >= 100)  w = 3;
   if (cols >= 1000) w = 4;

ShapeItem pcnt = 0;

   out << endl << "  N:";
   for (const DLX_Node * x = right; x != this; x = x->right)
       {
         const DLX_Header_Node & hdr =
               *reinterpret_cast<const DLX_Header_Node *>(x);
         Assert(hdr.row == -1);
         if (hdr.col_type == Col_PRIMARY)   ++pcnt;
         out << " " << setw(w) << hdr.count;
       }
   out << endl << "Col:";
   for (const DLX_Node * x = right; x != this; x = x->right)
       {
         const DLX_Header_Node & hdr =
               *reinterpret_cast<const DLX_Header_Node *>(x);
         Assert(hdr.row == -1);
         out << " " << setw(w) << hdr.col;
       }
   out << " (" << pcnt << ")" << endl << "----";
   for (const DLX_Node * x = right; x != this; x = x->right)
       {
         for (int ww = -1; ww < w; ++ww)   out << "-";
       }
   out << endl;

char rows_used[rows];
   memset(rows_used, 0, sizeof(rows_used));

   for (const DLX_Node * x = right; x != this; x = x->right)
   for (const DLX_Node * y = x->down; y != x; y = y->down)
       {
         rows_used[y->row] = 1;     // mark row used
       }

   for (ShapeItem r = 0; r < rows; ++r)
       {
         if (!rows_used[r])   continue;

         int row[cols];
         memset(row, 0, sizeof(row));
         for (const DLX_Node * x = right; x != this; x = x->right)
         for (const DLX_Node * y = x->down; y != x; y = y->down)
             {
               if (y->row == r)   row[y->col] = 1;
             }

         out << setw(3) << r << ":";
         for (const DLX_Node * x = right; x != this; x = x->right)
             {
               out << " " << setw(w) << row[x->col];
             }
         out << endl;
       }

   out << "----";
   for (const DLX_Node * x = right; x != this; x = x->right)
       {
         for (int ww = -1; ww < w; ++ww)   out << "-";
       }
   out << endl;

}
//----------------------------------------------------------------------------
void
DLX_Root_Node::deep_check() const
{
   for (const DLX_Node * x = right; x != this; x = x->right)
       {
         const DLX_Header_Node & hdr =
               *reinterpret_cast<const DLX_Header_Node *>(x);
         Assert(hdr.row == -1);
         hdr.check();
         ShapeItem ones = 0;
         for (const DLX_Node * y = x->down; y != x; y = y->down)
             {
               y->check();
              ++ones;
             }
         Assert(ones == hdr.count);
       }
}
//----------------------------------------------------------------------------
void
DLX_Root_Node::solve()
{
new_level:
   Assert(level <= cols);

   deep_check();

   if (LOG_Quad_DLX || attention_is_raised())
      {
        CERR << "⎕DLX[" << level << "]";
        loop(s, level)
            CERR << " " << (headers[s].item_r->row + Workspace::get_IO());
        CERR << endl;
        clear_attention_raised(LOC);
      }

   if (interrupt_is_raised())
      {
        clear_interrupt_raised(LOC);
        return;
      }

   // the problem is solved if there are no primary columns left
   //
DLX_Node * item = right;

   Log(LOG_Quad_DLX)   item->print_RC(CERR << "At header item ") << endl;

   if (primary_count == 0)
      {
        all_solutions.push_back(level);
        loop(s, level)
            all_solutions.push_back(headers[s].item_r->row);

        Log(LOG_Quad_DLX)
           {
             CERR << "!!!!! solution " << solution_count << ": rows are";
             loop(s, level)   CERR << " " << setw(2) << headers[s].item_r->row;
             CERR << endl;
           }

        ++solution_count;
        if (max_solutions && solution_count >= max_solutions)   return;

        goto level_done;
      }

   if (item != this)   // matrix not empty: select shortest column
      {
        ShapeItem col_size = 2*cols;
        for (DLX_Node * i = right; i != this; i = i->right)
            {
              if (headers[i->col].col_type != Col_PRIMARY)   continue;
              const ShapeItem i_size = headers[i->col].count;
              if (i_size < col_size)
                 {
                   item = i;
                   col_size = i_size;
                 }
            }

        if (col_size == 0)
           {
              Log(LOG_Quad_DLX)   CERR << "column " << item-> col
                                       << " is empty" << endl;
              goto level_done;   // empty column: no solution
           }
      }

   Assert(item->row == -1);
   Log(LOG_Quad_DLX)   indent(CERR) << "Choose and cover column c="
                                    << item->col << endl;
   cover(item->col);

   headers[level].item_r = item;

rloop:   // running ↓
   {
     DLX_Node * r = headers[level].item_r->down;

     Log(LOG_Quad_DLX)   r->print_RC(indent(CERR) << "rloop ↓ at r= ") << endl;

     for (DLX_Node * j = r->right; j != r; j = j->right)
         {
           Log(LOG_Quad_DLX)
              {
                indent(CERR) << "Covering column j=" << j->col << endl;
              }
           cover(j->col);
         }

         Log(LOG_Quad_DLX)
            {
              r->print_RC(indent(CERR) << "Picking item ") << endl;
            }
         headers[level].item_r = r;
         ++level;
         ++pick_count;
         goto new_level;
   }

level_done:
   if (level > 0)
      {
        --level;
        DLX_Node * r = headers[level].item_r;
        Log(LOG_Quad_DLX)
           {
             r->print_RC(indent(CERR) << "Backtracking") << endl;
           }

        for (DLX_Node * j = r->left; j != r; j = j->left)
            {
              Log(LOG_Quad_DLX)
                 {
                   indent(CERR) << "Uncovering column j=" << j->col << endl;
                 }
              uncover(j->col);
            }

        r = r->down;
        if (r->row == -1)   // end of rloop reached
           {
             Log(LOG_Quad_DLX)
                {
                  indent(CERR) << "Uncovering column c=" << r->col << endl;
                }
              uncover(r->col);
             goto level_done;
           }

        goto rloop;
      }
}
//----------------------------------------------------------------------------
ShapeItem
DLX_Root_Node::get_column_count() const
{
ShapeItem cols_Z = 0;
   for (DLX_Node * h = right; h != this; h = h->right)   ++cols_Z;
   return cols_Z;
}
//----------------------------------------------------------------------------
Token
DLX_Root_Node::preset(const Cell * steps, ShapeItem step_count, const Cell * cB)
{
const int qio = Workspace::get_IO();
   loop(a, step_count)
      {
        const APL_Integer row = steps++->get_int_value() - qio;
        if (row < 0 || row >= rows)
           {
             MORE_ERROR() << "bad row: " << row;
             DOMAIN_ERROR;
           }

        bool found_a = false;

        // find a '1' or '2' item in row
        //
        loop(b, nodes.size())
           {
             const DLX_Node & n = nodes[b];
             if (n.row == row)   // found '1' or '2'
                {
                  found_a = true;
                  Log(LOG_Quad_DLX)
                     n.print_RC(CERR << "Covering column " << n.col
                                     << " of rightmost item ") << endl;
                  cover(n.col);

                  for (DLX_Node * j = n.right; j != &n; j = j->right)
                      {
                        Log(LOG_Quad_DLX)
                           {
                             CERR << "Covering column j=" << j->col << endl;
                           }
                        cover(j->col);
                      }

                  break;   // next row a
                }
           }
        Assert(found_a);
      }

   // construct return value
   //
const ShapeItem cols_Z = get_column_count();

const Shape shape_Z(rows, cols_Z);
Value_P Z(shape_Z, LOC);

   // first init all cells in Z to whatever  means 0
   //
   loop(z, (rows*cols_Z))
      {
        if (first_0)                      Z->next_ravel_Cell(*first_0);
        else if (cB->is_integer_cell())   Z->next_ravel_0();
        else                              Z->next_ravel_Char(UNI_0);
      }

   // then override those cells in the linked structure to whatever means 1
   //
   Assert(is_header);

DLX_Node * h = right;
   loop(col, cols_Z)
       {
         Assert(h->is_header);   // therefore for (v...) stops
         for (DLX_Node * v = h->down; !v->is_header; v = v->down)
             {
               Assert(!v->is_header);
               Assert(h->col == v->col);
               const Cell & src = cB[h->col + cols*v->row];
               Z->get_wravel(col + cols_Z*v->row).init(src, *Z, LOC);
             }
         h = h->right;
       }

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//============================================================================
Token
Quad_DLX::eval_AB(Value_P A, Value_P B) const
{
   if (A->get_rank() > 1)   RANK_ERROR;
   if (A->element_count() < 1)   LENGTH_ERROR;

   if (B->get_rank() != 2)   RANK_ERROR;
const APL_Integer a0 = A->get_cfirst().get_int_value();

   if (a0 < -4)   DOMAIN_ERROR;
   if (a0 == -4)   // perform some dance steps with the constraints matrix
      {
        const ShapeItem rows = B->get_rows();
        const ShapeItem cols = B->get_cols();
        DLX_Root_Node root(rows, cols, 0, *B);
        return root.preset(&A->get_cravel(1), A->element_count() - 1,
                           &B->get_cfirst());
      }

   if (A->element_count() != 1)   LENGTH_ERROR;
   return do_DLX(a0, *B);
}
//----------------------------------------------------------------------------
Token
Quad_DLX::eval_B(Value_P B) const
{
   if (B->get_rank() != 2)   RANK_ERROR;
   return do_DLX(0, *B);
};
//----------------------------------------------------------------------------
Token
Quad_DLX::do_DLX(ShapeItem how, const Value & B)
{

const ShapeItem rows = B.get_rows();
const ShapeItem cols = B.get_cols();

ShapeItem result_count = how;
   if      (how ==  0)  result_count = 1;   // first solution
   else if (how == -1)  result_count = 0;   // all solution
   else if (how == -2)  result_count = 1;   // statistics: first solution
   else if (how == -3)  result_count = 0;   // statistics: all solutions

DLX_Root_Node root(rows, cols, result_count, B);
   root.solve();

   if (how <= -2)   // return statistics
      {
       Value_P Z(3, LOC);
       Z->next_ravel_Int(root.get_solution_count());
       Z->next_ravel_Int(root.get_pick_count());
       Z->next_ravel_Int(root.get_cover_count());
       Z->check_value(LOC);
        return Token(TOK_APL_VALUE1, Z);
      }

const APL_Integer qio = Workspace::get_IO();
   if (how == 0)   // return first (flat) solution
      {
       if (root.get_solution_count() == 0)   // but no solution found
          {
            return Token(TOK_APL_VALUE1, Idx0(LOC));
          }

       const ShapeItem len = root.all_solutions[0];
       Value_P Z(len, LOC);
       loop(z, len)
         {
           Z->next_ravel_Int(qio + root.all_solutions[z + 1]);
         }
       Z->check_value(LOC);
       return Token(TOK_APL_VALUE1, Z);
      }

   // return all solutions
   //
Value_P Z(root.get_solution_count(), LOC);
   if (root.get_solution_count() == 0)   // empty result
      {
        Value_P Z1 = Idx0(LOC);   // Z1 ← ⍬
        new (&Z->get_wproto())   PointerCell(Z1.get(), *Z);
      }
   else
      {
        ShapeItem a = 0;
        loop(s, root.get_solution_count())
           {
             const ShapeItem len = root.all_solutions[a++];
             Value_P Zs(len, LOC);
             loop(z, len)
                {
                  Zs->next_ravel_Int(qio + root.all_solutions[a++]);
                }
             Zs->check_value(LOC);
             Z->next_ravel_Pointer(Zs.get());
           }
      }

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------

