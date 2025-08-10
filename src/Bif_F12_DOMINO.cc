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

#include "Bif_F12_DOMINO.hh"
#include "Bif_F12_FORMAT.hh"
#include "ComplexCell.hh"
#include "Value.hh"
#include "Workspace.hh"

#include "LApack.hh"

# include "LAdebug.icc"   // print_matrix() etc.

Bif_F12_DOMINO Bif_F12_DOMINO   ::fun;    // ⌹

//----------------------------------------------------------------------------
Token
Bif_F12_DOMINO::eval_B(Value_P B) const
{
   if (B->is_scalar())
      {
        Value_P Z(LOC);

        B->get_cscalar().bif_reciprocal(&Z->get_wscalar());
        Z->check_value(LOC);
        return Token(TOK_APL_VALUE1, Z);
      }

   if (B->get_rank() == 1)   // inversion at the unit sphere
      {
        const double qct = Workspace::get_CT();
        const ShapeItem len = B->get_shape_item(0);
        APL_Complex r2(0.0);
        loop(l, len)
            {
              const APL_Complex b = B->get_cravel(l).get_complex_value();
              r2 += b*b;
            }

        if (r2.real() < qct && r2.real() > -qct &&
            r2.imag() < qct && r2.imag() > -qct)
            DOMAIN_ERROR;

        Value_P Z(len, LOC);

        if (r2.imag() < qct && r2.imag() > -qct)   // real result
           {
             loop(l, len)
                 {
                   const APL_Float b = B->get_cravel(l).get_real_value();
                   Z->next_ravel_Float(b / r2.real());
                 }
           }
        else                                       // complex result
           {
             loop(l, len)
                 {
                   const APL_Complex b = B->get_cravel(l).get_complex_value();
                   Z->next_ravel_Complex(b / r2);
                 }
           }

        Z->set_default(*B.get(), LOC);
        Z->check_value(LOC);
        return Token(TOK_APL_VALUE1, Z);
      }

   if (B->get_rank() > 2)   RANK_ERROR;

const ShapeItem rows = B->get_shape_item(0);
const ShapeItem cols = B->get_shape_item(1);
   if (cols > rows)
      {
        MORE_ERROR() <<
        "⌹B : B is under-specified (B has more columns than rows)";
        LENGTH_ERROR;
      }

   // create an identity matrix I and call eval_AB(I, B).
   //
const Shape shape_I(rows, rows);
Value_P I(shape_I, LOC);

   loop(y, rows)
   loop(x, rows)   I->next_ravel_Float(y == x ? 1.0 : 0.0);

Token result = eval_AB(I, B);
   return result;
}
//----------------------------------------------------------------------------
Token
Bif_F12_DOMINO::eval_XB(Value_P X, Value_P B) const
{
   // X shall be a scalar with:
   //
   // a. real EPS < 1 (aka. rcond) for backward compatibility (not used),
   // b. integer 0 for the QR factorization with the Helzer algorithm, or
   // c. integer 1 for the QR factorization with LApack
   //
   if (!X->is_scalar())   RANK_ERROR;

enum { ALGO_BAD, ALGO_HELZER, ALGO_LAPACK } algo = ALGO_BAD;
const Cell & X0 = X->get_cscalar();
double EPS = Workspace::get_CT();

   if (X0.is_float_cell())   // a.
      {
        EPS = X0.get_real_value();
        if (EPS < 0.1)   algo = ALGO_HELZER;
      }
   else if (X0.is_integer_cell())   // b. or c.
      {
        if      (X0.get_int_value() <= 1)   algo = ALGO_HELZER;
        else if (X0.get_int_value() == 2)   algo = ALGO_LAPACK;
      }

   if (algo == ALGO_BAD)
      {
        MORE_ERROR() << "Bad algorithm in X of ⌹[X]B";
        DOMAIN_ERROR;
      }

   if (B->get_rank() != 2)   RANK_ERROR;

   // if rank of A or B is < 2 then treat it as a
   // 1 by n (or 1 by 1) matrix..
   //
const ShapeItem M = B->get_rows();
const ShapeItem N = B->get_cols();
   if (M*N == 0)   LENGTH_ERROR;   // empty B

const bool need_complex = B->is_complex(true);
Value_P Z(3, LOC);

   if (algo == ALGO_HELZER)
      {
        LA_DEBUG && CERR << "QR factorization with G. Helzer's algorithm...\n";
        QR_Helzer(Z, need_complex, M, N, &B->get_cfirst(), EPS);
      }
   else
      {
        LA_DEBUG && CERR << "QR factorization with LA_pack::laqp2()...\n";
        if (need_complex)
           LA_pack::factorize_ZZ_matrix(*Z, M, N, &B->get_cfirst(), EPS);
        else
           LA_pack::factorize_DD_matrix(*Z, M, N, &B->get_cfirst(), EPS);
      }

   Z->set_proto_Int();   // never since M*cols__B ≠ 0. Just for clarity
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Bif_F12_DOMINO::eval_AB(Value_P A, Value_P B) const
{
ShapeItem rows_A = 1;
ShapeItem cols_A = 1;
ShapeItem rows_B = 1;
ShapeItem cols_B = 1;

   // if rank of A or B is < 2 then treat it as a
   // 1 by n (or 1 by 1) matrix..
   //
Shape shape_Z;   // ⍴Z ←→ (¯1↓⍴A), (1↓⍴B)
   switch(B->get_rank())
      {
         case 0:  break;

         case 1:  rows_B = B->get_shape_item(0);
                  break;

         case 2:  cols_B = B->get_shape_item(1);
                  rows_B = B->get_shape_item(0);
                  shape_Z.add_shape_item(cols_B);
                  break;

         default: RANK_ERROR;
      }

   switch(A->get_rank())
      {
         case 0:  break;

         case 1:  rows_A = A->get_shape_item(0);
                  break;

         case 2:  cols_A = A->get_shape_item(1);
                  rows_A = A->get_shape_item(0);
                  shape_Z.add_shape_item(cols_A);
                  break;

         default: RANK_ERROR;
      }

   if (rows_B < cols_B)
      {
        MORE_ERROR() <<
        "A⌹B : B is under-specified (B has more columns than rows)";
        LENGTH_ERROR;
       }

   if (rows_A != rows_B)
      {
        MORE_ERROR() << "A÷B : number of rows in A ≠ number of rows in B";
        LENGTH_ERROR;
      }

const bool need_complex = A->is_complex(true) || B->is_complex(true);
Value_P Z(shape_Z, LOC);
const sRank rank = need_complex ?  LA_pack::divide_ZZ_matrix(*Z, rows_A,
                                          cols_A, &A->get_cfirst(),
                                          cols_B, &B->get_cfirst())
                                :  LA_pack::divide_DD_matrix(*Z, rows_A,
                                          cols_A, &A->get_cfirst(),
                                          cols_B, &B->get_cfirst());

   if (rank < cols_B)
      {
        const char * type = need_complex ? "complex" : "real";
        MORE_ERROR() << "A⌹B : linearly dependent (" << type << ") B?"
                        " ⍴B is " << rows_A << " " << cols_B
                     << ", but the estimated rank is " << rank << ".\n"
                     << "      NOTE that the estimated rank "
                        "is controlled by ⎕CT.";
        DOMAIN_ERROR;
      }

   Z->set_default(*B.get(), LOC);

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Bif_F12_DOMINO::eval_fill_B(Value_P B) const
{
   return Bif_F12_TRANSPOSE::do_eval_B(B.get());
}
//----------------------------------------------------------------------------
Token
Bif_F12_DOMINO::eval_fill_AB(Value_P A, Value_P B) const
{
Shape shape_Z;
   loop(r, A->get_rank() - 1)  shape_Z.add_shape_item(A->get_shape_item(r + 1));
   loop(r, B->get_rank() - 1)  shape_Z.add_shape_item(B->get_shape_item(r + 1));

Value_P Z(shape_Z, LOC);
   while (Z->more())   Z->next_ravel_0();
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Bif_F12_DOMINO::eval_AXB(Value_P A, Value_P X, Value_P B) const
{
   TODO;
}
//----------------------------------------------------------------------------
/// print debug infos for \b this real matrix
template<>
void Bif_F12_DOMINO::Matrix<false>::debug(const char * name) const
{
#if DOMINO_DEBUG
const Shape shape_B(M, N);
Value_P B(shape_B, LOC);

   loop(y, M)
   loop(x, N)   B->next_ravel_Float(real(y, x));
   B->check_value(LOC);

Value_P A(2, LOC);   // A←0 4
   A->next_ravel_0();
   A->next_ravel_Int(4);   // number of fractional digits
   A->check_value(LOC);

Value_P Z = Bif_F12_FORMAT::format_by_specification(A, B);
   CERR << name;
   Z->print_boxed(CERR, 0);
#endif // DOMINO_DEBUG
}
//----------------------------------------------------------------------------
/// print debug infos for \b this complex matrix
template<>
void Bif_F12_DOMINO::Matrix<true>::debug(const char * name) const
{
#if DOMINO_DEBUG
const Shape shape_B(M, N);
Value_P B(shape_B, LOC);

   loop(y, M)
   loop(x, N)   B->next_ravel_Complex(real(y, x), imag(y, x));
   B->check_value(LOC);

Value_P A(2, LOC);
   A->next_ravel_0();
   A->next_ravel_Int(4);   // number of fractional digits
   A->check_value(LOC);

Value_P Z = Bif_F12_FORMAT::format_by_specification(A, B);
   CERR << name;
   Z->print_boxed(CERR, 0);
#endif // DOMINO_DEBUG
}
//----------------------------------------------------------------------------
void
Bif_F12_DOMINO::QR_Helzer(Value_P Z, bool need_complex, ShapeItem M,
                          ShapeItem N, const Cell * cB, double EPS)
{
   /* We want to store all floating point variables (including complex ones)
      in a single double[]. Before and after each variable we leave one double
      for storing numbers 42.0, 43.0, ...51.0. These numbers are used to check
      for overrides of the allocated space.

      Complex numbers are stored as their real part followed by imag part.

      The variables are B, Q, and R with B = Q +.× R, with Q real orthogonal
      and R real or complex upper triangular. Since being R is computed after
      Q, it can be used as a temporary variable in the computation of Q (i,e,
      in function householder()).
   */

   // start with the base addresses of the variables. All variables are
   // allocated as M * M so that we can freely rorate them...
   //
const int CPLX = need_complex ? 2 : 1;   // number of doubles per variable item
const ShapeItem max_MN  = M > N ? M : N;
const ShapeItem len_B   = M * N;
const ShapeItem len_QR  = max_MN * max_MN;
const ShapeItem base_B  = 1;
const ShapeItem base_Q  = 1 + base_B  + CPLX*len_QR + 1;
const ShapeItem base_Qi = 1 + base_Q  + CPLX*len_QR + 1;
const ShapeItem base_R  = 1 + base_Qi + CPLX*len_QR + 1;
const ShapeItem base_S  = 1 + base_R  + CPLX*len_QR + 1;
const ShapeItem end     = 1 + base_S  + CPLX*len_QR + 1;
#define base_AUG  base_Q   /* reuse Q */

double * data = new double[end*CPLX];   if (data == 0)   WS_FULL;
   memset(data, 0, end*sizeof(double));
   data[base_B - 1]  = 42.0;   data[base_B  + CPLX*len_B] = 43.0;
   data[base_Q - 1]  = 44.0;   data[base_Q  + CPLX*len_QR]   = 45.0;
   data[base_Qi - 1] = 46.0;   data[base_Qi + CPLX*len_QR]   = 47.0;
   data[base_R - 1]  = 48.0;   data[base_R  + CPLX*len_QR]   = 49.0;
   data[base_S - 1]  = 50.0;   data[base_S  + CPLX*len_QR]   = 51.0;

   // compute the QR factorization of B. That is:
   //
   // B = Q∘R where:
   //
   // ⍴B ←→ M N,
   // ⍴Q ←→ N N  (since M≥N) and orthogonal,
   // ⍴R is M N  (since ⍴B ←→ ⍴Q+.×R) and upper triangular
   if (need_complex)   // complex B
      {
        setup_complex_B(cB, data + base_B, len_B);
        double * Q = householder<true>(data + base_B, M, N, data + base_Q,
                          data + base_Qi, data + base_R, data + base_S, EPS);

        setup_complex_B(cB, data + base_B, len_B);   // restore B
        const Matrix<true> Bm(data + base_B, M, N);
        Matrix<true> Qm(Q, M, M);
        Qm.transpose(M);
        Matrix<true> Rm(data + base_R, M, N);
        Rm.init_inner_product(Qm, Bm);
        Qm.debug("final Q");
        Rm.debug("final R");
      }
   else                // real B
      {
   Assert(data[base_B + CPLX*len_B]  == 43.0);
        setup_real_B(cB, data + base_B, len_B);
   Assert(data[base_B + CPLX*len_B]  == 43.0);
        double * Q = householder<false>(data + base_B, M,N, data + base_Q,
                           data + base_Qi, data + base_R, data + base_S, EPS);

        setup_real_B(cB, data + base_B, len_B);   // restore B
        const Matrix<false> Bm(data + base_B, M, N);
        Matrix<false> Qm(Q, M, M);
        Qm.transpose(M);
        Matrix<false> Rm(data + base_R, M, N);
        Rm.init_inner_product(Qm, Bm);
   Assert(data[base_B + CPLX*len_B]  == 43.0);
      }

   // check that the memory areas were not overridden
   //
   Assert(data[base_B - 1]            == 42.0);
   Assert(data[base_B + CPLX*len_B]   == 43.0);
   Assert(data[base_Q - 1]            == 44.0);
   Assert(data[base_Q + CPLX*len_QR]  == 45.0);
   Assert(data[base_Qi - 1]           == 46.0);
   Assert(data[base_Qi + CPLX*len_QR] == 47.0);
   Assert(data[base_R - 1]            == 48.0);
   Assert(data[base_R + CPLX*len_QR]  == 49.0);
   Assert(data[base_S - 1]            == 50.0);
   Assert(data[base_S + CPLX*len_QR]  == 51.0);

   // Z[1] aka. Q
   {
     const Shape shape_Z1(M, M);
     Value_P Z1(shape_Z1, LOC);
     if (need_complex)
        {
          ALL_COLS(M)   // FORTRAN order
          ALL_ROWS(M)
             {
               const ShapeItem offset = 2*(col + row*M);
               const double real = data[base_Q + offset];
               const double imag = data[base_Q + offset + 1];
               if (!(isfinite(real) && isfinite(imag)))   DOMAIN_ERROR;
               Z1->next_ravel_Complex(real, imag);
             }
        }
     else
        {
          ALL_COLS(M)   // FORTRAN order
          ALL_ROWS(M)
             {
               const ShapeItem offset = col + row*M;
               const double real = data[base_Q + offset];
               if (!isfinite(real))   DOMAIN_ERROR;
               Z1->next_ravel_Float(real);
             }
        }
     Z1->check_value(LOC);
     Z->next_ravel_Pointer(Z1.get());
   }

   // Z[2] aka. R
   {
     const Shape shape_Z2(M, N);
     Value_P Z2(shape_Z2, LOC);
     if (need_complex)
        {
          ALL_ROWS(M)   // APL order
          ALL_COLS(N)   // APL order
             {
               const ShapeItem offset = col + row*N;   // APL order
               if (row > col)   // below diagonal: force 0.0
                  {
                     data[base_R + 2*offset]     = 0;
                     data[base_R + 2*offset + 1] = 0;
                  }
               const double real = data[base_R + 2*offset];
               const double imag = data[base_R + 2*offset + 1];
               if (!(isfinite(real) && isfinite(imag)))   DOMAIN_ERROR;
               Z2->next_ravel_Complex(real, imag);
              }
        }
     else
        {
          ALL_ROWS(M)   // APL order
          ALL_COLS(N)   // APL order
             {
               const ShapeItem offset = col + row*N;   // APL order
               if (row > col)   // below diagonal: force 0.0
                  {
                     data[base_R + offset]     = 0;
                  }
               const double real = data[base_R + offset];
               if (!isfinite(real))   DOMAIN_ERROR;
               Z2->next_ravel_Float(real);
              }
        }
     Z2->check_value(LOC);
     Z->next_ravel_Pointer(Z2.get());
   }


const ShapeItem D = M < N ? M : N;   // length of the diagonal

   // function householder above has computed R aka. UTM in APL order.
   // Function LA_pack::invert_T_UTM() wants it in FORTRAN order.
   // We therefore need to ⍉ R and possibly fix UTM
   //
   if (M < N)   // fix UTM
      {
        /* R is under-specified. We want to invert only M M↑R,
           which requires some re-ordering of R:
          
              ├──────── N ────────┤        ├────── M ──────┤
           ┬  ╔═════════C═════════╗     ┬  ╔═════════C═════╗
           │  ║               │   ║     │  ║               ║
           │  ║               │   ║     │  ║               ║
           M  ║               │   ║  →  M  ║               ║
           │  ║               │   ║     │  ║               ║
           │  ║               │   ║     │  ║               ║
           ┴  ╚═══════════════════╝     ┴  ╚═══════════════╝
              ├────── M ──────┤
         */
          ALL_ROWS(M)   // APL order
          ALL_COLS(M)   // APL order
             {
               const ShapeItem offset_from = col + N*row;
               const ShapeItem offset_to   = col + M*row;
               if (need_complex)
                  {
                    data[base_R + 2*offset_to] = data[base_R + 2*offset_from];
                    data[base_R + 2*offset_to + 1] =
                                             data[base_R + 2*offset_from + 1];
                  }
               else
                  {
                    data[base_R + offset_to] = data[base_R + offset_from];
                  }
             }
      }

   if (need_complex)
      {
        LA_pack::fMatrix<LA_pack::ZZ >UTM(data + base_R,   D, D, D);
        LA_pack::fMatrix<LA_pack::ZZ >AUG(data + base_AUG, D, D, D);

        UTM.set_transpose();   // ⍉R

        Value_P Z3 = LA_pack::invert_ZZ_UTM(M, N, UTM, AUG);
        Z->next_ravel_Pointer(Z3.get());
      }
   else
      {
        LA_pack::fMatrix<LA_pack::DD>UTM(data + base_R,   D, D, D);
        LA_pack::fMatrix<LA_pack::DD>AUG(data + base_AUG, D, D, D);

        UTM.set_transpose();   // ⍉R

        Value_P Z3 = LA_pack::invert_DD_UTM(D, D, UTM, AUG);
        Z->next_ravel_Pointer(Z3.get());
      }

   delete[] data;
#undef base_AUG
}
//----------------------------------------------------------------------------
void
Bif_F12_DOMINO::setup_complex_B(const Cell * cB, double * D, ShapeItem count)
{
   // initialize the homogeneous complex vector D from the mixed APL ravel cB
   //
   loop(b, count)
      {
        const Cell & cell = *cB++;
        if (cell.is_float_cell())
           { *D++ = cell.get_real_value();   *D++ = 0.0; }
        else if (cell.is_integer_cell())
           { *D++ = cell.get_real_value();   *D++ = 0.0; }
        else if (cell.is_complex_cell())
           { *D++ = cell.get_real_value(); *D++ = cell.get_imag_value(); }
        else   DOMAIN_ERROR;
      }
}
//----------------------------------------------------------------------------
void
Bif_F12_DOMINO::setup_real_B(const Cell * cB, double * D, ShapeItem count)
{
   // initialize the homogeneous real vector D from the mixed APL ravel cB
   //
   loop(b, count)
      {
        const Cell & cell = *cB++;
        if (cell.is_float_cell())          *D++ = cell.get_real_value();
        else if (cell.is_integer_cell())   *D++ = cell.get_real_value();
        else                               DOMAIN_ERROR;
      }
}
//----------------------------------------------------------------------------
template<bool cplx>
double *
Bif_F12_DOMINO::householder(double * pB, ShapeItem rows, ShapeItem cols,
                            double * pQ, double * pQi, double * pT, double * pS,
                            double EPS)
{
   // pB is the matrix to be factorized.
   // the caller has initialized pQ, pQi, and pT to 0.0
   //
   // the algorithm is essentially the one described in Garry Helzer's paper
   // "THE HOUSEHOLDER ALGORITHM AND APPLICATIONS" but using complex numbers
   // when needed.

ShapeItem dias = rows < cols ? rows : cols;   // number of diagonals
const double qct = Workspace::get_CT();
const double qct2 = qct*qct;
double BMAX = 0.0;

Matrix<cplx> mT (pT,  rows, rows);   // temporary storage
Matrix<cplx> mQ (pQ,  rows, rows);   // keeps size
Matrix<cplx> mB (pB,  rows, cols);   // shrinks
Matrix<cplx> mQi(pQi, rows, rows);   // keeps size

   // [0]  Q←HSHLDR2 B;N;BMAX;S;L2;QI;COL1
   // [1]  Q←ID N←↑⍴B

   // mQ was cleared, so setting the diagonal suffices
   loop(x, rows)   mQ.real(x, x) = 1.0;


   // [2]  →(0=(1↓⍴B),BMAX←⌈/∣,B)/0   ⍝ done if no or only near-0 columns

//    Q1(cols)
   if (cols == 0)   return pQ;
   loop(y, rows)
   loop(x, cols)
       {
         const double abs2 = mB.abs2(y, x);
         if (BMAX < abs2)   BMAX = abs2;
       }
   BMAX = sqrt(BMAX);
   if (BMAX < qct)
      {
//         Q1("B is 0")
        return pQ;   // all B[x;y] = 0
      }

   for (;;)
       {

   // [3]  SPRFLCTR: S←ID ↑⍴B ◊ Debug 'B'

        Matrix<cplx> mS (pS,  rows, rows);
mB.debug("[3] B");

   // [4]  L2←NORM2 COL1←B[;1] ◊ Debug 'COL1' ◊ Debug 'L2'

        Matrix<cplx> mCOL1 (pB,  rows, 1, mB.dY);   // COL1←B[;1]
mCOL1.debug("[4] COL1");
        norm_result L;   mB.col1_norm(L);
// Q1(L.norm2_real)
// Q1(L.norm2_imag)
        const bool significant = mCOL1.significant(BMAX, EPS);

   // [5]  IMBED → L2=0
   // [6]  IMBED → ∼0ϵ0=(1↓COL1) CMP_TOL EPS BMAX

        if (significant || (L.norm2_real + L.norm2_imag) > qct2)
           {
// Q1("SIGNIFICANT")
   // [7]  B[1;1]←(↑B) + (L2⋆÷2)×(0≤↑B)-0>↑B

#if 0
             Matrix<cplx>::add_sub(&mB.real(0, 0), &L.norm2_real);
#else
             double * B11_real = &mB.real(0, 0);
             double * B11_imag = B11_real + 1;
             if (*B11_real < 0)
                {
// Q1("SIGN ¯1")
// Q1(*B11_real)
// Q1(*B11_imag)
                  *B11_real -= L.norm_real;
                  *B11_imag -= L.norm_imag;
                }
             else
                {
// Q1("SIGN 1")
// Q1(*B11_real)
// Q1(*B11_imag)
                  *B11_real += L.norm_real;
                  *B11_imag += L.norm_imag;
                }
#endif

   // [8]   COL1←B[;1] ◊ Debug 'COL1'
   // [9]   SCALE←2÷NORM2 COL1 ◊ Debug 'NORM2 COL1' ◊ Debug 'SCALE'·
   // [10]  S←COL1∘.×COL1×SCALE ◊ (1 1⍉S)←1 1⍉S - 1.0


             // COL1←B[;1] changes nothing, so only S←... remains.
             // We have moved the initialization of S to the else
             // clause below. Therefore -S on the right does not work
             // here, but we can simply subtract 1.0 from the diagonal.
             //
mCOL1.debug("[8] COL1");
             norm_result scale;   mCOL1.col1_norm(scale);
             mS.init_outer_product(scale, mCOL1);
// Q1(scale.norm__2_real)
// Q1(scale.norm__2_imag)
             loop(y, rows)   mS.real(y, y) -= 1.0;   // subtract 1.0
           }
        else   // →IMBED but do [3] here
           {
             mS.init_identity(rows);
           }

   // [9] QI←ID N ◊ ((-2/↑⍴S)↑QI)←S ◊ Debug 'QI'

   mQi.imbed(mS);
mQi.debug("[11] QI");
mS .debug("[11] S");

mQ.debug("[12] Q before Q←Q+.×QI");
   // [12]   Q←Q+.×QI ◊ Debug 'Q'

   // use mT as temporary buffer for mB, so that init_inner_product() works
   mT.resize(mQ.M, mQ.N);   mT = mQ;

mT.debug("[12] T←Q before Q←Q+.×QI");
   mQ.init_inner_product(mT, mQi);
mQ.debug("[12] Q after Q←Q+.×QI");

   // since we are only interested in Q we can skip the final B←1 1↓S+.×B
   //
   if (0 == --dias)
      {
        mQ.debug("[end] Q");
        return pQ;
      }

   // [13]   Debug 'B' ◊ Debug 'S' ◊ B←1 1↓S+.×B ◊ Debug 'B'

   // use mT as temporary buffer for mB, so that init_inner_product() works
   mT.resize(mB.M, mB.N);   mT = mB;
mT.debug("[13] B");
mS.debug("[13] S");
   mB.resize(mS.M, mT.N);
   mB.init_inner_product(mS, mT);

   pB = mB.drop_1_1();   // 1 1↓B
mB.debug("[13] B");

   // [14]   →(0≠1↓⍴B)/SPRFLCTR

         --rows;
       }
}
//----------------------------------------------------------------------------

