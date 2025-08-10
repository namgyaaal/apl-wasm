/*
    This file is a port of the LApack 'dgelsy' and 'zgelsy' functions
    (and of the functions that they call) from liblapack to C++.

    liblapack has the following license/copyright notices,
    see also http://www.netlib.org/lapack/LICENSE:

    Copyright (c) 1992-2011 The University of Tennessee and The University
                            of Tennessee Research Foundation.
                            All rights reserved.
    Copyright (c) 2000-2011 The University of California Berkeley.
                            All rights reserved.
    Copyright (c) 2006-2011 The University of Colorado Denver.
                            All rights reserved.


    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:

    - Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer. 
  
    - Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer listed
      in this license in the documentation and/or other materials
      provided with the distribution.
  
    - Neither the name of the copyright holders nor the names of its
      contributors may be used to endorse or promote products derived from
      this software without specific prior written permission.
  
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT  
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT  
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
*/

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

#define LA_DEBUG     0   /* LApack.cc */
#define DOMINO_DEBUG 0   /* Bif_F12_DOMINO::householder */

typedef int Crow;   ///< a row number:      0..M (excluding M)
typedef int Ccol;   ///< a column number:   0..N (excluding N)
typedef int Cdia;   ///< a diagonal (row == col) number:   0..N (excluding N)

#define ALL_ROWS(M)   for (Crow row = 0; row < (M); ++row)
#define ALL_COLS(N)   for (Ccol col = 0; col < (N); ++col)
#define ALL_DIAS(N)   for (Cdia dia = 0; dia < (N); ++dia)

#define REV_ROWS(M)   for (Crow j = (M); j--;)
#define REV_COLS(N)   for (Ccol k = (N); k--;)
#define REV_DIAG(N)   for (Cdia d = (N); d--;)

/** A class that implements Dgelsy() and Zgelsy(), bundled with the helper
    functions that the implementation needs. Class LA_pack uses the following
    sub-classes:

   DD:   a single real number
   ZZ:   a single complex number
   fMatrix<T> a matrix of T (where T is DD or ZZ)

   fMatrix<T> is actually a view of an underlying T* so that makes it possible
   to access sub-matrices without copying them (in-place modification).

   In FORTRAN, every fMatrix A, B, C... is accompanied by an integer LDA, LDB,
   LDC ... (aka. the Leading Dimension of A, B, C, ..., which is the number of
   rows in the matrix.

   In C/C++ the FORTRAN LDA, LDB, LDC, ... are the member 'dx' of the
   corresponding fMatrix<T> classes A, B, C, ....
 */
class LA_pack
{
public:
  /// a real number
  typedef APL_Float DD;

#if 1

#include <complex>
  /// a complex number
  class ZZ: public std::complex<APL_Float>
     {
     public:
       ZZ()
          { real(0.0); imag(0.0); }

       ZZ(APL_Float re, APL_Float im)
          { real(re); imag(im); }

       ZZ(APL_Float re)
          { real(re); imag(0.0); }

       ZZ(const std::complex<APL_Float> & cpx)
          { real(cpx.real()); imag(cpx.imag()); }

#else

  /// a complex number
  class ZZ
     {
     protected:
       /// the real part
       APL_Float _r;
     
       /// the imaginary part
       APL_Float _i;

     public:
       /// constructor: 0.0J0.0   (real 0.0)
       ZZ() : _r(0.0), _i(0.0) {}
     
       /// constructor: reJ0.0   (real re)
       ZZ(APL_Float re) : _r(re), _i(0) {}
     
       /// constructor: reJim   (complex r re + I×im)
       ZZ(APL_Float re, APL_Float im) : _r(re), _i(im) {}
     
       /// return the real part of \b this ZZ
       APL_Float real() const
          { return _r; }
     
       /// return the imag part of \b this ZZ
       APL_Float imag() const
          { return _i; }
     
       /// set the real part of \b this ZZ
       void real(APL_Float value)
          { _r = value; }
     
       /// set the imag part of \b this ZZ
       void imag(APL_Float value)
          { _i = value; }
     
       /// add complex \b this and complex \b zz
       ZZ operator +(const ZZ & zz) const
          { return ZZ(_r + zz._r, _i + zz._i); }

       /// add complex \b zz to \b this
       void operator += (const ZZ & zz)
          { _r += zz._r; _i += zz._i; }

       /// negative of \b this ZZ
       ZZ operator -() const
          { return ZZ(-_r, -_i); }

       /// subtract complex \b z2 from complex \b this
       ZZ operator -(const ZZ & zz) const
          { return ZZ(_r - zz._r, _i - zz._i); }

       /// subtract complex \b zz from \b this
       void operator -=(const ZZ & zz)
          { _r -= zz._r; _i -= zz._i; }

       /// multiply complex \b z1 with complex \b zz
       ZZ operator *(const ZZ & zz) const
          { return ZZ(_r*zz._r - _i*zz._i,
                      _i*zz._r + _r*zz._i); }

       /// multiply *this with real \b dd
       void operator *=(const ZZ & zz)
          { *this = *this * zz; }

       /// divide complex \b this by complex \b zz
       ZZ operator /(const ZZ & zz) const
          { 
            const APL_Float denominator = square(zz);
            return ZZ((_r*zz._r + _i*zz._i) / denominator,
                      (_i*zz._r - _r*zz._i) / denominator);
          }

       /// divide *this by complex \b zz
       void operator /=(const ZZ & zz)
          { *this = *this / zz; }

       /// divide complex \b this by real \b z2
       ZZ operator /(const APL_Float & dd) const
          { return ZZ(_r/dd, _i/dd); }

#endif   // std::complex vs. class ZZ

       /// the square of the hypotenuse 
       APL_Float hypo_2() const
          { return real()*real() + imag()*imag(); }
     };   // class ZZ
 
  //----------------------------------------------------------------------------
  /// A double or complex matrix. Actually a matrix view of some data (where
  /// data is similar to an APL ravel)
  template<typename T>
  class fMatrix
     {
     public:
        /// constructor: _rows × _cols matrix with values vdata.
         /// Unlike e.g. std::vector<T> (which copies \b data), \b vdata must
         /// outlive \b this, and modifying items also modifies \b vdata!
         //
        fMatrix(void * vdata, Crow M, Ccol N, ShapeItem _dx)
          : data(reinterpret_cast<T *>(vdata)),
            rows(M),
            cols(N),
            transpose(false),
            dx(_dx)
        {}
     
        /// constructor: sub-matrix of \b other with \b new_col_count columns
        /// CAUTION: other must outlive \b this!
        fMatrix(const fMatrix & other, ShapeItem new_col_count)
          : data(other.data),
            rows(other.rows),
            cols(new_col_count),
            transpose(other.transpose),
            dx(other.dx)
        { Assert(new_col_count <= other.cols); }

        /// copy from \b other
        void operator =(const fMatrix & other)
           {
             transpose = other.transpose;
             Assert(rows*cols == other.rows*other.cols);
             ALL_ROWS(rows)
             ALL_COLS(cols)   at(row, col) = other.at(row, col);
           }

        /// return the sub-matrix starting at row and col.
        /// I.e. \b row col↓this
        fMatrix sub_matrix(Crow row, Crow col)
           {
             Assert(col <= cols && row <= rows);
             return fMatrix(&at(row, col), rows - row, cols - col, dx);
           }

        void set_rows(Ccol new_rows)
           {
             const_cast<ShapeItem &>(dx) = dx + new_rows - rows;
             const_cast<Crow &>(rows) = new_rows;
           }

        void set_columns(Ccol new_cols)
           { const_cast<Ccol &>(cols) = new_cols; }

        void set_transpose()
           { transpose = true; }

        /// return the sub-matrix of size new_len_y: new_len_x.
        /// I.e. \b new_rows new_rows↑this
        /// starting at row 0 and column 0
        fMatrix take(ShapeItem new_rows, ShapeItem new_cols)
           {
             Assert(new_rows <= rows);
             Assert(new_cols <= cols);
             return fMatrix(data, new_rows, new_cols, dx);
           }
     
        /// return the number of rows of \b this Matrox
        const ShapeItem get_row_count() const
           { return rows; }
     
        /// return the number of columns of \b this fMatrix
        const ShapeItem get_column_count() const
           { return cols; }
     
        /// return the number of items of \b this fMatrix
        const ShapeItem get_item_count() const
           { return rows * cols; }
     
        /// return the distance between 2 adjacent columns (of the same row)
        /// in FORTRAN this is LDA (aka. the Leading Dimension) of matrix A
        const ShapeItem get_dx() const
           { return dx; }
     
        /// return const \b this[row, col]
        const T & at(Crow row, Ccol col) const
           { Assert(row < rows);
             Assert(col < cols);
             return data[row + col*dx]; }
     
        /// return \b this[row, col]
        T & at(Crow row, Ccol col)
           { Assert(row < rows);
             Assert(col < cols);
             return data[row + col*dx]; }
     
        /// return \b this[row, col] or this[col, row]
        const T & AT(Crow row, Ccol col) const
           { Assert(row < rows);   Assert(col < cols);
             return transpose ? data[col + row*dx] : data[row + col*dx];
           }
     
        /// return \b this[row, col] or this[col, row]
        T & AT(Crow row, Ccol col)
           { Assert(row < rows);   Assert(col < cols);
             return transpose ? data[col + row*dx] : data[row + col*dx];
           }
     
        /// return \b this[i, i]
        T & diag(ShapeItem i)
           { Assert(i < rows);
             Assert(i < cols);
             return data[i*(1 + dx)]; }
     
        /// return \b this[i, i]
        const T & diag(ShapeItem i) const
           { Assert(i < rows);
             Assert(i < cols);
             return data[i*(1 + dx)]; }

        /// exchange columns \b c1 and \b c2
        void exchange_columns(ShapeItem c1, ShapeItem c2)
           {
             Assert(c1 < cols);
             Assert(c2 < cols);
             T * p1 = data + c1*dx;
             T * p2 = data + c2*dx;
             loop(r, rows)   exchange(*p1++, *p2++);
           }

     protected:                        // fMatrix<T>
        /// the elements of \b this matrix
        T * const data;
     
        /// the number of rows of \b this matrix
        const Crow rows;
     
        /// the number of columns of \b this matrix
        const Ccol cols;
     
        /// whether AT() shall transpose
        bool transpose;

        /// return the distance between two adjacent columns in \b data.
        /// this distance is usually called LDx (Leading Dimension of x)
        /// in FORTRAN, e.g. LDA for a FORTRAN matrix A with LDA rows.
        const ShapeItem dx;
     };                                // class fMatrix<T>

  /// a clone of matrix that destroys itself (used for debugging purpoases)
  template<typename T>
  class TempMatrix : public fMatrix<T>
     {
     public:
        TempMatrix(const fMatrix<T> & other)
          : fMatrix<T>(allocate(&other.diag(0),   // = other.data()
                                other.get_item_count()),
                      other.get_row_count(),
                      other.get_column_count(),
                      other.get_dx())
           {
           }
       
        ~TempMatrix()
           {
             delete[] fMatrix<T>::data;
           }

         static T * const allocate(const T * src, int item_count)
            {
             const int bytes = item_count * sizeof(T);
             char * dest = new char[bytes];
             memcpy(dest, src, bytes);
             return reinterpret_cast<T *>(dest);
            }
     };                                // class TempMatrix<T>

   /// FORTRAN work memories
   template<typename T>
   struct PTVVy
      {
        /// constructor for N column matrices
        PTVVy(Ccol N, bool with_pivot);
        ~PTVVy();                 ///< destructor
        Ccol *      pivot;        ///< column pivot
        T *         tau;          ///< diagonal elements
        APL_Float * vn1;          ///< column norms 1
        APL_Float * vn2;          ///< column norms 2
        T *         y;            ///< y for larf()
        T *         work_min;     ///< work_min
        T *         work_max;     ///< work_max

        /// print the column permutation, Returns \b len
        int print_pivot(ostream & out, Ccol len, const char * loc) const;

        /// print the diagonal factors, Returns \b len
        int print_tau(ostream & out, Ccol len, const char * loc) const;
      };                               // struct PTVVy<T>

  //==========================================================================

  // static real and complex functions. Most functions exist in 2 variants:
  // one for real (DD) and one for complex (ZZ) argumments.
  // The template<typename T> expansion of type T will later pick the
  // appropriate variant.

  /// the maximum of \b x and \b y
  static DD max(DD x, DD y)
     { return  x < y ? y : x; }
  
  /// return the absolute value (=length) of real dd
  static DD abs(DD dd)
     { return dd < 0.0 ? -dd : dd; }
  
  /// return the absolute value (= length) of complex zz
  static DD abs(ZZ zz)
     { return sqrt(square(zz)); }
  
  /// swap x and y
  template<typename T>
  static void exchange(T & x, T & y)
     {
       const T tmp = x;
       x = y;
       y = tmp;
     }

  /// return the square of the real dd
  static DD square(DD dd)
     { return dd * dd; }
  
  /// return the square of the absolute value of complex zz
  static DD square(ZZ zz)
     { return square(zz.real()) + square(zz.imag()); }
  
  /// A² + B² = C²
  template<typename T1, typename T2>
  static APL_Float hypotenuse(const T1 & kath_A, const T2 & kath_B)
     { return sqrt(square(kath_A) + square(kath_B)); }

  /// scale SIN and COS so that SIN² + COS² = 1.0
  template<typename T>
  static APL_Float normalize(T & SIN, T & COS)
     {
        const APL_Float hypo = hypotenuse(SIN, COS);
        SIN /= hypo;
        COS /= hypo;
        return hypo;
     }
 
  /// return the offset of the (absolute) largest element in vec
  static Ccol
  max_pos(const APL_Float * vec, ShapeItem len)
     {
       Ccol imax = 0;                 // start with index 0
       APL_Float vec_max = vec[0];   // and its value
       for (Ccol j = 1; j < len; ++j)
           {
             const APL_Float vec_j = abs(vec[j]);
             if (vec_max < vec_j)   { vec_max = vec_j;   imax = j; }
           }
     
        return imax;
     }

   /// return (the length of) the largest item in data. Always ≥ 0.
   static APL_Float max_item(const DD * data, size_t len)
      {
        APL_Float ret = 0;
        loop(l, len)
            {
              const APL_Float a = get_real(data[l]);
              if      (ret < a)   ret = a;    // larger a ≥ 0
              else if (ret < -a)  ret = -a;   // larger a < 0
            }
        return ret;
      }

   /// return (the length of) the largest item in data. Always ≥ 0.
   static APL_Float max_item(const ZZ * data, size_t len)
      {
        APL_Float ret_2 = 0;
        loop(l, len)
            {
              const APL_Float c_2 = data++->hypo_2();
              if (ret_2 < c_2)   ret_2 = c_2;
            }
        return sqrt(ret_2);
      }

  static void scale(DD * data, size_t len, APL_Float factor)
     { loop(l, len)   data[l] *= factor; }

  static void scale(ZZ * data, size_t len, APL_Float factor)
     { loop(l, len)   data[l] *= factor; }

  static void scale(ZZ * data, size_t len, const ZZ & factor)
     { loop(l, len)   data[l] *= factor; }

  //============================================================================
  // (static) functions ported from LAPACK (FORTRAN) .f files...
public:
  /// return ║ vec ║²
  static APL_Float norm_2(const DD * vec, size_t len)
     {
        APL_Float norm = 0.0;
        loop(j, len)   norm += square(get_real(*vec++));
        return norm;
     }

     /// return ║ vec ║²
  static APL_Float norm_2(const ZZ * vec, size_t len)
        {
          APL_Float norm = 0.0;
          loop(j, len)
             {
               norm += square(get_real(*vec));
               norm += square(get_imag(*vec++));
             }
          return norm;
        }

   /// LApack function unm2r. Apply reflectors A(i) to matrix C.
   template<typename T>
   static void unm2r(Crow K, const fMatrix<T> & A, const PTVVy<T> & ptvvy,
                     fMatrix<T> & C);
  
   /// template instantiation wrapper
   static Value_P invert_DD_UTM(Crow M, Ccol N,
                                fMatrix<DD> & QUTM, fMatrix<DD> & QAUG);

   /// template instantiation wrapper
   static Value_P invert_ZZ_UTM(Crow M, Ccol N,
                                fMatrix<ZZ> & QUTM, fMatrix<ZZ> & QAUG);

   template<typename T>
   static void invert_QUTM(Ccol N, fMatrix<T> & QUTM, fMatrix<T> & QAUG);

   /// LApack function ung2r: convert reflectors (in A) to
   /// orthogonal Q (returned in A)
   template<typename T>
   static void ung2r(fMatrix<T> & A, PTVVy<T> & ptvvy);

   /// dd is 0.0
   static bool is_zero(const DD & dd)
      { return dd == 0.0; }

   /// zz is 0.0j0.0
   static bool is_zero(const ZZ & zz)
      { return zz.real() == 0.0 && zz.imag() == 0.0; }

   /// never
   static bool is_complex(const DD &)
      { return false; }

   /// always
   static bool is_complex(const ZZ &)
      { return true; }

   /// compute Z←A⌹B (real A, B, and Z). Instatiation wrapper.
   static sRank divide_DD_matrix(Value & Z, Crow rows,
                              Ccol cols_A, const Cell * cA,
                              Ccol cols_B, const Cell * cB);

   /// compute Z←A⌹B (complex A or B, and Z). Instatiation wrapper.
   static sRank divide_ZZ_matrix(Value & Z, Crow rows,
                              Ccol cols_A, const Cell * cA,
                              Ccol cols_B, const Cell * cB);

   /// template instantiation wrapper. This wrapper forces the instantiation of
   /// factorize_matrix<DD>() which is defined in a different object file and
   /// may not be instantiated otherwise.
   static void factorize_DD_matrix(Value & Z, Crow M, Ccol N,
                                   const Cell * cB, APL_Float rcond);

   /// template instantiation wrapper. This wrapper forces the instantiation of
   /// factorize_matrix<ZZ>() which is defined in a different object file and
   /// may not be instantiated otherwise.
   static void factorize_ZZ_matrix(Value & Z, Crow M, Ccol N,
                                   const Cell * cB, APL_Float rcond);

protected:   // class LA_pack
   /// compute Z←A⌹B
   template<typename T>
   static sRank divide_matrix(Value & Z, Crow M,
                              Ccol cols_A, const Cell * cA,
                              Ccol cols_B, const Cell * cB);

   /// store the orthogonal factor Q of some HR in Z[1]. On entry is Q a copy
   /// of HR, on exit is Q the reflectors in HR applied to the unit matrix.
   template<typename T>
   static void grab_Q (Value & Z, fMatrix<T> & Q, PTVVy<T> & ptvvy);

   /// store the upper triangle matrix UTM of HR in Z[2] and UTM⁻¹ in Z[3]
   template<typename T>
   static void grab_R(fMatrix<T> & HR, Value & Z, fMatrix<T> & Rinv);

   /// compute the inverse of the upper triangular matrix \b utm.
   /// On return: the inverse of qutm is stored in qaug and utm was destroyed.
   /*  NOTES:
       1.  fMatrix utm is actually M×N but rows N..M are 0 so that inverting
           the upper N×N matix suffices.

       2.  The items below the diagonal of utm are only conceptionaly 0 and
           may in fact be ≠ 0 (e.g. the Q portion of a QR factorization).
           However, invert_QUTM() does not access these items and only
           sets columns 1..N of qaug. The caller is responsible for setting
           columns N..M to 0.
    */
   template<typename T>
   static Value_P invert_UTM(Crow M, Ccol N,
                             fMatrix<T> & UTM, fMatrix<T> & AUG);

   // factorize B
   template<typename T>
   static sRank factorize_matrix(Value & Z, Crow M, Ccol N,
                                const Cell * cB, APL_Float rcond);

   /// return the real part of dd (= dd)
   static DD get_real(const DD & dd)
      { return dd; }

   /// return the real part of zz
   static DD get_real(const ZZ & zz)
      { return zz.real(); }

   /// return the imag part of dd (= dd)
   static DD get_imag(const DD & dd)
      { return 0.0; }

   /// return the imag part of zz
   static DD get_imag(const ZZ & zz)
      { return zz.imag(); }

   /// set the real part of real dd
   static void set_real(DD & dd, APL_Float value)
      { dd = value; }

   /// set the real part of complex zz
   static void set_real(ZZ & zz, APL_Float value)
      { zz.real(value); }

   /// set the imag part of real dd
   static void set_imag(DD & dd, APL_Float value)
      { }

   /// set the real part of complex zz
   static void set_imag(ZZ & zz, APL_Float value)
      { zz.imag(value); }

   /// dd is not 0.0
   static bool is_nonzero(const DD & dd)
      { return dd != 0.0; }

   /// zz is not 0.0
   static bool is_nonzero(const ZZ & zz)
      { return zz.real() != 0.0 || zz.imag() != 0.0; }

      /// conjugate real dd in place (noop)
      static void conjugate(DD & dd)
         { }

      /// conjugate complex zz in place
      static void conjugate(ZZ & zz)
         { zz.imag(-zz.imag()); }

      /// return real dd conjugated
      static DD conjugated(DD dd)
         { return dd; }

      /// return complex zz conjugated
      static ZZ conjugated(const ZZ & zz)
         { return ZZ(zz.real(), - zz.imag()); }

     /// dgelsy and zgelsy
     template<typename T>
     static sRank gelsy(fMatrix<T> & A, fMatrix<T> &B, APL_Float rcond);
  
     /// dgelsy and zgelsy with nicely scaled A and B
     template<typename T>
     static sRank scaled_gelsy(fMatrix<T> & A, fMatrix<T> & B, double rcond);
  
     /// estimate the rank of matrix A
     template<typename T>
     static sRank estimate_rank(const fMatrix<T> & A, APL_Float rcond,
                                PTVVy<T> & ptvvy);
  
     /// LApack function laqp2: computes a QR factorization with
     /// column pivoting of the matrix A
     template<typename T>
     static void laqp2(fMatrix<T> & A, PTVVy<T> & ptvvy);

     /** LApack function laic1 (estimate largest singular value).
         apply one step of incremental condition estimation.
  
         SEST: largest Singular value ESTimate (in/out)
      **/
     template<typename T>
     static void laic1_MAX(APL_Float & SEST, T ALPHA, T GAMMA,
                           T & SIN, T & COS);
  
     /** LApack function laic1 (estimate smallest singular value).
         apply one step of incremental condition estimation.
  
         SEST: smallest Singular value ESTimate (in/out)
      **/
     template<typename T>
     static void laic1_MIN(APL_Float & SEST, T ALPHA, T GAMMA,
                           T & SIN, T & COS);
  
     /// LApack function larfg. It generates an elementary reflector
     /// (aka. a Householder matrix). Return the scalar tau.
     template<typename T>
     static T larfg(T * X, Crow len_X);
  
     /// LApack function trsm. Solve A ∘ X[;1:NRHS] = B[;1:NRHS]
     template<typename T>
     static void trsm(const fMatrix<T> & A, fMatrix<T> & B, Ccol NRHS);
  
    /// LApack functions ILADLC (double) and ILACLC (complex).
    /// Scan matrix \b A for its last non-zero column in its first \b M rows
     template<typename T>
     static inline Ccol ila_lc(Crow M, const fMatrix<T> & A);
  
     /// LApack functions DGEMV and ZGEMV. Let CC←M N↑C. Multiply every column
     /// of CC with v and add up the products.
     template<typename T>
     static inline void gemv(const fMatrix<T> & C, Crow M, Ccol N,
                             const T * v, T * y);
 
     /// LApack function gerc. Multiply the submatrix M N↑C of C with:
     /// scalar ALPHA, row vector y, and column vector x.
     template<typename T>
     static void gerc(fMatrix<T> & C, Crow M, Ccol N,
                      T ALPHA, const T * x, const T * y);
 
     // LApack function larf: apply one elementary reflector v to matrix C
     template<typename T>
     static void larf(const T * v, T tau, fMatrix<T> & C, T * y);
};                                     // class LA_pack
//============================================================================
