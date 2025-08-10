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

#include "FloatCell.hh"
#include "Quad_FFT.hh"
#include "Workspace.hh"

#include "Workspace.icc"

Quad_FFT   Quad_FFT::fun;

bool Quad_FFT::system_wisdom_loaded = false;

#if defined(HAVE_LIBFFTW3) && defined(HAVE_FFTW3_H)

#include <fftw3.h>
#include "ComplexCell.hh"

//----------------------------------------------------------------------------
Token
Quad_FFT::eval_B(Value_P B) const
{
   return do_fft(FFTW_FORWARD, B, 0);
}
//----------------------------------------------------------------------------
void
Quad_FFT::init_in(void * _in, Value_P B, window_function win)
{
fftw_complex * in = reinterpret_cast<fftw_complex *>(_in);
const APL_Integer N = B->element_count();

   if (N < 2)
      {
        in[0][0] = B->get_cfirst().get_real_value();
        in[0][1] = B->get_cfirst().get_imag_value();
      }

   if (win == 0)
      {
        loop(n, N)
           {
             in[n][0] = B->get_cravel(n).get_real_value();
             in[n][1] = B->get_cravel(n).get_imag_value();
           }
      }
   else if (B->get_rank() == 1)
      {
        loop(n, N)
           {
             const double w = win(n, N);
             in[n][0] = w * B->get_cravel(n).get_real_value();
             in[n][1] = w * B->get_cravel(n).get_imag_value();
           }
      }
   else
      {
        double * wp = new double[N];
        if (wp == 0)   WS_FULL;
        fill_window(wp, B->get_shape(), win);
        loop(n, N)
           {
             const double w = wp[n];
             in[n][0] = w * B->get_cravel(n).get_real_value();
             in[n][1] = w * B->get_cravel(n).get_imag_value();
           }
        delete [] wp;
      }
}
//----------------------------------------------------------------------------
Token
Quad_FFT::do_fft(int dir, Value_P B, window_function win)
{
   if (!system_wisdom_loaded)
      {
        fftw_import_system_wisdom();
        system_wisdom_loaded = true;   // try only once
      }

const APL_Integer N = B->element_count();
   if (N == 0)   LENGTH_ERROR;

const ShapeItem io_size = N * sizeof(fftw_complex);

fftw_complex * in  =  reinterpret_cast<fftw_complex *>(fftw_malloc(io_size));
   if (in == 0)    WS_FULL;
fftw_complex * out =  reinterpret_cast<fftw_complex *>(fftw_malloc(io_size));
   if (out == 0)    { fftw_free(in);   WS_FULL; }

   enum { flags = FFTW_ESTIMATE | FFTW_DESTROY_INPUT };

   // fill in[] with B
   //
   if (B->get_rank() <= 1)   // one-dimensional FFT
      {
        fftw_plan plan = fftw_plan_dft_1d(N, in, out, dir, flags);
        if (plan == 0)
           {
             fftw_free(in);
             fftw_free(out);
             WS_FULL;
           }

        init_in(in, B, win);   // do this after plan was created
        fftw_execute(plan);
        fftw_destroy_plan(plan);
      }
   else if (B->get_rank() == 2)   // two-dimensional FFT
      {
        fftw_plan plan = fftw_plan_dft_2d(B->get_shape_item(0),
                                          B->get_shape_item(1),
                                          in, out, dir, flags);
        if (plan == 0)
           {
             fftw_free(in);
             fftw_free(out);
             WS_FULL;
           }

        init_in(in, B, win);   // do this after plan was created
        fftw_execute(plan);
        fftw_destroy_plan(plan);
      }
   else if (B->get_rank() == 3)   // two-dimensional FFT
      {
        fftw_plan plan = fftw_plan_dft_3d(B->get_shape_item(0),
                                          B->get_shape_item(1),
                                          B->get_shape_item(2),
                                          in, out, dir, flags);
        if (plan == 0)
           {
             fftw_free(in);
             fftw_free(out);
             WS_FULL;
           }

        init_in(in, B, win);   // do this after plan was created
        fftw_execute(plan);
        fftw_destroy_plan(plan);
      }
   else                           // k-dimensional FFT
      {
        int ish[MAX_RANK];
        loop(r, B->get_rank())   ish[r] = B->get_shape_item(r);

        fftw_plan plan = fftw_plan_dft(B->get_rank(), ish, in, out, dir, flags);
        if (plan == 0)
           {
             fftw_free(in);
             fftw_free(out);
             WS_FULL;
           }

        init_in(in, B, win);   // do this after plan was created
        fftw_execute(plan);
        fftw_destroy_plan(plan);
      }

Value_P Z(B->get_shape(), LOC);
const double norm = sqrt(N);
   loop(n, N)   Z->next_ravel_Complex(out[n][0]/norm, out[n][1]/norm);

   fftw_free(in);
   fftw_free(out);

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Quad_FFT::do_window(Value_P B, window_function win)
{
   Assert(win);

   if (B->get_rank() == 0)   return Token(TOK_APL_VALUE1, IntScalar(1, LOC));

const ShapeItem N = B->element_count();
   if (N < 2)   LENGTH_ERROR;

Value_P Z(B->get_shape(), LOC);
   if (B->get_rank() == 1)
      {
        loop(n, N)
           {
             const double w = win(n, N);
             const Cell & cell_B = B->get_cravel(n);
             if (cell_B.is_complex_cell())
                Z->next_ravel_Complex(w*cell_B.get_real_value(),
                                      w*cell_B.get_imag_value());
             else
                Z->next_ravel_Float(w * cell_B.get_real_value());
           }
      }
   else
      {
        double * wp = new double[N];
        if (wp == 0)   WS_FULL;
        fill_window(wp, B->get_shape(), win);

        loop(n, N)
           {
             const double w = wp[n];
             const Cell & cell_B = B->get_cravel(n);
             if (cell_B.is_complex_cell())
                Z->next_ravel_Complex(w*cell_B.get_real_value(),
                                      w*cell_B.get_imag_value());
             else
                Z->next_ravel_Float(w*cell_B.get_real_value());
           }
        delete [] wp;
      }

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Token
Quad_FFT::eval_AB(Value_P A, Value_P B) const
{
   if (A->get_rank() > 1)         RANK_ERROR;
   if (A->element_count() != 1)   LENGTH_ERROR;

const APL_Integer what = A->get_cfirst().get_int_value();
   switch(what)
      {
        case  15: return do_fft(FFTW_FORWARD, B, &flat_top);
        case  14: return do_fft(FFTW_FORWARD, B, &blackman_nuttall_window);
        case  13: return do_fft(FFTW_FORWARD, B, &blackman_harris_window);
        case  12: return do_fft(FFTW_FORWARD, B, &blackman_window);
        case  11: return do_fft(FFTW_FORWARD, B, &hamming_window);
        case  10: return do_fft(FFTW_FORWARD, B, &hann_window);

        case   0: return do_fft(FFTW_FORWARD,  B, 0);
        case  -1: return do_fft(FFTW_BACKWARD, B, 0);

        case -10: return do_window(B, &hann_window);
        case -11: return do_window(B, &hamming_window);
        case -12: return do_window(B, &blackman_window);
        case -13: return do_window(B, &blackman_harris_window);
        case -14: return do_window(B, &blackman_nuttall_window);
        case -15: return do_window(B, &flat_top);
      }

   MORE_ERROR() << "Invalid mode A (= " << what
        << ") of A ⎕FFT B. Valid modes are:\n"
"    A=¯15: no FFT, return (B × Flat-Top window)\n"
"    A=¯14: no FFT, return (B × Blackman-Nuttal window)\n"
"    A=¯13: no FFT, return (B × Blackman-Harris window)\n"
"    A=¯12: no FFT, return (B × Blackman window)\n"
"    A=¯11: no FFT, return (B × Hamming window)\n"
"    A=¯10: no FFT, return (B × Hann window)\n"
"\n"
"    A=¯1: inverse FFT\n"
"    A=0:  forward FFT (no window) of B\n"
"\n"
"    A=10: forward FFT(B × Hann window)\n"
"    A=11: forward FFT(B × Hamming window)\n"
"    A=12: forward FFT(B × Blackman window)\n"
"    A=13: forward FFT(B × Blackman-Harris window)\n"
"    A=14: forward FFT(B × Blackman-Nuttal window)\n"
"    A=15: forward FFT(B × Flat-Top window)\n";

   DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
void
Quad_FFT::fill_window(double * result, const Shape & shape, window_function win)
{
ShapeItem rlen = 1;
   result[0] = 1.0;

   for (sRank r = shape.get_rank() - 1; r >= 0; --r)
       {
         const ShapeItem axis_len = shape.get_shape_item(r);
         double * e = result + rlen * axis_len;
         for (ShapeItem a = axis_len - 1; a >= 0; --a)
             {
               const double wa = win(a, axis_len);
               for (ShapeItem r = rlen - 1; r >= 0; --r)
                   {
                     *--e = wa * result[r];
                   }
             }

         rlen *= axis_len;
       }
}

#else

extern Token missing_files(const char * qfun,  const char ** libs,
                           const char ** hdrs, const char ** pkgs);

Token Quad_FFT::eval_AB(Value_P A, Value_P B) const { return eval_B(B); }

Token
Quad_FFT::eval_B(Value_P B) const
{
const char * libs[] = { "libfftw3.so",   0 };
const char * hdrs[] = { "fftw3.h",      0 };
const char * pkgs[] = { "libfftw3-dev", 0 };

   return missing_files("⎕FFT", libs, hdrs, pkgs);
}

#endif // defined(HAVE_LIBFFTW3) && defined(HAVE_FFTW3_H)

