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

#include <iomanip>

#include <math.h>

#include "Common.hh"
#include "Performance.hh"
#include "PrintOperator.hh"
#include "UCS_string.hh"

/* Note:
   perfo_1: monadic cell statistics
   perfo_2: dyadic cell statistics
   perfo_3: APL statistics
   perfo_4: misc other statistics
   optim:   optimization counters
 */

#define perfo_1(id, ab, _name, _thr) \
   CellFunctionStatistics Performance::cfs_ ## id ## ab(PFS_## id ## ab);
#define perfo_2(id, ab, _name, _thr) \
   CellFunctionStatistics Performance::cfs_ ## id ## ab(PFS_ ## id ## ab);
#define perfo_3(id, ab, _name, _thr) \
   FunctionStatistics Performance::fs_ ## id ## ab (PFS_ ## id ## ab);
#define perfo_4(id, ab, _name, _thr) \
   FunctionStatistics Performance::fs_ ## id ## ab (PFS_ ## id ## ab);
#include "Performance.def"

int64_t OptmizationStatistics::optimization_counters[OPT_COUNTER_COUNT];

//----------------------------------------------------------------------------
Statistics::~Statistics()
{
}
//----------------------------------------------------------------------------
Statistics *
Performance::get_statistics(Pfstat_ID id)
{
   switch(id)
      {
#define perfo_1(id, ab, _name, _thr)   \
        case PFS_ ## id ## ab:   return &cfs_ ## id ## ab;
#define perfo_2(id, ab, _name, _thr)   \
        case PFS_ ## id ## ab:   return &cfs_ ## id ## ab;
#define perfo_3(id, ab, name, _thr)    \
        case PFS_ ## id ## ab:   return &fs_ ## id ## ab;
#define perfo_4(id, ab, name, _thr)    \
        case PFS_ ## id ## ab:   return &fs_ ## id ## ab;
#include "Performance.def"
        default: return 0;
      }

   // not reached
   return 0;
}
//----------------------------------------------------------------------------
int
Performance::get_statistics_type(Pfstat_ID id)
{
   switch(id)
      {
#define perfo_1(id, ab, _name, _thr)   case PFS_ ## id ## ab:   return 1;
#define perfo_2(id, ab, _name, _thr)   case PFS_ ## id ## ab:   return 2;
#define perfo_3(id, ab, _name, _thr)   case PFS_ ## id ## ab:   return 3;
#define perfo_4(id, ab, _name, _thr)   case PFS_ ## id ## ab:   return 3;
#include "Performance.def"
        default: return 0;
      }

   // not reached
   return 0;
}
//----------------------------------------------------------------------------
const char *
Statistics::get_name(Pfstat_ID id)
{
   switch(id)
      {
#define perfo_1(id, ab, name, _thr)   case PFS_ ## id ## ab:   return name;
#define perfo_2(id, ab, name, _thr)   case PFS_ ## id ## ab:   return name;
#define perfo_3(id, ab, name, _thr)   case PFS_ ## id ## ab:   return name;
#define perfo_4(id, ab, name, _thr)   case PFS_ ## id ## ab:   return name;
#include "Performance.def"
        case PFS_SCALAR_B_overhead:  return "  f B+overhead";
        case PFS_SCALAR_AB_overhead: return "A f B+overhead";
        default: return "Unknown Pfstat_ID";
      }

   // not reached
   return 0;
}
//----------------------------------------------------------------------------
void
Performance::print(Pfstat_ID which, ostream & out)
{
   if (which != PFS_ALL)   // individual statistics
      {
         Statistics * stat = get_statistics(which);
         if (!stat)
            {
              out << "No such statistics: " << which << endl;
              return;
            }

         if (which < PFS_MAX2)   // cell function statistics
            {
              out <<
"╔═════════════════╦══════════╤═════════╤════════╦══════════╤═════════╤════════╗"
                  << endl;
              stat->print(out);
              out <<
"╚═════════════════╩══════════╧═════════╧════════╩══════════╧═════════╧════════╝"
                  << endl;
            }
         else
            {
              out <<
"╔═════════════════╦════════════╤══════════╤══════════╤══════════╤══════════╗"
                  << endl;
              stat->print(out);
              out <<
"╚═════════════════╩════════════╧══════════╧══════════╧══════════╧══════════╝"
                   << endl;
            }
         return;
      }

   // print all statistics and compute column sums
   //
uint64_t sum_first_N_B       = 0;
uint64_t sum_first_cycles_B  = 0;
uint64_t sum_subsq_N_B       = 0;
uint64_t sum_subsq_cycles_B  = 0;
uint64_t sum_first_N_AB      = 0;
uint64_t sum_first_cycles_AB = 0;
uint64_t sum_subsq_N_AB      = 0;
uint64_t sum_subsq_cycles_AB = 0;

   out <<
"╔═════════════════════════════════════════════════════════════════════╗\n"
"║         Performance Statistics (CPU cycles per vector item)         ║\n"
"╠═════════════════╦═════════════════════════╦═════════════════════════╣\n"
"║                 ║        first pass       ║    subsequent passes    ║\n"
"║      Cell       ╟───────┬───────┬─────────╫───────┬───────┬─────────╢\n"
"║    Function     ║   N   │⌀cycles│   σ÷μ % ║   N   │⌀cycles│   σ÷μ % ║\n"
"╠═════════════════╬═══════╪═══════╪═════════╬═══════╪═══════╪═════════╣\n";


#define perfo_1(id, ab, _name, _thr)                                     \
   cfs_ ## id ## ab.print(out);                                          \
   sum_first_N_B += cfs_ ## id ## ab.get_first_record()->get_count();    \
   sum_first_cycles_B += cfs_ ## id ## ab.get_first_record()->get_sum(); \
   sum_subsq_N_B += cfs_ ## id ## ab.get_record()->get_count();          \
   sum_subsq_cycles_B += cfs_ ## id ## ab.get_record()->get_sum();

#define perfo_2(id, ab,  name,  thr)                                      \
   cfs_ ## id ## ab.print(out);                                           \
   sum_first_N_AB += cfs_ ## id ## ab.get_first_record()->get_count();    \
   sum_first_cycles_AB += cfs_ ## id ## ab.get_first_record()->get_sum(); \
   sum_subsq_N_AB += cfs_ ## id ## ab.get_record()->get_count();          \
   sum_subsq_cycles_AB += cfs_ ## id ## ab.get_record()->get_sum();
#define perfo_3(id, ab, _name, _thr)
#define perfo_4(id, ab, _name, _thr)
#include "Performance.def"

const uint64_t first_avg_B = Statistics_record::average(sum_first_cycles_B,
                                                        sum_first_N_B);
const uint64_t subsq_avg_B = Statistics_record::average(sum_subsq_cycles_B,
                                                      sum_subsq_N_B);

   out <<
"╟─────────────────╫───────┼───────┼─────────╫───────┼───────┼─────────╢\n"
"║           SUM B ║ ";
   Statistics_record::print5(out, sum_first_N_B);
   out <<                 " │ ";
   Statistics_record::print5(out, first_avg_B);
   out <<                        " │         ║ ";
   Statistics_record::print5(out, sum_subsq_N_B);
   out <<                                          " │ ";
   Statistics_record::print5(out, subsq_avg_B);
   out <<                                                  " │         ║\n";

const uint64_t first_avg_AB = Statistics_record::average(sum_first_cycles_AB,
                                                         sum_first_N_AB);
const uint64_t subsq_avg_AB = Statistics_record::average(sum_subsq_cycles_AB,
                                                         sum_subsq_N_AB);
   out <<
"║          SUM AB ║ ";
   Statistics_record::print5(out, sum_first_N_AB);
   out <<                 " │ ";
   Statistics_record::print5(out, first_avg_AB);
   out <<                        " │         ║ ";
   Statistics_record::print5(out, sum_subsq_N_AB);
   out <<                                          " │ ";
   Statistics_record::print5(out, subsq_avg_AB);
   out <<                                                  " │         ║\n";

   out <<
"╚═════════════════╩═══════╧═══════╧═════════╩═══════╧═══════╧═════════╝\n"
"╔═════════════════╦═══════════════╤═══════════════╤═══════╗\n"
"║     Function    ║     Total     │    Average    │Cycles ║\n"
"║        or       ╟───────┬───────┼───────┬───────┤  per  ║\n"
"║    Operation    ║     N │Cycles │ Items │Cycles │ Item  ║\n"
"╟─────────────────╫───────┼───────┼───────┼───────┼───────╢"
       << endl;

   // other statistics...
   //
#define perfo_1(id, ab, _name, _thr)
#define perfo_2(id, ab, _name, _thr)
#define perfo_3(id, ab, _name, _thr) fs_ ## id ## ab.print(out);
#define perfo_4(id, ab, name, thr) perfo_3(id, ab, name, thr)

#include "Performance.def"

   out <<
"╚═════════════════╩═══════╧═══════╧═══════╧═══════╧═══════╝"
       << endl;
}
//----------------------------------------------------------------------------
void
Performance::save_data(ostream & out, ostream & out_file)
{
#define perfo_1(id, ab, _name, _thr)  cfs_ ## id ## ab.save_data(out_file, #id);
#define perfo_2(id, ab, _name, _thr)  cfs_ ## id ## ab.save_data(out_file, #id);
#define perfo_3(id, ab, _name, _thr)  fs_ ## id ## ab.save_data(out_file, #id);
#define perfo_4(id, ab, _name, _thr)  fs_ ## id ## ab.save_data(out_file, #id);
#include "Performance.def"
}
//----------------------------------------------------------------------------
void
Performance::reset_all()
{
#define perfo_1(id, ab, _name, _thr)   cfs_ ## id ## ab.reset();
#define perfo_2(id, ab, _name, _thr)   cfs_ ## id ## ab.reset();
#define perfo_3(id, ab, _name, _thr)   fs_ ## id ## ab.reset();
#define perfo_4(id, ab, _name, _thr)   fs_ ## id ## ab.reset();
#include "Performance.def"
}
//----------------------------------------------------------------------------
void
Statistics_record::print(ostream & out)
{
uint64_t mu = 0;
int sigma_percent = 0;

   if (count)
      {
        mu = data/count;
        const double sigma = sqrt(data2/count - mu*mu);
        sigma_percent = int((sigma/mu)*100);
      }

                 print5(out, count);
   out << " │ "; print5(out, mu);
   out << " │ "; print5(out, sigma_percent);
   out << " %";
}
//----------------------------------------------------------------------------
void
Statistics_record::save_record(ostream & outf)
{
uint64_t mu = 0;
double sigma = 0;
   if (count)
      {
        mu = data/count;
        sigma = sqrt(data2/count - mu*mu);
      }

   outf << setw(8) << count << ","
        << setw(8) << mu << ","
        << setw(8) << uint64_t(sigma + 0.5);
}
//----------------------------------------------------------------------------
void
Statistics_record::print5(ostream & out, uint64_t num)
{
char cc[40];
   if (num < 100000)   // special case: no multiplier
      {
        SPRINTF(cc, "%5u", uint32_t(num));
        out << cc;
        return;
      }

   // kilo, Mega, Giga, Tera, Peta, Exa, Zetta, Yotta, Xona, Weka, Vunda, Una
   // 1E3   1E6   1E9   1E12  1E15  1E18
   // max uint64_t is 1.8E19 = 18 Exa
   //
const char * units = "-kMGTPE??????";
double fnum = num;
   while (fnum > 1000.0)
      {
        ++units;
        fnum = fnum / 1000.0;
      }

   SPRINTF(cc, "%f", fnum);
   if (cc[3] == '.')    { cc[3] = 0;   out << " " << cc << *units; }
   else                 { cc[4] = 0;   out << cc << *units;        }
}
//============================================================================
void
FunctionStatistics::print(ostream & out)
{
UTF8_string utf(get_name());
UCS_string uname(utf);
   out << "║ " << utf;
   loop(n, 15 - uname.size())   out << " ";

const uint64_t div = vec_lengths.get_average() ? vec_lengths.get_average() : 1;

   out << " ║ ";   Statistics_record::print5(out, vec_lengths.get_count());
   out << " │ ";   Statistics_record::print5(out, vec_cycles.get_sum());
   out << " │ ";   Statistics_record::print5(out, vec_lengths.get_average());
   out << " │ ";   Statistics_record::print5(out, vec_cycles.get_average());
   out << " │ ";   Statistics_record::print5(out, vec_cycles.get_average()/div);
   out << " ║" << endl;
}
//----------------------------------------------------------------------------
void
FunctionStatistics::save_data(ostream & outf, const char * perf_name)
{
char cc[100];
   SPRINTF(cc, "%s,", perf_name);
   outf << "prf_3 (PFS_" << left << setw(12) << cc << right;
   vec_cycles.save_record(outf);
   outf << ")" << endl;
}
//============================================================================
void
CellFunctionStatistics::print(ostream & out)
{
UTF8_string utf(get_name());
UCS_string uname(utf);
   out << "║ " << uname;
   loop(n, 12 - uname.size())   out << " ";
   out << "    ║ ";

   first.print(out);
   out << " ║ ";
   subsequent.print(out);
   out << " ║" << endl;
}
//----------------------------------------------------------------------------
void
CellFunctionStatistics::save_data(ostream & outf, const char * perf_name)
{
char cc[100];
   SPRINTF(cc, "%s,", perf_name);
   outf << "prf_12(PFS_" << left << setw(12) << cc << right;
   first.save_record(outf);
   outf << ",";
   subsequent.save_record(outf);
   outf << ")" << endl;
}
//============================================================================

