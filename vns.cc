#    Copyright: Matthijs den Besten
#
#    This file is part of Metascheduling.
#
#    Metascheduling is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    Metascheduling is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with Metascheduling.  If not, see <http://www.gnu.org/licenses/>.
#

/* $Id: vns.cc,v 1.9 2004/04/15 20:39:21 mldb Exp $ */
#include "Localsearch.h"
#include "Run.h"
#include "Parallel.h"
#include "Flow.h"
#include "Timer.h"
#include <unistd.h>
#include <fstream>
#include <vector>

const class Localsearch* strtols(const char* params) {
  bool bestImpr = false;
  bool bwd = false;
  bool fwd = false;
  bool swp = false;
  
  int i = 0;
  bool eos = false;
  do {
    switch(params[i]) {
    case 'B':
      bestImpr = true;
      break;
    case 'F':
      bestImpr = false;
      break;
    case 'b':
      bwd = true;
      break;
    case 'f':
      fwd = true;
      break;
    case 's':
      swp = true;
      break;
    case ' ':
      break;
    default:
      eos = true;
      break;
    }
    i++;
  } while(!eos);
  
  return new Localsearch(bwd, fwd, swp, bestImpr);
}

int main(int argc, char* argv[]) {
  char* instance = NULL;
  char env = 'S';
  char obj = 'T';
  bool run = false;
  bool print_eval = false;

  vector<const class Localsearch*> ls;

  int opt;
  while((opt = getopt(argc, argv, "+i:L:SPFTURv")) != EOF) {
    extern char* optarg;
    switch(opt) {
    case 'i':
      instance = optarg;
      break;
    case 'S':
    case 'P':
    case 'F':
      env = (char) opt;
      break;
    case 'T':
    case 'U':
      obj = (char) opt;
      break;
    case 'L': {
      ls.push_back(strtols(optarg));
    } break;
    case 'R':
      run = true;
      break;
    case 'v':
      print_eval = true;
      break;
    default:
      break;
    }
  }
  
  ifstream ifs(instance);
  
  unsigned int sz = 0;
  vector<int> v;
  unsigned int job;
  while(cin >> job) {
    v.push_back(job);
    sz++;
  }
  unsigned int sequence[sz];
  copy(v.begin(), v.end(), sequence);

  class Solution* sol = NULL;
  class Problem problem(ifs, env);
  switch(env) {
  case 'S':
    if(run) {
      sol = new SRuns(sequence, sz, &problem, obj);
    } else {
      sol = new SRSol(sequence, sz, &problem, obj);
    }
    break;
  case 'P':
    sol = new PSol(sequence, sz, &problem, obj);
    break;
  case 'F':
    sol = new Flow(sequence, sz, &problem, obj);
    break;
  }

  class Timer clock;

  int start;
  do {
    start = sol->eval();
    vector<const class Localsearch*>::iterator it = ls.begin();
    while(it != ls.end()) {
      (*it)->search(sol);
      it++;
    }
  } while(sol->eval() < start);
  
  if(print_eval) {
    cout << sol->eval() << endl;
    cout << clock.elapsed_time(Timer::VIRTUAL) << endl;
  } else {
    sol->print();
    cerr << clock.elapsed_time(Timer::VIRTUAL) << endl;
  }

  delete sol;

  return 0;
}
