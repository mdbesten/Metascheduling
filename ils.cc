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

/* $Id: ils.cc,v 1.12 2004/04/15 20:39:21 mldb Exp $ */
#include "Localsearch.h"
#include "Flow.h"
#include "Parallel.h"
#include "Run.h"
#include "Timer.h"
#include <stdlib.h>
#include <time.h>
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
  bool print_eval = false;
  bool print_trace = false;
  bool verbose = false;
  int optimum = 0;

  vector<const class Localsearch*> ls;
  char perturb = 'b';
  int lower = 0;
  int upper = 10;
  int backtrack = 0;
  int limit = 0;
  unsigned int seed = time(NULL);
  
  int opt;
  while((opt = getopt(argc, argv, "+i:t:s:L:X:l:u:A:SPFTUvRVo:")) != EOF) {
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
    case 't':
      limit = atoi(optarg);
      break;
    case 's':
      seed = atoi(optarg);
      break;
    case 'L': {
      ls.push_back(strtols(optarg));
    } break;
    case 'X':
      perturb = optarg[0];
      break;
    case 'l':
      lower = atoi(optarg);
      break;
    case 'u':
      upper = atoi(optarg);
      break;
    case 'A':
      backtrack = atoi(optarg);
      break;
    case 'v':
      print_eval = true;
      break;
    case 'R':
      print_trace = true;
      break;
    case 'V':
      verbose = true;
      break;
    case 'o':
      optimum = atoi(optarg);
      break;
    default:
      break;
    }
  }
  
  srand(seed);
  
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
  class Solution* candidate = NULL;
  class Problem problem(ifs, env);
  switch(env) {
  case 'S':
    sol = new SRSol(sequence, sz, &problem, obj);
    candidate = new SRSol(sequence, sz, &problem, obj);
    break;
  case 'P':
    sol = new PSol(sequence, sz, &problem, obj);
    candidate = new PSol(sequence, sz, &problem, obj);
    break;
  case 'F':
    sol = new Flow(sequence, sz, &problem, obj);
    candidate = new Flow(sequence, sz, &problem, obj);
    break;
  }

  class Timer clock;
  int kicks = lower;
  int badSteps = 0;
  if(print_trace) {
    cout << "0" << "\t" << sol->eval();
    cout << endl;
  }
  if(verbose) {
    sol->print(cerr);
    cout << "0" << "\t" << sol->eval();
    cout << endl;
  }

  do {

    // Perturbation
    unsigned int sz = candidate->size();
    for(int k = 0; k < kicks; k++) {
      int a = rand() % sz;
      int b;
      do {
	b = rand() % sz;
      } while(b == a);
      
      unsigned int i = min(a, b);
      unsigned int j = max(a, b);
      switch(perturb) {
      case 'b':
	candidate->doBwd(i, j);
	break;
      case 'f':
	candidate->doFwd(i, j);
	break;
      case 's':
	candidate->doSwp(i, j);
	break;
      default: {
	  int mv = rand();
	  if(mv < RAND_MAX/3) {
	    candidate->doBwd(i, j);
	  } else if(mv < 2*RAND_MAX/3) {
	    candidate->doFwd(i, j);
	  } else {
	    candidate->doSwp(i, j);
	  }
	}
      }
    }

    // Local Search
    int start;
    do {
      start = candidate->eval();
      vector<const class Localsearch*>::iterator it = ls.begin();
      while(it != ls.end() && !clock.alarm(limit)) {
	(*it)->search(candidate);
	it++;
      }
    } while(candidate->eval() < start && !clock.alarm(limit));
    
    // Acceptance
    if(candidate->eval() >= sol->eval()) {
      kicks = (kicks < upper) ? kicks+1 : lower;
      badSteps++;
      if(badSteps > backtrack) {
	//candidate->clone(sol);
	delete candidate;
	sol->setSeq(sequence);
	switch(env) {
	case 'S':
	  candidate = new SRSol(sequence, sz, &problem, obj);
	  break;
	case 'P':
	  candidate = new PRSol(sequence, sz, &problem, obj);
	  break;
	case 'F':
	  candidate = new Flow(sequence, sz, &problem, obj);
	  break;
	}
	badSteps = 0;
      }
    } else {
      delete sol;
      candidate->setSeq(sequence);
      switch(env) {
      case 'S':
	sol = new SRSol(sequence, sz, &problem, obj);
	break;
      case 'P':
	sol = new PRSol(sequence, sz, &problem, obj);
	break;
      case 'F':
	sol = new Flow(sequence, sz, &problem, obj);
	break;
      }
      badSteps = 0;
      if(print_trace) {
	cout << clock.elapsed_time(Timer::VIRTUAL) << "\t" << sol->eval();
	cout << endl;
      }
      if(verbose) {
	sol->print(cerr);
	cout << clock.elapsed_time(Timer::VIRTUAL) << "\t" << sol->eval();
	cout << endl;
      }
    }
    
  } while(!clock.alarm(limit) && sol->eval() > optimum);

  if(!print_trace && !verbose) {
    if(print_eval) {
      cout << sol->eval() << endl;
    } else {
      sol->print();
    }
  }
  delete candidate;
  delete sol;

  return 0;
}

