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

#include <fstream>

#include <stdlib.h>
#include <unistd.h>

#include "Run.h"
#include "Flow.h"

int main(int argc, char* argv[]) {
  char* instance = "";
  char env = 'S';
  char obj = 'T';

  int opt;
  while((opt = getopt(argc, argv, "+i:SPFTU")) != EOF) {
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
    default:
      cerr << "Warning: " << (char) opt << " not implemented yet" << endl;
      break;
    }
  }

  ifstream ifs(instance);
  if(!ifs) {
    cerr << "Warning: Cannot open " << instance << endl;
    return 1;
  }
  
  Problem problem(ifs, env);

  switch(env) {
  case 'S': {
    unsigned int job;
    cin >> job;
    unsigned int seq[1];
    seq[0] = job;
    class SRSol solution(seq, 1, &problem, obj);
    
    while(cin >> job) {
      // evaluate
      pair<unsigned int, int> best(INT_MAX, INT_MAX);
      for(int i = (int) solution.size(); i >= 0 && best.second > 0; i--) {
	int change = solution.whatifAdd(job, i, best.second);
	if(change < best.second) {
	  best.first = i;
	  best.second = change;
	}
      }
      solution.doAdd(job, best.first);
    }
    solution.print(cout);
  } break;
  case 'F': {
    unsigned int job;
    cin >> job;
    unsigned int seq[1];
    seq[0] = job;
    class Flow solution(seq, 1, &problem, obj);
    
    while(cin >> job) {
      solution.push_back(job);
      // evaluate
      const unsigned int last = solution.size() - 1;
      pair<unsigned int, int> best(last, 0);
      for(unsigned int i = 0; i < last; i++) {
	int change = solution.whatifFwd(i, last, best.second);
	if(change < best.second) {
	  best.first = i;
	  best.second = change;
	}
      }
      solution.doFwd(best.first, last);
    }
    solution.print(cout);
  } break;
  case 'P': {
    const unsigned int m = problem.mprocs();
    class SRSol* ssols[m];
    unsigned int job;
    for(unsigned int i = 0; i < m; i++) {
      cin >> job;
      unsigned int seq[1];
      seq[0] = job;
      ssols[i] = new SRSol(seq, 1, &problem, obj);
    }
    while(cin >> job) {
      // find best insert
      pair<pair<unsigned int, unsigned int>, int> best(pair<unsigned int, 
						       unsigned int>(m, 
								     INT_MAX),
						       INT_MAX);
      for(unsigned int i = 0; i < m; i++) {
	const class SRSol* sol = ssols[i];
	for(int j = (int) sol->size(); j >= 0 && best.second > 0; j--) {
	  int change = sol->whatifAdd(job, j, best.second);
	  if(change < best.second) {
	    best.first.first = i;
	    best.first.second = j;
	    best.second = change;
	  }
	}
      }
      // insert
      ssols[best.first.first]->doAdd(job, best.first.second);
#ifndef NDEBUG
      PR(best.second);
      int val = 0;
      for(unsigned int i = 0; i < m; i++) {
	val += ssols[i]->eval();
      }
      PR(val);
#endif
    }

    // print
    for(unsigned int i = 0; i < m; i++) {
      for(unsigned int j = 0; j < ssols[i]->size(); j++) {
	cout << ssols[i]->which(j) + i*problem.njobs() << " ";
      }
      delete ssols[i];
    }
    cout << endl;

  } break;
  }
  
  return 0;
}


