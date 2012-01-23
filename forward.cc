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

/* $Id: forward.cc,v 1.1 2004/04/09 15:18:57 mldb Exp $ */

#include <fstream>
#include <multimap.h>
#include <stack>
#include <stdlib.h>
#include <unistd.h>
#include "Problem.h"

/**
 * The problem $1||\sum U_j$ can be solved easily with a {\em forward\/}
 * algorithm \cite{Pin95}. In words the algorithm may be described as follows.
 * Jobs are added to the set of on-time jobs in increasing order of due dates.
 * If the inclusion of job $j^*$ results in this job being completed late, 
 * the scheduled job with the largest processing time, say job $k^*$,
 * is marked late and discarded.
 **/

int main(int argc, char* argv[]) {
  char* instance = "";
  int pause = 0;

  int opt;
  while((opt = getopt(argc, argv, "+i:s:")) != EOF) {
    extern char* optarg;
    switch(opt) {
    case 'i':
      instance = optarg;
      break;
    case 's':
      pause = atoi(optarg);
      break;
    default:
      cerr << "Warning: " << (char) opt << " is not an option" << endl;
      break;
    }
  }

  ifstream ifs(instance);
  if(!ifs) {
    cerr << "Warning: Cannot open " << instance << endl;
    return 1;
  }
  
  Problem problem(ifs, 'S');

  // Step 1: initialization
  multimap<int, unsigned int, greater<int> > scheduled;
  stack<unsigned int> discarded;
  multimap<int, unsigned int> candidates;
  
  for(unsigned int i = 0; i < problem.njobs(); i++) {
    candidates.insert(pair<int, unsigned int>(problem.due(i), i));
  }

  // Step 2: push
  {
    int makespan = 0;
    multimap<int, unsigned int>::iterator it = candidates.begin();
    while(it != candidates.end()) {
      unsigned int job = (*it).second;
      assert((*it).first == problem.due(job));
      makespan += problem.ptime(job);
      scheduled.insert(pair<int, unsigned int>(problem.ptime(job), job));
      
      // Step 3: pop
      if(makespan > problem.due(job)) {
	multimap<int, unsigned int, greater<int> >::iterator k_it = 
	  scheduled.begin();
	unsigned int k = (*k_it).second;
	assert((*k_it).first == problem.ptime(k));
	scheduled.erase(k_it);
	discarded.push(k);
	makespan -= problem.ptime(k);
      }
      it++;
    }
  }

  // Step 4: print
  multimap<int, unsigned int> early;
  {
    multimap<int, unsigned int, greater<int> >::iterator it = 
      scheduled.begin();
    while(it != scheduled.end()) {
      unsigned int job = (*it).second;
      early.insert(pair<int, unsigned int>(problem.due(job), job));
      it++;
    }
  }
  {
    multimap<int, unsigned int>::iterator it = early.begin();
    while(it != early.end()) {
      cout << (*it).second << " ";
      it++;
    }
    cout << flush;
  }
  
  sleep(pause);
  
  while(!discarded.empty()) {
    cout << discarded.top() << " ";
    discarded.pop();
  }
  cout << endl;

  return 0;
}
