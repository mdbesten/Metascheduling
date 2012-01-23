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
/*
 * find max set of jobs with objective value lower or equal to bound
 */
int main(int argc, char* argv[]) {
  char* instance = "";
  char env = 'S';
  char obj = 'T';
  int limit = 0;

  int opt;
  while((opt = getopt(argc, argv, "+i:SPFTUl:")) != EOF) {
    extern char* optarg;
    switch(opt) {
    case 'i':
      instance = optarg;
      break;
    case 'S':
      env = (char) opt;
      break;
    case 'T':
    case 'U':
      obj = (char) opt;
      break;
    case 'l':
      limit = atoi(optarg);
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

  // initial sequence
  unsigned int seq[problem.njobs()];
  unsigned int sz = 0;
  while(cin >> seq[sz++]);
  sz--;
  assert(sz <= problem.njobs());

  // initial solution
  SRSol solution(seq, sz, &problem, obj);
  PR(solution.eval());
  
  // reduce
  while(solution.eval() > limit) {
    pair<unsigned int, int> best(INT_MAX, INT_MAX);
    // find biggest reduction
    for(unsigned int i = 0; i < solution.size(); i++) {
      int change = solution.whatifRemove(i);
      if(change < best.second) {
	best.first = i;
	best.second = change;
      }
    }
    cerr << solution.which(best.first) << " " << flush;
    solution.doRemove(best.first);
    PR(best.second);
    PR(solution.eval());
  }
  cerr << endl;
  
  solution.print();
  return 0;
}


