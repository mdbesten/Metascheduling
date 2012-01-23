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

#include "Run.h"
#include <unistd.h>
#include <fstream>
#include <vector>

void localsearch(Solution* sol) {
  bool improvement = false;
  do {
    unsigned int n = sol->size();
    int min = 0;
    unsigned int best_i = 0;
    for(unsigned int i = 0; i < n-1; i++) {
      int cand = sol->whatifSwp(i, i+1, min);
      if(cand < min) {
	min = cand;
	best_i = i;
      }
    }
    if(min < 0) {
      sol->doSwp(best_i, best_i+1);
      improvement = true;
    } else {
      improvement = false;
    }
  } while(improvement);
}

int main(int argc, char* argv[]) {
    char* instance = NULL;
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

  switch(env) {
  case 'S':
    sol = new SRSol(sequence, sz, new Problem(ifs, env), obj);
    break;
  case 'P':
    sol = new PSol(sequence, sz, new Problem(ifs, env), obj);
    break;
  case 'F':
    sol = new FSol(sequence, sz, new Problem(ifs, env), obj);
    break;
  }
  
  localsearch(sol);

  sol->print();

  delete sol;

  return 0;
}
