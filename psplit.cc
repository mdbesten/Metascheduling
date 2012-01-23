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

#include <algorithm>
#include <fstream>
#include <unistd.h>
#include "Problem.h"

int main(int argc, char* argv[]) {
  char* instance = NULL;

  int opt;
  while((opt = getopt(argc, argv, "+i:")) != EOF) {
    extern char* optarg;
    switch(opt) {
    case 'i':
      instance = optarg;
      break;
    default:
      break;
    }
  }
  
  ifstream ifs(instance);
  assert(ifs);

  Problem problem(ifs, 'P');

  unsigned int seq[problem.njobs()];
  unsigned int sz = 0;
  while(cin >> seq[sz]) sz++;
  assert(sz <= problem.njobs());
  
  unsigned int m_assign[sz];
  const unsigned int m = problem.mprocs();
  // assign jobs to processors
  unsigned int frontier[m];
  fill(frontier, frontier+m, 0);
    
  for(unsigned int i = 0; i < sz; i++) {
    unsigned int* front = min_element(frontier, frontier+m);
    const unsigned int processor = front-frontier;
    frontier[processor] += problem.ptime(seq[i]);
    m_assign[i] = processor;
  } 


  copy(seq, seq+sz, ostream_iterator<int>(cerr, " "));
  cerr << endl;
  copy(m_assign, m_assign+sz, ostream_iterator<int>(cerr, " "));
  cerr << endl;

  for(unsigned int i = 0; i < sz; i++) {
    seq[i] += m_assign[i]*problem.njobs();
  }

  copy(seq, seq+sz, ostream_iterator<int>(cout, " "));
  cout << endl;

  return 0;
}
