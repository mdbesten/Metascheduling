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
#include <functional>
#include <map>
#include <numeric>
#include <vector>

#include <stdlib.h>
#include <unistd.h>

#include "Solution.h"


int main(int argc, char* argv[]) {
  char* instance = "inst";
  char env = 'S';
  char obj = 'T';

  unsigned int n = 100; // number of jobs in sequence
  unsigned int m = 1000; // number of sequences
  bool print_val = false;
 
  int opt;
  while((opt = getopt(argc, argv, "+i:SPFTUn:m:v")) != EOF) {
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
    case 'n':
      n = atoi(optarg);
      break;
    case 'm':
      m = atoi(optarg);
      break;
    case 'v':
      print_val = true;
      break;
    default:
      break;
    }
  }

  ifstream ifs(instance);
  assert(ifs);
  const Problem problem(ifs, env);

  unsigned int container[m][n];
  {
    unsigned int seq[n];
    iota(seq, seq+n, 0);
    for(unsigned int i = 0; i < m; i++) {
      random_shuffle(seq, seq+n);
      copy(seq, seq+n, container[i]);
    }
  }
  multimap<const int, const unsigned int> qmap; // key = quality; data = seqnr
  for(unsigned int i = 0; i < m; i++) {
    int val = 0;
    switch(env) {
    case 'S':
      val = SSol(container[i], n, &problem, obj).eval();
      break;
    case 'P':
      val = PSol(container[i], n, &problem, obj).eval();
      break;
    case 'F':
      val = FSol(container[i], n, &problem, obj).eval();
      break;
    default:
      break;
    }
    qmap.insert(pair<const int, const unsigned int>(val, i));
  }

  vector<pair<const int, const unsigned int> > qvect(qmap.begin(), qmap.end());

  int idx;
   while(cin >> idx) {
     if(idx < 0 || idx > (int) m-1) {
       cerr << "Warning: Keep within [0, " << m << "]" << endl;
     } else {
       if(print_val) {
	 cout << qvect[idx].first << endl;
       } else {
	 unsigned int* seq = container[qvect[idx].second];
	 copy(seq, seq+n, ostream_iterator<int>(cout, " "));
	 cout << endl;
       }
     }
  }

  return 0;
}
