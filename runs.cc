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
#include <iostream>
#include <unistd.h>
#include "Run.h"



int main(int argc, char* argv[]) {

  char* instance = NULL;
  char env = 'S';
  char obj = 'U';

  int opt;
  while((opt = getopt(argc, argv, "+i:SPF")) != EOF) {
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
  vector<unsigned int> v;
  {
    int job;
    while(cin >> job) {
      v.push_back(job);
    }
  }
  const unsigned int sz = (int) v.size();
  unsigned int seq[sz];
  copy(v.begin(), v.end(), seq);

  cout << SRuns(seq, sz, new Problem(ifs, env), obj).size() << endl;


  return 0;
}




