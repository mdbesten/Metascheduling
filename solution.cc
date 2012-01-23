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

#include "Parallel.h"
#include <fstream>
#include <vector>
#include <unistd.h>

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
  class Problem problem(ifs, env);  

  unsigned int sz = 0;
  vector<int> v;
  unsigned int job;
  while(cin >> job) {
    v.push_back(job);
    sz++;
  }
  unsigned int sequence[sz];
  copy(v.begin(), v.end(), sequence);
  
  switch(env) {
  case 'S':
    cout << SSol(sequence, sz, &problem, obj).eval() << endl;
    break;
  case 'P':
    cout << PSol(sequence, sz, &problem, obj).eval() << endl;
    break;
  case 'F':
    cout << FSol(sequence, sz, &problem, obj).eval() << endl;
    break;
  }

  return 0;
}

