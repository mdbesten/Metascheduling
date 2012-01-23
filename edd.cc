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

#include "Problem.h"
#include <fstream>
#include <map>
#include <unistd.h>

int main(int argc, char* argv[]) {
  char* instance = NULL;
  char env = 'S';
  
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
    default:
      break;
    }
  }
  
  ifstream ifs(instance);
  Problem problem(ifs, env);

  multimap<int, unsigned int> m;
  for(unsigned int i = 0; i < problem.njobs(); i++) {
    m.insert(pair<int, unsigned int>(problem.due(i), i));
  }
  
  for(multimap<int, unsigned int>::iterator it = m.begin();
      it != m.end(); it++) {
    cout << (*it).second << " ";
    cerr << (*it).first << " ";
  }
  cout << endl;
  cerr << endl;
  return 0;
}
