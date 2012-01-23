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

#include "Flow.h"
#include <fstream>
#include <unistd.h>

int main(int argc, char* argv[]) {
  char* instance = NULL;
  char obj = 'T';
  unsigned int width = 80;
  bool TeX = false;
  int m = -1;

  int opt;
  while((opt = getopt(argc, argv, "+i:TUw:pm:")) != EOF) {
    extern char* optarg;
    switch(opt) {
    case 'i':
      instance = optarg;
      break;
    case 'T':
    case 'U':
      obj = (char) opt;
      break;
    case 'w':
      width = atoi(optarg);
      break;
    case 'p':
      TeX = true;
      break;
    case 'm':
      m = atoi(optarg);
      break;
    default:
      break;
    }
  }
  
  ifstream ifs(instance);
  Problem problem(ifs, 'F');

  unsigned int seq[problem.njobs()];
  unsigned int cnt = 0;
  while(cin >> seq[cnt]) cnt++;
  
  Flow(seq, cnt, &problem, obj).gantt(cout, width, TeX, 
				      (m < (int) problem.njobs() ? m : 
				      problem.njobs()));
  
  return 0;
}
