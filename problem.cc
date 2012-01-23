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
#include <unistd.h>

int main(int argc, char* argv[]) {
  char* instance = NULL;
  char env = 'S';
  
  int opt;
  while((opt = getopt(argc, argv, "+i:SPF")) != EOF) {
    extern char* optarg;
    switch(opt) {
    case 'i':
      instance = optarg;
      break;
    default:
      env = (char) opt;
      break;
    }
  }
  
  ifstream ifs(instance);
  Problem problem(ifs, env);

  cout << "weight\t due date\t";
  unsigned int m = problem.mprocs();
  for(unsigned int i = 0; i < m; i++) {
    cout << "p" << i << " ";
  }
  cout << endl;
  for(unsigned int i = 0; i < problem.njobs(); i++) {
    cout << problem.weight(i) << "\t";
    cout << problem.due(i) << "\t";
    for(unsigned int j = 0; j < m; j++) {
      cout << problem.ptime(i, problem.env == 'P' ? 0 : j) << " ";
    }
    cout << endl;
  }
  
  PR(problem.tf());
  PR(problem.rdd());
  PR(problem.common_weights());
  return 0;
}

