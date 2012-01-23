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

/**
 * Convert single machine sequence of jobs into parallel machine sequence
 * i.e. greedy assignment of jobs to machines
 **/
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
  Problem problem(ifs, 'P');

  const unsigned int m = problem.mprocs();
  unsigned int frontier[m];
  fill(frontier, frontier+m, 0);

  int job;
  while(cin >> job) {
    unsigned int front = min_element(frontier, frontier+m) - frontier;
    frontier[front] += problem.ptime(job);
    cout << job + front * problem.njobs() << " ";
  }
  cout << endl;

  return 0;
}
