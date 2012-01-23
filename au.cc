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
#include <algorithm>
#include <fstream>
#include <math.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char* argv[]) {
  char* instance = NULL;
  char env = 'S';
  char obj = 'T';
  double k = 0.5;
  bool no_k = true;
  
  int opt;
  while((opt = getopt(argc, argv, "+i:SPFTUk:")) != EOF) {
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
    case 'k':
      k = strtod(optarg, NULL);
      no_k = false;
      break;
    default:
      break;
    }
  }
  
  ifstream ifs(instance);
  Problem problem(ifs, env);

  unsigned int sz = problem.njobs();
  unsigned int seq[sz];

  if(no_k) {
    const double tf = problem.tf();
    const double rdd = problem.rdd();
    bool weighted = !problem.common_weights();
    k = pow(2, (6.061291 +
		-26.055721 * pow(tf - 0.5, 2) +
		-2.831333 * tf +
		-8.110665 * rdd +
		-2.475556 * (obj == 'T' ? 1 : 0) +
		0.107778 * (weighted ? 1 : 0) +
		0.005100 * sz +
		21.238426 * pow(tf - 0.5, 2) * tf +
		-7.415675 * pow(tf - 0.5, 2) * rdd +
		(obj == 'T' && weighted ?  -0.561111 : 0)));
  }

  unsigned int sum_p = 0;
  for(unsigned int i = 0; i < sz; i++) {
    sum_p += problem.ptime(i);
  }

  bool todo[sz];
  fill(todo, todo+sz, true);
  for(unsigned int i = 0, t = 0; i < sz; i++) {
    double avg_p = (double) (sum_p - t)/(sz - i);
    class pair<double, unsigned int> best(0.0, sz);
    for(unsigned int j = 0; j < sz; j++) {
      if(todo[j]) {
	int slack = max((int) (problem.due(j) - problem.ptime(j) - t), 0);
	double au = ((double) problem.weight(j) / problem.ptime(j)) *
	  exp(-((double)slack/(k*avg_p)));
	if(au > best.first) {
	  best.first = au;
	  best.second = j;
	}
      }
    }
    seq[i] = best.second;
    t += problem.ptime(best.second);
    todo[best.second] = false;
  }

  copy(seq, seq+sz, ostream_iterator<int>(cout, " "));
  cout << endl;
  return 0;
}
