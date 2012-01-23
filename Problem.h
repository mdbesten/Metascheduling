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

#ifndef PROBLEM_H
#define PROBLEM_H
#define PR(X) cerr << #X << " == " << X << endl;
#define NDEBUG

#include <iostream>
#include <assert.h>
/**
 * Problem data container
 */
class Problem {
protected:
  unsigned int n; // number of jobs
  unsigned int m; // number of machines
  int* p; // processing times
  int* w; // weights
  int* d; // due dates

public:
  const char env; // machine environment in {S, P, F}

  Problem(istream& in = cin, const char& env = 'S');
  ~Problem();

  const unsigned int njobs() const { 
    return n; 
  }
  const unsigned int mprocs() const { 
    return m; 
  }
  const int ptime(const unsigned int& i,
			   const unsigned int& j = 0) const {
    assert(i < n);
    assert(j < m+1);
    assert(j==0 || env == 'F');
    return p[i+j*n];
  }
  const int weight(const unsigned int& i) const {
    assert(i < n);
    return w[i];
  }
  const int due(const unsigned int& i) const {
    assert(i < n);
    return d[i];
  }

  const double tf() const;
  const double rdd() const;
  const bool common_weights() const;
};
#endif
