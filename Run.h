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

#ifndef RUN_H
#define RUN_H
#define PR(X) cerr << #X << " == " << X << endl;
#include <algorithm>
#include <functional>
#include <vector>
#include "Solution.h"



class Run {
public:
  const unsigned int first;
  const unsigned int last;
  const bool late;
  
  Run(const unsigned int& begin, const unsigned int& end, const bool& flag) :
    first(begin), last(end), late(flag) {
    assert(begin <= end);
  }
  
  Run(const Run* r) :
    first(r->first), last(r->last), late(r->late) {
  }
  
  const unsigned int size() const {
    return last - first;
  }

  void print(ostream& out = cout) const {
    out << "[" << first << ", " << last << ") = ";
    out << (late ? "T" : "F") << endl;
  }
};

struct rm : public unary_function<const Run*, void>
{
  void operator()(const Run* r) { delete r; }
};

class SRuns : public Solution {
protected:
  bool* late;
  int* release;
  unsigned int* run;
  vector<const class Run*> R;
  int* U;
  int* T;

  void init();
  void update(const Run* i, const Run* j);
  void setVal() {
    switch(o) {
    case 'U':
      curVal = U[sz];
      break;
    case 'T':
      curVal = T[sz];
      break;
    case 'R':
      curVal = R.size();
      break;
    default:
      curVal = -1;
      break;
    }
  }

  int whatifSwp(const Run* r_i, const Run* r_j, const int& min) const {
    switch(o) {
    case 'U':
      return whatifSwpU(r_i, r_j, min, R, run);
    case 'T':
      return whatifSwpT(r_i, r_j, min, R, run);
    case 'R':
      return whatifSwpR(r_i, r_j, min);
    }
    return -1;
  }
  int whatifSwpU(const Run* i, const Run* j, 
		 const int& min,
		 const vector<const Run*>& v,
		 const unsigned int myrun[]) const;
  int whatifSwpT(const Run* i, const Run* j, 
		 const int& min, 
		 const vector<const Run*>& v,
		 const unsigned int myrun[]) const;
  int whatifSwpR(const Run* i, const Run* j, 
		 const int& min) const;
  int evaluate(const unsigned int *, const unsigned int &) const {
    return -1;
  }
  
  


public:
  SRuns(unsigned int seq[], const unsigned int& sz,
       const class Problem* p, const char& o = 'T') : 
    Solution(seq, sz, p, o) {
    const unsigned int n = p->njobs()+1; // 1 extra for temp storage
    late = new bool[n];
    release = new int[n+1];
    run = new unsigned int[n];
    R.reserve(n);
    U = new int[n+1];
    if(o == 'T')
      T = new int[n+1];
    init();
  }
 

  ~SRuns() {
    delete []late;
    delete []release;
    delete []run;
    delete []U;
    if(o == 'T')
      delete []T;
    for_each(R.begin(), R.end(), rm());
  }
  

  const unsigned int size() const {
    return R.size();
  }

  void doSwp(const unsigned int& i, const unsigned int& j) {
    update(R[i], R[j]);
  }
  void doBwd(const unsigned int& i, const unsigned int& j) {
    Run empty(R[j]->first, R[j]->first, R[j]->late);
    update(R[i], &empty);
  }
  void doFwd(const unsigned int& i, const unsigned int& j) {
    Run empty(R[i]->first, R[i]->first, R[i]->late);
    update(&empty, R[j]);
  }
  
  int whatifSwp(const unsigned int& i, const unsigned int& j, 
		const int& min) const {
    return whatifSwp(R[i], R[j], min);
    
  }
  
  int whatifBwd(const unsigned int& i, const unsigned int& j, 
		const int& min) const {
    Run empty(R[j]->first, R[j]->first, R[j]->late);
    return whatifSwp(R[i], &empty, min);
  }
  
  int whatifFwd(const unsigned int& i, const unsigned int& j, 
		const int& min) const {
    return whatifSwp(new Run(R[i]->first, R[i]->first, R[i]->late), R[j], min);
  }
};

class SRSol : public SRuns {
protected:
  int whatifBwdU(const unsigned int& i, const unsigned int& j, 
		 const int& min) const;

  int whatifFwdU(const unsigned int& i, const unsigned int& j, 
		 const int& min) const;
  
  int whatifSwpU(const unsigned int& i, const unsigned int& j, 
		 const int& min) const;

  int whatifBwdT(const unsigned int& i, const unsigned int& j, 
		 const int& min) const;

  int whatifFwdT(const unsigned int& i, const unsigned int& j, 
		 const int& min) const;
  
  int whatifSwpT(const unsigned int& i, const unsigned int& j, 
		 const int& min) const;

  int whatifAddU(const unsigned int& job, const unsigned int& pos, 
		 const int& min) const; 
  int whatifAddT(const unsigned int& job, const unsigned int& pos, 
		 const int& min) const;
  
  int whatifRemoveU(const unsigned int& pos, const int& min) const;
  int whatifRemoveT(const unsigned int& pos, const int& min) const;

  int whatifReplaceU(const unsigned int& pos, const unsigned int& job,
		     const int& min) const;
  int whatifReplaceT(const unsigned int& pos, const unsigned int& job,
		     const int& min) const;

  int evaluate(const unsigned int sequence[], const unsigned int& size) const {
    int val = 0;
    for(unsigned int i = 0, t = 0; i < size; i++) {
      unsigned job = sequence[i];
      t += p->ptime(job);
      int lateness = t - p->due(job);
      val += p->weight(job) * (lateness > 0 ? (o != 'T' ? 1 : lateness) : 0);
    }
    return val;
  }  
public:
  SRSol(unsigned int seq[], const unsigned int& sz,
	const class Problem* p, const char& o = 'T') :
    SRuns(seq, sz, p, o) {
  }
 
  ~SRSol() {
  }
  
  const unsigned int size() const {
    return Solution::size();
  }
  
  void doBwd(const unsigned int& i, const unsigned int& j) {
    Run ri(i, i+1, late[i]);
    Run rj(j, j, late[j]);
    update(&ri, &rj);
  }

  void doFwd(const unsigned int& i, const unsigned int& j) {
    Run ri(i, i, late[i]);
    Run rj(j, j+1, late[j]);
    update(&ri, &rj);
  }

  void doSwp(const unsigned int& i, const unsigned int& j) {
    Run ri(i, i+1, late[i]);
    Run rj(j, j+1, late[j]);
    update(&ri, &rj);
  }

  void doAdd(const unsigned int& job, const unsigned int& pos);
  void doRemove(const unsigned int& pos);
  void doReplace(const unsigned int& pos, const unsigned int& job);

  int whatifBwd(const unsigned int& i, const unsigned int& j, 
		const int& min) const {
    switch(o) {
    case 'U':
      return whatifBwdU(i, j, min);
    case 'T':
      return whatifBwdT(i, j, min);
    }
    return -1;
  }

  int whatifFwd(const unsigned int& i, const unsigned int& j, 
		const int& min) const {
    switch(o) {
    case 'U':
      return whatifFwdU(i, j, min);
    case 'T':
      return whatifFwdT(i, j, min);
    }
    return -1;
  }
  
  int whatifSwp(const unsigned int& i, const unsigned int& j, 
		const int& min) const {
    switch(o) {
    case 'U':
      return whatifSwpU(i, j, min);
    case 'T':
      return whatifSwpT(i, j, min);
    }
    return INT_MAX;
  }

  int whatifAdd(const unsigned int& job, 
		const unsigned int& pos, 
		const int& min = INT_MAX) const {
    if(false) {
      return Solution::whatifAdd(job, pos, min);
    } else {
    switch(o) {
    case 'U':
      assert(whatifAddU(job, pos, INT_MAX) == Solution::whatifAdd(job, pos));
      return whatifAddU(job, pos, min);
    case 'T': {
      int rval = whatifAddT(job, pos, INT_MAX);
#ifndef NDEBUG
      if(rval != Solution::whatifAdd(job, pos, INT_MAX)) {
	PR(sz);
	PR(Solution::whatifAdd(job, pos));
	PR(rval);
	PR(job);
	PR(pos);
      }
#endif
      return rval;//whatifAddT(job, pos, min);
    }
    }
    return INT_MAX;
  }
  }
  int whatifRemove(const unsigned int& pos,
		   const int& min = 0) const {
    if(false) {
      return Solution::whatifRemove(pos, min);
    } else {
     switch(o) {
     case 'U':
       return whatifRemoveU(pos, min);
     case 'T': 
       return whatifRemoveT(pos, min);
     }
     return INT_MAX;
  }
  }

  int whatifReplace(const unsigned int& pos, 
		    const unsigned int& job,
		    const int& min = INT_MAX) const {
    if(true) {
      return Solution::whatifReplace(pos, job, min);
    } else {
    switch(o) {
    case 'U':
      return whatifReplaceU(pos, job, min);
    case 'T':
      return whatifReplaceT(pos, job, min);
    }
    return INT_MAX;
  }
  }
};
#endif
