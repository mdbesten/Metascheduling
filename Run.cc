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
#include "Run.h"

void
SRuns::init() {
  // release dates & lateness
  release[0] = 0;
  for(unsigned int i = 0, t = 0; i < sz; i++) {
    unsigned int job = seq[i];
    t += p->ptime(job);
    release[i+1] = t;
    late[i] = ((int) t > p->due(job));
  }
  
  // runs
  {
    unsigned int start = 0;
    unsigned int next = start;
    bool flag = !late[0];
    unsigned int nruns = 0;
    do {
      next = find(late+start, late+sz, flag) - late;
      assert(start < next);
      R.push_back(new Run(start, next, !flag));
      
      fill(run+start, run+next, nruns++);
      
      start = next;
      flag = !flag;
    } while(next < sz);
  }

  // accumulated weight & tardiness
  U[0] = 0;
  if(o == 'T')
    T[0] = 0;
  for(unsigned int i = 1; i < sz+1; i++) {
    unsigned int job = seq[i-1];
    U[i] = U[i-1];
    if(o == 'T')
      T[i] = T[i-1];
    if(late[i-1]) {
      U[i] += p->weight(job);
      if(o == 'T')
	T[i] += p->weight(job) * 
	  (release[i] - p->due(job));  
    }
  }
  
  // value
  setVal();
}

void 
SRuns::update(const class Run* r_i, const class Run* r_j) {
  // update job order
  rotate(seq+r_i->first, seq+r_i->last, seq+r_j->first);
  rotate(seq+r_i->first, seq+r_j->first, seq+r_j->last);

  // update release between r_i->first and r_j->last
  for(unsigned int k = r_i->first; k < r_j->last; k++) {
    const unsigned int job = seq[k];
    release[k+1] = release[k] + p->ptime(job);
    late[k] = (release[k+1] > p->due(job));
  }
  
  // update sum of weights & tardiness
  const int prevT = (o == 'T') ? T[r_j->last]: 0;
  const int prevU = U[r_j->last];
  for(unsigned int i = r_i->first; i < r_j->last; i++) {
    unsigned int job = seq[i];
    U[i+1] = U[i];
    if(o == 'T')
      T[i+1] = T[i];
    if(late[i]) {
      U[i+1] += p->weight(job);
      if(o == 'T')
	T[i+1] += p->weight(job) * 
	  (release[i+1] - p->due(job));  
    }
  }
  const int shiftT = (o == 'T') ? T[r_j->last] - prevT : 0;
  const int shiftU = U[r_j->last] - prevU;
  for(unsigned int i = r_j->last; i < sz; i++) {
    if(o == 'T')
      T[i+1] += shiftT;
    U[i+1] += shiftU;
  }

  // update runs
  for_each(R.begin(), R.end(), rm());
  R.clear();
  {
    unsigned int start = 0;
    unsigned int next = start;
    bool flag = !late[0];
    unsigned int nruns = 0;
    do {
      next = find(late+start, late+sz, flag) - late;
#ifndef NDEBUG
      if(start >= next) {
	PR(start);
	PR(next);
	PR(sz);
	PR(R.size());
	PR(nruns);
	abort();
      }
#endif
      R.push_back(new Run(start, next, !flag));
      fill(run+start, run+next, nruns++);
      start = next;
      flag = !flag;
    } while(next < sz);
  }



  setVal();
}

int 
SRuns::whatifSwpU(const Run* r_i, const Run* r_j, 
		  const int& minImp,
		  const vector<const Run*>& locR, 
		  const unsigned int locrun[]) const {
  const int makespan_i = release[r_i->last] - release[r_i->first];
  const int makespan_j = release[r_j->last] - release[r_j->first];
  const int shift = makespan_j - makespan_i;

  int penalty = U[r_i->first]; 
  // run j starts at release date r_i->first;
  if(r_j->late) { // late jobs become less late
    for(unsigned int k = r_j->first, t = release[r_i->first]; 
	k < r_j->last; k++) {
      const unsigned int job = seq[k];
      t += p->ptime(job);
      if((int) t > p->due(job)) {
	penalty += p->weight(job);
      }
    }
  }
  const int delta_j = (penalty - U[r_i->first]) - 
    (U[r_j->last] - U[r_j->first]);

  // run i starts at r_j->first + shift
  
  const int tstart = penalty;
  for(unsigned int k = r_i->first, t = release[r_j->first] + shift;
      k < r_i->last; k++) {
    const unsigned int job = seq[k];
    t += p->ptime(job);
    if((int) t > p->due(job)) {
      penalty += p->weight(job);
    }
  }
  const int delta_i = (penalty - tstart) - 
    (U[r_i->last] - U[r_i->first]);

  // compute bounds (aka pretesting)
  if(shift > 0) {
    if(delta_i + delta_j < minImp) {
      for(unsigned int l = locrun[r_i->last]; l < locrun[r_j->first]; l++) {
	const class Run* rtmp = locR[l];
	if(rtmp->late) {
	  penalty += (U[rtmp->last] - U[rtmp->first]);
	} else {
	  for(unsigned int k = rtmp->first; k < rtmp->last; k++) {
	    const unsigned int job = seq[k];
	    const int t = release[k] + shift + p->ptime(job);
	    if((int) t > p->due(job)) {
	      penalty += p->weight(job);
	    }
	  }
	}
      }
    } else {
      return minImp + 1;
    }
  } else {
    if((delta_i + delta_j + - (U[r_j->first] - U[r_i->last])) 
       < minImp) {
      for(unsigned int l = locrun[r_i->last]; l < locrun[r_j->first]; l++) {
	const class Run* rtmp = locR[l];
	if(rtmp->late) {
	  for(unsigned int k = rtmp->first; k < rtmp->last; k++) {
	    const unsigned int job = seq[k];
	    const int t = release[k] + shift + p->ptime(job);
	    if((int) t > p->due(job)) {
	      penalty += p->weight(job);
	    }
	  }
	}
      }
    } else {
      return minImp + 1;;
    }
  }

  return penalty - U[r_j->last];
}

int 
SRuns::whatifSwpT(const Run* r_i, const Run* r_j,
		  const int& minImp,
		  const vector<const Run*>& locR, 
		  const unsigned int locrun[]) const {
  const int makespan_i = release[r_i->last] - release[r_i->first];
  const int makespan_j = release[r_j->last] - release[r_j->first];
  const int shift = makespan_j - makespan_i;

  int tardiness = T[r_i->first]; 
  // run j starts at release date r_i->first;
  if(r_j->late) { // late jobs become less late
    for(unsigned int k = r_j->first, t = release[r_i->first]; 
	k < r_j->last; k++) {
      const unsigned int job = seq[k];
      t += p->ptime(job);
      if((int) t > p->due(job)) {
	tardiness += p->weight(job) * 
	  (t - p->due(job));
      }
    }
  }
  const int delta_j = (tardiness - T[r_i->first]) - 
    (T[r_j->last] - T[r_j->first]);

  // run i starts at r_j->first + shift
  
  const int tstart = tardiness;
  for(unsigned int k = r_i->first, t = release[r_j->first] + shift;
      k < r_i->last; k++) {
    const unsigned int job = seq[k];
    t += p->ptime(job);
    if((int) t > p->due(job)) {
      tardiness += p->weight(job) * 
	(t - p->due(job));
    }
  }
  const int delta_i = (tardiness - tstart) - 
    (T[r_i->last] - T[r_i->first]);

  // compute bounds (aka pretesting)
  if(shift > 0) {
    if(delta_i + delta_j < minImp) {
      for(unsigned int l = locrun[r_i->last]; l < locrun[r_j->first]; l++) {
	const class Run* rtmp = locR[l];
	if(rtmp->late) {
	  tardiness += (T[rtmp->last] - T[rtmp->first]) + 
	    shift * (U[rtmp->last] - U[rtmp->first]);
	} else {
	  for(unsigned int k = rtmp->first; k < rtmp->last; k++) {
	    const unsigned int job = seq[k];
	    const int t = release[k] + shift + p->ptime(job);
	    if((int) t > p->due(job)) {
	      tardiness += p->weight(job) * 
		(t - p->due(job));
	    }
	  }
	}
      }
    } else {
      return minImp + 1;
    }
  } else {
    if((delta_i + delta_j + 
	max(-(T[r_j->first] - T[r_i->last]), 
	    shift * (U[r_j->first] - U[r_i->last]))) < minImp) {
      for(unsigned int l = locrun[r_i->last]; l < locrun[r_j->first]; l++) {
	const class Run* rtmp = locR[l];
	if(rtmp->late) {
	  for(unsigned int k = rtmp->first; k < rtmp->last; k++) {
	    const unsigned int job = seq[k];
	    const int t = release[k] + shift + p->ptime(job);
	    if((int) t > p->due(job)) {
	      tardiness += p->weight(job) * 
		(t - p->due(job));
	    }
	  }
	}
      }
    } else {
      return minImp + 1;;
    }
  }

  return tardiness - T[r_j->last];
}

int 
SRuns::whatifSwpR(const Run* r_i, const Run* r_j, 
		  const int& minImp) const {
  const unsigned int ii = run[r_i->last];
  const unsigned int jj = run[r_j->first];

  const int partLen = r_j->last - r_i->first;
  bool status[partLen];
  if(r_j->late) {
    for(unsigned int k = r_j->first, t = release[r_i->first], i = 0;
	k < r_j->last; k++, i++) {
      const unsigned int job = seq[k];
      t += p->ptime(job);
      status[i] = ((int) t > p->due(job));
    }
  } else {
    fill(status, status+(r_j->last-r_j->first), false);
  }

  const int makespan_i = release[r_i->last] - release[r_i->first];
  const int makespan_j = release[r_j->last] - release[r_j->first];
  const int shift = makespan_j - makespan_i;
  const int jshift = (r_j->last - r_j->first) - (r_i->last - r_i->first);

  for(unsigned int k = r_i->first, t = release[r_j->first] + shift,
	i = r_j->first - r_i->first + jshift;
      k < r_i->last; k++, i++) {
    const unsigned int job = seq[k];
    t += p->ptime(job);
    status[i] = ((int) t > p->due(job));
  }

  if(shift > 0) {
    for(unsigned int l = ii; l < jj; l++) {
      const class Run* rtmp = R[l];
      if(rtmp->late) {
	fill(status+rtmp->first+jshift-r_i->first, 
	     status+rtmp->last+jshift-r_i->first, true);
      } else {
	for(unsigned int k = rtmp->first; k < rtmp->last; k++) {
	  const unsigned int job = seq[k];
	  const int t = release[k] + shift + p->ptime(job);
	  status[k+jshift-r_i->first] = ((int) t > p->due(job));
	}
      }
    }
  } else { // reduction in release dates
    for(unsigned int l = ii; l < jj; l++) {
      const class Run* rtmp = R[l];
      if(rtmp->late) {
	for(unsigned int k = rtmp->first; k < rtmp->last; k++) {
	  const unsigned int job = seq[k];
	  const int t = release[k] + shift + p->ptime(job);
	  status[k+jshift-r_i->first] = ((int) t > p->due(job));
	}
      } else {
	fill(status+rtmp->first+jshift-r_i->first, 
	     status+rtmp->last+jshift-r_i->first, false);
      }
    }
  }
  
  int uniq_sz = 0;
  bool flag = !status[0];
  for(int i = 0; i < partLen; i++) {
    if(flag != status[i]) {
      flag = !flag;
      uniq_sz++;
    }
  }
  
  int delta_r = uniq_sz - (jj-ii);
  if(jj + 1 < R.size() && R[jj+1]->late == status[partLen-1]) 
    delta_r--;
  if(ii > 1 && R[ii-2]->late == status[0])
    delta_r--;

  return delta_r;
}

int
SRSol::whatifBwdT(const unsigned int& i, const unsigned int& j, 
		  const int& min_imp) const {
  assert(min_imp < 1); // consider only minimization moves
  assert(i < j);

  assert(run[i] <= run[j]);

  if(run[i] == run[j]) { // nr runs will not decrease
    if(late[i]) {

      int deltaT = 0;
      
      // tardiness of job i increases with wi(Rj+pj-pi+pi-di) - wi(Ri+pi-di)
      deltaT += p->weight(seq[i]) * (release[j] - release[i+1]);

      const int shift = - p->ptime(seq[i]);
  
      // upperbound on improvement
      const int upperbound = deltaT + 
	max(-(T[j]-T[i+1]), shift*(U[j] - U[i+1])); 
      if(upperbound < min_imp) {
	for(unsigned int k = i+1; k < j; k++) {
	  if(release[k+1]+shift > p->due(seq[k])) { 
	    deltaT += shift*p->weight(seq[k]);
	  } else {
	    deltaT -= (T[k+1]-T[k]);
	  }
	}
      } else {
	return 0;
      }
      assert(deltaT == Solution::whatifBwd(i, j, min_imp));
      return deltaT;
    } else {
      // no improvement possible
      return 0;
    }
  } else if(run[j] - run[i] == 1) {
    const unsigned int pivot = R[run[i]]->last;
    assert(R[run[i]]->last == R[run[j]]->first);
    const int shift =  - p->ptime(seq[i]);
 
    // shift < 0
    if(late[i]) {
      // tardiness of j remains 0
      int deltaT = 0; 
      const int upperbound = max(-(T[pivot]-T[i]), shift*(U[pivot] - U[i]));
      if(deltaT + upperbound > min_imp) {
	return 0;
      } else {
	// tardiness of i increases
	deltaT += p->weight(seq[i]) * (release[j] - release[i+1]);
	if(deltaT + upperbound > min_imp) {
	  return 0;
	} else {
	  for(unsigned int k = i+1; k < pivot; k++) {
	    if(release[k+1]+shift > p->due(seq[k])) { // still late
	      deltaT += shift*p->weight(seq[k]);
	    } else {
	      deltaT -= (T[k+1]-T[k]);
	    }
	  }
	  assert(deltaT == Solution::whatifBwd(i, j, min_imp));
	  return deltaT;
	}
      }
    } else { // [pivot, j] is late
      int deltaT = 0;
      const int upperbound = max(-(T[j]-T[pivot-1]), 
				 shift*(U[j] - U[pivot-1]));
      if(deltaT + upperbound > min_imp) {
	return 0;
      } else {
	// job i might become late
	const unsigned int job = seq[i];
	deltaT += (release[j] > p->due(job) ?
		   p->weight(job) * (release[j] - p->due(job)) : 0);
	if(deltaT + upperbound > min_imp) {
	  return 0;
	} else {
	  
	  for(unsigned int k = pivot; k < j; k++) {
	    if(release[k+1]+shift > p->due(seq[k])) { 
	      deltaT += shift*p->weight(seq[k]);
	    } else {
	      deltaT -= (T[k+1]-T[k]);
	    }
	  }
	  assert(deltaT == Solution::whatifBwd(i, j, min_imp));
	  return deltaT;
	}
      }
    }
  } else { // several runs in between
    const unsigned int nr_i = run[i];
    const unsigned int nr_j = run[j];
    const class Run* r_i = R[nr_i];
    const class Run* r_j = R[nr_j];
    
    unsigned int tmprun[sz];
    vector<const Run*> tmpR;
    tmpR.reserve(R.size());
    
    unsigned int nr = 0;
    
    // split r_i
    if(i > r_i->first) {
      fill(tmprun+r_i->first, tmprun+i, nr);
      tmpR.push_back(new Run(r_i->first, i, r_i->late));
      nr++;
    }
    
    tmpR.push_back(new Run(i, i+1, r_i->late));
    nr++;
    
    if(i+1 < r_i->last) {
      fill(tmprun+i+1, tmprun+r_i->last, nr);
      tmpR.push_back(new Run(i+1, r_i->last, r_i->late));
      nr++;
    }

    // copy (nr_i, nr_j)
    for(unsigned int k = nr_i+1; k < nr_j; k++) {
      const class Run* r = R[k];
      fill(tmprun+r->first, tmprun+r->last, nr);
      tmpR.push_back(new Run(r));
      nr++;
    }

    // split r_j
    if(j > r_j->first) {
      fill(tmprun+r_j->first, tmprun+j, nr);
      tmpR.push_back(new Run(r_j->first, j, r_j->late));
      nr++;
    }
    
    fill(tmprun+j, tmprun+r_j->last, nr);
    tmpR.push_back(new Run(j, r_j->last, r_j->late));
    nr++;
    
    // compute tardiness
    Run rii(i, i+1, r_i->late);
    Run rjj(j, j, r_j->late);

    const int improvement = SRuns::whatifSwpT(&rii, &rjj, min_imp,
					      tmpR, tmprun);
    
    // clean up
    for_each(tmpR.begin(), tmpR.end(), rm());
    
    // return
    return improvement;
  }
}


int
SRSol::whatifFwdT(const unsigned int& i, const unsigned int& j, 
		  const int& min_imp) const {
  assert(min_imp < 1); // consider only minimization moves
  assert(i < j);

  assert(run[i] <= run[j]);

  if(run[i] == run[j]) { // nr runs will not decrease
    if(late[i]) {

      int deltaT = 0;
      // tardiness of job j decreases
      deltaT += max(-(T[j+1]-T[j]),
  		    p->weight(seq[j]) * (release[i] - release[j]));

      const int shift = p->ptime(seq[j]);
  
      deltaT += shift*(U[j] - U[i]);
      assert(deltaT == Solution::whatifFwd(i, j, min_imp));
      return deltaT;
    } else {
      // no improvement possible
      return 0;
    }
  } else if(run[j] - run[i] == 1) {
    const unsigned int pivot = R[run[i]]->last;
    assert(R[run[i]]->last == R[run[j]]->first);
    const int shift = p->ptime(seq[j]);
 
    if(late[i]) { // no feasible improvement
      return 0;
    } else { // [pivot .. j) is late
      // tardiness reduction of job j;
      int deltaT = max(-(T[j+1]-T[j]),  
		       p->weight(seq[j]) * (release[i] - release[j]));

      if(deltaT > min_imp) {
	return 0;
      } else {
	// Early jobs in run of i may become late
	for(unsigned int k = i; k < pivot; k++) {
	  const unsigned int job = seq[k];
	  if(release[k+1]+shift > p->due(job)) { // still late
	    deltaT += p->weight(job) * (release[k+1] + shift - p->due(job));
	  }
	}
	// Tardiness of late jobs in run of j increases
	deltaT += shift * (U[j] - U[pivot-1]);
	assert(deltaT == Solution::whatifFwd(i, j, min_imp));
	return deltaT;
      }
    }
  } else { // several runs in between
    if(late[j]) {
      const unsigned int nr_i = run[i];
      const unsigned int nr_j = run[j];
      const class Run* r_i = R[nr_i];
      const class Run* r_j = R[nr_j];

      unsigned int tmprun[sz];
      vector<const Run*> tmpR;
      tmpR.reserve(R.size());

      unsigned int nr = 0;
      
      // split r_i
      if(i > r_i->first) {
	fill(tmprun+r_i->first, tmprun+i, nr);
	tmpR.push_back(new Run(r_i->first, i, r_i->late));
	nr++;
      }

      fill(tmprun+i, tmprun+r_i->last, nr);
      tmpR.push_back(new Run(i, r_i->last, r_i->late));
      nr++;

      // copy (nr_i, nr_j)
      for(unsigned int k = nr_i+1; k < nr_j; k++) {
	const class Run* r = R[k];
	fill(tmprun+r->first, tmprun+r->last, nr);
	tmpR.push_back(new Run(r));
	nr++;
      }

      // split r_j
      if(j > r_j->first) {
	fill(tmprun+r_j->first, tmprun+j, nr);
	tmpR.push_back(new Run(r_j->first, j, r_j->late));
	nr++;
      }

      tmprun[j] = nr;
      tmpR.push_back(new Run(j, j+1, r_j->late));
      nr++;

      if(j+1 < r_j->last) {
	fill(tmprun+j+1, tmprun+r_j->last, nr);
	tmpR.push_back(new Run(j+1, r_j->last, r_j->late));
	nr++;
      }

      // compute tardiness
      Run rii(i, i, r_i->late);
      Run rjj(j, j+1, r_j->late);
      const int improvement = SRuns::whatifSwpT(&rii, &rjj, min_imp,
						tmpR, tmprun);
      
      // clean up
      for_each(tmpR.begin(), tmpR.end(), rm());
      
      // return
      return improvement;

    } else {
      return 0;
    }
  }
}


int 
SRSol::whatifSwpT(const unsigned int& i, const unsigned int& j, 
		  const int& min_imp) const {
  assert(min_imp < 1); // consider only minimization moves
  assert(i < j);

  assert(run[i] <= run[j]);

  if(run[i] == run[j]) { // nr runs will not decrease
    if(late[i]) {

      int deltaT = 0;
      // tardiness of job j decreases
      deltaT += max(-(T[j+1]-T[j]),
  		    p->weight(seq[j]) * (release[i] - release[j]));
      // tardiness of job i increases with wi(Rj+pj-pi+pi-di) - wi(Ri+pi-di)
      deltaT += p->weight(seq[i]) * (release[j+1] - release[i+1]);


      // nothing happens for U objective between i and j
      const int shift = p->ptime(seq[j]) - p->ptime(seq[i]);
  
      if(shift > 0) { // tardiness of jobs (i, j) increases by shift*W
	deltaT += shift*(U[j] - U[i+1]);
      } else {
	// upperbound on improvement
	const int upperbound = deltaT + 
	  max(-(T[j]-T[i+1]), shift*(U[j] - U[i+1])); 
	if(upperbound < min_imp) {
	  for(unsigned int k = i+1; k < j; k++) {
	    if(release[k+1]+shift > p->due(seq[k])) { // still late
	      deltaT += shift*p->weight(seq[k]);
	    } else {
	      deltaT -= (T[k+1]-T[k]);
	    }
	  }
	} else {
	  return 0;
	}
      }
      assert(deltaT == Solution::whatifSwp(i, j, min_imp));
      return deltaT;
    } else {
      // no improvement possible
      return 0;
    }
  } else if(run[j] - run[i] == 1) {
    const unsigned int pivot = R[run[i]]->last;
    assert(R[run[i]]->last == R[run[j]]->first);
    const int shift = p->ptime(seq[j]) - p->ptime(seq[i]);
 
    if(shift > 0) {
      if(late[i]) { // no feasible improvement
	return 0;
      } else { // [pivot .. j) is late
	// tardiness reduction of job j;
	int deltaT = max(-(T[j+1]-T[j]),  
			 p->weight(seq[j]) * (release[i] - release[j]));
	// job i might become late
	{
	  const unsigned int job = seq[i];
	  deltaT += (release[j+1] > p->due(job) ?
		     p->weight(job) * (release[j+1] - p->due(job)) : 0);
	}
	if(deltaT > min_imp) {
	  return 0;
	} else {
	  // Early jobs in run of i may become late
	  for(unsigned int k = i+1; k < pivot; k++) {
	    const unsigned int job = seq[k];
	    if(release[k+1]+shift > p->due(job)) { // still late
	      deltaT += p->weight(job) * (release[k+1] + shift - p->due(job));
	    }
	  }
	  // Tardiness of late jobs in run of j increases
	  deltaT += shift * (U[j] - U[pivot-1]);
	  assert(deltaT == Solution::whatifSwp(i, j, min_imp));
	  return deltaT;
	}
      }
    } else { // shift < 0
      if(late[i]) {
	// tardiness of j remains 0
	int deltaT = 0; 
	const int upperbound = max(-(T[pivot]-T[i]), shift*(U[pivot] - U[i]));
	if(deltaT + upperbound > min_imp) {
	  return 0;
	} else {
	  // tardiness of i increases
	  deltaT += p->weight(seq[i]) * (release[j+1] - release[i+1]);
	  if(deltaT + upperbound > min_imp) {
	    return 0;
	  } else {
	    // early jobs of j remain 0
	    // late jobs of i may become early
	    for(unsigned int k = i+1; k < pivot; k++) {
	      if(release[k+1]+shift > p->due(seq[k])) { // still late
		deltaT += shift*p->weight(seq[k]);
	      } else {
		deltaT -= (T[k+1]-T[k]);
	      }
	    }
	    assert(deltaT == Solution::whatifSwp(i, j, min_imp));
	    return deltaT;
	  }
	}
      } else { // [pivot, j) is late
	// tardiness of job j decreases
	int deltaT = max(-(T[j+1]-T[j]),  
			 p->weight(seq[j]) * (release[i] - release[j]));
	
	const int upperbound = max(-(T[j]-T[pivot-1]), 
				   shift*(U[j] - U[pivot-1]));
	if(deltaT + upperbound > min_imp) {
	  return 0;
	} else {
	  // job i might become late
	  const unsigned int job = seq[i];
	  deltaT += (release[j+1] > p->due(job) ?
		     p->weight(job) * (release[j+1] - p->due(job)) : 0);
	  if(deltaT + upperbound > min_imp) {
	    return 0;
	  } else {

	    for(unsigned int k = pivot; k < j; k++) {
	      if(release[k+1]+shift > p->due(seq[k])) { // still late
		deltaT += shift*p->weight(seq[k]);
	      } else {
		deltaT -= (T[k+1]-T[k]);
	      }
	    }
	    assert(deltaT == Solution::whatifSwp(i, j, min_imp));
	    return deltaT;
	  }
	}
      }
    }
  } else { // several runs in between
    if(late[i] && !late[j] && p->ptime(seq[j]) > p->ptime(seq[i])) {
      return 0;
    } else {
      const unsigned int nr_i = run[i];
      const unsigned int nr_j = run[j];
      const class Run* r_i = R[nr_i];
      const class Run* r_j = R[nr_j];

      unsigned int tmprun[sz];
      vector<const Run*> tmpR;
      tmpR.reserve(R.size());

      unsigned int nr = 0;
      
      // split r_i
      if(i > r_i->first) {
	fill(tmprun+r_i->first, tmprun+i, nr);
	tmpR.push_back(new Run(r_i->first, i, r_i->late));
	nr++;
      }

      tmpR.push_back(new Run(i, i+1, r_i->late));
      nr++;

      if(i+1 < r_i->last) {
	fill(tmprun+i+1, tmprun+r_i->last, nr);
	tmpR.push_back(new Run(i+1, r_i->last, r_i->late));
	nr++;
      }

      // copy (nr_i, nr_j)
      for(unsigned int k = nr_i+1; k < nr_j; k++) {
	const class Run* r = R[k];
	fill(tmprun+r->first, tmprun+r->last, nr);
	tmpR.push_back(new Run(r));
	nr++;
      }

      // split r_j
      if(j > r_j->first) {
	fill(tmprun+r_j->first, tmprun+j, nr);
	tmpR.push_back(new Run(r_j->first, j, r_j->late));
	nr++;
      }

      tmprun[j] = nr;
      tmpR.push_back(new Run(j, j+1, r_j->late));
      nr++;

      if(j+1 < r_j->last) {
	fill(tmprun+j+1, tmprun+r_j->last, nr);
	tmpR.push_back(new Run(j+1, r_j->last, r_j->late));
	nr++;
      }

      // compute tardiness
      Run rii(i, i+1, r_i->late);
      Run rjj(j, j+1, r_j->late);
      const int improvement = SRuns::whatifSwpT(&rii, &rjj, min_imp,
						tmpR, tmprun);
      
      // clean up
      for_each(tmpR.begin(), tmpR.end(), rm());
      
      // return
      return improvement;
    }
  }
}

/*****
      Unit penalty
*****/

int
SRSol::whatifBwdU(const unsigned int& i, const unsigned int& j, 
		  const int& min_imp) const {
  assert(min_imp < 1); // consider only minimization moves
  assert(i < j);

  assert(run[i] <= run[j]);

  if(run[i] == run[j]) { // nr runs will not decrease
    if(late[i]) {
      int penalty = 0;
      
      const int shift = - p->ptime(seq[i]);
  
      // upperbound on improvement
      const int upperbound = (U[j] - U[i+1]); 
      if(upperbound < min_imp) {
	for(unsigned int k = i+1; k < j; k++) {
	  const unsigned int job = seq[k];
	  if(release[k+1]+shift <= p->due(job)) { // not late anymore
	    penalty -= p->weight(job);
	  }
	}
      } else {
	return 0;
      }
      assert(penalty == Solution::whatifBwd(i, j, min_imp));
      return penalty;
    } else {
      // no improvement possible
      return 0;
    }
  } else if(run[j] - run[i] == 1) { // adjacent runs
    const unsigned int pivot = R[run[i]]->last;
    assert(R[run[i]]->last == R[run[j]]->first);
    const int shift =  - p->ptime(seq[i]);
 
    // shift < 0
    if(late[i]) {
      // tardiness of j remains 0
      int penalty = 0; 
      const int upperbound = -(U[pivot] - U[i]);
      if(penalty + upperbound > min_imp) {
	return 0;
      } else {
	for(unsigned int k = i+1; k < pivot; k++) {
	  if(release[k+1]+shift <= p->due(seq[k])) { // not late anymore
	    penalty -= p->weight(seq[k]);
	  }
	}
	assert(penalty == Solution::whatifBwd(i, j, min_imp));
	return penalty;
      }
    } else { // [pivot, j] is late
      int penalty = 0;
      const int upperbound = shift*(U[j] - U[pivot-1]);
      if(penalty + upperbound > min_imp) {
	return 0;
      } else {
	// job i might become late
	const unsigned int job = seq[i];
	penalty += (release[j] > p->due(job) ? p->weight(job) : 0);
	if(penalty + upperbound > min_imp) {
	  return 0;
	} else {
	  
	  for(unsigned int k = pivot; k < j; k++) {
	    if(release[k+1]+shift <= p->due(seq[k])) { 
	      penalty -= p->weight(seq[k]);
	    }
	  }
	  assert(penalty == Solution::whatifBwd(i, j, min_imp));
	  return penalty;
	}
      }
    }
  } else { // several runs in between
    const unsigned int nr_i = run[i];
    const unsigned int nr_j = run[j];
    const class Run* r_i = R[nr_i];
    const class Run* r_j = R[nr_j];
    
    unsigned int tmprun[sz];
    vector<const Run*> tmpR;
    tmpR.reserve(R.size());
    
    unsigned int nr = 0;
    
    // split r_i
    if(i > r_i->first) {
      fill(tmprun+r_i->first, tmprun+i, nr);
      tmpR.push_back(new Run(r_i->first, i, r_i->late));
      nr++;
    }
    
    tmpR.push_back(new Run(i, i+1, r_i->late));
    nr++;
    
    if(i+1 < r_i->last) {
      fill(tmprun+i+1, tmprun+r_i->last, nr);
      tmpR.push_back(new Run(i+1, r_i->last, r_i->late));
      nr++;
    }

    // copy (nr_i, nr_j)
    for(unsigned int k = nr_i+1; k < nr_j; k++) {
      const class Run* r = R[k];
      fill(tmprun+r->first, tmprun+r->last, nr);
      tmpR.push_back(new Run(r));
      nr++;
    }

    // split r_j
    if(j > r_j->first) {
      fill(tmprun+r_j->first, tmprun+j, nr);
      tmpR.push_back(new Run(r_j->first, j, r_j->late));
      nr++;
    }
    
    fill(tmprun+j, tmprun+r_j->last, nr);
    tmpR.push_back(new Run(j, r_j->last, r_j->late));
    nr++;
    
    // compute penalty
    Run rii(i, i+1, r_i->late);
    Run rjj(j, j, r_j->late);

    const int improvement = SRuns::whatifSwpU(&rii, &rjj, min_imp,
					      tmpR, tmprun);
    
    // clean up
    for_each(tmpR.begin(), tmpR.end(), rm());
    
    // return
    return improvement;
  }
}


int
SRSol::whatifFwdU(const unsigned int& i, const unsigned int& j, 
		  const int& min_imp) const {
  assert(min_imp < 1); // consider only minimization moves
  assert(i < j);

  assert(run[i] <= run[j]);

  if(run[i] == run[j]) { // nr runs will not decrease
    if(late[i]) {

      // tardiness of job j decreases
      const int penalty = (release[i] + p->ptime(seq[j]) <= p->due(seq[j])) ?
	- p->weight(seq[j]) : 0;
      
      assert(penalty == Solution::whatifFwd(i, j, min_imp));
      return penalty;
    } else {
      // no improvement possible
      return 0;
    }
  } else if(run[j] - run[i] == 1) {
    const unsigned int pivot = R[run[i]]->last;
    assert(R[run[i]]->last == R[run[j]]->first);
    const int shift = p->ptime(seq[j]);
 
    if(late[i]) { // no feasible improvement
      return 0;
    } else { // [pivot .. j) is late
      // tardiness reduction of job j;
      int penalty = (release[i] + p->ptime(seq[j]) <= p->due(seq[j])) ?
	- p->weight(seq[j]) : 0;

      if(penalty > min_imp) {
	return 0;
      } else {
	// Early jobs in run of i may become late
	for(unsigned int k = i; k < pivot; k++) {
	  const unsigned int job = seq[k];
	  if(release[k+1]+shift > p->due(job)) { //  late
	    penalty += p->weight(job);
	  }
	}
	assert(penalty == Solution::whatifFwd(i, j, min_imp));
	return penalty;
      }
    }
  } else { // several runs in between
    if(late[j]) {
      const unsigned int nr_i = run[i];
      const unsigned int nr_j = run[j];
      const class Run* r_i = R[nr_i];
      const class Run* r_j = R[nr_j];

      unsigned int tmprun[sz];
      vector<const Run*> tmpR;
      tmpR.reserve(R.size());

      unsigned int nr = 0;
      
      // split r_i
      if(i > r_i->first) {
	fill(tmprun+r_i->first, tmprun+i, nr);
	tmpR.push_back(new Run(r_i->first, i, r_i->late));
	nr++;
      }

      fill(tmprun+i, tmprun+r_i->last, nr);
      tmpR.push_back(new Run(i, r_i->last, r_i->late));
      nr++;

      // copy (nr_i, nr_j)
      for(unsigned int k = nr_i+1; k < nr_j; k++) {
	const class Run* r = R[k];
	fill(tmprun+r->first, tmprun+r->last, nr);
	tmpR.push_back(new Run(r));
	nr++;
      }

      // split r_j
      if(j > r_j->first) {
	fill(tmprun+r_j->first, tmprun+j, nr);
	tmpR.push_back(new Run(r_j->first, j, r_j->late));
	nr++;
      }

      tmprun[j] = nr;
      tmpR.push_back(new Run(j, j+1, r_j->late));
      nr++;

      if(j+1 < r_j->last) {
	fill(tmprun+j+1, tmprun+r_j->last, nr);
	tmpR.push_back(new Run(j+1, r_j->last, r_j->late));
	nr++;
      }

      // compute tardiness
      Run rii(i, i, r_i->late);
      Run rjj(j, j+1, r_j->late);
      const int improvement = SRuns::whatifSwpU(&rii, &rjj, min_imp,
						tmpR, tmprun);
      
      // clean up
      for_each(tmpR.begin(), tmpR.end(), rm());
      
      // return
      return improvement;

    } else {
      return 0;
    }
  }
}


int 
SRSol::whatifSwpU(const unsigned int& i, const unsigned int& j, 
		  const int& min_imp) const {
  assert(min_imp < 1); // consider only minimization moves
  assert(i < j);

  assert(run[i] <= run[j]);

  if(run[i] == run[j]) { // nr runs will not decrease
    if(late[i]) {

      int penalty = 0;
      // tardiness of job j decreases
      penalty -= (release[i] + p->ptime(seq[j]) <= p->due(seq[j])) ?
	p->weight(seq[j]) : 0;

      const int shift = p->ptime(seq[j]) - p->ptime(seq[i]);
      if(shift < 0) {
	for(unsigned int k = i+1; k < j; k++) {
	  if(release[k+1] + shift <= p->due(seq[k]))
	    penalty -= p->weight(seq[k]);
	}
      } 
      assert(penalty == Solution::whatifSwp(i, j, min_imp));
      return penalty;
    } else {
      // no improvement possible
      return 0;
    }
  } else if(run[j] - run[i] == 1) {
    const unsigned int pivot = R[run[i]]->last;
    assert(R[run[i]]->last == R[run[j]]->first);
    const int shift = p->ptime(seq[j]) - p->ptime(seq[i]);
 
    if(shift > 0) {
      if(late[i]) { // no feasible improvement
	return 0;
      } else { // [pivot .. j) is late
	// tardiness reduction of job j;
	int penalty = 0;
	penalty -= (release[i] + p->ptime(seq[j]) <= p->due(seq[j])) ?
	p->weight(seq[j]) : 0;
	// job i might become late
	{
	  const unsigned int job = seq[i];
	  penalty += (release[j+1] > p->due(job) ? p->weight(job) : 0);
	}
	if(penalty > min_imp) {
	  return 0;
	} else {
	  // Early jobs in run of i may become late
	  for(unsigned int k = i+1; k < pivot; k++) {
	    const unsigned int job = seq[k];
	    if(release[k+1]+shift > p->due(job)) { // still late
	      penalty += p->weight(job);
	    }
	  }
	  assert(penalty == Solution::whatifSwp(i, j, min_imp));
	  return penalty;
	}
      }
    } else { // shift < 0
      if(late[i]) {
	// tardiness of j remains 0
	int penalty = 0; 
	const int upperbound = -(U[pivot]-U[i]);
	if(penalty + upperbound > min_imp) {
	  return 0;
	} else {
	  // early jobs of j remain 0
	  // late jobs of i may become early
	  for(unsigned int k = i+1; k < pivot; k++) {
	    if(release[k+1]+shift <= p->due(seq[k])) { 
	      penalty -= p->weight(seq[k]);
	    } 
	  }
	  assert(penalty == Solution::whatifSwp(i, j, min_imp));
	  return penalty;
	}
      } else { // [pivot, j) is late
	// tardiness of job j decreases
	int penalty = 0;
	penalty -= (release[i] + p->ptime(seq[j]) <= p->due(seq[j])) ?
	p->weight(seq[j]) : 0;
	
	const int upperbound = -(U[j]-U[pivot-1]); 
	if(penalty + upperbound > min_imp) {
	  return 0;
	} else {
	  // job i might become late
	  const unsigned int job = seq[i];
	  penalty += (release[j+1] > p->due(job) ? p->weight(job) : 0);
	  if(penalty + upperbound > min_imp) {
	    return 0;
	  } else {

	    for(unsigned int k = pivot; k < j; k++) {
	      if(release[k+1]+shift <= p->due(seq[k])) { // early
		penalty -= p->weight(seq[k]);
	      } 
	    }
	    assert(penalty == Solution::whatifSwp(i, j, min_imp));
	    return penalty;
	  }
	}
      }
    }
  } else { // several runs in between
    if(late[i] && !late[j] && p->ptime(seq[j]) > p->ptime(seq[i])) {
      return 0;
    } else {
      const unsigned int nr_i = run[i];
      const unsigned int nr_j = run[j];
      const class Run* r_i = R[nr_i];
      const class Run* r_j = R[nr_j];

      unsigned int tmprun[sz];
      vector<const Run*> tmpR;
      tmpR.reserve(R.size());

      unsigned int nr = 0;
      
      // split r_i
      if(i > r_i->first) {
	fill(tmprun+r_i->first, tmprun+i, nr);
	tmpR.push_back(new Run(r_i->first, i, r_i->late));
	nr++;
      }

      tmpR.push_back(new Run(i, i+1, r_i->late));
      nr++;

      if(i+1 < r_i->last) {
	fill(tmprun+i+1, tmprun+r_i->last, nr);
	tmpR.push_back(new Run(i+1, r_i->last, r_i->late));
	nr++;
      }

      // copy (nr_i, nr_j)
      for(unsigned int k = nr_i+1; k < nr_j; k++) {
	const class Run* r = R[k];
	fill(tmprun+r->first, tmprun+r->last, nr);
	tmpR.push_back(new Run(r));
	nr++;
      }

      // split r_j
      if(j > r_j->first) {
	fill(tmprun+r_j->first, tmprun+j, nr);
	tmpR.push_back(new Run(r_j->first, j, r_j->late));
	nr++;
      }

      tmprun[j] = nr;
      tmpR.push_back(new Run(j, j+1, r_j->late));
      nr++;

      if(j+1 < r_j->last) {
	fill(tmprun+j+1, tmprun+r_j->last, nr);
	tmpR.push_back(new Run(j+1, r_j->last, r_j->late));
	nr++;
      }

      // compute tardiness
      Run rii(i, i+1, r_i->late);
      Run rjj(j, j+1, r_j->late);
      const int improvement = SRuns::whatifSwpU(&rii, &rjj, min_imp,
						tmpR, tmprun);
      
      // clean up
      for_each(tmpR.begin(), tmpR.end(), rm());
      
      // return
      return improvement;
    }
  }
}

void
SRSol::doAdd(const unsigned int& job, const unsigned int& pos) {
  assert(sz <= p->njobs());
  
  // add to back of seq
  seq[sz] = job;
  release[sz+1] = release[sz] + p->ptime(job);
  const bool lateFlag = late[sz] = release[sz+1] > p->due(job);
  U[sz+1] = U[sz];
  if(o == 'T') {
    T[sz+1] = T[sz];
  }
  if(lateFlag) {
    U[sz+1] += p->weight(job);
    if(o == 'T') {
      T[sz+1] += p->weight(job) * (release[sz+1] - p->due(job));
    }
  }
  if(sz > 0) {// consider merging
//  #ifndef NDEBUG
//      if(R.back()->first >= sz) {
//        PR(R.back()->first);
//        PR(R.back()->last);
//        PR(sz);
//      }
//  #endif
    while(R.back()->first >= sz) {
      R.pop_back();
    }
    if(R.back()->late != lateFlag) {
      R.push_back(new class Run(sz, sz+1, lateFlag));
      run[sz] = run[sz-1] + 1;
    } else {
      const class Run* lastRun = R.back();
      R.pop_back();
      assert(lastRun->first < sz);
      R.push_back(new class Run(lastRun->first, sz+1, lateFlag));
      delete lastRun;
    }
  } else {
    run[0] = 0;
  }

  sz++;
  setVal();
  
  // move to pos
  if(pos < sz - 1) {
    doFwd(pos, sz - 1);
  } 
}

void
SRSol::doRemove(const unsigned int& pos) {
  // move to end
  if(pos < sz - 1) {
    doBwd(pos, sz);
  }
  // shrink
  sz--;
  setVal();
  // update last Run
  while(!R.empty() && R.back()->first >= sz) {
    R.pop_back();
  }
  if(!R.empty()) {
    const class Run * lastRun = R.back();
    assert(lastRun->last >= sz);
    R.pop_back();
#ifndef NDEBUG
    if(lastRun->first > sz) {
      PR(lastRun->first);
      PR(sz);
      abort();
    }
#endif
    R.push_back(new class Run(lastRun->first, sz, lastRun->late));
    delete lastRun;
  }
}

void
SRSol::doReplace(const unsigned int& pos, const unsigned int& job) {
  assert(pos < sz);
  assert(job < p->njobs());
  assert(find(seq, seq+sz, job) == seq+sz);
  assert(sz < p->njobs() + 1);
  const unsigned int end = sz; // sz is changed by doFoo
  doAdd(job, end);
  doSwp(pos, end);
  doRemove(end);
}

int
SRSol::whatifAddT(const unsigned int& job, 
		  const unsigned int& pos, 
		  const int& min_imp) const {
  // tardiness of job at pos
  int delta = 0;
  if(release[pos]+ p->ptime(job) > p->due(job)) {
    delta += p->weight(job) * (release[pos] + p->ptime(job) - p->due(job));
  }

  if(pos < sz) {
    if(delta < min_imp) {
      // compute tardiness of first run <pos, r.end>
      int shift = p->ptime(job);
      
      const class Run* r1 = R[run[pos]];
      if(r1->late) {
	delta += shift * (U[r1->last] - U[pos]);
      } else {
	for(unsigned int k = pos; k < r1->last; k++) {
	  const unsigned int locjob = seq[k];
	  const int t = release[k+1] + shift;
	  if(t > p->due(locjob)) {
	    delta += p->weight(locjob) * (t - p->due(locjob));
	  }
	}
      }
      
      // compute tardiness of other runs
      if(r1->last < sz) {
	const unsigned int maxRun = R.size();
	unsigned int curRun = run[r1->last];
	while(delta < min_imp && curRun < maxRun) {
	  const class Run* rtmp = R[curRun];
	  if(rtmp->late) {
	    delta += shift * (U[rtmp->last] - U[rtmp->first]);
	  } else {
	    for(unsigned int k = rtmp->first; k < rtmp->last; k++) {
	      const unsigned int locjob = seq[k];
	      const int t = release[k+1] + shift;
	      if(t > p->due(locjob)) {
		delta += p->weight(locjob) * (t - p->due(locjob));
	      }
	    }
	  }
	  curRun++;
	}
      }
    }
  }
  return delta;
}

int
SRSol::whatifAddU(const unsigned int& job, 
		  const unsigned int& pos, 
		  const int& min_imp) const {
  // penalty for new job
  int delta = 0;
  if(release[pos] + p->ptime(job) > p->due(job)) {
    delta += p->weight(job);
  }

  if(delta < min_imp && pos < sz) {
    // compute tardiness of first run <pos, r.end>
    int shift = p->ptime(job);
    
    const class Run* r1 = R[run[pos]];
    if(!r1->late) {
      for(unsigned int k = pos; k < r1->last; k++) {
	const unsigned int locjob = seq[k];
	const int t = release[k+1] + shift;
	if(t > p->due(locjob)) {
	  delta += p->weight(locjob);
	}
      }
    }
    if(r1->last < sz) {
      // compute tardiness of other runs
      const unsigned int maxRun = R.size();
      assert(maxRun == run[sz-1]+1);
      unsigned int curRun = run[r1->last];
      while(delta < min_imp && curRun < maxRun) {
	const class Run* rtmp = R[curRun];
	if(!rtmp->late) {
	  for(unsigned int k = rtmp->first; k < rtmp->last; k++) {
	    const unsigned int locjob = seq[k];
	    const int t = release[k+1] + shift;
	    if(t > p->due(locjob)) {
	      delta += p->weight(locjob);
	    }
	  }
	}
	curRun++;
      }
    }
  }
  return delta;
}

int
SRSol::whatifRemoveT(const unsigned int& pos, const int& min_imp) const {
  // minimum decrease with tardiness of job at pos
  int delta = - (T[pos+1] - T[pos]);
  assert(delta <= 0);
  // [pos+1, sz> shifts backwards
  if(pos + 1 < sz) {
    const int shift = p->ptime(seq[pos]);
    if(- min(T[sz] - T[pos], 
	     shift * (U[sz] - U[pos])) < min_imp) {
      // compute first run
      const class Run* r1 = R[run[pos+1]];
      if(r1->late) {
	for(unsigned int k = pos+1; k < r1->last; k++) {
	  delta -= min(T[k+1] - T[k], shift * p->weight(seq[k]));
	}
      }
      // compute other runs
      if(r1->last < sz) {
	for(unsigned int i = run[r1->last]; i < run[sz-1]; i++) {
	  const class Run* rtmp = R[i];
	  if(rtmp->late) {
#ifndef NDEBUG
	    int d_oud = delta;
	    int tardiness = 0;
#endif
	    for(unsigned int k = rtmp->first; k < rtmp->last; k++) {
#ifndef NDEBUG
	      if(release[k+1] - shift > p->due(seq[k])) {
		tardiness += p->weight(seq[k]) *
		  (release[k+1] - shift - p->due(seq[k]));
	      }
#endif
	      delta -= min(T[k+1] - T[k], shift * p->weight(seq[k]));
	    }
	    assert(tardiness - (T[rtmp->last] - T[rtmp->first]) ==
		   delta - d_oud);
	  }
	}
      }
      return delta;
    } else {
      // min improvement not feasible
      return 0;
    }
  } else {
    return delta;
  }
}

int 
SRSol::whatifRemoveU(const unsigned int& pos, const int& min_imp) const {
  int delta = - (U[pos+1] - U[pos]);
  if(pos < sz - 1) {
    const int shift = p->ptime(seq[pos]);
    if(- (U[sz] - U[pos]) < min_imp) {
      // compute first run
      const class Run* r1 = R[run[pos+1]];
      if(r1->late) {
	for(unsigned int k = pos+1; k < r1->last; k++) {
	  const unsigned int job = seq[k];
	  if(shift >= release[k] + p->ptime(job) - p->due(job)) {
	    delta -= p->weight(job);
	  }
	}
      }
      // compute other runs
      if(r1->last < sz) {
	for(unsigned int i = run[r1->last]; i < run[sz-1]; i++) {
	  const class Run* rtmp = R[i];
	  if(rtmp->late) {
	    for(unsigned int k = rtmp->first; k < rtmp->last; k++) {
	      const unsigned int job = seq[k];
	      if(shift >= release[k] + p->ptime(job) - p->due(job)) {
		delta -= p->weight(job);
	      }
	    }
	  }
	}
      }
      return delta;
    } else {
      // min improvement not feasible
      return 0;
    }
  } else {
    return delta;
  }
}

int 
SRSol::whatifReplaceT(const unsigned int& pos, const unsigned int& job,
		      const int& min_imp) const {
  // 
  const int t_old = T[pos+1] - T[pos];
  int t_new = 0;
  if(release[pos] + p->ptime(job) > p->due(job)) {
    t_new += p->weight(job) * (release[pos] + p->ptime(job) - p->due(job));
  }
  int delta = t_new - t_old;
  if(pos+1 >= sz) {
    return delta;
  } else {
    const int shift = p->ptime(job) - p->ptime(seq[pos]);
    if(shift < 0) {
      if(delta - min(T[sz] - T[pos+1],
		     -shift * (U[sz] - U[pos+1])) < min_imp) {
	// compute first run
	const class Run* r1 = R[run[pos+1]];
	if(r1->late) {
	  for(unsigned int k = pos+1; k < r1->last; k++) {
	    delta -= min(T[k+1] - T[k], -shift * p->weight(seq[k]));
	  }
	}
	// compute other runs
	if(r1->last < sz) {
	  for(unsigned int i = run[r1->last]; i < run[sz-1]; i++) {
	    const class Run* rtmp = R[i];
	    if(rtmp->late) {
	      for(unsigned int k = rtmp->first; k < rtmp->last; k++) {
		delta -= min(T[k+1] - T[k], -shift * p->weight(seq[k]));
	      }
	    }
	  }
	}
	return delta;
      } else {
	return INT_MAX/2;
      }
    } else if(delta < min_imp) {
      const class Run* r1 = R[run[pos+1]];
      if(r1->late) {
	delta += shift * (U[r1->last] - U[pos+1]);
      } else {
	for(unsigned int k = pos+1; k < r1->last; k++) {
	  const unsigned int locjob = seq[k];
	  const int t = release[k+1] + shift;
	  if(t > p->due(locjob)) {
	    delta += p->weight(locjob) * (t - p->due(locjob));
	  }
	}
      }
      // compute tardiness of other runs
      if(r1->last < sz) {
	const unsigned int maxRun = R.size();
	unsigned int curRun = run[r1->last];
	while(delta < min_imp && curRun < maxRun) {
	  const class Run* rtmp = R[curRun];
	  if(rtmp->late) {
	    delta += shift * (U[rtmp->last] - U[rtmp->first]);
	  } else {
	    for(unsigned int k = rtmp->first; k < rtmp->last; k++) {
	      const unsigned int locjob = seq[k];
	      const int t = release[k+1] + shift;
	      if(t > p->due(locjob)) {
		delta += p->weight(locjob) * (t - p->due(locjob));
	      }
	    }
	  }
	  curRun++;
	}
      }
    }
    return delta;
  }
}
  

int 
SRSol::whatifReplaceU(const unsigned int& pos, const unsigned int& job,
		      const int& min_imp) const {
  const int u_old = U[pos+1] - U[pos];
  int u_new = 0;
  if(release[pos] + p->ptime(job) > p->due(job)) {
    u_new += p->weight(job);
  }
  int delta = u_new - u_old;
  if(pos + 1 >= sz) {
    return delta;
  } else {
    const int shift = p->ptime(job) - p->ptime(seq[pos]);
    if(shift < 0) {
      if(delta - (U[sz] - U[pos+1]) < min_imp) {
	// compute first run
	const class Run* r1 = R[run[pos+1]];
	if(r1->late) {
	  for(unsigned int k = pos+1; k < r1->last; k++) {
	    const unsigned int job = seq[k];
	    if(release[k+1] + shift <= p->due(job)) {
	      delta -= p->weight(job);
	    }
	  }
	}
	// compute other runs
	if(r1->last < sz) {
	  for(unsigned int i = run[r1->last]; i < run[sz-1]; i++) {
	    const class Run* rtmp = R[i];
	    if(rtmp->late) {
	      for(unsigned int k = rtmp->first; k < rtmp->last; k++) {
		const unsigned int job = seq[k];
		if(release[k+1] + shift <= p->due(job)) {
		  delta -= p->weight(job);
		}
	      }
	    }
	  }
	}
	return delta;
      } else {
	// min improvement not feasible
	return INT_MAX/2;
      }
    } else if(delta < min_imp) {
      const class Run* r1 = R[run[pos+1]];
      if(!r1->late) {
	for(unsigned int k = pos+1; k < r1->last; k++) {
	  const unsigned int locjob = seq[k];
	  const int t = release[k+1] + shift;
	  if(t > p->due(locjob)) {
	    delta += p->weight(locjob);
	  }
	}
      }
      if(r1->last < sz) {
	// compute tardiness of other runs
	const unsigned int maxRun = R.size();
	unsigned int curRun = run[r1->last];
	while(delta < min_imp && curRun < maxRun) {
	  const class Run* rtmp = R[curRun];
	  if(!rtmp->late) {
	    for(unsigned int k = rtmp->first; k < rtmp->last; k++) {
	      const unsigned int locjob = seq[k];
	      const int t = release[k+1] + shift;
	      if(t > p->due(locjob)) {
		delta += p->weight(locjob);
	      }
	    }
	  }
	  curRun++;
	}
      }
      return delta;
    } else {
      return INT_MAX/2;
    }
  }
}

