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

#ifndef SOLUTION_H
#define SOLUTION_H

#include "Problem.h"
#include <algorithm>

class Solution {
protected:
  const class Problem* p;
  const char o;

  unsigned int* seq;
  unsigned int sz;
  int curVal;

  virtual int evaluate(const unsigned int* sequence, const unsigned int& size)
    const = 0;

public:
  Solution(unsigned int seq[], const unsigned int& sz,
	   const class Problem* p, const char& o = 'T');

  virtual ~Solution();

  virtual void setSeq(unsigned int target[]) const {
    copy(seq, seq+sz, target);
  }

  const int eval() const {
    return curVal;
  }
  virtual const unsigned int size() const {
    return sz;
  }
  virtual void print(ostream& out = cout) const {
    copy(seq, seq+sz, ostream_iterator<int>(out, " "));
    out << endl;
  }

  virtual int whatifBwd(const unsigned int& i,
			const unsigned int& j,
			const int& min) const {
    unsigned int tmp[sz];
    copy(seq, seq+sz, tmp);
    rotate(tmp+i, tmp+i+1, tmp+j);
    
    return evaluate(tmp, sz) - curVal;
  }
  virtual int whatifFwd(const unsigned int& i,
			const unsigned int& j,
			const int& min) const {
    unsigned int tmp[sz];
    copy(seq, seq+sz, tmp);
    rotate(tmp+i, tmp+j, tmp+j+1);
    
    return evaluate(tmp, sz) - curVal;
  }
  virtual int whatifSwp(const unsigned int& i, 
			const unsigned int& j,
			const int& min) const {
    unsigned int tmpseq[sz];
    copy(seq, seq+sz, tmpseq);
    swap(tmpseq[i], tmpseq[j]);

    return evaluate(tmpseq, sz) - curVal;
  };

  virtual int whatifAdd(const unsigned int& job, 
			const unsigned int& pos,
			const int& min = INT_MAX) const {
    assert(pos <= sz);
    assert(job < p->njobs());
    assert(find(seq, seq+sz, job) == seq + sz);
    unsigned int tmpseq[sz+1];
    copy(seq, seq+pos, tmpseq);
    tmpseq[pos] = job;
    copy(seq+pos, seq+sz, tmpseq+pos+1);
    return evaluate(tmpseq, sz+1) - curVal;
  }
  
  virtual int whatifRemove(const unsigned int& pos,
			   const int& min = 0) const {
    assert(pos < sz);
    unsigned int tmpseq[sz-1];
    copy(seq, seq+pos, tmpseq);
    copy(seq+pos+1, seq+sz, tmpseq+pos);
    return evaluate(tmpseq, sz-1) - curVal;
  }
  
  virtual int whatifReplace(const unsigned int& pos, 
			    const unsigned int& job,
			    const int& min = INT_MAX) const {
    assert(job < p->njobs());
    assert(pos < sz);
    assert(find(seq, seq+sz, job) == seq+sz);
    unsigned int tmpseq[sz];
    copy(seq, seq+sz, tmpseq);
    tmpseq[pos] = job;
    return evaluate(tmpseq, sz) - curVal;
  }

  /*
    Return job at pos
  */
  virtual unsigned int which(const unsigned int& pos) const {
    assert(pos < sz);
    return seq[pos];
  }

  virtual void doBwd(const unsigned int& i, const unsigned int& j) {
    // TODO: change into rotate(, , seq+j+1) (mutatis mutandum)
    rotate(seq+i, seq+i+1, seq+j);
    curVal = evaluate(seq, sz);
  }
  virtual void doFwd(const unsigned int& i, const unsigned int& j) {
    rotate(seq+i, seq+j, seq+j+1);
    curVal = evaluate(seq, sz);
  }
  virtual void doSwp(const unsigned int& i, const unsigned int& j) {
    swap(seq[i], seq[j]);
    curVal = evaluate(seq, sz);
  }

  virtual void doAdd(const unsigned int& job, const unsigned int& pos) {
    assert(pos <= sz);
    assert(job < p->njobs());
    assert(find(seq, seq+sz, job) == seq+sz);

    unsigned int tmpseq[sz-pos];
    copy(seq+pos, seq+sz, tmpseq);
    seq[pos] = job;
    copy(tmpseq, tmpseq+sz-pos, seq+pos+1);
    sz++;
    curVal = evaluate(seq, sz);
  }
  
  virtual void doRemove(const unsigned int& pos) {
    assert(pos < sz);
    rotate(seq+pos, seq+pos+1, seq+sz);
    sz--;
    curVal = evaluate(seq, sz);
  }

  virtual void doReplace(const unsigned int& pos, const unsigned int& job) {
    assert(pos < sz);
    assert(job < p->njobs());
    assert(find(seq, seq+sz, job) == seq+sz);
    seq[pos] = job;
    curVal = evaluate(seq, sz);
  }
};

class SSol : public Solution {
protected:
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
  SSol(unsigned int seq[], const unsigned int& sz,
       const class Problem* p, const char& o = 'T') : 
    Solution(seq, sz, p, o) {
    curVal = evaluate(seq, sz);
  }

};


class FSol : public Solution {
protected:
  int evaluate(const unsigned int sequence[], const unsigned int& size) const;
public:
  FSol(unsigned int seq[], const unsigned int& sz,
       const class Problem* p, const char& o = 'T') : 
    Solution(seq, sz, p, o) {
    curVal = evaluate(seq, sz);
  }
 
};
#endif
