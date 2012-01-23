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

#ifndef PTEMPLATE_H
#define PTEMPLATE_H
#include "Run.h"

template<class S>
class Parallel : public Solution {
protected:
  class Move {
  public:
    unsigned int machR;
    unsigned int machC;
    unsigned int row;
    unsigned int col;

    Move() : machR(0), machC(0), row(0), col(1) {
    }
    Move(const class Move* mv) : 
      machR(mv->machR), machC(mv->machC), row(mv->row), col(mv->col) {
    }
    Move(const unsigned int& processor, 
	 const unsigned int& pos1, const unsigned int& pos2) :
      machR(processor), machC(processor), row(pos1), col(pos2) {
      assert(row < col);
    }
    Move(const unsigned int& m1, const unsigned int& p1,
	 const unsigned int& m2, const unsigned int& p2) :
      machR(m1), machC(m2), row(p1), col(p2) {
      assert(m1 < m2);
      assert(p1 < p2);
    }
  
    bool intern() const {
      return machR == machC;
    }

  };

  class Block {
  public:
    const unsigned int machR;
    const unsigned int szR;
    const unsigned int machC;
    const unsigned int szC;
    const bool intern;
  private:
    unsigned int mv_sz;
    class Move** moves;

    void setSizeInt() {
      mv_sz = 0;
      for(unsigned int i = 0; i < szR; i++) {
	mv_sz += i;
      } 
    }      
    void setSizeExt() {
      mv_sz = szR * szC;
    }
    void fill() {
      if(mv_sz > 0) {
	int cnt = 0;
	class Move cur;
	first(&cur);
	do {
	  assert(cnt < (int) mv_sz);
	  moves[cnt] = new Move(&cur);
	  cnt++;
	} while(next(&cur));
      }
    }
  public:
    Block(const unsigned int& m, const unsigned int& sz) :
      machR(m), szR(sz), machC(m), szC(sz), intern(true) {
      setSizeInt();
      moves = new Move*[mv_sz];
      fill();
    }
    Block(const unsigned int& m1, const unsigned int& s1,
	  const unsigned int& m2, const unsigned int& s2) :
      machR(m1), szR(s1), machC(m2), szC(s2), intern(false) {
      assert(machR < machC);
      setSizeExt();
      moves = new Move*[mv_sz];
      fill();
    }
    Block(const class Block* src) :
      machR(src->machR), szR(src->szR), machC(src->machC), szC(src->szC),
      intern(src->intern), mv_sz(src->mv_sz) {
//        if(intern)
//  	setSizeInt();
//        else
//  	setSizeExt();
      moves = new Move*[mv_sz];
      copy(src->moves, src->moves+mv_sz, moves);
      //fill();
    }
    ~Block() {
      for(unsigned int i = 0; i < mv_sz; i++) {
	delete moves[i];
      }
      delete []moves;
    }

    unsigned int size() const {
      return mv_sz;
    }

    void first(Move* mv) const {
      mv->machR = machR;
      mv->machC = machC;
      mv->row = 0;
      mv->col = intern ? 1 : 0;
    }

    bool next(Move* mv) const {
      assert(mv->machR == machR);
      assert(mv->machC == machC);
      if(mv->col < szC - 1) {
	mv->col++;
	return true;
      } else {
	if((int) mv->row < (int) szR - (intern ? 2 : 1)) {
	  mv->row++;
	  mv->col = intern ? mv->row + 1 : 0;
	  return true;
	} else {
	  return false;
	}
      }
    }
  
    const Move* which(const unsigned int& idx) const {
      assert(idx < mv_sz);
      return moves[idx];
    }
    
    const bool is_square() const {
      return intern;
    }
  };

protected:
  S ** ssols;
  const unsigned int m;
  
  unsigned int ** mapping;
  unsigned int mv_total;
  unsigned int b_sz;
  class Block ** blocks;
  unsigned int * release;
  unsigned int * procmap;

  int evaluate(const unsigned int sequence[], const unsigned int& size) const {
    cerr << "Warning: Depricated" << endl;
    return 0;
  }


  const Move* getMove(const unsigned int& k) const {
     // which block
    const unsigned int b_nr = procmap[k];
    assert(b_nr == (unsigned) (find_if(release, release+b_sz+1, 
			   bind2nd(greater<unsigned int>(), k)) 
		    - release) -1);
    // which move
    return blocks[b_nr]->which(k-release[b_nr]);
  }

  void update(const unsigned int& rmv, const unsigned int& add) {
    assert(rmv < m);
    assert(add < m);
    // update blocks
    class Block * b_rmv = blocks[rmv];
    class Block * b_add = blocks[add];

    assert(b_rmv->is_square());
    assert(b_add->is_square());
    const unsigned int rmv_sz = b_rmv->szR-1;
    const unsigned int add_sz = b_add->szR+1;

    delete b_rmv;
    delete b_add;

    b_rmv = new class Block(rmv, rmv_sz);
    b_add = new class Block(add, add_sz);
    
    blocks[rmv] = b_rmv;
    blocks[add] = b_add; // ???!!!!!!!!!!!????

    unsigned int last_changed = max(rmv, add);
    for(unsigned int i = m; i < b_sz; i++) {
      class Block * b_tmp = blocks[i];
      assert(!b_tmp->is_square());
      if(b_tmp->machR == rmv) {
	if(b_tmp->machC == add) {
	  delete b_tmp;
	  b_tmp = new class Block(rmv, rmv_sz, add, add_sz);
	  last_changed = i;
	} else {
	  const unsigned int col = b_tmp->machC;
	  const unsigned int col_sz = b_tmp->szC;
	  delete b_tmp;
	  b_tmp = new class Block(rmv, rmv_sz, col, col_sz);
	  last_changed = i;
	}
      } else if(b_tmp->machR == add) {
	if(b_tmp->machC == rmv) {
	  delete b_tmp;
	  b_tmp = new class Block(add, add_sz, rmv, rmv_sz);
	  last_changed = i;
	} else {
	  const unsigned int col = b_tmp->machC;
	  const unsigned int col_sz = b_tmp->szC;
	  delete b_tmp;
	  b_tmp = new class Block(add, add_sz, col, col_sz);
	  last_changed = i;
	} 
      } else {
	if(b_tmp->machC == rmv) {
	  const unsigned int row = b_tmp->machR;
	  const unsigned int row_sz = b_tmp->szR;
	  delete b_tmp;
	  b_tmp = new class Block(row, row_sz, rmv, rmv_sz);
	  last_changed = i;
	} else if(b_tmp->machC == add) {
	  const unsigned int row = b_tmp->machR;
	  const unsigned int row_sz = b_tmp->szR;
	  delete b_tmp;
	  b_tmp = new class Block(row, row_sz, add, add_sz);
	  last_changed = i;
	}
      }
      blocks[i] = b_tmp; // ?!!!!?????
    }

    // update release dates of blocks
//      for(unsigned int i = 0; i < b_sz; i++) {
//        release[i+1] = release[i] + blocks[i]->size();
//        fill(procmap+release[i], procmap+release[i+1], i);
//      }

    unsigned int old_release[b_sz+1];
    const unsigned int first_changed = min(rmv, add);
    copy(release+first_changed, release+min(last_changed+2, b_sz+1), 
	 old_release+first_changed);
    for(unsigned int i = first_changed; i <= last_changed; i++) {
      release[i+1] = release[i] + blocks[i]->size();
      //      fill(procmap+release[i], procmap+release[i+1], i);
    }

    assert(release[first_changed] == old_release[first_changed]);
    assert(release[last_changed+1] == old_release[last_changed+1]);
    // update procmap
    for(unsigned int i = first_changed; i <= last_changed; i++) {
      const unsigned int ol = old_release[i];
      const unsigned int ou = old_release[i+1];
      const unsigned int nl = release[i];
      const unsigned int nu = release[i+1];
      if(ol <= nl) {
	if(ou < nu) {
	  if(ou > nl) { // (ol <= nl < ou < nu)
	    fill(procmap+ou, procmap+nu, i);
	  } else { // (ol < ou <= nl < nu)
	    fill(procmap+nl, procmap+nu, i);
	  } 
	} else { // ol <= nl < nu <= ou
	}
      } else {
	if(ol < nu) { // (nl < ol < nu)
	  fill(procmap+nl, procmap+ol, i);
	  if(ou < nu) { // (nl < ol < ou < nu)
	    fill(procmap+ou, procmap+nu, i);
	  }
	} else { // (nl < nu < ol < ou)
	  fill(procmap+nl, procmap+nu, i);
	}
      }
    }
  }

public:
  Parallel(unsigned int seq[], const unsigned int& sz,
	   const class Problem* p, const char& o = 'T') :
    Solution(seq, sz, p, o), m(p->mprocs()) {

    // split into subproblems
    ssols = new S*[m];
    unsigned int matrix[m][sz];
    unsigned int szs[m];
    fill(szs, szs+m, 0);
#ifndef NDEBUG
    bool assigned[p->njobs()];
    fill(assigned, assigned+p->njobs(), false);
#endif
    for(unsigned int i = 0; i < sz; i++) {
      unsigned int processor = seq[i]/p->njobs(); // floor, i.e. not round
      unsigned int job = seq[i] % p->njobs();
#ifndef NDEBUG
      assert(!assigned[job]);
      assigned[job] = true;
#endif
      matrix[processor][szs[processor]] = job;
      szs[processor]++;
    }
    for(unsigned int i = 0; i < m; i++) {
      ssols[i] = new S(matrix[i], szs[i], p, o);
    }

    curVal = 0;
    for(unsigned int i = 0; i < m; i++) {
      curVal += ssols[i]->eval();
    }

    // map indices i, j in parallel problem to indices in subproblems
    mapping = new unsigned int*[sz];
    for(unsigned int i = 0; i < sz; i++) 
      mapping[i] = new unsigned int[sz];
    for(unsigned int i = 0, k = 0; i < sz-1; i++) {
      for(unsigned int j = i+1; j < sz; j++, k++) {
	mapping[i][j] = k;
      }
    }

    mv_total = 0;
    for(unsigned int i = 1; i < sz; i++) {
      mv_total += i;
    }
    b_sz = 0;
    for(unsigned int i = 1; i < m + 1; i++) {
      b_sz += i;
    }

    blocks = new class Block*[b_sz];
    for(unsigned int i = 0; i < m; i++) {
      blocks[i] = new class Block(i, szs[i]);
    }
    for(unsigned int i = 0, k = m; i < m; i++) {
      for(unsigned int j = i+1; j < m; j++, k++) {
	blocks[k] = new class Block(i, szs[i], j, szs[j]);
      }
    }

    release = new unsigned int[b_sz+1];
    procmap = new unsigned int[mv_total];
    release[0] = 0;
    for(unsigned int i = 0; i < b_sz; i++) {
      release[i+1] = release[i] + blocks[i]->size();
      fill(procmap+release[i], procmap+release[i+1], i);
    }

  }


  ~Parallel() {
    for(unsigned int i = 0; i < m; i++) {
      delete ssols[i];
    }
    delete []ssols;

    for(unsigned int i = 0; i < sz; i++) {
      delete []mapping[i];
    }
    delete []mapping;
    for(unsigned int i = 0; i < b_sz; i++) {
      delete blocks[i];
    }
    delete []blocks;
    delete []release;
    delete []procmap;
  }


  void print(ostream& out = cout) const {
    for(unsigned int i = 0; i < m; i++) {
      for(unsigned int j = 0; j < ssols[i]->size(); j++) {
	out << ssols[i]->which(j) + i*p->njobs() << " ";
      }
    }
    out << endl;
  }

  void setSeq(unsigned int target[]) const {
    for(unsigned int i = 0, k = 0; i < m; i++) {
      for(unsigned int j = 0; j < ssols[i]->size(); j++) {
	target[k++] = ssols[i]->which(j) + i*p->njobs();
      }
    }
  }

  unsigned int which(const unsigned int& pos) const {
    cerr << "Warning: Not implemented" << endl;
    return INT_MAX;
  }

  int whatifBwd(const unsigned int& i, const unsigned int& j,
		const int& min) const { 
    const class Move* mv = getMove(mapping[i][j]);
    if(mv->intern()) {
      return ssols[mv->machR]->whatifBwd(mv->row, mv->col, min);
    } else {
      const int deltaR = ssols[mv->machR]->whatifRemove(mv->row, 0);
      if(min - deltaR >= 0) {
	const int deltaC = 
	  ssols[mv->machC]->whatifAdd(ssols[mv->machR]->which(mv->row), 
				      mv->col, min - deltaR);
	return deltaR + deltaC;
      } else {
	return INT_MAX;
      }
    }
  }
  
  int whatifFwd(const unsigned int& i, const unsigned int& j,
		const int& min) const { 
    const class Move* mv = getMove(mapping[i][j]);
    if(mv->intern()) {
      return ssols[mv->machR]->whatifFwd(mv->row, mv->col, min);
    } else {
      const int deltaC = ssols[mv->machC]->whatifRemove(mv->col);
      if(min - deltaC >= 0) {
	const int deltaR = 
	  ssols[mv->machR]->whatifAdd(ssols[mv->machC]->which(mv->col), 
				      mv->row, min - deltaC);
	return deltaR + deltaC;
      } else {
	return INT_MAX;
      }
    }
  }

  int whatifSwp(const unsigned int& i, const unsigned int& j,
		const int& min) const { 
    const class Move* mv = getMove(mapping[i][j]);
    if(mv->intern()) {
      return ssols[mv->machR]->whatifSwp(mv->row, mv->col, min);
    } else {
      const int deltaR = 
	ssols[mv->machR]->whatifReplace(mv->row, 
					ssols[mv->machC]->which(mv->col));
      const int deltaC =
	ssols[mv->machC]->whatifReplace(mv->col, 
					ssols[mv->machR]->which(mv->row),
					min - deltaR);
      return deltaR + deltaC;
    }
  }
  
  int whatifAdd(const unsigned int& job, 
		const unsigned int& pos) const {
    cerr << "Warning: whatifAdd/2 is not implemented" << endl;
    return 0;
  }
  
  void doBwd(const unsigned int& i, const unsigned int& j) {
    const class Move* mv = getMove(mapping[i][j]);
    const unsigned int mC = mv->machC;
    const unsigned int mR = mv->machR;
    if(mv->intern()) {
      const int prev = ssols[mR]->eval();
      ssols[mR]->doBwd(mv->row, mv->col);
      curVal += ssols[mR]->eval() - prev;
    } else {
      const int prev = ssols[mR]->eval() + ssols[mC]->eval();
      ssols[mC]->doAdd(ssols[mR]->which(mv->row), mv->col);
      ssols[mR]->doRemove(mv->row);
      update(mR, mC); // invalidates mv
      curVal += (ssols[mR]->eval() + ssols[mC]->eval()) - prev;
    }
  }
  
  void doFwd(const unsigned int& i, const unsigned int& j) {
    const class Move* mv = getMove(mapping[i][j]);
    const unsigned int mC = mv->machC;
    const unsigned int mR = mv->machR;
    if(mv->intern()) {
      const int prev = ssols[mR]->eval();
      ssols[mR]->doFwd(mv->row, mv->col);
      curVal += ssols[mR]->eval() - prev;
    } else {
      const int prev = ssols[mR]->eval() + ssols[mC]->eval();
      ssols[mR]->doAdd(ssols[mC]->which(mv->col), mv->row);
      ssols[mC]->doRemove(mv->col);
      update(mC, mR);
      curVal += (ssols[mR]->eval() + ssols[mC]->eval()) - prev;
    }
#ifndef NDEBUG
    int val = 0;
    for(unsigned int i = 0; i < m; i++) {
      val += ssols[i]->eval();
    }
    if(val!=curVal) {
      PR(val);
      PR(curVal);
    }
#endif
  }


  void doSwp(const unsigned int& i, const unsigned int& j) {
    const class Move* mv = getMove(mapping[i][j]);
    const unsigned int mC = mv->machC;
    const unsigned int mR = mv->machR;
    if(mv->intern()) {
      const int prev = ssols[mR]->eval();
      ssols[mR]->doSwp(mv->row, mv->col);
      curVal += ssols[mR]->eval() - prev;
    } else {
      const int prev = ssols[mR]->eval() + ssols[mC]->eval();
      // doReplace invalidates which\1
      const unsigned int job1 = ssols[mC]->which(mv->col);
      const unsigned int job2 = ssols[mR]->which(mv->row);
      ssols[mR]->doReplace(mv->row, job1);
      ssols[mC]->doReplace(mv->col, job2);
      curVal += (ssols[mR]->eval() + ssols[mC]->eval()) - prev;
    }
  }

  void doAdd(const unsigned int& job, const unsigned int& pos) {
    cerr << "Warning: doAdd/2 is not implemented" << endl;
  }
 
};

typedef class Parallel<SSol> PSol;
typedef class Parallel<SRSol> PRSol;
#endif
