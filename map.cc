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
#include <functional>
#include <unistd.h>

#include "Problem.h"

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
       const unsigned int& pos1, const unsigned pos2) :
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
    int cnt = 0;
    class Move cur;
    first(&cur);
    do {
      moves[cnt++] = new Move(&cur);
    } while(next(&cur));
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
  ~Block() {
    for(unsigned int i = 0; i < mv_sz; i++) {
      delete moves[i];
    }
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
      if(mv->row < szR - (intern ? 2 : 1)) {
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

  void operator++() {
    
  }
};



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
  assert(ifs);

  Problem problem(ifs, 'P');
  const unsigned int n = problem.njobs();
  const unsigned int m = problem.mprocs();
  unsigned int seq[n];
  unsigned int sz = 0;
  while(cin >> seq[sz]) sz++;
  assert(sz <= problem.njobs());

  unsigned int matrix[m][sz];
  unsigned int szs[m];
  fill(szs, szs+m, 0);
  for(unsigned int i = 0; i < sz; i++) {
    unsigned int processor = seq[i]/n; // floor, i.e. not round
    unsigned int job = seq[i]%n;
    matrix[processor][szs[processor]] = job;
    szs[processor]++;
  }

  // print solution
  for(unsigned int i = 0; i < m; i++) {
    copy(matrix[i], matrix[i]+szs[i], ostream_iterator<int>(cerr, " "));
    cerr << endl;
  }
  
  // estimate of map size
  unsigned int mv_total = 0;
  for(unsigned int i = 1; i < sz; i++) {
    mv_total += i;
  }
  PR(mv_total);
  
  unsigned int b_sz = 0;
  for(unsigned int i = 1; i < m + 1; i++) {
    b_sz += i;
  }
  PR(b_sz);
  
  class Block* blocks[b_sz];
  for(unsigned int i = 0; i < m; i++) {
    blocks[i] = new Block(i, szs[i]);
  }
  for(unsigned int i = 0, k = m; i < m; i++) {
    for(unsigned int j = i+1; j < m; j++, k++) {
      blocks[k] = new Block(i, szs[i], j, szs[j]);
    }
  }
  
  unsigned int mapping[sz][sz];

  for(unsigned int i = 0, k = 0; i < sz-1; i++) {
    for(unsigned int j = i+1; j < sz; j++, k++) {
      mapping[i][j] = k;
    }
  }

  unsigned int release[b_sz+1];
  release[0] = 0;
  for(unsigned int i = 0; i < b_sz; i++) {
    release[i+1] = release[i] + blocks[i]->size();
    PR(release[i+1]);
  }

  for(unsigned int i = 0; i < sz - 1; i++) {
    for(unsigned int j = i+1; j < sz; j++) {
      const unsigned int k = mapping[i][j];
      // which block
      unsigned int b_nr =  
	find_if(release, release+b_sz, 
		bind2nd(greater<unsigned int>(), k)) - release - 1;
      // which move
      const class Move* mv = blocks[b_nr]->which(k-release[b_nr]);
      cout << mv->machR << " " << mv->row << " ";
      cout << mv->machC << " " << mv->col << endl;
    }
  }  
  pair<unsigned int, unsigned int> listing[mv_total];
  for(unsigned int k = 0, b =0; k < mv_total; k++) {
    if(k > release[b+1]) {
      b++;
    }
    listing[k].first = b;
    listing[k].second = k - release[b];
  }

  blocks[0]++;
  return 0;
}

