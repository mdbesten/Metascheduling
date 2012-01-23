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

#ifndef LOCALSEARCH_H
#define LOCALSEARCH_H
#include "Solution.h"
class Localsearch {
private:
  class Move {
  public:
    unsigned int i, j;
    char mv;
    int val;
    
    Move() : i(0), j(0), mv('x'), val(0) {
    }
    Move(const unsigned int& k, const unsigned int& l, const char& m, 
	 const int& improvement) : i(k), j(l), mv(m), val(improvement) {
    }
    void set(const unsigned int& k, const unsigned int& l, const char& m, 
	     const int& improvement) {
      i = k;
      j = l;
      mv = m;
      val = improvement;
    }
  };

  const class Move bestImprovement(class Solution* sol) const;
  const class Move firstImprovement(class Solution* sol) const;

public:
  const bool bwd;
  const bool fwd;
  const bool swp;

  const bool bestImpr;

  Localsearch(const bool& bFlag = false, const bool& fFlag = false, 
	      const bool& sFlag = false, const bool& PFlag = false) :
    bwd(bFlag), fwd(fFlag), swp(sFlag), bestImpr(PFlag) {
  }
  
  void search(class Solution* sol) const;

  void print(ostream& out = cout) const {
    out << (bestImpr ? "best" : "first") << " improvement";
    out << (bwd ? " bwd" : "") << (fwd ? " fwd" : "") << (swp ? " swp" : "");
    out << " local search" << endl;
  }
};

#endif
