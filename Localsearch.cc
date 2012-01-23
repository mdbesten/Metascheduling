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

#include "Localsearch.h"
#define ERR(X) cerr << #X << " == " << X << "\t" << flush;

void
Localsearch::search(class Solution* sol) const {
  if(bwd || fwd || swp) {
    bool improvement = false;
    do {
#ifndef NDEBUG
      const int oud = sol->eval();
#endif
      const class Move best = (bestImpr ? bestImprovement(sol):
			       firstImprovement(sol));
      if(best.val < 0) {
	switch(best.mv) {
	case 'b':
	  sol->doBwd(best.i, best.j);
	  break;
	case 'f':
	  sol->doFwd(best.i, best.j);
	  break;
	case 's':
	  sol->doSwp(best.i, best.j);
	  break;
	}
	improvement = true;
      } else {
	improvement = false;
      }
#ifndef NDEBUG
      PR(best.val);
      PR(sol->eval());
      assert(oud + best.val == sol->eval());
#endif      
    } while(improvement);
  }
}

const Localsearch::Move 
Localsearch::bestImprovement(class Solution* sol) const {
  unsigned int n = sol->size();
  Move best(0, 0, 'x', 0);
  for(unsigned int i = 0; i < n-1; i++) {
    for(unsigned int j = i+1; j < n; j++) {
      int tmp = 0;
      if(bwd) {
	tmp = sol->whatifBwd(i, j, best.val);
	if(tmp < best.val) {
	  best.set(i, j , 'b', tmp);
	}
      }
      if(fwd) {
	tmp = sol->whatifFwd(i, j, best.val);
	if(tmp < best.val) {
	  best.set(i, j, 'f', tmp);
	}
      }
      if(swp) {
	tmp = sol->whatifSwp(i, j, best.val);
	if(tmp < best.val) {
	  best.set(i, j, 's', tmp);
	}
      }
    }
  }
  return best;
}

const Localsearch::Move 
Localsearch::firstImprovement(class Solution* sol) const {
  unsigned int n = sol->size();
  Move best(0, 0, 'x', 0);
  for(unsigned int i = 0; i < n-1; i++) {
    for(unsigned int j = i+1; j < n; j++) {
      int tmp = 0;
      if(bwd) {
	tmp = sol->whatifBwd(i, j, best.val);
	if(tmp < best.val) {
	  best.set(i, j , 'b', tmp);
	  return best;
	}
      }
      if(fwd) {
	tmp = sol->whatifFwd(i, j, best.val);
	if(tmp < best.val) {
	  best.set(i, j, 'f', tmp);
	  return best;
	}
      }
      if(swp) {
	tmp = sol->whatifSwp(i, j, best.val);
	if(tmp < best.val) {
	  best.set(i, j, 's', tmp);
	  return best;
	}
      }
    }
  }
  return best;
}
