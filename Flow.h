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

#ifndef FLOW_H
#define FLOW_H
#include "Solution.h"

class Flow : public Solution {
protected:
  int ** completion;
  int * V; // cumulative objective function value
  void init();

  int evaluate(const unsigned int sequence[], const unsigned int& size) const;
  void update_cost(const unsigned int& first, const unsigned int& second);
  bool is_corrupted();
public:
  Flow(unsigned int seq[], const unsigned int& sz,
       const class Problem* p, const char& o = 'T');
  

  ~Flow();


  void gantt(ostream& out = cerr, const unsigned int& width = 80,
	     const bool& TeX = false,
	     const int& m = -1) const;
  
  int whatifBwd(const unsigned int& i, const unsigned int& j, 
		const int& min) const;
  int whatifFwd(const unsigned int& i, const unsigned int& j, 
		const int& min) const;
  int whatifSwp(const unsigned int& i, const unsigned int& j, 
		const int& min) const;

  void doBwd(const unsigned int& i, const unsigned int& j);
  void doFwd(const unsigned int& i, const unsigned int& j);
  void doSwp(const unsigned int & i, const unsigned int & j);

  void push_back(const unsigned int& job);
};
#endif
