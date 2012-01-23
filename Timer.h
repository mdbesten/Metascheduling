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

#ifndef TIMER_H
#define TIMER_H
#include <sys/time.h>
#include <sys/resource.h>

class Timer {
private:
  struct rusage res;
  struct timeval tp;
  double virtual_time, real_time;

public:
  enum TYPE {REAL, VIRTUAL};
  Timer(void);
  double elapsed_time(const TYPE& type = VIRTUAL);
  bool alarm(const int& limit) {
    return (elapsed_time() > limit);
  }
};
#endif
