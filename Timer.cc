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

#include "Timer.h"
#include <stdlib.h> // for NULL define

/*
 *  The virtual time of day and the real time of day are calculated and
 *  stored for future use.  The future use consists of subtracting these
 *  values from similar values obtained at a later time to allow the user
 *  to get the amount of time used by the algorithm.
 */
Timer::Timer(void) {
  getrusage( RUSAGE_SELF, &res );
  virtual_time = (double) res.ru_utime.tv_sec +
    (double) res.ru_stime.tv_sec +
    (double) res.ru_utime.tv_usec * 1.0E-6 +
    (double) res.ru_stime.tv_usec * 1.0E-6;
  gettimeofday( &tp, NULL );
  real_time =    (double) tp.tv_sec + (double) tp.tv_usec * 1.0E-6;
}

/*
 *  Stop the stopwatch and return the time used in seconds (either
 *  REAL or VIRTUAL time, depending on ``type'').
 */
double Timer::elapsed_time(const TYPE& type = VIRTUAL) {
  if (type == REAL) {
    gettimeofday( &tp, NULL );
    return( (double) tp.tv_sec + (double) tp.tv_usec * 1.0E-6 - real_time );
  }
  else {
    getrusage( RUSAGE_SELF, &res );
    return( (double) res.ru_utime.tv_sec +
	    (double) res.ru_stime.tv_sec +
	    (double) res.ru_utime.tv_usec * 1.0E-6 +
	    (double) res.ru_stime.tv_usec * 1.0E-6
	    - virtual_time );
  }
}
