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

# ---------------------------------------- -*- mode: r; mode: font-lock -*- #
# $Id: rtd.race.wrapper.R,v 1.2 2003/12/31 22:26:34 mldb Exp $ #				

race.init <- function() {
  candidates <- expand.grid(limit=60,
                            bound=seq(7777, 9999, 500),
                            sq.scale=seq(77, 99, 5),
                            rt.scale=6:10);
  
  return(list(no.candidates=nrow(candidates),
              no.tasks=100,
              log.file="rtd.race.log",
              candidates=candidates));
}

race.info <- function(data) {
  return(list(race.name="rtd.sh",
              no.candidates=data$no.candidates,
              no.tasks=data$no.tasks,
              extra=paste(system("cat rtd.sh", intern=T), collapse="\n")));
}

race.wrapper <- function(candidate, task, data) {
  stopifnot(candidate > 0, candidate <= data$no.candidates);
  
  argv <- data$candidates[candidate,];
  
  rtd <- as.numeric(system(paste("./rtd.sh",
                                 argv$limit, argv$bound,
                                 argv$sq.scale, argv$rt.scale,
                                 "| tr ' ' '\n'"),
                           intern=TRUE));
  sz <- length(rtd);
  
  if(sz > 1) {
    runtimes <- rtd[seq(1, sz, 2)];
    solquals <- rtd[seq(2, sz, 2)];
  } else {
    runtimes <- argv$limit;
    solquals <- argv$bound;
  }

  if(data$log.file!="") {
    cat(runtimes, "\n", append=T, file=data$log.file);
    cat(solquals, "\n", append=T, file=data$log.file);
  }
  
  return(list(time=runtimes, quality=solquals));
}

race.describe <- function(candidate, data) {
  return(data$candidates[candidate,]);
}


