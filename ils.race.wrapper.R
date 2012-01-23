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

# $Id: ils.race.wrapper.R,v 1.8 2004/02/18 16:48:40 mldb Exp $ #

race.init <- function(problem, candidates, 
                      task.fun, cand.fun, time.limit=60,
                      race.name="ils", ils.log="", no.testCases=100,
                      continue=T) {
  boot.record <- NULL;
  if(continue) {
    ## determine problem id
    scheduling.problems <- cbind(expand.grid(env=c("Fm", "Fm",
                                               "Pm", "Pm", "Pm", "S"),
                                             obj=c("T", "U"), weight=c(T, F)),
                                 n=rep(c(50, rep(100, 5)), 4),
                                 m=rep(c(20, 20, 2, 3, 4, 1), 4));
    p.id <- which.max(sapply(1:nrow(scheduling.problems),
                             function(i) {
                               sum(scheduling.problems[i,] %in% problem);
                             }));
    ## load data
    boot.file <- paste("e", p.id, "Rdata", sep=".");
    stopifnot(file.exists(boot.file));
    load(boot.file);
    boot.record <- race.out$results;
  } 

  the.tasks <- lapply(1:no.testCases, task.fun, problem);
  save(the.tasks,
       file=paste(race.name, "tasks", "Rdata", sep="."),
       compress=T);
  return(list(no.candidates=nrow(candidates),
              candidates=candidates,
              cand.fun=cand.fun,
              problem=problem,
              no.tasks=no.testCases,
              tasks=the.tasks,
              race.name=race.name,
              time.limit=time.limit,
              log.file=ils.log,
              boot.record=boot.record));
}

race.info <- function(data) {
  spec2str <- function(spec) {
    return(paste(spec$n, "job", spec$m, "machine",
                 "total",
                 ifelse(spec$weight, "weighted", ""),
                 ifelse(spec$obj=="T", "tardiness", "unit penalty"),
                 ifelse(spec$env=="Fm", "flowshop",
                        ifelse(spec$env=="Pm",
                               "parallel machine",
                               "single machine")),
                 "scheduling problem"));
  }
  hostspec <- function() {
    return(paste(system("hostname", intern=T), "(",
                 paste(system("grep bogo /proc/cpuinfo | awk '{print $NF}'",
                              intern=T), collapse=" & "),
                 "bogomips)"));
  }
  
  return(list(race.name=data$race.name,
              no.candidates=data$no.candidates,
              no.tasks=data$no.tasks,
              extra=paste("Iterated Local Search for the",
                spec2str(data$problem),
                "at", hostspec())));
}

race.wrapper <- function(candidate, task, data) {

  command <- data$cand.fun(data$candidates[candidate,], data$problem,
                           data$tasks[[task]]);
  
  cat("(", task, ",", candidate, ")", command, "\n",
      file=paste(tempdir(), "/", data$race.name, ".who", sep=""));
  
  rtd <- as.numeric(system(paste(command, "| tr '\t' '\n'"), intern=TRUE));
  
  sz <- length(rtd);
  
  if(sz > 1) {
    runtimes <- rtd[seq(1, sz, 2)];
    solquals <- rtd[seq(2, sz, 2)];
  } else {
    stop(paste(command, "failed to return results"));
  }
  
  if(data$log.file!="") {
    cat(runtimes, "\n", append=T, file=data$log.file);
    cat(solquals, "\n", append=T, file=data$log.file);
  }
  
  return(list(time=runtimes, quality=solquals));
}

race.describe <- function(candidate, data) {
  return(cbind(data$candidates[candidate,], data$problem));
}


race.cleanup <- function(data) {
  lapply(data$tasks,
         function(task) {
           unlink(task$seq);
           unlink(task$inst);
         });
}
