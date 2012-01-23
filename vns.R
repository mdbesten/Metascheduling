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

# $Id: vns.R,v 1.12 2004/01/28 21:54:49 mldb Exp $ #
source("problem.R");
source("biRace.R");
rm(tf);
rm(rdd);

neighborhoods <- factor(c("", "b", "f", "s", "bf", "bs", "fs", "bsf"));
pivoting <- factor(c("B", "F"));
vns.design <- expand.grid(P = pivoting,
                      A = neighborhoods, B = neighborhoods, C = neighborhoods);
cand.sz <- nrow(vns.design);

vns.str <- function(descr) {
  return(paste(descr$P, descr$A,"|",  descr$B, "|", descr$C, sep=""));
}

vns.str <- function(idx) {
  attach(vns.design[idx,]);
  str <- paste(P, A, "|", B, "|", C, sep="");
  detach(vns.design[idx,]);
  return(str);
}


vns.params <- function(idx) {
  attach(vns.design[idx,]);
  prefix <- paste("-L", P, sep="");
  i <- ifelse(A=="", "", paste(prefix, A, sep=""));
  ii <- ifelse(B=="", "", paste(prefix, B, sep=""));
  iii <- ifelse(C=="", "", paste(prefix, C, sep=""));
  detach(vns.design[idx,]);
  return(paste(i, ii, iii, collapse=""));
}

vns.string <- function(candidate, env, obj, inst, seq) {
  problem <- paste(ifelse(env=="S", "-S", ifelse(env=="Fm", "-F", "-P")),
                   ifelse(obj=="U", "-U", "-T"));
  return(paste("cat", seq,
               ifelse(env=="Pm", paste("| ./s2p.out -i", inst), ""),
               "| ./vns.out", "-i", inst, problem,
               vns.params(candidate),
               "-v 2>/dev/null"));
}

vns.seq <- function(n) {
  seq <- tempfile("seq");
  cat(sample(n)-1, file=seq);
  return(seq);
}

vns.inst <- function(specs, tf=runif(1), rdd=runif(1)) {
  inst <- tempfile("inst");
  env <- specs$env;
  n <- specs$n;
  m <- specs$m;
  wght <- ifelse(specs$weight, "unif", "common");
  if(env=="Fm") {
    random = round(runif(1, 1, 10));
    instance <- read.instance.taillard(number = random+ifelse(n==50, 50, 80),
                                       TF = tf, RDD = rdd,
                                       wdis = wght,
                                       dir="../dat/");
    write.instance.Fm(instance, inst);
  } else {
    write.instance(getInstance(njobs=n, TF=tf, RDD=rdd,
                               wdis= wght,
                               mProcs=m),
                   inst);
  }
  return(inst);
}

vns.race <- function(id, test.sz,
                     bootstrap=NULL, first.test=5) {
  experiment <- cbind(expand.grid(env=c("Fm", "Fm", "Pm", "Pm", "Pm", "S"),
                           obj=c("T", "U"), weight=c(T, F)),
                      n=rep(c(50, rep(100, 5)), 4),
                      m=rep(c(20, 20, 2, 3, 4, 1), 4));


  vns.data <- lapply(1:test.sz,
                     function(i) {
                       tf <- runif(1);
                       rdd <- runif(1);
                       problem <- experiment[sample(1:nrow(experiment), 1),];
                       return(vns.init(specs=problem, tf=tf, rdd=rdd));
                     });

  boot.results <- matrix(NA, 0, 0);
  boot.times <- matrix(NA, 0, 0);
  if(!is.null(bootstrap)) {
    boot.results <- bootstrap$results[apply(!is.na(bootstrap$results),
                                            1, sum)==cand.sz,];
    boot.times <- bootstrap$times[apply(!is.na(bootstrap$times),
                                        1, sum)==cand.sz,];
    stopifnot(nrow(boot.times) == nrow(boot.results));
  }
  
  vns.race.wrapper <- function(candidate=NULL, testCase=NULL) {
    
    if(is.null(candidate) && is.null(testCase)) {
      return(list(no.candidates=cand.sz,
                  no.testCases=test.sz,
                  program.name=id,
                  precis="Variable Neighborhood Search\n"));
    } else if(is.numeric(candidate)&&is.numeric(testCase)&&
              (candidate==as.integer(candidate))&&
              (testCase==as.integer(testCase))&&
              (length(candidate)==1)&&(length(testCase)==1)&&
              (candidate>=1)&&(testCase>=1)&&
              (candidate<=cand.sz)&&(testCase<=test.sz)) {

      if(nrow(boot.results) < testCase) {
        seq <- vns.data[[testCase]]$seq;
        inst <- vns.data[[testCase]]$inst;
        description <- vns.data[[testCase]]$specs;
       
        command <- vns.string(candidate, description$env, description$obj,
                              inst, seq);
        cat("(", candidate, testCase, ")",
            command, "\n", file="/tmp/whoisracing");
        output <- as.numeric(system(command, intern=T));
        stopifnot(length(output)==2);

        return(list(output=output[1], runtime=output[2],
                    logList=list(seed=testCase, tasks=vns.data)));
      } else {
        return(list(output=boot.results[testCase,candidate],
                    runtime=boot.times[testCase,candidate],
                    logList=list(seed=testCase, tasks=vns.data)));
      }
    } else {
      stop("Something wrong in wrapper args");
    }
  }

  race.out <- pareto.race(vns.race.wrapper, cand.sz*10,
                          log.file=paste(id, "log", sep="."),
                          first.test=first.test, adjust="none");
  save(race.out, vns.data, file=paste(id, "out", sep="."), compress=T);
  
  lapply(vns.data,
         function(X) {
           unlink(X$seq);
           unlink(X$inst);
         });

  return(which(race.out$alive));
}

vns.test <- function(wrapper, ...) {
  par <- wrapper();
  candidate <- sample(par$no.candidates, 1);
  print(vns.str(candidate));
  print(wrapper(candidate, sample(100, 1)));
}

vns.batch <- function(subset=1:nrow(experiment)) {
  experiment <- cbind(expand.grid(env=c("Fm", "Fm", "Pm", "Pm", "Pm", "S"),
                           obj=c("T", "U"), weight=c(T, F)),
                      n=rep(c(50, rep(100, 5)), 4),
                      m=rep(c(20, 20, 2, 3, 4, 1), 4));
  for(i in subset) {
    prefix <- paste("vns", i, sep=".");
    boot <- NULL;
    boot.file <- paste(prefix, "bootstrap", sep=".");
    if(file.exists(boot.file)) {
      load(boot.file);
      boot <- log;
    }
    
    selection <- vns.race(experiment[i,], id = prefix, test.sz = 100,
                          bootstrap=boot);
    cat(selection, "\n", file=paste(prefix, "alive", sep="."));
  }
  
}

vns.init <- function(seed = round(runif(1, 1, 1000)), specs,
                     tf = runif(1), rdd = runif(1)) {
  set.seed(seed);
  seq <- vns.seq(specs$n);
  inst <- vns.inst(specs, tf, rdd);
  
  return(list(seq=seq, inst=inst, tf=tf, rdd=rdd, specs=specs));
}

vns.retry <- function(idx=15, exp.scale=10, ...) {
  file.name <- paste("vns", idx, "out", sep=".");
  stopifnot(idx > 0, file.exists(file.name));
  load(file.name);

  test.sz <- race.out$no.testCases;
  cand.sz <- race.out$no.alive;

  test.order <- sample(1:test.sz, test.sz);
  N <- race.out$results[test.order,race.out$alive];
  M <- race.out$times[test.order,race.out$alive];
  rm(race.out);
  
  vns.race.wrapper <- function(candidate=NULL, testCase=NULL) {
    if(is.null(candidate) && is.null(testCase)) {
      return(list(no.candidates=cand.sz,
                  no.testCases=test.sz,
                  program.name=paste(file.name, "retry", sep="."),
                  precis="Variable Neighborhood Search\n"));
    } else if(is.numeric(candidate)&&is.numeric(testCase)&&
              (candidate==as.integer(candidate))&&
              (testCase==as.integer(testCase))&&
              (length(candidate)==1)&&(length(testCase)==1)&&
              (candidate>=1)&&(testCase>=1)&&
              (candidate<=cand.sz)&&(testCase<=test.sz)) {

      return(list(output=N[testCase,candidate],
                  runtime=M[testCase,candidate],
                  logList=list(seed=testCase)));
    } else {
      stop("Something wrong in wrapper args");
    }
  }

  return(pareto.race(vns.race.wrapper, cand.sz*exp.scale, ...));
  
}

vns.restart <- function(idx=15, test.sz=100, exp.scale=10, ...) {
  experiment <- cbind(expand.grid(env=c("Fm", "Fm", "Pm", "Pm", "Pm", "S"),
                                  obj=c("T", "U"), weight=c(T, F)),
                      n=rep(c(50, rep(100, 5)), 4),
                      m=rep(c(20, 20, 2, 3, 4, 1), 4));
  file.name <- paste("vns", idx, "out", sep=".");
  stopifnot(idx > 0, idx < nrow(experiment), file.exists(file.name));
  load(file.name);
  cand.sz <- race.out$no.alive;
  description <- experiment[idx,];
  vns.data <- lapply((1:test.sz)+race.out$no.testCases, vns.init, description);
  cands <- which(race.out$alive);
  rm(race.out);

  vns.race.wrapper <- function(candidate=NULL, testCase=NULL) {
    
    if(is.null(candidate) && is.null(testCase)) {
      return(list(no.candidates=cand.sz,
                  no.testCases=test.sz,
                  program.name=paste(file.name, "restart", sep="."),
                  precis="Variable Neighborhood Search\n"));
    } else if(is.numeric(candidate)&&is.numeric(testCase)&&
              (candidate==as.integer(candidate))&&
              (testCase==as.integer(testCase))&&
              (length(candidate)==1)&&(length(testCase)==1)&&
              (candidate>=1)&&(testCase>=1)&&
              (candidate<=cand.sz)&&(testCase<=test.sz)) {
      seq <- vns.data[[testCase]]$seq;
      inst <- vns.data[[testCase]]$inst;
      
      command <- vns.string(cands[candidate], description$env, description$obj,
                            inst, seq);
      cat("(", candidate, testCase, ")",
          command, "\n", file="/tmp/whoisracing");
      output <- as.numeric(system(command, intern=T));
      stopifnot(length(output)==2);
      
      return(list(output=output[1], runtime=output[2],
                  logList=list(seed=testCase)));
    } else {
      stop("Something wrong in wrapper args");
    }
  }

  race.out <- pareto.race(vns.race.wrapper, cand.sz*exp.scale, ...);
  lapply(vns.data,
         function(X) {
           unlink(X$seq);
           unlink(X$inst);
         });
  return(race.out);
}

vns.proceed <- function(idx=15, exp.scale=1000, test.sz=100, ...) {
  description <- cbind(expand.grid(env=c("Fm", "Fm", "Pm", "Pm", "Pm", "S"),
                                   obj=c("T", "U"), weight=c(T, F)),
                       n=rep(c(50, rep(100, 5)), 4),
                       m=rep(c(20, 20, 2, 3, 4, 1), 4))[idx,];
  file.name <- paste("vns", idx, "out", sep=".");
  stopifnot(file.exists(file.name));
  load(file.name);
  old.sz <- race.out$no.testCases;
  if(old.sz >= test.sz) {
    warning(paste("test.sz", test.sz,
                  "does not exceed the number of testcases", old.sz,
                  "that have been tested already"));
    return(F);
  }
  cand.sz <- race.out$no.alive;
  vns.data <- lapply((old.sz+1):test.sz, vns.init, description);
  cands <- which(race.out$alive);
  N <- race.out$results[, race.out$alive];
  M <- race.out$times[, race.out$alive];
  rm(race.out);
  
  vns.race.wrapper <- function(candidate=NULL, testCase=NULL) {
    if(is.null(candidate) && is.null(testCase)) {
      return(list(no.candidates=cand.sz,
                  no.testCases=test.sz,
                  program.name=paste(file.name, "retry", sep="."),
                  precis="Variable Neighborhood Search\n"));
    } else if(is.numeric(candidate)&&is.numeric(testCase)&&
              (candidate==as.integer(candidate))&&
              (testCase==as.integer(testCase))&&
              (length(candidate)==1)&&(length(testCase)==1)&&
              (candidate>=1)&&(testCase>=1)&&
              (candidate<=cand.sz)&&(testCase<=test.sz)) {
      if(testCase <= old.sz) {
        return(list(output=N[testCase,candidate],
                    runtime=M[testCase,candidate],
                    logList=list(seed=testCase)));
      } else {
        seq <- vns.data[[testCase-old.sz]]$seq;
        inst <- vns.data[[testCase-old.sz]]$inst;
        
        command <- vns.string(cands[candidate],
                              description$env, description$obj,
                              inst, seq);
        cat("(", candidate, testCase, ")",
            command, "\n", file="/tmp/whoisracing");
        output <- as.numeric(system(command, intern=T));
        stopifnot(length(output)==2);
        
        return(list(output=output[1], runtime=output[2],
                    logList=list(seed=testCase)));
      }
    } else {
      stop("Something wrong in wrapper args");
    }
  }

  race.out <- pareto.race(vns.race.wrapper, first.test=old.sz,
                          cand.sz*old.sz+cand.sz*exp.scale, ...);
  lapply(vns.data,
         function(X) {
           unlink(X$seq);
           unlink(X$inst);
         });

  names(race.out$alive) <- cands;
  colnames(race.out$results) <- colnames(race.out$times) <- cands;
  colnames(race.out$dominance) <- rownames(race.out$dominance) <- cands;

  cat(names(which(race.out$alive)), "\n",
      file=paste("vns", idx, "contd", "alive", sep="."));
  return(race.out);
}

vns.complete <- function(subset=1:24, ...) {
  for(i in subset) {
    race.out <- vns.proceed(i, ...);
    if(is.list(race.out))
      save(race.out, file=paste("vns", i, "contd"));
  }
}
