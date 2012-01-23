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



pareto.race <- function (Rwrapper,
                         maxExp,
                         log.file="",
                         adjust=p.adjust.methods,
                         interactive=TRUE,
                         first.test=2,
                         threshold=0.5,
                         conf.level=0.95,
                         watch=c(),
                         video=FALSE){

  domination.matrix <- function(X, Y) {
    stopifnot(length(X) == length(Y));
    return(t(sapply(1:length(X),
                    function(i) {
                      X <= X[i] & Y <= Y[i];
                    })));
  }
  
  dom.update <- function(N, X, Y) {
    return(N + ifelse(domination.matrix(t(X), t(Y)), 1, 0));
  }

  timestamp.start<-date();

  if (!missing(log.file))
    if (system(paste("touch",log.file),ignore.stderr=TRUE)!=0)
      stop(paste("I cannot create file ",log.file,sep=""))

  adjust<-match.arg(adjust);

  if (!missing(conf.level) &&
      (!is.numeric(conf.level) || length(conf.level) != 1 ||
       !is.finite(conf.level) || conf.level < 0 || conf.level > 1))
    stop("conf.level must be a single number between 0 and 1")


  
  if (!missing(maxExp)){
    if (!is.numeric(maxExp))
      stop("maxExp must be a single number larger than 0")
    maxExp<-as.integer(maxExp)
    if (!missing(maxExp) &&
        (length(maxExp) != 1 || !is.finite(maxExp) || maxExp < 1))
      stop("maxExp must be a single number larger than 0")
  }

  par<-Rwrapper();

  precis<-paste("The dominance based bi-objective racing procedure.\n",
                "Program: ",par$program.name,"\n",
                "Number of candidates: ",par$no.candidates,"\n",
                "Number of available test cases: ",par$no.testCases,"\n",
                "Max number of experiments: ",maxExp,"\n",
                "Statistical test: ", "binomial","\n",
                "\tCorrection: ", adjust, "\n",
                "Probability of domination: ", threshold, "\n",
                "Test cases before discarding: ",first.test,"\n",
                sep="");

  if (!is.null(par$precis))
    precis<-paste(precis,par$precis,sep="")

  if (interactive)
    cat(paste(precis,"\n"))

  TestCases<-1:par$no.testCases

  N<-matrix(data = 0, nrow = par$no.candidates, ncol = par$no.candidates);
  Q<-matrix(data = NA, nrow = par$no.testCases, ncol = par$no.candidates);
  R<-matrix(data = NA, nrow = par$no.testCases, ncol = par$no.candidates);

  
  alive<-array(TRUE,par$no.candidates);
  e<-0;

  b <- sample(1:par$no.candidates,1)

  no.testCases.sofar <- 0;


  for (i in 1:par$no.testCases) {
    if (e+sum(alive)>maxExp)
      break
    if (sum(alive)==1)
      break

    e<-e+sum(alive)


    for (cf in which(alive)){

      runResult<-Rwrapper(candidate=cf, testCase=TestCases[i])

      Q[i,cf]<-runResult$output;
      R[i,cf]<-runResult$runtime;
      
      timestamp.current<-date();

      if (log.file!=""){
        log<-list(precis=precis,
                  results=Q[1:i,],
                  times=R[1:i,],
                  dominance=N,
                  testCases=TestCases[1:i],
                  no.experiments=e,
                  no.testCase=i,
                  alive=alive)
        if (is.list(runResult$logList))
          log<-c(log,runResult$logList);
        log<-c(log,list(timestamp.start=timestamp.start,
                        timestamp.current=timestamp.current))
        save(log,file=log.file)
      }
    }

    N[alive,alive] <- dom.update(N[alive,alive], Q[i,alive], R[i,alive]);


    scores <- apply(N[alive,alive], 1, sum);

    b <- which(alive)[which.min(scores)];

    no.testCases.sofar <- i

    if(video) {
      x <- t(R[i,alive]);
      y <- t(Q[i,alive]);
      plot(x, y, type="n", main=paste("TestCase:", no.testCases.sofar));
      text(x, y, apply(N[alive,alive], 1, sum), col=2);
    }

    if (i>=first.test) {

      purge <- numeric(0);
      for(row in which(alive)) {
        p.vect <- numeric(0);

        for(col in setdiff(which(alive), row)) {
          p.vect <- c(p.vect, binom.test(N[row,col], no.testCases.sofar,
                                         p = threshold, alternative="greater",
                                         conf.level=conf.level)$p.value);
        }
        p.vals <- p.adjust(p.vect, adjust);

        if(min(p.vals) < (1 - conf.level)) {
          #cat(row, "\t", which.min(p.vals), "\n");
          purge <- c(purge, row);
        }
      }

      alive[purge] <- F;

      
    } else {
      if (interactive)
        cat(" ")

    }

    if (interactive) {
      cat(paste("TestCase:",formatC(i,width=5),
                "\tAlive:",formatC(sum(alive),width=5),
                "\tbest:",formatC(b,width=5),
                "\texperiments so far:",formatC(e,width=8)));

      watch<-watch[alive[watch]];
      if (length(watch)>0) {
        cat("\twatch: [");
        cat(watch,sep=",");
        if (sum(alive)>length(watch))
          cat(",...")
        cat("]")
      }
      cat("\n");
    }

    timestamp.current<-date()

    if (log.file!=""){
      log<-list(precis=precis,
                results=Q[1:i,],
                times=R[1:i,],
                dominance=N,
                testCases=TestCases[1:i],
                no.experiments=e,
                no.testCase=i,
                alive=alive)
      if (is.list(runResult$logList))
        log<-c(log,runResult$logList);
      log<-c(log,list(timestamp.start=timestamp.start,
                      timestamp.current=timestamp.current))
      save(log,file=log.file)

    }
  }


  mean.value<-mean(Q[1:no.testCases.sofar,b]);
  mean.time<-mean(R[1:no.testCases.sofar,b]);

  if (interactive) {
    cat(paste("\nSelected candidate: ",formatC(b,width=5),
              "\tmean value:",formatC(mean.value,width=8),
              "\tmean time:", formatC(mean.time,width=8),
              "\n"));
  }

  timestamp.end<-date()


  returnList<-list(precis=precis,
                   results=Q[1:no.testCases.sofar,],
                   times=R[1:no.testCases.sofar,],
                   dominance=N,
                   no.testCases=no.testCases.sofar,
                   testCases=TestCases[1:no.testCases.sofar],
                   no.experiments=e,
                   no.alive=sum(alive),
                   alive=alive,
                   selected=b,
                   mean.value.selected=mean.value,
                   timestamp.start=timestamp.start,
                   timestamp.end=timestamp.end);

  invisible(returnList);
}





n.min <- function(k = 10, alpha=0.05, ...) {
  n <- k;
  while(binom.test(n, k, ...)$p.value < alpha)
    n <- n - 1;
  return(n);
}



best.alive <- function(race.out, cutoff = 20) {
  for(i in 1:nrow(race.out$dominance))
    race.out$dominance[i,i] <- 0;
  race.out$dominance[!race.out$alive,1] <- race.out$no.testCases;
  return(which(order(apply(proceed$dominance, 1, max)) <= cutoff))
}

race.merge <- function(first, second) {
  stopifnot(ncol(second$results) == first$no.alive,
            second$no.testCases > first$no.testCases);
  alive <- first$alive;
  alive[which(first$alive)[!second$alive]] <- FALSE;
  cands <- which(first$alive);
  no.alive <- sum(alive);
  stopifnot(cands >= no.alive);
  
  dominance <- first$dominance;
  dominance[cands,cands] <- second$dominance;

  no.testCases <- second$no.testCases;
  
  results <- matrix(NA, nrow=no.testCases, ncol=ncol(first$results));
  results[1:first$no.testCases,] <- first$results;
  results[first$no.testCases+1:no.testCases,
          cands] <- second$results[first$no.testCases+1:no.testCases,];

  times <- matrix(NA, nrow=no.testCases, ncol=ncol(first$times));
  times[1:first$no.testCases,] <- first$times;
  times[first$no.testCases+1:no.testCases,
          cands] <- second$times[first$no.testCases+1:no.testCases,];

  no.experiments = (first$no.experiments + second$no.experiments
                    - (first$no.testCases * first$no.alive));
    
  return(list(precis=first$precis,
              results=results,
              times=times,
              dominance=dominance,
              no.testCases=no.testCases,
              testCases=second$testCases,
              no.experiments=no.experiments,
              no.alive=no.alive,
              alive=alive,
              selected=second$selected,
              mean.value.selected=second$mean.value.selected,
              timestamp.start=first$timestamp.start,
              timestamp.end=second$timestamp.end));
}

pareto.frontier <- function(sq, rt, I=1:length(sq)) {
  
  dominated.by <- function(idx, X, Y, strict=FALSE) {
    stopifnot(length(X) == length(Y), -1 < idx, idx <= length(X));
    ifelse(strict,
           return(intersect(which(X < X[idx]), which(Y < Y[idx]))),
           return(setdiff(intersect(which(X <= X[idx]), which(Y <= Y[idx])),
                          intersect(which(X == X[idx]), which(Y == Y[idx])))));
  }
  
  dom.rank <- function(I, X, Y, strict=FALSE) {
    stopifnot(length(X) == length(Y));
    score <- numeric(length(X));
    score[-I] <- length(X);
    score[I] <- unlist(lapply(sapply(1:length(I),
                                     dominated.by, X[I], Y[I], strict),
                              length));
    return(score);
  }
  
  score <- dom.rank(I, sq, rt);
  return(which(score==min(score)));
}
