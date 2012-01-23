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




friedman.comet <- function(y,I=1:ncol(y),n=nrow(y),conf.level=0.95,
                           verbose=FALSE,interactive=TRUE) {

  if (!interactive)
    verbose<-FALSE

  k<-length(I)
  r <- t(apply(y[1:n,I], 1, rank));
  R<- apply(r, 2, sum)
  A<- sum(as.vector(r)^2);
  alpha<-1-conf.level
  TIES <- tapply(r, row(r), table)
  STATISTIC.1 <- ((12 * sum((R - n * (k + 1) / 2)^2)) /
                  (n * k * (k + 1)
                   - (sum(unlist(lapply(TIES, function (u) {u^3 - u}))) /
                      (k - 1))))
  PARAMETER.1 <- k-1
  PVAL.1 <- pchisq(STATISTIC.1, PARAMETER.1, lower = FALSE)


  PARAMETER.2 <- (n-1)*(k-1)
  STATISTIC.2 <- (n-1)*STATISTIC.1/(n*(k-1)-STATISTIC.1)
  PVAL.2 <- pf(STATISTIC.2, PARAMETER.1, PARAMETER.2, lower = FALSE)

  if (verbose) {
    if (PVAL.1<alpha){
      t<-qt(1-alpha/2,(n-1)*(k-1)) * ( 2*(n*A - sum(R^2))/((n-1)*(k-1)))^(1/2)
      o<-order(R);
      printed <- 0;
      for (i in 1:(k-1)) {
        str <- I[o[i]];
        toprint<-i;
        for (j in (i+1):k) {
          if (abs(R[o[j]]-R[o[i]])>t) {
            break
          }else{
            str <- paste(str,I[o[j]],sep=",");
            toprint<-j
          }
        }
        if ( toprint>printed ) {
          cat(paste(str,"\n"))
          printed<-toprint
        }
      }
      if (printed<k)
        cat(paste(I[o[k]],"\n"));
    }
  } else {



    if (interactive) {
      if (!is.nan(PVAL.1) & (PVAL.1<alpha))
        if (!is.nan(PVAL.2) & (PVAL.2<alpha))
          cat("*")
        else
          cat("1")
      else
        if (!is.nan(PVAL.2) & (PVAL.2<alpha))
          cat("2")
        else
          cat("-")
    }

    J<-I[order(R)];

    if (!is.nan(PVAL.1) & (PVAL.1<alpha)){
      t<-qt(1-alpha/2,(n-1)*(k-1)) * (2*(n*A-sum(R^2))/((n-1)*(k-1)))^(1/2)
      o<-order(R);
      J<-I[o[1]];
      for (j in 2:k)
        if (abs(R[o[j]]-R[o[1]])>t)
          break
        else
          J<-c(J,I[o[j]]);
    }
    return(J)
  }
}



race <- function (Rwrapper,
                  maxExp,
                  log.file="",
                  test=c("friedman.test","t.test"),
                  adjust=c("none",p.adjust.methods),
                  interactive=TRUE,
                  first.test=2,
                  conf.level=0.95,
                  watch=c()){

  timestamp.start<-date();

  if (!missing(log.file))
    if (system(paste("touch",log.file),ignore.stderr=TRUE)!=0)
      stop(paste("I cannot create file ",log.file,sep=""))

  test<-match.arg(test);
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

  precis<-paste("The racing procedure.\n",
                "Program: ",par$program.name,"\n",
                "Number of candidates: ",par$no.candidates,"\n",
                "Number of available test cases: ",par$no.testCases,"\n",
                "Max number of experiments: ",maxExp,"\n",
                "Statistical test: ",test,"\n",
                sep="")
  if (test=="t.test")
    precis<-paste(precis,
                  "\tCorrection: ",adjust,"\n",sep="")
  precis<-paste(precis,
                "Test cases before discarding: ",first.test,"\n"
                ,sep="")

  if (!is.null(par$precis))
    precis<-paste(precis,par$precis,sep="")

  if (interactive)
    cat(paste(precis,"\n"))

  TestCases<-1:par$no.testCases

  N<-matrix(data = NA, nrow = 0, ncol = par$no.candidates)


  alive<-array(TRUE,par$no.candidates);
  e<-0;

  b <- sample(1:par$no.candidates,1)

  no.testCases.sofar <- 0;


  for (i in 1:par$no.testCases) {
    N <- rbind(N, matrix(NA, 1, par$no.candidates));
    
    if (e+sum(alive)>maxExp)
      break
    if (sum(alive)==1)
      break

    e<-e+sum(alive)



    for (cf in which(alive)){

      runResult<-Rwrapper(candidate=cf, testCase=TestCases[i])

      N[i,cf]<-runResult$output;

      timestamp.current<-date();
      
      if (log.file!=""){
        log<-list(precis=precis,
                  results=N[1:i,],
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

    no.testCases.sofar <- i


    if (i>=first.test) {

      switch(test,

             friedman.test = {
               if (sum(alive)==2) {
                 a <- which(alive)
                 if (interactive)
                   cat("=")

                 wilcox.p.value<-wilcox.test(N[1:i,a[1]],N[1:i,a[2]],
                                             paired=TRUE,exact=FALSE)$p.value

                 if (!is.nan(wilcox.p.value) & !is.na(wilcox.p.value)
                     & wilcox.p.value<1-conf.level)
                   if (mean(N[1:i,a[1]]>N[1:i,a[2]])>0.5)
                     alive[a[1]]<-FALSE
                   else
                     alive[a[2]]<-FALSE
               } else {
                 J<-friedman.comet(N[1:i,],which(alive),
                                   interactive=interactive,
                                   conf.level=conf.level)
                 alive[-J]<-FALSE;
                 b<-J[1];
               }
             },

             t.test = {
               mn<-array(0,c(ncol(N)));
               for (j in 1:ncol(N))
                 mn[j]<-sum(N[1:i,j]/i)
               b<-match(min(mn[alive]),mn);

               PJ<-array(0,dim=c(2,0));
               for (j in (1:length(alive))[alive]) {
                 p <- t.test(N[1:i,b],N[1:i,j],paired=TRUE,
                             alternative="less")$p.value
                 if (!is.nan(p) & !is.na(p))
                   PJ<-array(c(PJ,j,p),dim=dim(PJ)+c(0,1))
               }
               PJ[2,]<-p.adjust(PJ[2,],method=adjust)
               dropped.any<-FALSE;
               for (j in 1:ncol(PJ))
                 if (PJ[2,j]<(1-conf.level)){
                   alive[PJ[1,j]]<-FALSE
                   dropped.any<-TRUE
                 }
               if (interactive){
                 if (dropped.any)
                   cat("*")
                 else
                   cat("-")
               }
             });


    } else {
      if (interactive)
        cat(" ")
      if (i==1)
        b<-order(N[1,])[1]
      else
        b<-order(apply(t(apply(N[1:i,], 1, rank)), 2, mean))[1]
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
                results=N[1:i,],
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


  mean.value<-mean(N[1:no.testCases.sofar,b]);

  if (interactive) {
    cat(paste("\nSelected candidate: ",formatC(b,width=5),
              "\tmean value:",formatC(mean.value,width=8),
              "\n"));
  }

  timestamp.end<-date()


  returnList<-list(precis=precis,
                   results=N[1:no.testCases.sofar,],
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

dom.race <- function (Rwrapper,
                      maxExp,
                      log.file="",
                      test=c("friedman.test","t.test"),
                      adjust=c("none",p.adjust.methods),
                      interactive=TRUE,
                      first.test=2,
                      conf.level=0.95,
                      watch=c(),
                      dom.fun=dom.rank){

  timestamp.start<-date();

  if (!missing(log.file))
    if (system(paste("touch",log.file),ignore.stderr=TRUE)!=0)
      stop(paste("I cannot create file ",log.file,sep=""))

  test<-match.arg(test);
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
                "Statistical test: ",test,"\n",
                sep="")
  if (test=="t.test")
    precis<-paste(precis,
                  "\tCorrection: ",adjust,"\n",sep="")
  precis<-paste(precis,
                "Test cases before discarding: ",first.test,"\n"
                ,sep="")

  if (!is.null(par$precis))
    precis<-paste(precis,par$precis,sep="")

  if (interactive)
    cat(paste(precis,"\n"))

  TestCases<-1:par$no.testCases

  N<-matrix(data = NA, nrow = par$no.testCases, ncol = par$no.candidates)
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
                  dominance=N[1:i,],
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

    N[i,] <- dom.fun(which(alive), t(Q[i,]), t(R[i,]));
      
    no.testCases.sofar <- i
    

    if (i>=first.test) {
      switch(test,

             friedman.test = {
               if (sum(alive)==2) {
                 a <- which(alive)
                 if (interactive)
                   cat("=")

                 wilcox.p.value<-wilcox.test(N[1:i,a[1]],N[1:i,a[2]],
                                             paired=TRUE,exact=FALSE)$p.value

                 if (!is.nan(wilcox.p.value) & !is.na(wilcox.p.value)
                     & wilcox.p.value<1-conf.level)
                   if (mean(N[1:i,a[1]]>N[1:i,a[2]])>0.5)
                     alive[a[1]]<-FALSE
                   else
                     alive[a[2]]<-FALSE
               } else {
                 J<-friedman.comet(N[1:i,],which(alive),
                                   interactive=interactive,
                                   conf.level=conf.level)
                 alive[-J]<-FALSE;
                 b<-J[1];
               }
             },

             t.test = {
               mn<-array(0,c(ncol(N)));
               for (j in 1:ncol(N))
                 mn[j]<-sum(N[1:i,j]/i)
               b<-match(min(mn[alive]),mn);

               PJ<-array(0,dim=c(2,0));
               for (j in (1:length(alive))[alive]) {
                 p <- t.test(N[1:i,b],N[1:i,j],paired=TRUE,
                             alternative="less")$p.value
                 if (!is.nan(p) & !is.na(p))
                   PJ<-array(c(PJ,j,p),dim=dim(PJ)+c(0,1))
               }
               PJ[2,]<-p.adjust(PJ[2,],method=adjust)
               dropped.any<-FALSE;
               for (j in 1:ncol(PJ))
                 if (PJ[2,j]<(1-conf.level)){
                   alive[PJ[1,j]]<-FALSE
                   dropped.any<-TRUE
                 }
               if (interactive){
                 if (dropped.any)
                   cat("*")
                 else
                   cat("-")
               }
             });
    } else {
      if (interactive)
        cat(" ")
      if (i==1)
        b<-order(N[1,])[1]
      else
        b<-order(apply(t(apply(N[1:i,], 1, rank)), 2, mean))[1]
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
                dominance=N[1:i,],
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


  mean.value<-mean(N[1:no.testCases.sofar,b]);
  mean.time<-mean(M[1:no.testCases.sofar,b]);

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
                   dominance=N[1:no.testCases.sofar,],
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

dominated.by <- function(idx, X, Y, strict=FALSE) {
  stopifnot(length(X) == length(Y), -1 < idx, idx <= length(X));
  ifelse(strict,
         return(intersect(which(X < X[idx]), which(Y < Y[idx]))),
         return(intersect(which(X <= X[idx]), which(Y <= Y[idx]))));
}

dom.rank <- function(I, X, Y, strict=FALSE) {
  stopifnot(length(X) == length(Y));
  rank <- numeric(length(X));
  rank[-I] <- NA;
  rank[I] <- unlist(lapply(sapply(1:length(I),
                                  dominated.by, X[I], Y[I], strict),
                           length));
  return(rank);
}

dummy.race.wrapper <- function(candidate=NULL, testCase=NULL,
                               cand.sz=dim(N)[2],test.sz=dim(N)[1],
                               id="", abstr="") {
  if(is.null(candidate) && is.null(testCase)) {
    return(list(no.candidates=cand.sz,
                no.testCases=test.sz,
                program.name=id,
                precis=paste(abstr, "\n")));
  } else if(is.numeric(candidate)&&is.numeric(testCase)&&
            (candidate==as.integer(candidate))&&
            (testCase==as.integer(testCase))&&
            (length(candidate)==1)&&(length(testCase)==1)&&
            (candidate>=1)&&(testCase>=1)&&
            (candidate<=cand.sz)&&(testCase<=test.sz)) {
    set.seed(testCase);
    best.value <- N[testCase, candidate];
    time <- M[testCase, candidate];
    return(list(output=best.value, runtime=time, logList=list(seed=testCase)));
  } else {
    stop("Something wrong in wrapper args");
  }
}

myplot <- function(i, subset=c(5, 9, 10, 13)) {
   sq <- apply(t(apply(N[1:i,], 1, rank)), 2, mean);
   rt <- apply(t(apply(M[1:i,], 1, rank)), 2, mean);
   plot(sq, rt, col=1, xlab="time", ylab="quality", main=i);
   points(sq[subset],rt[subset], pch=20, col=2);
   front <- pareto.frontier(sq,rt, subset);
   points(sq[front], rt[front], pch=20, col=4);
}

