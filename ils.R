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

# $Id: ils.R,v 1.21 2004/02/18 16:48:40 mldb Exp $ #

ils.race <- function(problem.id=sample(1:24, 1), maxExpScaling=10,
                     time.limit=60) {
  source("eRace.R", local=TRUE);
  scheduling.problems <- cbind(expand.grid(env=c("Fm", "Fm",
                                           "Pm", "Pm", "Pm", "S"),
                                           obj=c("T", "U"), weight=c(T, F)),
                               n=rep(c(50, rep(100, 5)), 4),
                               m=rep(c(20, 20, 2, 3, 4, 1), 4));

 
  vns.params <- function(idx) {
    vns.design <- expand.grid(P = factor(c("B", "F")),
                              A = factor(c("", "b", "f", "s",
                                "bf", "bs", "fs", "bsf")),
                              B = factor(c("", "b", "f", "s",
                                "bf", "bs", "fs", "bsf")),
                              C = factor(c("", "b", "f", "s",
                                "bf", "bs", "fs", "bsf")));
    attach(vns.design[idx,]);
    prefix <- paste("-L", P, sep="");
    i <- ifelse(A=="", "", paste(prefix, A, sep=""));
    ii <- ifelse(B=="", "", paste(prefix, B, sep=""));
    iii <- ifelse(C=="", "", paste(prefix, C, sep=""));
    detach(vns.design[idx,]);
    return(paste(i, ii, iii, collapse=""));
  }

  ls.cands <- sapply(c(419,182,406,1,405,453,307,168,167,181,279,280,451,420,40,566,789,790,54,454,676,22,21,229,231,535,565,675,35,8,53,23,24,39,661,85,679,694,791,917,919,118,950,819,308,452,72,58,6,30,57,103,285,339,51,548,71,595,613,693,741,918,935,230,232,343,533,536,551,597,599,86,662,855,936,947,998,7,70,116,69,120,123,10,5,99,126,75,101,117,16,3,29,100,932,104,108,236,413,534,563,614,619,87,709,742,792,982,983,186,340,804,314,36,68,803,838,172,564,612,43,171,235,344,552,555,616,680,854,856,920,939,997,310,4,25,67,102,106,44,107,14,9,76,296,119,15), vns.params);
  ils.candidates <- expand.grid(L = ls.cands,
                                P = "r",
                                l= 1, d = 9,
                                A = 0);
    
  make.task <- function(idx, specs) {
    source("problem.R", local=TRUE);
    source("dispatch.R", local=TRUE);
    inst <- tempfile("inst");
    seq <- tempfile("seq");
    
    this.tf <- runif(1);
    this.rdd <- runif(1);
    env <- specs$env;
    obj <- specs$obj;
    n <- specs$n;
    m <- specs$m;
    wght <- factor(ifelse(specs$weight, "unif", "common"),
                   levels=c("unif", "common"));

    ta.no <- sample(1:10,1) + ifelse(n==50, 50, 80);
    
    
    library(rpart);
    load("k.rp.Rbin");
    k <- 2^predict(prune(k.rp, 2^-8),
                   list(tf=this.tf,
                        rdd=this.rdd,
                        obj=obj,
                        w=wght,
                        n=n,
                        m=m,
                        env=env,
                        p=factor("unif")));
    if(env=="Fm") {
      instance <- read.instance.taillard(number = ta.no,
                                         TF = this.tf, RDD = this.rdd,
                                         wdis = wght,
                                         dir="../dat/");
      write.instance.Fm(instance, inst);
      
      cat(au.Fm(instance$p, instance$w, instance$d, k) -1, file=seq);
      
    } else {
      instance <- getInstance(njobs=n, TF=this.tf, RDD=this.rdd, wdis= wght,
                              mProcs=m);
      write.instance(instance, inst);
      
      if(env=="Pm") {
        cat(au.Pm(instance$p, instance$w, instance$d, m, k) -1, file=seq);
      } else {
        cat(au(instance$p, instance$w, instance$d, k) -1, file=seq);
      }
      
    }
    
    return(list(seq=seq, inst=inst,
                tf=this.tf, rdd=this.rdd, env=env, obj=obj, n=n, m=m, w=wght));
  }

  ils.string <- function(cand.spec, problem.spec, task.spec) {
    spec <- problem.spec;
    inst <- task.spec$inst;
    seq <- task.spec$seq;
    
    problem.str <- paste(ifelse(spec$env=="S", "-S",
                                ifelse(spec$env=="Fm", "-F", "-P")),
                         ifelse(spec$obj=="U", "-U", "-T"));
    attach(cand.spec);
    return(paste("cat", seq,
                 "| ./neh.out", problem.str, "-i", inst,
                 "| ./ils.out -R", problem.str, "-i", inst, 
                 "-t", time.limit, 
                 "-X", P, "-l", l, "-u", l+d,
                 "-A", A, L, "2>/dev/null"));
  }

  race.name <- paste("ils", problem.id, sep=".");
  
  invisible(race(wrapper.file="ils.race.wrapper.R",
                 maxExp=nrow(ils.candidates)*maxExpScaling,
                 log.file=paste(race.name, "race-log", sep="."),
                 problem=scheduling.problems[problem.id,],
                 candidates=ils.candidates,
                 task.fun=make.task,
                 cand.fun=ils.string,
                 race.name=race.name,
                 time.limit=time.limit,
                 ils.log=paste(race.name, "rtd-log", sep=".")));
}


