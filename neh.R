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

# $Id: neh.R,v 1.3 2004/03/23 20:40:39 mldb Exp $ #

neh.batch <- function(log="") {
  source("dispatch.R", local=T);
  source("problem.R", local=T);

  problems <- cbind(expand.grid(env=c("Fm", "Fm",
                                  "Pm", "Pm", "Pm", "S"),
                                obj=c("T", "U"), weight=c(T, F)),
                    n=rep(c(50, rep(100, 5)), 4),
                    m=rep(c(20, 20, 2, 3, 4, 1), 4));

  library(rpart);
  load("k.rp.Rbin");

  cat(paste("\t",
              c("tf", "rdd", "p.id", "rnd", "edd", "au",
                paste(c("rnd", "edd", "au"),"|neh", sep=""))),
      "\n",
      file=log, append=T);


  NR <- 0;
  repeat {
    NR <- NR+1;
    ## define instance
    TF <- runif(1);
    RDD <- runif(1);
    p.id <- sample(1:nrow(problems), 1);
    
    
    cat(NR, "\t",
        format(TF, digits=2), "\t",
        format(RDD, digits=2), "\t", p.id, file=log, append=T);
    
    attach(problems[p.id,]);
   
    
    inst <- NULL;
    if(env=="Fm") {
      ta.no <- sample(1:10,1) + ifelse(n==50, 50, 80);
      inst <- read.instance.taillard(number = ta.no,
                                     TF = TF, RDD = RDD,
                                     wdis = ifelse(weight, "unif", "common"),
                                     dir="../dat/");
      
    } else {
      inst <- getInstance(njobs=n, TF=TF, RDD=RDD,
                          wdis=ifelse(weight, "unif", "common"),
                          mProcs=m);
    }

    ## deterimine initial order
    rnd.seq <- sample(1:n);
    edd.seq <- edd(inst$d);
    
    k <- 2^predict(prune(k.rp, 2^-10),
                   list(tf=TF,
                        rdd=RDD,
                        obj=factor(ifelse(obj=="T", "T", "U"),
                          levels=c("T", "U")),
                        w=factor(ifelse(weight, "unif", "common"),
                          levels=c("unif", "common")),
                        n=n,
                        m=m,
                        env=factor(ifelse(env=="Fm","Fm",
                          ifelse(env=="Pm","Pm","S")),
                          levels=c("S", "Pm", "Fm")),
                        p=factor("unif")));
    au.seq <- NULL;
    attach(inst);
    if(env=="Fm") {
      au.seq <- au.Fm(p, w, d, k);
    } else if(env=="Pm") {
      au.seq <- au.Pm(p, w, d, m, k);
    } else {
      au.seq <- au(p, w, d, k);
    }
    detach(inst);

    ## write tempfiles
    inst.file <- tempfile("inst");
    rnd.file <- tempfile("rnd");
    edd.file <- tempfile("edd");
    au.file <- tempfile("au");
    if(env=="Fm") {
      write.instance.Fm(inst, inst.file);
    } else {
      write.instance(inst, inst.file);
    }
    cat(rnd.seq-1, "\n", file=rnd.file);
    cat(edd.seq-1, "\n", file=edd.file);
    cat(au.seq-1, "\n", file=au.file);

    ## construct commands
    p.str <- paste(ifelse(env=="Fm", "-F", ifelse(env=="Pm", "-P", "-S")),
                   ifelse(obj=="T", "-T", "-U"), collapse=" ");
    commands <- c(paste(paste("cat", c(rnd.file, edd.file, au.file), "|"),
                        ifelse(env=="Pm",
                               paste("./s2p.out -i", inst.file, "|"),
                               ""),
                        paste("./solution.out -i", inst.file, p.str)),
                  paste(paste("cat", c(rnd.file, edd.file, au.file)),
                        paste("./neh.out -i", inst.file, p.str),
                        paste("./solution.out -i", inst.file, p.str),
                        sep=" | "));

    detach(problems[p.id,]);
    
    ## test
    for(cmd in commands) {
      out.val <- system(cmd, intern=T);
      cat("\t", out.val, file=log, append=T);
    }
    cat("\n", file=log, append=T);
    
    ## delete tempfiles
    unlink(inst.file);
    unlink(rnd.file);
    unlink(edd.file);
    unlink(au.file);
  }
}

