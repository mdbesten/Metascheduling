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

# $Id: localsearch.R,v 1.18 2004/01/29 11:04:34 mldb Exp $ #
source("problem.R");

problems <- cbind(expand.grid(env=c("Fm", "Fm",
                                "Pm", "Pm", "Pm", "S"),
                              obj=c("T", "U"), weight=c(T, F)),
                  n=rep(c(50, rep(100, 5)), 4),
                  m=rep(c(20, 20, 2, 3, 4, 1), 4));



sys.load <- function() {
  return(as.numeric(system("uptime | awk '{print $(NF-2)}' | tr -d ','",
                           intern=T)));
}

ls.batch <- function(log="ls.log.Rdata") {
  ls.str <- function(ls.spec, env, obj, seq, inst) {
    problem <- paste("-", c(ifelse(env=="Fm", "F",
                                   ifelse(env=="Pm", "P", "S")),
                            ifelse(obj=="T", "T", "U")),
                     sep="", collapse=" ");
    
    attach(ls.spec);
    return(paste("cat", seq,
                 ifelse(env=="Pm", paste("| ./s2p.out -i", inst), ""),
                 "| ./localsearch.out -v", "-i", inst, problem,
                 ifelse(piv, "-B", ""),
                 ifelse(b, "-b", ""),
                 ifelse(f, "-f", ""),
                 ifelse(s, "-s", "")));
  }

  ls.grid <- expand.grid(piv=c(F,T), b=c(F,T), f=c(F,T), s=c(F,T))[-(1:2),];

  
  ls.frame <- data.frame(sq=numeric(0), rt=numeric(0),
                         ls.grid[0,], 
                         TF=numeric(0), RDD=numeric(0),
                         problems[0,],
                         load.avg=numeric(0));

  repeat {
    testCase <- cbind(TF=runif(1), RDD=runif(1),
                      problems[sample(1:nrow(problems), 1),]);
    attach(testCase);
    inst <- tempfile("inst");
    seq <- tempfile("seq");
    cat(sample(n)-1, "\n", file=seq);

    if(env=="Fm") {
      random = round(runif(1, 1, 10));
      instance <- read.instance.taillard(number = random+ifelse(n==50, 50, 80),
                                         TF = TF, RDD = RDD,
                                         wdis=ifelse(weight,"unif","common"),
                                         dir="../dat/");
      write.instance.Fm(instance, inst);
    } else {
      write.instance(getInstance(njobs=n,
                                 TF=TF, RDD=RDD, wdis=ifelse(weight, "unif",
                                                   "common"), mProcs=m),
                     inst);
    }

    for(i in 1:nrow(ls.grid)) {
      command <- ls.str(ls.grid[i,], env, obj, seq, inst);
      ls.out <- as.numeric(system(command,intern=T));
      ls.frame <- rbind(ls.frame,
                        cbind(sq=ls.out[1], rt=ls.out[2], ls.grid[i,],
                              testCase,
                              load.avg = sys.load()));
    }
    rownames(ls.frame) <- 1:nrow(ls.frame);
    save(ls.frame, file=log, compress=T);
    
    unlink(seq);
    unlink(inst);
  }
}

vns.batch <- function(log="vns.log.Rdata",
                      subset=c(1, 3, 4, 5, 6, 7, 8, 9, 10, 14, 15, 16, 21, 22, 23, 24, 25, 29, 30, 35, 36, 39, 40, 43, 44, 51, 53, 54, 57, 58, 67, 68, 69, 70, 71, 72, 75, 76, 85, 86, 87, 99, 100, 101, 102, 103, 104, 106, 107, 108, 116, 117, 118, 119, 120, 123, 126, 167, 168, 171, 172, 181, 182, 186, 229, 230, 231, 232, 235, 236, 279, 280, 285, 296, 307, 308, 310, 314, 339, 340, 343, 344, 405, 406, 413, 419, 420, 451, 452, 453, 454, 533, 534, 535, 536, 548, 551, 552, 555, 563, 564, 565, 566, 595, 597, 599, 612, 613, 614, 616, 619, 661, 662, 675, 676, 679, 680, 693, 694, 709, 741, 742, 789, 790, 791, 792, 803, 804, 819, 838, 854, 855, 856, 917, 918, 919, 920, 932, 935, 936, 939, 947, 950, 982, 983, 997, 998)) {

    
  neighborhoods <- factor(c("", "b", "f", "s", "bf", "bs", "fs", "bsf"));
  pivoting <- factor(c("B", "F"));
  
  vns.grid <- expand.grid(P = pivoting,
                          A = neighborhoods,
                          B = neighborhoods,
                          C = neighborhoods);

  vns.params <- function(idx) {
    attach(vns.grid[idx,]);
    prefix <- paste("-L", P, sep="");
    i <- ifelse(A=="", "", paste(prefix, A, sep=""));
    ii <- ifelse(B=="", "", paste(prefix, B, sep=""));
    iii <- ifelse(C=="", "", paste(prefix, C, sep=""));
    detach(vns.grid[idx,]);
    return(paste(i, ii, iii, collapse=""));
  }

  vns.str <- function(candidate, env, obj, seq, inst) {
    problem <- paste(ifelse(env=="S", "-S", ifelse(env=="Fm", "-F", "-P")),
                     ifelse(obj=="U", "-U", "-T"));
    return(paste("cat", seq,
                 ifelse(env=="Pm", paste("| ./s2p.out -i", inst), ""),
                 "| ./vns.out", "-i", inst, problem,
                 vns.params(candidate),
                 "-v 2>/dev/null"));
  }
  
  vns.frame <- data.frame(sq=numeric(0), rt=numeric(0),
                          vns.id=numeric(0), vns.grid[0,], 
                          TF=numeric(0), RDD=numeric(0),
                          problems[0,],
                          load.avg=numeric(0));
  
  repeat {
    testCase <- cbind(TF=runif(1), RDD=runif(1),
                      problems[sample(1:nrow(problems), 1),]);
    attach(testCase);
    inst <- tempfile("inst");
    seq <- tempfile("seq");
    cat(sample(n)-1, "\n", file=seq);

    if(env=="Fm") {
      random = round(runif(1, 1, 10));
      instance <- read.instance.taillard(number = random+ifelse(n==50, 50, 80),
                                         TF = TF, RDD = RDD,
                                         wdis=ifelse(weight,"unif","common"),
                                         dir="../dat/");
      write.instance.Fm(instance, inst);
    } else {
      write.instance(getInstance(njobs=n,
                                 TF=TF, RDD=RDD, wdis=ifelse(weight,"unif",
                                                   "common"), mProcs=m),
                     inst);
    }

    for(i in subset) {
      command <- vns.str(i, env, obj, seq, inst);
      vns.out <- as.numeric(system(command,intern=T));
      vns.frame <- rbind(vns.frame,
                        cbind(sq=vns.out[1], rt=vns.out[2],
                              vns.id=i, vns.grid[i,],
                              testCase,
                              load.avg = sys.load()));
    }
    rownames(vns.frame) <- 1:nrow(vns.frame);
    save(vns.frame, file=log, compress=T);
    
    unlink(seq);
    unlink(inst);
  }
}

de.factor <- function(factor.vect) {
  return((as.numeric(levels(factor.vect)))[as.numeric(factor.vect)]);
}

vect2matrix <- function(vect, n=14) {
  return(sapply(seq(1, length(vect), n),
                function(i) {
                  return(vect[i:(i+n-1)]);
                }));
}

problem.ids <- function(frame) {
  p.id <- numeric(nrow(frame));
  attach(frame);
  for(i in 1:nrow(problems)) {
    p.id[env==problems[i,"env"] &
         weight==problems[i,"weight"] &
         obj==problems[i,"obj"] &
         n==problems[i,"n"] &
         m==problems[i,"m"]] <- i;
  }
  detach(frame);
  return(p.id);
}

foobar <- function() {
  
  xyplot(sq.scld ~ rt.scld|ifelse(piv, "B", "F")*env,
         groups=nghbrhd, type="p",
         key=list(columns=7,
           text=levels(nghbrhd),
           points= Rows(sps, 1:7)));
}
