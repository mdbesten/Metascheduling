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

library(spatial);
library(lattice);
library(MASS);

exp.str <- function(descr) {
  return(paste("$",
               ifelse(descr$env=="Fm", "F",
                      ifelse(descr$env=="Pm", "P", "S")),
               ifelse(descr$env=="S", "", paste("_{", descr$m, "}", sep="")),
               "|", ifelse(descr$env=="Fm", "\\mathrm{prmu}", ""), "|",
               "\\sum_{i=0}^{", descr$n, "}",
               ifelse(descr$weight, "w_i", ""),
               descr$obj, "_i",
               "$",
               sep=""));
}
#vns.table <- data.frame(problem=exp.str(experiment[c(1:13, 17:24),]), alive=sapply(vns.list, length), choice=sapply(lapply(vns.list, sapply, vns.str), paste, collapse=" "))
#write.table(vns.table, file="vns.table", sep=" & ", quote=F, eol="\\\\\n")

ls.str <-  function(params) {
  attach(params);
  return(paste(ifelse(R, "R", ""),
               ifelse(piv, "B", "F"),
               ifelse(b, "b",""),
               ifelse(f, "f", ""),
               ifelse(s, "s",""),
               sep="")); 
}

ls.split <- function(set, pivot) { # set is subset of params grid
  attach(set);
  return(list(A=set[which(pivot==T),], B=set[which(pivot==F),]));
}

ls.merge <- function(set) {
  attach(set);
  return(paste(ifelse(b, "b", ""),
               ifelse(f, "f", ""),
               ifelse(s, "s", ""), sep="", collapse=","));
}

ls.summary <- function(set) { # set: e.g. c(1, 9, 3)
  params <- expand.grid(piv=c(F,T), b=c(F,T), f=c(F,T), s=c(F,T), R=c(F,T));
  piv.split <- ls.split(params[set,], piv);
  split <- list(A=ls.split(piv.split$A, R), B=ls.split(piv.split$B, R));

  return(paste(ifelse(dim(split$A$A)[1] > 0,
                      paste("RB{", ls.merge(split$A$A), "}", sep=""),
                      ""),
               ifelse(dim(split$A$B)[1] > 0,
                      paste("B{", ls.merge(split$A$B), "}", sep=""),
                      ""),
               ifelse(dim(split$B$A)[1] > 0,
                      paste("RF{", ls.merge(split$B$A), "}", sep=""),
                      ""),
               ifelse(dim(split$B$B)[1] > 0,
                      paste("F{", ls.merge(split$B$B), "}", sep=""),
                      ""),
               sep="; "));
}


ls.plot <- function(id = "S.wU", stat = mean, n = 4) {
  times <- read.table(paste(id, "times", sep="."))[,3:16];
  x <- apply(times, 2, stat);
  results <- read.table(paste(id, "results", sep="."))[,3:16];
  y <- apply(results, 2, stat);
  plot(x, y, type="n", xlab="time", ylab="fitness", main=id);
  params <- expand.grid(piv=c(F,T), b=c(F,T), f=c(F,T), s=c(F,T))[3:16,];
  km <- kmeans(data.frame(x, y), n);#, t(times), t(results)), n);
  text(x, y, lsstr(params), col=1+km$cluster);#, pos=4);
  lines(spline(x, predict(lm(y ~ I(1/x)))), lty=2);
#  points(x, y, pch=(25:19)[km$cluster]);
  grid();

#  printcp(rpart(factor(km$cluster) ~ ifelse(piv, 1, 0) + ifelse(b, 1, 0) + ifelse(f, 1, 0) + ifelse(s, 1, 0), params, method="class", cp=0.000001));
}

textplot <- function(prefix, i, n) {
  x <- unlist(read.table(paste(prefix, "times", sep="."))[i,]);
  y <- unlist(read.table(paste(prefix, "results", sep="."))[i,]);
  x <- log(x);
  plot(x, y, type="n",
       xlab="run time", ylab="solution quality",
       main=paste(prefix, i));
  params <- expand.grid(piv=c(F,T), b=c(F,T), f=c(F,T), s=c(F,T), R=c(F,T));
  km <- kmeans(data.frame(x, y), n);
  text(x, y, ls.str(params), col=1+km$cluster);
}

dominates <- function(idx, X, Y, strict=TRUE) {
  stopifnot(length(X) == length(Y), -1 < idx, idx <= length(X));
  ifelse(strict,
         return(intersect(which(X > X[idx]), which(Y > Y[idx]))),
         return(intersect(which(X >= X[idx]), which(Y >= Y[idx]))));
}

dominated.by <- function(idx, X, Y, strict=FALSE) {
  stopifnot(length(X) == length(Y), -1 < idx, idx <= length(X));
  ifelse(strict,
         return(intersect(which(X < X[idx]), which(Y < Y[idx]))),
         return(intersect(which(X <= X[idx]), which(Y <= Y[idx]))));
}

dom.score <- function(x, y) {
  return(unlist(lapply(sapply(1:length(x),
                              dominated.by, x, y),
                       length)));
}

dom.score.frame <- function(exp,start, time, result) {
  sz <- length(exp);
  scores <- numeric(sz);
  for(i in 1:sz) {
    sub <- exp==exp[i] & start==start[i];
    scores[i] <- length(intersect(which(time[sub] <= time[i]),
                                  which(result[sub] <= result[i])));
  }
  return(scores);
}
pareto.front <- function(x, y) {
  score <- dom.score(x,y);
  return(which(score==min(score)));
}
  
canberra <- function(x, ref) {
  return(abs(x - ref) / abs(x + ref));
}

canberra.dominates <- function(idx, X, Y) {
  stopifnot(length(X) == length(Y), -1 < idx, idx <= length(X));
  dom <- dominates(idx, X, Y);
  return(sum(canberra(dom, X[idx]), canberra(dom, Y[idx])));
}

dominance.matrix <- function(X, Y, method="canberra") {
  stopifnot(length(dim(X)) == 2, length(dim(Y)) == 2, dim(X) == dim(Y));
  n.exp <- dim(X)[1];
  n.alg <- dim(X)[2];
  Z <- matrix(0, n.exp, n.alg);
  for(i in 1:n.exp) {
    for(j in 1:n.alg) {
      Z[i,j] <- switch(method,
                       canberra =
                       canberra.dominates(j, unlist(X[i,]), unlist(Y[i,])),
                       number =
                       length(dominates(j, unlist(X[i,]), unlist(Y[i,]))));
    }
  }
  return(Z);
}

                                        # D : dominance matrix
                                        # map: mapping of row numbers
                                        #      to tf & rdd values
                                        # n: number of clusters
plot.most.dominant <- function(D, map, n, main="") {
  plot(map, type="n", main=main);
  mc <- max.col(D);
  text(map, names(D)[mc], col=kmeans(t(D), n)$cluster[mc]);
}

                                        # d <- D[,i]
plot.contour <- function(z, map, main="", np=2, d=0.7) {
  tf <- map[,1];
  rdd <- map[,2];
  lm <- surf.gls(np, expcov, tf, rdd, z, d=d);
  print(summary(lm));
  tr <- semat(lm, min(tf), max(tf), min(rdd), max(rdd), 100);
                                        #grid <- expand.grid(x=tr$x, y=tr$y);
                                        #grid$z <- tr$z;
                                        #levelplot(z ~ x * y, grid);
  colors <- rev(heat.colors(ceiling(max(z)+1)));
  contour(tr, nlevels=5, xlab="Tardiness Factor", ylab="Range of Due Dates",
          main=paste(main, ""));
  text(tf, rdd, round(z), col=colors[round(z)+1]);
}

plot.level <- function(tf, rdd, val, np=2, d=0.7, density=100) {
  lm <- surf.gls(np, expcov, tf, rdd, val, d=d);
  print(summary(lm));
  mat <- trmat(lm, min(tf), max(tf), min(rdd), max(rdd), density);
  grid <- con2tr(mat);
  levelplot(z ~ x * y, grid);
}

ls.boxplot <- function(D) {
#  boxplot(split(apply(D, 1, c), rep(names(D), dim(D)[1])), notch=T);
  bwplot(rep(names(D), rep(dim(D)[1], dim(D)[2])) ~ unlist(D))
}


matrix2frame <- function(matrix) {
  rl <- dim(matrix)[1];
  cl <- dim(matrix)[2];
  return(data.frame(val=unlist(matrix), col=rep(1:cl, rep(rl, cl)), row=rep(1:rl, cl)));
}

vns.names <- function(d=expand.grid(swap=c(T, F), RBbfs=c(T, F), 
                                    vns=c("b|f|s", "f|b|s", "f|s|b", 
                                           "s|f|b", "s|b|f", "b|s|f"))) {
  return(paste(ifelse(d$swap, "S|",""), ifelse(d$RBbfs, "R|", ""), 
               d$vns, sep=""));
}

collect <- function() {
  collection <- list();
  for(o in c("T", "U")) {
    for(w in c("t", "w")) {
      id <- paste(w, o, sep="");
      results <- read.table(paste("vnsS", id, "results", sep="."))[1:200,];
      names(results) <- vns.names();
      times <- read.table(paste("vnsS", id, "times", sep="."))[1:200,];
      names(times) <- vns.names();
      collection <- c(collection, list(sq=results, rt=times));
    }
  }
  return(list(tT=list(sq=collection[1]$sq, rt=collection[2]$rt), 
              wT=list(sq=collection[3]$sq, rt=collection[4]$rt), 
              tU=list(sq=collection[5]$sq, rt=collection[6]$rt), 
              wU=list(sq=collection[7]$sq, rt=collection[8]$rt)));
}

foobar <- function(A, B, subset, id) {
  tmp <- par()$mfrow;
  par(mfrow=c(2,1));
  ra <- data.frame(t(apply(A[,subset], 1, rank)));
  rb <- data.frame(t(apply(B[,subset, 1, rank)));
  names(ra) <- names(rb) <- names(A);
  ma <- apply(ra, 2, mean);
  mb <- apply(rb, 2, mean);
  names(ma) <- names(mb) <- 1:ncol(A);
  o <- as.numeric(names(sort(ma[subset]))); 
  boxplot(ra[,o], 
          main=paste(id, "rank solution quality"));
  boxplot(rb[,o], 
          main=paste(id, "rank run time"));
  par(mfrow=tmp);
}

vns.boxplot <- function(idx) {
  tmp <- par()$mfrow;
  par(mfrow=c(2,1));
   
  load(paste("vns", idx, "out", sep="."));

  quality <- data.frame(t(apply(race.out$results[,race.out$alive], 1, rank)));
  runtime <- data.frame(t(apply(race.out$times[,race.out$alive], 1, rank)));

  names(quality) <- names(runtime) <- vns.str(vns.design[race.out$alive,]);

  ma <- apply(quality, 2, mean);
  mb <- apply(runtime, 2, mean);
  names(ma) <- names(mb) <- 1:ncol(quality);
  o <- as.numeric(names(sort(ma)));
  id <- exp.str(experiment[idx,]);
  boxplot(quality[,o], 
          main=paste(id, "rank solution quality"));
  boxplot(runtime[,o], 
          main=paste(id, "rank run time"));
  par(mfrow=tmp);
}

#lapply(coll, function(p) { N <- p$sq; M <- p$rt; return(pareto.race(dummy.race.wrapper, 1000, first.test=20, graphics=T, test="t.test")$alive); })
commentedout <- function() {
foobar(coll$tT$sq, coll$tT$rt, prf.tT$alive, "Friedman tT")
foobar(coll$tT$sq, coll$tT$rt, prt.tT$alive, "t test tT")
foobar(coll$wT$sq, coll$wT$rt, prf.wT$alive, "Friedman wT")
foobar(coll$wT$sq, coll$wT$rt, prt.wT$alive, "t test wT")
foobar(coll$tU$sq, coll$tU$rt, prf.tU$alive, "Friedman tU")
foobar(coll$tU$sq, coll$tU$rt, prt.tU$alive, "t test tU")
foobar(coll$wU$sq, coll$wU$rt, prf.wU$alive, "Friedman wU")
foobar(coll$wU$sq, coll$wU$rt, prt.wU$alive, "t test wU")
 
N <- coll$tT$sq
M <- coll$tT$rt
prt.tT <- pareto.race(dummy.race.wrapper, 1000, first.test=20, graphics=T, test="t.test")
prf.tT <- pareto.race(dummy.race.wrapper, 1000, first.test=20, graphics=T)
N <- coll$wT$sq
M <- coll$wT$rt
prt.wT <- pareto.race(dummy.race.wrapper, 1000, first.test=20, graphics=T, test="t.test")
prf.wT <- pareto.race(dummy.race.wrapper, 1000, first.test=20, graphics=T)
N <- coll$tU$sq
M <- coll$tU$rt
prt.tU <- pareto.race(dummy.race.wrapper, 1000, first.test=20, graphics=T, test="t.test")
prf.tU <- pareto.race(dummy.race.wrapper, 1000, first.test=20, graphics=T)
N <- coll$wU$sq
M <- coll$wU$rt
prt.wU <- pareto.race(dummy.race.wrapper, 1000, first.test=20, graphics=T, test="t.test")
prf.wU <- pareto.race(dummy.race.wrapper, 1000, first.test=20, graphics=T)
}
