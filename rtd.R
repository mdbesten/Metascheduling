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

# $Id: rtd.R,v 1.8 2004/08/29 09:52:02 mldb Exp $ #
source("epsilon.R");

epsilon.ratios <- function(...) {
  rtd.list <- list(...);
  rtd.no <- length(rtd.list);
  the.matrix <- matrix(NA, rtd.no, rtd.no);
  for(i in 1:rtd.no) {
    for(j in 1:rtd.no) {
      the.matrix[i,j] <-
        epsilon.dominates(rtd.list[[i]], rtd.list[[j]])/
        epsilon.dominates(rtd.list[[j]], rtd.list[[i]]);
    }
  }
  return(the.matrix);
}

ratio.table <- function(n=3) {
  rtab <- data.frame(matrix(NA, n, n));
  names(rtab) <- LETTERS[1:n];
  rownames(rtab) <- names(rtab);
  rns <- names(rtab);
  for(i in 1:ncol(rtab)) {
    for(j in 1:nrow(rtab)) {
      if(j == i) {
        rtab[i,j] <- 1;
      } else {
        rtab[i,j] <- paste("${\\epsilon_{", rns[i], rns[j], "}",
                           "\\over",
                           "\\epsilon_{", rns[j], rns[i], "}}$", sep="");
      }
    }
  }
  return(rtab);
}

rank.rtds <- function(rtd.list) {
  rtd.no <- length(rtd.list);
  e.matrix <- data.frame(matrix(NA, rtd.no, rtd.no));
  if(!is.null(names(rtd.list))) {
    names(e.matrix) <- names(rtd.list);
    rownames(e.matrix) <- names(rtd.list);
  }
  for(i in 1:rtd.no) {
    for(j in 1:rtd.no) {
      if(i != j) {
        e.matrix[i,j] <- epsilon.dominates(rtd.list[[i]], rtd.list[[j]]);
      } else {
        e.matrix[i,j] <- 1;
      }
    }
  }
  e.ratios <- e.matrix;
  for(i in 1:rtd.no) {
    for(j in 1:rtd.no) {
      e.ratios[i,j] <- e.matrix[i,j]/e.matrix[j,i];
    }
  }
  e.weights <- ahp(e.ratios);
  e.ranks <- rank(e.weights);
  e.avg <- 1/rtd.no;
  e.dev <- (e.weights - e.avg)/e.avg;
  return(list(matrix=e.matrix,
              ratios=e.ratios,
              weights=e.weights,
              ranks=e.ranks,
              dev=e.dev));
  
}

plot.rtds <- function(...) {
  plot(rbind(...), type="n", axes=F, xlab="", ylab="");
  rtd.info <- rank.rtds(...);
  ctr <- 0;
  for(rtd in list(...)) {
    ctr <- ctr + 1;
    lines(rtd, type="S", lty=2, col=rtd.info$ranks[ctr]);
  }

  legend(locator(),
         paste(names(rtd.info$ranks),
               "(", round(rtd.info$dev*100), ")"),
         lty=2, col=rtd.info$ranks);
  return(rtd.info);
}

epsilon.dominates <- function(ref=rz(10), cand=rz(10), video=F) {
  r.ref <- c(0, ref$x)/c(ref$y,0);
  r.cand <- cand$x/cand$y;
  part <- data.frame(l.bound = r.ref[-length(r.ref)],
                     r.bound = r.ref[-1],
                     x.bound = ref$x,
                     y.bound = ref$y);
  epsilon <- 1;
  if(video) {
    plot(rbind(apply(ref, 2, min),
               apply(ref, 2, max),
               apply(cand, 2, min),
               apply(cand, 2, max)),
         type="n");
    rtd.lines(ref, col=1);
    rtd.lines(cand, lty=2, col=2);
    legend.text <- c("ref", "cand");
    legend.col <- c(1, 2);
    legend.lty <- c(1, 2);
  }
  for(i in 1:nrow(part)) {
    subset <- which(r.cand >= part[i,]$l.bound &
                    r.cand <= part[i,]$r.bound &
                    cand$x*epsilon <= part[i,]$x.bound &
                    cand$y*epsilon <= part[i,]$y.bound);
    if(length(subset) > 0) {
      e.set <- sapply(subset,
                      function(element) {
                        # prevent NaN
                        ratio <- function(a, b) {
                          return(ifelse(a==0 && b==0, 1, a/b));
                        }
                        return(max(epsilon,
                                   min(ratio(part[i,]$x.bound,
                                             cand[element,]$x),
                                       ratio(part[i,]$y.bound,
                                             cand[element,]$y))));
                      });
      epsilon <- max(e.set);
      if(video) {
        rtd.lines(cand*epsilon, lty=2+i, col=3);
        legend.text <- c(legend.text, format(epsilon, digits=2));
        legend.col <- c(legend.col, 3);
        legend.lty <- c(legend.lty, 2+i);
        the.point <- subset[which.max(e.set)];
        lines(rbind(cand[the.point,], cand[the.point,]*epsilon),
              lty=2+i, col=4);
      }
    }
  }

  if(video) {
    legend(0.7*max(ref$x), 0.9*max(ref$y),
           legend=legend.text, col=legend.col, lty=legend.lty);
  }
  return(epsilon);
  
}

rtd.lines <- function(z, ...) {
  lines(z, type="S", ...);
  text(z, rownames(z), ...);
}

rtd.plot <- function(z1, z2, epsilon=1, ...) {
  plot(rbind(0,
             apply(z1, 2, max),
             0,
             apply(z2*epsilon, 2, max)),
       type="n", axes=F, xlab="", ylab="", ...);
  lines(z1, type="S", lty=2, lwd=2);
  lines(z2, type="S", lty=3);
  text(z1$x, z1$y, toupper(rownames(z1)), pos=4);
  text(z2$x, z2$y, rownames(z2), pos=4);
  if(epsilon > 1) {
    lines(z2*epsilon, type="s", lty=3, col=3);
    text(z2*epsilon, rownames(z2), col=3);
  }
}


dominates<-function(z1=data.frame(x=sort(runif(10)), y=rev(sort(runif(10)))),
                    z2=data.frame(x=sort(runif(10)), y=rev(sort(runif(10))))) {
  return(all(z1$x <= z2$x) & all(z1$y <= z2$y));
}


rz <- function(n=10) {
  z.frame <- data.frame(x=sort(runif(n)), y=rev(sort(runif(n))));
  rownames(z.frame) <- letters[1:n];
  return(z.frame);
}

rrtd <- function(n=10, rtd.no=round(runif(1, 2, 20))) {
  rtd.names <- paste("X", 1:n, sep="");
  return(eval(parse(text=paste("list(",
                      paste(rtd.names, "= rz(rtd.no)",
                            collapse=", "), ")"))));
}

get.rtd <- function(limit=99, lowerbound=9999, scale.a=99, scale.b=9) {
  rtd <- as.numeric(system(paste("./rtd.sh",
                                 limit, lowerbound, scale.a, scale.b,
                                 "| tr ' ' '\n'"),
                           intern=TRUE));
  sz <- length(rtd);
  if(sz > 1) {
    return(list(time=rtd[seq(1, sz, 2)], quality=rtd[seq(2, sz, 2)]));
  } else {
    return(list(time=limit, quality=lowerbound));
  }
}

get.rtd.list <- function(design=expand.grid(
                           limit=60,
                           bound=seq(7777, 9999, 500),
                           sq.scale=seq(77, 99, 5),
                           rt.scale=6:10)) {
  return(eval(parse(text=
                    paste("list(",
                          paste(paste("X", rownames(design), sep=""),
                                "= get.rtd(",
                                paste(design$limit, design$bound,
                                      design$sq.scale, design$rt.scale,
                                      sep=", "),
                                ")",
                                collapse=", "),
                          ")"))));
}

rtd2z <- function(rtd, best.found=0) {
  return(data.frame(x= rtd$time,
                    y= 100 *
                    (rtd$quality-best.found)/ifelse(best.found==0,
                                                    1, best.found)));
}

find.best.sq <- function(rtd.list) {
  return(min(sapply(rtd.list,
                    function(rtd) {
                      return(rtd$quality[length(rtd$quality)]);
                    })));
}

plot.z.list <- function(z.list, ...) {
  plot(c(min(sapply(z.list,
                    function(z) {
                      return(z$x[1])
                    })),
         max(sapply(z.list,
                    function(z) {
                      return(z$x[length(z$x)])
                    }))),
       c(min(sapply(z.list,
                    function(z) {
                      return(z$y[length(z$y)])
                    })),
         max(sapply(z.list,
                    function(z) {
                      return(z$y[1])
                    }))),
       type="n", ...);
  for(z in z.list) {
    lines(z, type="S", lty=2);
  }
}

saaty <- function(reciprocal.paired.comparison.matrix) {
  eigen.matrix <- eigen(reciprocal.paired.comparison.matrix);
  lamda.max.idx <- which.max(eigen.matrix$values);
  lamda.max.vector <- eigen.matrix$vectors[,lamda.max.idx];
  return(lamda.max.vector/sum(lamda.max.vector));
}

ahp <- function(reciprocal.paired.comparison.matrix) {
  return(1/apply(reciprocal.paired.comparison.matrix, 2, sum));
}

epsilon.AB <- function(A=list(x=runif(1, 0.1, 0.5), y=runif(1, 0.1, 0.5)),
                       B=list(x=runif(1, 0.6, 0.9), y=runif(1, 0.6, 0.9)),
                       video=T) {
  epsilon <- min(B$x/A$x, B$y/A$y);
  if(video) {
    plot(c(0,B$x), c(0,B$y), type="n", xlab="", ylab="", axes=F);
    lines(data.frame(x=c(0,B$x, B$x), y=c(B$y, B$y, 0)), lty=2);
    lines(data.frame(x=c(0,2*epsilon*A$x), y=c(0, 2*epsilon*A$y)),
          lty=3, col=3);
    text(A, "A");
    text(B, "B");
    text(epsilon*A$x, epsilon*A$y, "A'");
    text(0, 0, "o");
  }
  return(epsilon);
}

epsilon.partition <- function(ref=rz(), cand, ...) {
  plot(c(0, max(ref$x)), c(max(ref$y), 0), type="n",
       axes=F, xlab="", ylab="", ...);
  lines(ref, type="S",lwd=2, lty=2);
  points(ref, pch=20);
  xes <- c(0, ref$x);
  ys <- c(ref$y,0);
  for(i in 1:length(xes)) {
    lines(data.frame(x=c(0, xes[i]), y=c(0, ys[i])), lty=3);
  }
  text(ref, LETTERS[1:nrow(ref)], pos=4);
  if(!missing(cand)) {
    text(cand, letters[1:nrow(cand)]);
  }
}
#epsilon.partition(ref); text(ref, LETTERS[1:10], pos=4); text(cand, letters[1:10])

epsilon.pairs <- function(ref=rz(), cand=rz()) {
  e.pairs <- NULL;
  epsilon <- 1;
  for(i in sample(1:nrow(cand))) {
    x <- cand[i,"x"];
    y <- cand[i,"y"];
    i.ref <- match.point(ref$x, ref$y, epsilon*x, epsilon*y);
    if(!is.na(i.ref)) {
      x.ref <- ref[i.ref,"x"];
      y.ref <- ref[i.ref,"y"];
      epsilon <- min(x.ref/x, y.ref/y);
      e.pairs <- rbind(e.pairs, t(c(i, epsilon)));
    }
  }
  if(!is.null(e.pairs)) {
    delta <- e.pairs[,2] - 1;
    e.pairs <- cbind(e.pairs, delta/max(delta));
    colnames(e.pairs) <- c("p", "e", "d"); 
  }
  
  return(data.frame(e.pairs));
}

plot.epsilon <- function(ref=rz(), cand=rz(), ...) {
  e.pairs <- epsilon.pairs(ref, cand);
  if(!is.null(e.pairs)) {
    plot(c(0, max(ref$x)), c(max(ref$y), 0),
                                        #rbind(ref, cand, max(e.pairs$e)*cand),
         type="n", axes=F, xlab="", ylab="", ...);
    lines(ref, type="S", lwd=2, lty=2);
                                        #points(ref, pch=20);
    lines(cand, type="S", lty=3);
                                        #points(cand, pch=20);
    prev=1;
    for(i in 1:nrow(e.pairs)) {
      epsilon <- e.pairs[i,"e"];
      proportion <- e.pairs[i,"d"];
      lines(epsilon*cand, type="S", lty=3);
                                        #points(epsilon*cand, pch=20);
      p <- e.pairs[i,"p"];
      text(prev*cand[p,],
           paste(letters[p], paste(rep("'", i-1), collape=""), sep=""));
      text(epsilon*cand[p,],
           paste(letters[p], paste(rep("'", i), collapse=""), sep=""));
      prev=epsilon;
    }
  }
}

epsilon.plot <- function(A=rz(), B=rz()) {
  par(mfrow=c(3,1));
  rtd.plot(A, B, main="traces");
  epsilon.partition(A, B, main="match");
  plot.epsilon(A, B, main="map");
  invisible(list(A=A, B=B));
}

read.rtd.log <- function(file.name) {
  stopifnot(file.exists(file.name));
  ncol <- max(as.numeric(system(paste("awk '{print NF}'",
                                      file.name), intern=T)));
  rtd.table <- read.table(file.name, col.names=1:ncol, fill=T);
  rtd.sz = nrow(rtd.table)/2;
  stopifnot(rtd.sz == nrow(rtd.table)%/%2);
  
  return(lapply(1:rtd.sz,
                function(idx) {
                  x <- ((idx-1)*2)+1;
                  y <- idx*2;
                  return(list(time=unlist(rtd.table[x,!is.na(rtd.table[x,])]),
                              quality=unlist(rtd.table[y,!is.na(rtd.table[y,
                                ])])));
                }));
}
