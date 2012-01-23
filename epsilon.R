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

# $Id: epsilon.R,v 1.10 2004/02/10 09:54:10 mldb Exp $ #
find.best.solution.quality <- function(rtd.list) {
  return(min(sapply(rtd.list,
                    function(rtd) {
                      return(rtd$quality[length(rtd$quality)]);
                    })));
}

find.worst.solution.quality <- function(rtd.list) {
  return(max(sapply(rtd.list,
                    function(rtd) {
                      return(rtd$quality[1]);
                    })));
}

scale.rtd <- function(rtd, min.rt=0, max.rt=60, min.sq=0, max.sq=1000,
                      scaling.factor=1) {
  return(data.frame(x= 1e-6 + rtd$time,# - min.rt)/(max.rt - min.rt),
                    y= 1e-6 + sqrt(
                      100 *
                      (rtd$quality - min.sq)/ifelse(min.sq==0, 1, min.sq) *
                      scaling.factor)));
}

match.point <- function(X, Y, x, y) {
  r <- x/y;
  lo <- 1;
  up <- length(X);
  while(lo < up) {
    if(y < Y[lo] && x < X[up]) {
      if(r > X[lo]/Y[lo+1]) {
        if(r < X[up-1]/Y[up]) {
          up <- up - 1;
          lo <- lo + 1;
        } else {
          if(y < Y[up]) {
            return(up);
          } else {
            return(NA);
          }
        }
      } else {
        if(x < X[lo]) {
          return(lo);
        } else {
          return(NA);
        }
      }
    } else {
      return(NA);
    }
  }
  if(length(X) > 1) {
    return(up);
  } else {
    return(NA);
  }
}

match.point.recursively <- function(X, Y, x, y, r=x/y, lo=1, up=length(X)) {
  ##  if(lo - 1 < 1 || up + 1 > length(X)) {
  ##    stopifnot(lo > 0, up < length(X) + 1,
  ##              length(X)==length(Y), x >= 0, y >= 0);
  ##  } else {
  ##    stopifnot(y < Y[lo-1], x < X[up+1],
  ##              r > X[lo-1]/Y[lo], r < X[up]/Y[up+1]);
  ##  }

  ## stopping criterion
  if(up - lo < 1) {
    if(length(X) > 1) {
      return(up);
    } else {
      return(NA);
    }
  } else {
    if(y < Y[lo] && x < X[up]) {
      if(r > X[lo]/Y[lo+1]) {
        if(r < X[up-1]/Y[up]) {
          return(Recall(X, Y, x, y, r, lo+1, up-1));
        } else {
          if(y < Y[up]) {
            return(up);
          } else {
            return(NA);
          }
        }
      } else {
        if(x < X[lo]) {
          return(lo);
        } else {
          return(NA);
        }
      }
    } else {
      return(NA);
    }
  }
}


epsilon.rtd <- function(cand, ref) {
  epsilon <- 1;
  for(i in sample(1:nrow(cand))) {
    x <- cand[i,"x"];
    y <- cand[i,"y"];
    i.ref <- match.point(ref$x, ref$y, epsilon*x, epsilon*y);
    if(!is.na(i.ref)) {
      x.ref <- ref[i.ref,"x"];
      y.ref <- ref[i.ref,"y"];
      epsilon <- min(x.ref/x, y.ref/y);
    }
  }
  return(epsilon);
}
    
epsilon.matrix <- function(rtd.list) {
  rtd.no <- length(rtd.list);
  rtd.names <- names(rtd.list);
      
  e.matrix <- data.frame(matrix(NA, rtd.no, rtd.no));
  if(!is.null(rtd.names)) {
    names(e.matrix) <- rownames(e.matrix) <- rtd.names;
  }
      
  for(i in 1:rtd.no) {
    for(j in 1:rtd.no) {
      if(i != j) {
        e.matrix[i,j] <- epsilon.rtd(rtd.list[[i]], rtd.list[[j]]);
      } else {
        e.matrix[i,j] <- 1;
      }
    }
  }
  return(e.matrix);
}

ratio.matrix <- function(paired.comparisons) {
   return(sapply(1:nrow(paired.comparisons),
                 function(i) {
                   unlist(paired.comparisons[,i]/paired.comparisons[i,]);
                 }));
}

## in this case, small means better
ratios2weights <- function(ratios) {
  return(1/apply(ratios, 1, sum));
}
