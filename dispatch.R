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

# $Id: dispatch.R,v 1.4 2004/01/06 21:31:23 mldb Exp $
edd <- function(d) {
  names(d) <- seq(1, length(d));
  return(as.numeric(names(sort(d))));
}

ms <- function(p, d) {
  n <- length(p);
  seq <- 1:n;
  t <- 0;
  todo <- rep(TRUE, n);
  for(idx in 1:n) {
                                        # find min slack
    mnslck <- Inf;
    minjob <- n+1;
    for(job in 1:n) {
      if(todo[job]) {
        slack <- max(d[job] - p[job] - t, 0);
        if(slack < mnslck) {
          mnslck <- slack;
          minjob <- job;
        }
      }
    }
    seq[idx] <- minjob;
    t <- t + p[minjob];
    todo[minjob] <- FALSE;
  }
  return(seq);
}

ms.Pm <- function(p, d, m) {
  n <- length(p);
  order <- numeric(n);
  todo <- rep(TRUE, n);

  mfront <- numeric(m);
  names(mfront) <- 1:m;

  for(idx in 1:n) {
                                        # find min slack
    mnslck <- Inf;
    minjob <- 0;
    t <- sort(mfront)[1];

    for(job in 1:n) {
      if(todo[job]) {
        slack <- max(d[job] - p[job] - t, 0);

        if(slack < mnslck) {
          mnslck <- slack;
          minjob <- job;
        }
      }
    }
    order[idx] <- minjob;
    mfront[as.numeric(names(t))] <- t + p[minjob];
    todo[minjob] <- FALSE;
  }
  return(order);	
}

ms.Fm <- function(p, d) {
  m <- dim(p)[1];
  n <- dim(p)[2];

  order <- numeric(n);
  todo <- rep(TRUE, n);
  cost <- matrix(NA, m, n);

                                        # first job
  slack <- (d - apply(p, 2, sum));
  names(slack) <- 1:n;
  mnslck <- sort(slack)[1];
  minjob <- as.numeric(names(mnslck));
    
  order[1] <- minjob;
  todo[minjob] = FALSE;    
	
  for(i in 1:m) {
    cost[i,1] <- sum(p[,minjob][1:i]);
  }

  for(idx in 2:n) {
                                        # find min slack
    mnslck <- Inf;
    minjob <- Inf;
    mincost <- numeric(m);
	
    for(job in 1:n) {
      if(todo[job]) {
        tmpcost <- numeric(m);
        tmpcost[1] <- p[1,job];

        for(machine in 2:m) {

          tmpcost[machine] <- max(cost[machine, idx-1],
                                  tmpcost[machine-1]) + 
                                    p[machine,job];
        }
        slack <- max(d[job] - tmpcost[m], 0);

        if(slack < mnslck) {
          mnslck <- slack;
          minjob <- job;
          mincost <- tmpcost;
        }
      }

    }
    order[idx] <- minjob;
	
    cost[,idx] <- mincost;

    todo[minjob] <- FALSE;
  }

  return(order);
}

mdd <- function(p, d) {
  n <- length(p);
  seq <- 1:n;
  t <- 0;
  todo <- rep(TRUE, n);
  for(idx in 1:n) {
                                        # find min slack
    mnslck <- Inf;
    minjob <- n+1;
    for(job in 1:n) {
      if(todo[job]) {
        slack <- max(d[job] - p[job] - t, 0);
        mod <- ifelse(slack > 0,  d[job], p[job]+t);
        if(mod < mnslck) {
          mnslck <- mod;
          minjob <- job;
        }
      }
    }
    seq[idx] <- minjob;
    t <- t + p[minjob];
    todo[minjob] <- FALSE;
  }
  return(seq);
}

wspt <- function(p, w) {
  wp <- p/w;
  names(wp) <- 1:length(wp);
  as.numeric(names(sort(wp)));
}

au.thumb <- function() { 
  k.tf <- numeric(5);
	
  i <- 1;
  for(tf in seq(0.2, 1, 0.2)) {
    k.tf[i] <- ifelse(tf >0.4, 2.0, ifelse(tf <0.3, 0.5, 0.9));
    i <- i+1;
  }
  return(rep(k.tf,5));
}

au.lm <- function(specification) {
  tf <- specification$tf;
  rdd <- specification$rdd;
  obj <- specification$obj;
  w <- specification$w;
  return(2^(6.061291 +
            -26.055721*(tf-0.5)^2 +
            -2.831333*tf +
            -8.110665*rdd +
            -2.475556*ifelse(obj, 1, 0) +
            0.107778*ifelse(w, 1, 0) +
            0.005100*specification$n +
            21.238426*(tf-0.5)^2*tf +
            -7.415675*(tf-0.5)^2*rdd +
            ifelse(obj && w, -0.561111, 0)));
}

au.rpart <- function(specification) {
  obj <- specification$obj;
  tf <- specification$tf;
  rdd <- specification$rdd;

  return(2^(ifelse(obj,
                   ifelse(rdd >= 0.3,
                          -0.7395833,
                          ifelse(tf >=0.9,
                                 -2.0972220,
                                 2.0208330)),
                   ifelse(tf < 0.3,
                          ifelse(rdd >= 0.5,
                                 -2.9953700,
                                 1.7847220),
                          3.2756940))));
}

au.k <- function(tf,rdd, k = au.thumb()) { 
  return(k[round((tf*10)/2)*round((rdd*10)/2)]);
}

au <- function(p, w, d, k=0.5) {
  n <- length(p);
  wp <- w/p;
  seq <- 1:n;
  t <- 0;
  todo <- rep(TRUE, n);
  for(idx in 1:n) {
    mx <- -Inf;
    mjb <- n+1;
    pavg <- mean(p[todo==TRUE]);
    for(job in (1:n)[todo==TRUE]) {
      slack <- max(d[job] - p[job] - t, 0);
      tmp <- (w[job]/p[job])*exp(-(slack/(k*pavg)))
      if(tmp > mx) {
        mx <- tmp;
        mjb <- job;
      }

    }
    seq[idx] <- mjb;
    t <- t + p[mjb];
    todo[mjb] <- FALSE;
  }
  return(seq);
}

au.Pm <- function(p, w, d, m, k=0.5) {
  n <- length(p);
  seq <- 1:n;

  todo <- rep(TRUE, n);

  mfront <- numeric(m);
  names(mfront) <- 1:m;

  for(idx in 1:n) {
    mx <- -Inf;
    mjb <- n+1;
    pavg <- mean(p[todo==TRUE]);
		
    t <- sort(mfront)[1];

    for(job in (1:n)[todo==TRUE]) {
      slack <- max(d[job] - p[job] - t, 0);
      tmp <- (w[job]/p[job])*exp(-(slack/(k*pavg)))
      if(tmp > mx) {
        mx <- tmp;
        mjb <- job;
      }

    }
    seq[idx] <- mjb;
    mfront[as.numeric(names(t))] <- t + p[mjb];
    todo[mjb] <- FALSE;
  }
  return(seq);
}

au.Fm <- function(p, w, d, k=0.5) {
  m <- dim(p)[1];
  n <- dim(p)[2];

  order <- numeric(n);
  todo <- rep(TRUE, n);
  cost <- matrix(NA, m, n);

                                        # first job
  tproc <- apply(p, 2, sum);
  urgency <- numeric(n);
  pavg <- mean(tproc);
  for(i in 1:n) {
    urgency[i] <- (w[i]/tproc[i])*exp(- max(d[i] - tproc[i], 0)/(k*pavg));
  }
  names(urgency) <- 1:n;
  mx <- sort(urgency)[n];
  minjob <- as.numeric(names(mx));
    
  order[1] <- minjob;
  todo[minjob] = FALSE;    
	
  for(i in 1:m) {
    cost[i,1] <- sum(p[,minjob][1:i]);
  }

  for(idx in 2:n) {
                                        # find min slack
    mx <- -Inf;
    minjob <- Inf;
    mincost <- numeric(m);
		
    pavg <- sum(p[todo==TRUE])/(n+1-idx);
    for(job in 1:n) {
      if(todo[job]) {
        tmpcost <- numeric(m);
        tmpcost[1] <- p[1,job];

        for(machine in 2:m) {

          tmpcost[machine] <- max(cost[machine, idx-1],
                                  tmpcost[machine-1]) + 
                                    p[machine,job];
        }
        slack <- max(d[job] - tmpcost[m], 0);
		
        urgency <- (w[job]/tmpcost[m])*exp(- slack/(k*pavg));
        if(urgency > mx) {
          mx <- urgency;
          minjob <- job;
          mincost <- tmpcost;
        }
      }

    }
    order[idx] <- minjob;
	
    cost[,idx] <- mincost;

    todo[minjob] <- FALSE;
  }

  return(order);	
}
