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

# $Id: problem.R,v 1.6 2004/01/02 16:32:19 mldb Exp $
rdd <- function(due.dates, makespan) {
  return((max(due.dates) - min(due.dates))/makespan);
}

tf <- function(due.dates, makespan) {
  return(1 - mean(due.dates)/makespan);
}

read.instance <- function(file = stdin()) {
  inst <- read.table(file, fill = T, row.names = c("n", "p", "w", "d"));
  n <- inst["n",1];
  m <- inst["m",2];
  if(is.na(m))
    m <- 1;
  p <- unlist(inst["p",]);
  names(p) <- NULL;
  w <- unlist(inst["w",]);
  names(w) <- NULL;
  d <- unlist(inst["d",]);
  names(d) <- NULL;
  return(list(n=n, p=p, w=w, d=d, m=m));
}

read.instance.Fm <- function(file = stdin()) {
  inst <- read.table(file, fill = T);
  n <- inst[1,1];
  m <- inst[1,2];
  p.matrix <- matrix(0, m, n);
  for(i in 1:n) {
    for(j in 1:m) {
      p.matrix[j,i] <- inst[i+1,j*2];
    }
  }
  
  w <- numeric(0);
  range <- ceiling(n/(2*m));
  for(i in (n+2):(n+1+range)) {
    w <- c(w, unlist(inst[i,]));
  }
  
  d <- numeric(0);
  for(i in (n+2+range):(n+1+2*range)) {
    d <- c(d, unlist(inst[i,]));
  }
  
  names(w) <- NULL;
  names(d) <- NULL;
  
  return(list(n=n, p=p.matrix, w=w[1:n], d=d[1:n], m = m));
}

read.instance.taillard <- function(number= round(runif(1, 51, 60)),
                                   TF = runif(1, 0, 1), RDD = runif(1, 0, 1),
                                   wdis = "unif", dir="../dat/") {
  inst <- read.table(paste(dir, "ta",
                           formatC(number, format="fg", flag="0", digits=2),
                           sep=""), fill = T);
  n <- inst[1,1];
  m <- inst[1,2];
  p.matrix <- matrix(0, m, n);
  for(i in 1:n) {
    for(j in 1:m) {
      p.matrix[j,i] <- inst[i+1,j*2];
    }
  }

  w <- switch(wdis,
		unif = round(runif(n, 1, 10)),
		common = rep(sample(10,1), n),
		exp = round(rexp(n, 1)+1),
		norm = round(rnorm(n, 5, 1)));
  
  bounds <- read.table(paste(dir, "makespan.Fm", sep=""))[number,];
  P <- ifelse(is.na(bounds$max), bounds$min, (bounds$min+bounds$max)/2);
  d <- round(runif(n, P*(1-TF-RDD/2), P*(1-TF+RDD/2)));

  return(list(n=n, p=p.matrix, w=w, d=d, m = m));
}

write.instance <- function(inst, ofs = stdout()) {
  cat(inst$n, ifelse(inst$m > 1, inst$m, ""), "\n",
      inst$p, "\n", inst$w, "\n", inst$d, "\n", file=ofs)
}

write.instance.Fm <- function(inst, ofs = stdout()) {
  cat(inst$n, inst$m, "\n", file=ofs);
  for(i in 1:inst$n) {
    for(j in 1:inst$m) {
      cat(j, inst$p[j,i], " ", file=ofs, append=TRUE);
    }
    cat("\n", file=ofs, append=TRUE);
  }
  cat(inst$w, "\n", inst$d, "\n", file=ofs,append=TRUE);
}
               
getInstance <- function(njobs = 40, TF = 0.2, RDD = 0.2, pdis = "unif", wdis = "unif", mProcs = 1) {
	p <- switch(pdis,
		unif = round(runif(njobs, 1, 100)),
		common = rep(sample(100, 1), njobs),
		exp = round(rexp(njobs, .1)+1),
		norm = round(rnorm(njobs, 50, 10)));	
	w <- switch(wdis,
		unif = round(runif(njobs, 1, 10)),
		common = rep(sample(10,1), njobs),
		exp = round(rexp(njobs, 1)+1),
		norm = round(rnorm(njobs, 5, 1)));
	P <- sum(p);
	d <- round(runif(njobs, P*(1-TF-RDD/2), P*(1-TF+RDD/2))/mProcs);
	return(list(n=njobs, p=p, w=w, d=d, m = mProcs));
}

getInstance.Fm <- function(n = 20, TF = 0.2, RDD = 0.2, pdis = "unif", wdis = "unif", m = 5) {
	p <- switch(pdis,
		unif = round(runif(n*m, 1, 100)),
		common = rep(sample(100, 1), n*m),
		exp = round(rexp(n*m, .1)+1),
		norm = round(rnorm(n*m, 50, 10)));
	p.matrix <- matrix(0, m, n);
	for(i in 1:m) {
		for(j in 1:n) {
			p.matrix[i,j] <- p[i*j];
		}
	}
	w <- switch(wdis,
		unif = round(runif(n, 1, 10)),
		common = rep(sample(10,1), n),
		exp = round(rexp(n, 1)+1),
		norm = round(rnorm(n, 5, 1)));
	P <- sum(p); # != cmax (unlike single machine problems)
	d <- round(runif(n, P*(1-TF-RDD/2), P*(1-TF+RDD/2))/m);
	
	return(list(n=n, p=p.matrix, w=w, d=d, m = m));
}

eval.S <- function(seq, p, w, d, obj = FALSE) {
  tT <- 0;
  Cj <- 0;
  for(job in seq) {
    Cj <- Cj + p[job];
    tardiness <- max(Cj - d[job], 0);
    if(tardiness > 0) {
      tT <- tT + w[job]*ifelse(obj, tardiness, 1);
    }
  }
  return(tT);
}

eval.Pm <- function(seq, p, w, d, m, obj = FALSE) {
	proc <- rep(0, m);
	names(proc) <- 1:m;
	tT <- 0;

	for(job in seq) {
		front <- sort(proc)[1];
		idx <- as.numeric(names(front));
		proc[idx] <- front + p[job];
		
		tardiness <- max(proc[idx] - d[job], 0);
		if(tardiness > 0) {
			tT <- tT + w[job] * ifelse(obj, tardiness, 1);
		}
	}
	return(tT);
}

getTime.Pm <- function(order, p, w, d, m) {
	n <- length(p);

	mach <- numeric(m);
	names(mach) <- 1:m;

	for(job in order[order > 0]) {
		front <- sort(mach)[1];
		idx <- as.numeric(names(front));
		mach[idx] <- front + p[job];
	}
	return(min(mach));
}



eval.Fm <- function(order, p, w, d, obj = FALSE) {
    m <- dim(p)[1];
    n <- dim(p)[2];

    # See Pinedo, p.94
    # NB rows ordered according to sequence

    ptrans <- matrix(0, m, n);
    for(i in 1:n) { 
        ptrans[,i] <- p[,order[i]];
    }

    cost <- matrix(0, m, n);

    for(i in 1:m) {
	cost[i,1] <- sum(ptrans[,1][1:i]);
    }
    for(k in 1:n) { 
	cost[1,k] <- sum(ptrans[1,][1:k]);
    }
    for(i in 2:m) { 
	for(k in 2:n) { 
	    cost[i,k] <- max(cost[i-1,k], cost[i,k-1]) + ptrans[i,k]; 
	} 
    }

    # compute total tardiness
    dtrans <- numeric(n);
    wtrans <- numeric(n);
    for(i in 1:n) {
	dtrans[i] <- d[order[i]];
	wtrans[i] <- w[order[i]];
    }	
    lateness <- (cost[m,] - dtrans)*wtrans;

    return(ifelse(obj, sum(lateness[lateness>0]), 
	length(lateness[lateness>0])));
    
}

getTime.Fm <- function(order, p, w, d) {
    m <- dim(p)[1];
    n <- length(order);

    # See Pinedo, p.94
    # NB rows ordered according to sequence

    ptrans <- matrix(0, m, n);
    for(i in 1:n) { 
        ptrans[,i] <- p[,order[i]];
    }

    cost <- matrix(0, m, n);

    for(i in 1:m) {
	cost[i,1] <- sum(ptrans[,1][1:i]);
    }
    for(k in 1:n) { 
	cost[1,k] <- sum(ptrans[1,][1:k]);
    }
    for(i in 2:m) { 
	for(k in 2:n) { 
	    cost[i,k] <- max(cost[i-1,k], cost[i,k-1]) + ptrans[i,k]; 
	} 
    }
    return(cost);	
}

getTestCase <- function(param, seed=NULL) {
	if(is.numeric(seed)) {
		set.seed(seed);
	}
	return(getInstance(param$n, param$tf, param$rdd, param$p, param$w, param$m));
}




write.inst <- function(specs = foobar(1)[1,], file.name = "") {
  tf <- specs$tf;
  rdd <- specs$rdd;
  env <- specs$env;
  n <- specs$n;
  m <- specs$m;
  wght <- specs$weight;
  
  if(env=="Fm") {
    random = round(runif(1, 1, 10));
    instance <- read.instance.taillard(number = random+ifelse(n==50, 50, 80),
                                       TF = tf, RDD = rdd,
                                       wdis = wght,
                                       dir="../dat/");
    write.instance.Fm(instance, file.name);
  } else {
    write.instance(getInstance(njobs=n, TF=tf, RDD=rdd,
                               wdis= wght,
                               mProcs=m),
                   file.name);
  }
}

foobar <- function(n) {
  skeleton <- expand.grid(tfrdd=1:n, class=1:6, weight=c("unif","common"));
  skeleton[,"tf"] <- runif(n)[skeleton$tfrdd];
  skeleton[,"rdd"] <- runif(n)[skeleton$tfrdd];
  skeleton[,"env"] <- c("S", "Pm", "Pm", "Pm", "Fm", "Fm")[skeleton$class];
  skeleton[,"n"] <- c(rep(100, 4), 50, 100)[skeleton$class];
  skeleton[,"m"] <- c(1:4, 20, 20)[skeleton$class];
  return(skeleton[,c(4:8,3)]);
}

#skeleton <- foobar(100)
#for(i in 1:nrow(skeleton)) { write.inst(skeleton[i,], paste("/tmp/inst", i, sep=".")); }
