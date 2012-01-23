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

source("problem.R");

rnd.fiveseq <- function(inst = "inst60ef0119", n = 100, m = 10000,
                        env = '-S', obj = '-T',
                        as.file=TRUE, prefix = tempfile("seq")) {
  if(as.file) {
    system(paste("echo 0", round(m/4), round(m/2),
                 round(3*m/4), m-1, "| ./rnd.out -i",
                 inst, "-n", n, "-m", m, env, obj, 
                 "| split -1 -", prefix));
    return(paste(prefix, "a", letters[1:5], sep=""));
  } else {
    return(lapply(lapply(lapply(system(paste("echo 0",
                                             round(m/4), round(m/2),
                                             round(3*m/4), m-1,
                                             "| ./rnd.out -i",
                                             inst, "-n", n, "-m", m,
                                             env, obj),
                                       intern=TRUE),
                                strsplit, " "),
                         unlist),
                  as.numeric));
  }
}

rnd.quantile <- function(inst = "inst60ef0119", n = 100, m = 10000,
                         probs = seq(0, 1, 0.25)) {
  return(as.numeric(system(paste("echo",
                                 paste(round(probs*(m-1)), collapse=" "),
                                 "| ./rnd.out -v -i", inst,
                                 "-n", n, "-m", m), intern=T)));
}

rnd.huber <- function(n = 100, njobs=100, wdis="unif") {
  tf <- numeric(n);
  rdd <- numeric(n);
  mu <- numeric(n);
  s <- numeric(n);
  min <- numeric(n);
  max <- numeric(n);
  median <- numeric(n);
  mad <- numeric(n);
  
  for(i in 1:n) {
    tf[i] <- runif(1);
    rdd[i] <- runif(1);

    inst <- tempfile("inst");
    write.instance(getInstance(njobs=njobs, TF=tf[i], RDD=rdd[i], wdis=wdis),
                   inst);
    rnd <- rnd.quantile(inst=inst, n=njobs, probs=seq(0, 1, 0.001));
    hub <- huber(rnd);
    mu[i] <- hub$mu;
    s[i] <- hub$s;
    min[i] <- rnd[1];
    sz <- length(rnd);
    max[i] <- rnd[sz];
    median[i] <- rnd[round(sz/2)];
    mad[i] <- mad(rnd);
  }
  return(data.frame(tf, rdd, mu, s, min, max, median, mad));
}
