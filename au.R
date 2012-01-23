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

#!/usr/bin/R BATCH --no-restore
# $Id: au.R,v 1.5 2004/03/17 22:09:13 mldb Exp $
source("race.R");
source("problem.R");
source("dispatch.R");

au.params <- 2^seq(-10,10,.5);

#expA <- expand.grid(tf = seq(1,9)/10, rdd = seq(1,9)/10, p = "unif", w = c("common","unif"), n=100, obj = c(T,F));
#exp0 <- expand.grid(tf = 0.5, rdd = 0.5, p = "unif", w = "unif", n = 10, obj = F);

#expB <- expand.grid(tf = 0.5, rdd = 0.5, p = c("exp", "norm"), w = c("norm", "unif"), n = 100, obj = T);

#expC <- expand.grid(tf = 0.5, rdd = 0.5, p = "unif", w = "unif", n = seq(10,100,10), obj = T);

#expD <- expand.grid(tf = seq(2,8,2)/10, rdd = seq(2,8,2)/10, p = c("exp", "norm"), w = "unif", n = 100, obj = T);

#expE <- expand.grid(tf = seq(2,8,2)/10, rdd = seq(2,8,2)/10, p = "unif", w = "unif", n = c(50, 150), obj = T);

#expF <- expand.grid(tf = seq(2,8,2)/10, rdd = seq(2,8,2)/10, p = "unif", w = "unif", n = 100, obj = T, m = seq(2,10,2));

#expG <- expand.grid(tf = seq(2,8,2)/10, rdd = seq(2,8,2)/10, p = c("unif", "exp", "norm"), w = c("common", "unif"), n = c(25, 50, 100), m = seq(2,10,2), env = c("Pm", "Fm", "S"), obj= c(T,F));

expFm <- expand.grid(tf = seq(0.2, 1, 0.2), rdd = seq(0.2, 1, 0.2), p = "unif",w = c("common", "unif"), n = c(50, 100), m = 20, env = "Fm", obj = c(T, F));

expP <- expand.grid(tf = seq(0.2, 1, 0.2), rdd = seq(0.2, 1, 0.2), p = "unif",w = c("common", "unif"), n = 100, m = 2:4, env = "Pm",  obj = c(T, F));

expS <- expand.grid(tf = seq(0.2, 1, 0.2), rdd = seq(0.2, 1, 0.2), p = "unif",w = c("common", "unif"), n = 100, m = 1, env = "S",  obj = c(T, F));

#for(experimentList in list(expP, expS)) {
#experimentList <- expand.grid(tf = seq(0.2, 1, 0.2), rdd = seq(0.2, 1, 0.2),
#                              p = "unif", w = c("common", "unif"), n = 100,
#                              m = 3, env = "Pm",  obj = F);

experimentList <- expFm; #<- rbind(expS, expP, expFm);
exp.sz <- nrow(experimentList);
experimentList[,"k.best"] <- rep(NA, exp.sz);
experimentList[,"k.min"] <- rep(NA, exp.sz);
experimentList[,"k.max"] <- rep(NA, exp.sz);

bname <- as.character(experimentList$env[1]);

Instance <- sample(1000);

no.exps <- dim(experimentList)[1];
for(i in 1:no.exps) {
    experiment <- experimentList[i,];
    print(experiment);
    ofs <- paste(bname, i, sep=".");

    au.race.wrapper <- function(candidate=NULL, testCase=NULL) {
	kvals <- au.params;
	no.candidates <- length(kvals);
	no.testCases <- 100;
	id <- "AU"

	if(is.null(candidate) && is.null(testCase)) {
		return(list(no.candidates=no.candidates,
			    no.testCases=no.testCases,
			    program.name=id,
			    precis="Apparent Urgency Dispatching\n"));
	} else if(is.numeric(candidate)&&is.numeric(testCase)&&
		  (candidate==as.integer(candidate))&&
		  (testCase==as.integer(testCase))&&
		  (length(candidate)==1)&&(length(testCase)==1)&&
		  (candidate>=1)&&(testCase>=1)&&
		  (candidate<=no.candidates)&&(testCase<=no.testCases)) {

          seed <- Instance[testCase];
          set.seed(seed);
          task <- testCase;
          if(experiment$env=="S") {
            i <- getInstance(experiment$n, experiment$tf, experiment$rdd,
                             experiment$p, experiment$w, 1);
            au.seq <- au(i$p, i$w, i$d, kvals[candidate]);
            return(list(output=eval.S(au.seq, i$p, i$w, i$d,
                          experiment$obj)));
          } else if(experiment$env=="Pm") {
            i <- getInstance(experiment$n, experiment$tf, experiment$rdd,
                             experiment$p, experiment$w, experiment$m);
            au.seq <- au.Pm(i$p, i$w, i$d, i$m, kvals[candidate]);
            return(list(output=eval.Pm(au.seq, i$p, i$w, i$d, i$m,
                          experiment$obj)));
          } else {
            random = sample(1:10, 1);
            i <- read.instance.taillard(number=random+
                                        ifelse(experiment$n==50, 50, 80),
                                        TF = experiment$tf,
                                        RDD = experiment$rdd,
                                        wdis = experiment$w,
                                        dir="taillard");

            au.seq <- au.Fm(i$p, i$w, i$d, kvals[candidate]);

            return(list(output=eval.Fm(au.seq, i$p, i$w, i$d,
                           experiment$obj)));
          }
                
	} else{
          stop("Something wrong in race.wrapper's arguments");
	}
    }



    race.out <- race(au.race.wrapper,
		    length(au.params)*10,
		    first.test=5);
    save(race.out, file=paste(ofs, "out", sep="."));
    experimentList[i,"k.best"] <- au.params[race.out$selected];
    experimentList[i,"k.min"] <- au.params[min(which(race.out$alive))];
    experimentList[i,"k.max"] <- au.params[max(which(race.out$alive))];
    save(experimentList, file="auFm.race.out", compress=T);
}
#}

