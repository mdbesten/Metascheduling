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

# ---------------------------------------- -*- mode: r; mode: font-lock -*- #
# $Id: eRace.R,v 1.9 2004/02/18 16:48:40 mldb Exp $ #

## Configuration variables
.race.warn.quiet<--1
.race.warn.level<-1
.slave.init.function<-"race.init"
.slave.destruct.function <- "race.cleanup"
.slave.wrapper.function<-"race.wrapper"
.slave.info.function<-"race.info"
.slave.describe.function<-"race.describe"
.title.width<-30
.value.width<-33

.race.warn.save<-getOption("warn")
options(warn=.race.warn.quiet)
options(warn=.race.warn.save)

## The master:
race<-function(wrapper.file=stop("Argument \"wrapper.file\" is mandatory"),
               maxExp=0,
               stat.test=c("friedman","t.bonferroni","t.none"),
               conf.level=0.95,
               first.test=5,
               interactive=TRUE,
               log.file="",
               weight=1,
               ...){

  #############################################################################
  
  epsilon.wrapper <- function(candidate.vector, instance.id, race.data,
                              weight) {
    if(!is.null(race.data$boot.record) &&
       !(nrow(race.data$boot.record) < instance.id)) {

      stopifnot(all(!is.na(race.data$boot.record[instance.id,
                                                 candidate.vector])));

      return(race.data$boot.record[instance.id, candidate.vector]);

    } else {
      ## some internal functions
      source("epsilon.R", local=TRUE);
      
      ## collect run time distributions
      rtd.list <- lapply(candidate.vector,
                         function(current.candidate) {
                           return(do.call(.slave.wrapper.function,
                                          list(current.candidate,
                                               instance.id, race.data)));
                         });
      
      ## scale data
      min.sq <- find.best.solution.quality(rtd.list);
      max.sq <- find.worst.solution.quality(rtd.list);
      min.rt <- 0;
      max.rt <- race.data$time.limit;
      
      if(min.sq < max.sq) {
        z.list <- lapply(rtd.list, scale.rtd,
                         min.rt=min.rt, max.rt=max.rt,
                         min.sq=min.sq, max.sq=max.sq,
                         scaling.factor=weight);
        
        ## compute relative performance
        results <- ratios2weights(ratio.matrix(epsilon.matrix(z.list)));
        
        ## no. candidates should not matter
        ## (perhaps useful in case of t.test)
        avg.result <- 1/length(candidate.vector);
        results <- (results - avg.result)/avg.result;
        
        return(results);
      } else {
        return(rep(0, length(rtd.list)));
      }
    }
  }
  #############################################################################
  
  
  timestamp.start<-date()

  # Change warning behavior
  .race.warn.save<-getOption("warn")
  on.exit(options(warn=.race.warn.save))
  options(warn=.race.warn.level)
  
  # Check argument: wrapper.file
  wrapper.file # Just to check if it is defined
  if (!is.character(wrapper.file))
    stop("Option \"wrapper.file\" must be a string")
  wrapper.file<-path.expand(wrapper.file)       
  if (substr(wrapper.file,1,1)!="/")
    wrapper.file<-file.path(getwd(),wrapper.file)
  try(source(wrapper.file,local=TRUE),silent=TRUE)
  if (!exists(.slave.wrapper.function,inherits=FALSE,mode="function")||
      !exists(.slave.info.function,inherits=FALSE,mode="function"))
    stop(paste("Either file \"",wrapper.file,"\" does not exist,\n",
               "or it does not define properly the wrapper \"",
               .slave.wrapper.function,"\"\n",
               "and/or the function \"",
               .slave.info.function,"\"",
               sep=""))
  
  # Check argument: maxExp
  if (!missing(maxExp) &&
      (!is.numeric(maxExp) ||
       length(maxExp)!=1 ||
       !is.finite(maxExp)))
    stop("maxExp must be an single number")
  maxExp<-ifelse(maxExp>0,maxExp,0)
  maxExp<-floor(maxExp)

  # Check argument: stat.test
  stat.test<-match.arg(stat.test)

  # Check argument: conf.level
  if (!missing(conf.level) &&
      (!is.numeric(conf.level) || length(conf.level)!=1 ||
       !is.finite(conf.level) || conf.level<0 || conf.level>1)) 
    stop("conf.level must be a single number between 0 and 1")

  # Check argument: first.test
  if (!missing(first.test) &&
      (!is.numeric(first.test) ||
       length(first.test)!=1 ||
       !is.finite(first.test)))
    stop("first.test must be an single number")
  first.test<-ifelse(first.test>0,first.test,0)
  first.test<-floor(first.test)

  # Check argument: interactive
  if (!missing(interactive) &&
      (!is.logical(interactive) || length(interactive)!=1))
    stop("interactive must be a logical")

  # Check argument: log.file  
  if (!missing(log.file) &&
      (system(paste("touch",log.file),ignore.stderr=TRUE)!=0))
    stop(paste("I cannot create file ",log.file,sep=""))
  
  # Check argument: weight
  if (!missing(weight) &&
      (!is.numeric(weight) ||
       length(weight)!= 1 ||
       !is.finite(weight) ||
       weight != 0))
    stop("weight must be a single non-zero number");
  
  # Run init function (if any defined in wrapper.file)
  if (exists(.slave.init.function,inherits=FALSE,mode="function")){
    race.data<-do.call(.slave.init.function,list(...))
    if(!is.list(race.data))
      stop(paste("Error while running",.slave.init.function))
    precis.init<-TRUE
  }else{
    race.data<-list()
    precis.init<-FALSE
  }
 
  # Collect info on race from wrapper
  race.info<-do.call(.slave.info.function,list(race.data))

  # Check race.info
  if (# race.info$race.name must be a string
      is.na(match("race.name",names(race.info)))||
      !is.character(race.info$race.name)||
      length(race.info$race.name)!=1 ||
      # race.info$no.candidates must be an integer
      is.na(match("no.candidates",names(race.info)))||
      !is.numeric(race.info$no.candidates) ||
      length(race.info$no.candidates)!=1 ||
      !is.finite(race.info$no.candidates) ||
      race.info$no.candidates!=as.integer(race.info$no.candidates) ||
      # race.info$no.tasks must be an integer
      is.na(match("no.tasks",names(race.info)))||
      !is.numeric(race.info$no.tasks) ||
      length(race.info$no.tasks)!=1 ||
      !is.finite(race.info$no.tasks) ||
      race.info$no.tasks!=as.integer(race.info$no.tasks)||
      # race.info$no.subtasks is a non-compulsory integer.
      (!is.na(match("no.subtasks",names(race.info)))&&
      (!is.numeric(race.info$no.subtasks) ||
       (length(race.info$no.subtasks)!=1 &&
        length(race.info$no.subtasks)!=race.info$no.tasks) ||
       any(!is.finite(race.info$no.subtasks)) ||
       any(race.info$no.subtasks!=as.integer(race.info$no.subtasks))))||
      # race.info$extra is a non-compulsory string or paragraph.
      (!is.na(match("extra",names(race.info)))&&
       !is.character(race.info$extra)))
    stop(paste("Function \"",.slave.wrapper.function,
               "\" returned an invalid object",sep=""))

  # Default for no.subtasks
  if (is.na(match("no.subtasks",names(race.info))))
    race.info$no.subtasks<-1

  # copy race.info contents to workspace for convenience
  race.name<-race.info$race.name
  no.candidates<-race.info$no.candidates
  no.tasks<-race.info$no.tasks
  no.subtasks<-race.info$no.subtasks   
  
  # Prepare a precis for documentation
  format.precis<-function(title,value){
    dots<-function(n)
      return(paste(rep(".",n),collapse=""))
    spaces<-function(n)
      return(paste(rep(" ",n),collapse=""))
    string<-paste(title,dots(.title.width-nchar(title)),sep="")
    if (nchar(value)<=.value.width){
      string<-paste(string,dots(.value.width-nchar(value)),value,sep="")
    }else{
      f.vec<-strwrap(value,width=.value.width)
      first<-f.vec[1]
      first<-paste(dots(.title.width-nchar(first)),first,sep="")
      rest<-format(f.vec[-1])
      rest<-paste(spaces(.title.width+.value.width-max(nchar(rest))),
                  rest,sep="",collapse="\n")
      string<-paste(string,paste(first,rest,sep="\n"),sep="")
    }
    return(paste(string,"\n"))
  }

  precis<-paste("\n",
                "Racing methods for the selection of the best\n",
                "Copyright (C) 2003 Mauro Birattari & Matthijs den Besten\n",
                "This software comes with ABSOLUTELY NO WARRANTY\n\n",
                format.precis("Race name",race.name),
                format.precis("Number of candidates",no.candidates),
                format.precis("Number of available tasks",no.tasks),
                ifelse(length(no.subtasks)>1,
                       format.precis("Subtasks per task","task-dependent"),
                       ifelse(no.subtasks>1,
                              format.precis("Subtasks per task",no.subtasks),
                              "")),
                format.precis("Max number of experiments",
                              ifelse(maxExp,maxExp,"unlimited")),
                format.precis("Statistical test",
                              switch(stat.test,
                                     friedman="Friedman test",
                                     t.bonferroni=paste("t-test with",
                                       "Bonferroni correction",
                                       "for multiple comparisons"),
                                     t.none=paste("t-test with",
                                       "no correction",
                                       "for multiple comparisons"))),
                format.precis("Tasks seen before discarding",first.test),
                format.precis("Initialization function",
                              ifelse(precis.init,"ok","none found")),
                sep="")

  if (!is.null(race.info$extra)){
    extra<-paste(strwrap(race.info$extra,width=60,prefix="\t"),collapse="\n")
    precis<-paste(precis,"\n",extra,"\n")
  }


  # Print out precis if interactive
  if (interactive) 
    cat(paste(precis,"\n\n"))

  check.result<-function(result){
    expected.length<-ifelse(length(no.subtasks)==1,
                            no.subtasks,
                            no.subtasks[current.task])
    if (length(result)!=expected.length)
      stop(paste("Bad output returned by \"",
                 .slave.wrapper.function,"\"",sep=""))
  }

    
  # Initialize some variables...
  Tasks<-1:no.tasks
  Results<-matrix(data=NA,
                  nrow=ifelse(length(no.subtasks)==1,
                    no.tasks*no.subtasks,sum(no.subtasks)),
                  ncol=no.candidates)
  alive<-array(TRUE,no.candidates)
  no.experiments.sofar<-0
  no.subexperiments.sofar<-0
  best<-0
  no.tasks.sofar<-0
  no.subtasks.sofar<-0
  
  # Define some functions...
  log.list<-function(end=FALSE){
    timestamp.current<-date()
    log<-list(precis=precis,
              results=Results[1:no.subtasks.sofar,],
              no.candidates=no.candidates,
              no.tasks=no.tasks.sofar,
              no.subtasks=no.subtasks,
              no.experiments=no.experiments.sofar,
              no.alive=sum(alive),
              alive=alive,
              best=best,
              mean.best=mean.best,
              timestamp.start=timestamp.start)
    if (end)
      log<-c(log,list(timestamp.end=timestamp.current,
                      description.best=description.best))
    else
      log<-c(log,list(timestamp.current=timestamp.current))
    return(log)
  }
  
  logger<-function(){
    if (log.file!=""){
      log<-log.list()
      save(log,file=log.file)
    }
  }
   
  aux2.friedman<-function(y,I=1:ncol(y),n=nrow(y),conf.level=0.95){
    k<-length(I)
    r<-t(apply(y[1:n,I], 1, rank))
    A<-sum(as.vector(r)^2)
    R<-apply(r, 2, sum)
    J<-I[order(R)]
    alpha<-1-conf.level
    TIES<-tapply(r, row(r), table)
    STATISTIC<-((12 * sum((R - n * (k + 1) / 2)^2)) /
                (n * k * (k + 1)
                 - (sum(unlist(lapply(TIES, function (u) {u^3 - u}))) /
                    (k - 1))))
    PARAMETER<-k-1
    PVAL<-pchisq(STATISTIC, PARAMETER, lower = FALSE)
      
    if (!is.nan(PVAL) && (PVAL<alpha)){
      if (interactive)
        cat("|-|")
      t<-qt(1-alpha/2,(n-1)*(k-1))*(2*(n*A-sum(R^2))/((n-1)*(k-1)))^(1/2)
      o<-order(R)
      J<-I[o[1]]
      for (j in 2:k) 
        if (abs(R[o[j]]-R[o[1]])>t) 
          break
        else
          J<-c(J,I[o[j]])
    }else{
      if (interactive)
        cat("|=|")
    }
    return(J)
  }
  
  aux.friedman<-function(){
    if (no.alive==2) {
      # If only 2 candidates are left, switch to Wilcoxon
      V1<-Results[1:(no.subtasks.sofar),which.alive[1]]
      V2<-Results[1:(no.subtasks.sofar),which.alive[2]]
      PVAL<-wilcox.test(V1,V2,paired=TRUE,exact=FALSE)$p.value
      if (!is.nan(PVAL)&&!is.na(PVAL)&&(PVAL<1-conf.level)){
        if (interactive)
          cat("|-|")
        if (median(V1-V2)<0){
          best<<-which.alive[1]
          alive[which.alive[2]]<<-FALSE
        }else{
          best<<-which.alive[2]
          alive[which.alive[1]]<<-FALSE
        }
      }else{
        if (interactive)
          cat("|=|")
        if (median(V1-V2)<0){
          best<<-which.alive[1]
        }else{
          best<<-which.alive[2]
        }
      }
    }else{
      # If more then 2 candidates are left, use Friedman
      J<-aux2.friedman(Results[1:(no.subtasks.sofar),],which.alive,
                       conf.level=conf.level)
      alive[-J]<<-FALSE
      best<<-J[1]
    }
  }
  
  aux.ttest<-function(adjust=c("none","bonferroni")){
    adjust<-match.arg(adjust)
    mean.all<-array(0,c(ncol(Results)))
    for (j in 1:ncol(Results))
      mean.all[j]<-sum(Results[1:no.subtasks.sofar,j]/no.subtasks.sofar)
    best<<-match(min(mean.all[alive]),mean.all)
    
    PJ<-array(0,dim=c(2,0))
    for (j in which.alive) {
      Vb<-Results[1:no.subtasks.sofar,best]
      Vj<-Results[1:no.subtasks.sofar,j]
      p <- t.test(Vb,Vj,paired=TRUE,
                  alternative="less")$p.value
      if (!is.nan(p) & !is.na(p))
        PJ<-array(c(PJ,j,p),dim=dim(PJ)+c(0,1))
    }
    PJ[2,]<-p.adjust(PJ[2,],method=adjust)
    dropped.any<-FALSE
    for (j in 1:ncol(PJ))
      if (PJ[2,j]<(1-conf.level)){
        alive[PJ[1,j]]<<-FALSE
        dropped.any<-TRUE
      }
    if (interactive){
      if (dropped.any) 
        cat("|-|")
      else
        cat("|=|")
    }
  }

  if (interactive)
    cat("                            Markers:                           \n",
        "                               x No test is performed.         \n",
        "                               - The test is performed and     \n",
        "                                 some candidates are discarded.\n", 
        "                               = The test is performed but     \n",
        "                                 no candidate is discarded.    \n",
        "                                                               \n",
        "                                                               \n",
        "+-+-----------+-----------+-----------+-----------+-----------+\n",
        "| |       Task|      Alive|       Best|  Mean best| Exp so far|\n",
        "+-+-----------+-----------+-----------+-----------+-----------+\n",
        sep="")

                                         # catch exceptions
  tryCatch({
    # Start main loop
    for (current.task in 1:no.tasks) {
      which.alive<-which(alive)
      no.alive<-length(which.alive)
      if (maxExp && no.experiments.sofar+no.alive>maxExp)
        break
      if (no.alive==1)
        break
      
      current.no.subtasks<-ifelse(length(no.subtasks)==1,
                                  no.subtasks,
                                  no.subtasks[current.task])
      
      subtasks.range<-
        if(length(no.subtasks)==1)
          (current.task-1)*no.subtasks+1:no.subtasks
        else
          cumsum(c(0,no.subtasks))[current.task]+1:no.subtasks[current.task]
      
      
      # PVM not available: running on a single processor
      results <- epsilon.wrapper(which.alive, current.task, race.data,
                                 weight);
      
      # assume no.subtask==1
      stopifnot(length(results) == no.alive);
      Results[subtasks.range, which.alive] <- results;
      
      no.experiments.sofar<-no.experiments.sofar+no.alive
      no.subexperiments.sofar<-no.subexperiments.sofar+
        current.no.subtasks*no.alive
      
      no.tasks.sofar<-no.tasks.sofar+1
      no.subtasks.sofar<-no.subtasks.sofar+current.no.subtasks
      
      # Drop bad candidates
      if (no.tasks.sofar>=first.test) 
        switch(stat.test,
               friedman=aux.friedman(),
               t.none=aux.ttest("none"),
               t.bonferroni=aux.ttest("bonferroni"))
      else {
        if (interactive)
          cat("|x|")
        if (no.subtasks.sofar==1)
          best<-order(Results[1,])[1]
        else
          best<-order(apply(t(apply(Results[1:(no.subtasks.sofar),],1,rank)),
                            2,mean))[1]
      }
      
      mean.best<-mean(Results[1:(no.subtasks.sofar),best])
      
      if (interactive) 
        cat(paste(formatC(no.tasks.sofar,width=11),"|",
                  formatC(sum(alive),width=11),"|",
                  formatC(best,width=11),"|",
                  formatC(mean.best,width=11),"|",
                  formatC(no.experiments.sofar,width=11),"|\n",
                  sep=""))
      
      logger()
    }
  }, finally = {
    if(exists(.slave.destruct.function, inherits=FALSE, mode="function")) {
      do.call(.slave.destruct.function, list(race.data));
    }
  });

  if (exists(.slave.describe.function,inherits=FALSE,mode="function"))
    description.best<-do.call(.slave.describe.function,list(best,race.data))
  else
    description.best<-NULL
  
  if (interactive) {
   cat(paste("+-+-----------+-----------+-----------+-----------+-----------+",
              "\n\n",
              "Selected candidate:",formatC(best,width=12),
              "\tmean value:",formatC(mean.best,width=11),
              "\n\n",sep=""))
    if (!is.null(description.best)){
      cat("Description of the selected candidate:\n")
      print(description.best)
      cat("\n\n")
    }
  }
 
           
  invisible(log.list(end=TRUE))
}



  
