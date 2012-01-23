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

source("vns.R");

exhaust.vns <- function(no.iterations=1,
                       algs=1:nrow(vns.design),
                       log="", ...) {
  runtime <- quality <- matrix(NA, nrow=no.iterations, ncol=length(algs));
  
  
  for(i in 1:no.iterations) {
    test.output <- test.vns(algs, ...);
    cat(unlist(test.output["runtime",]), "\n", file=log);
    cat(unlist(test.output["quality",]), "\n", file=log);
    runtime[i,] <- unlist(test.output["runtime",]);
    quality[i,] <- unlist(test.output["quality",]);
  }

  runtime <- data.frame(runtime);
  quality <- data.frame(quality);

  names(runtime) <- names(quality) <- sapply(algs, vns.str);
  
  return(list(runtime=runtime, quality=quality));
}

sample.vns <- function(n=1, algs=sample(1:nrow(vns.design)),
                       log="", ...) {
  runtimes <- numeric(n);
  cat("runtime\tquality\tcand\tseed\n", file=log);
  for(i in 1:n) {
    test.output <- test.vns(sample(algs, 1), ...);
    cat(unlist(test.output[,1]), "\n", sep="\t", file=log);
    runtimes[i] <- unlist(test.output["runtime",]);
  }
  return(runtimes);
}

test.vns <- function(cands=1:nrow(vns.design), seed=sample(1:999,1),
                     problem=list(env="S", obj="T", weight=T, n=100, m=1),
                     verbose=F) {
  input.data <- vns.init(seed, problem);

  results <- sapply(cands,
                    function(cand) {
                      command <- vns.string(cand,
                                            problem$env, problem$obj,
                                            input.data$inst, input.data$seq);
                      if(verbose) {
                        cat(command, "\n");
                      }
                      output <- as.numeric(system(command, intern=T));
                      if(verbose) {
                        cat(output, "\n");
                      }
                      return(list(runtime=output[2], quality=output[1],
                                  cand=cand, seed=seed));
                    });
  unlink(input.data$inst);
  unlink(input.data$seq);
  return(results);
}
       
