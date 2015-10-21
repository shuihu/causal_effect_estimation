# setwd("/Users/thaipham/Desktop/R-Simulation/causal_effect_estimation/causalForest_test_thai/")
setwd("/farmshare/user_data/thaipham/R-Simulation/causal_effect_estimation/causalForest_test_thai/")

rm(list = ls())

source("case_3_parallel_func_thai.R")
library(snow)
library(xtable)

NREP = 20

dvals = c(2, 3, 4, 5, 6, 8)
JsamFracVals = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

for (j in 1:length(JsamFracVals)) {
  JsamFrac = JsamFracVals[j]
  for (i in 1:length(dvals)) {
    d = dvals[i]
    
    cl <- makeSOCKcluster(rep("localhost", NREP)) 
    clusterEvalQ(cl, source("case_3_parallel_func_thai.R"))
    raw_rets = clusterApply(cl, 1:NREP, simu.fun, d, JsamFrac)
    stopCluster(cl);
    
    save.image(paste0("Test_Run_Results/output_paper1014_3_0.5_", JsamFrac, "_", d, "_", ".RData"))
    
    results.mat = Reduce(rbind, raw_rets)
    rownames(results.mat) = 1:nrow(results.mat)
    
    results = data.frame(results.mat)
    
    condense = function(results) {
      RR.mu = colMeans(results)
      RR.var = sapply(results, var) / (nrow(results) - 1)
      rbind("mu"=RR.mu, "se"=sqrt(RR.var))
    }
    
    results.condensed = condense(results)
    
    prettyprint = function(RR) {
      apply(RR, 2, function(arg) {
        paste0(round(arg[1], 2), " (", round(100 * arg[2], 0), ")")
      })
    }
    
    results.pretty = c(d, prettyprint(results.condensed))
    
    results.pretty
    
    idxvec = c(1, 6, 11, 16, 2, 7, 12, 5, 10, 15)
    results.table = results.pretty[idxvec]
    results.table = data.frame(matrix(results.table, 1, 10))
    names(results.table) = names(results.pretty)[c(1, 6, 11, 16, 2, 7, 12, 5, 10, 15)]
    xtab = xtable(results.table)
#     print(xtab, include.rownames = FALSE)
  }
}


