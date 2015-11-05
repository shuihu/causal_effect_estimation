library(xtable)

setwd("/farmshare/user_data/thaipham/R-Simulation/causal_effect_estimation/causalForest_test_thai/")

dd = c(2, 5, 10, 15, 20, 30)
aVals = c(1, 2, 3, 4, 5, 6, 7, 8, 9)

for (i in 1:length(aVals)) {
  a = aVals[i]
  
  dlist = lapply(dd, function(d) {
    all.res = list()
    fnm = paste0("Test_Run_Results/output_paper1014_1_0.", a, "_0.5_", d, ".RData")
    load(fnm)
    all.res = c(all.res, raw_rets)
    all.res
  }) 
  
  res.short = lapply(dlist, function(res) {
    results.mat = Reduce(rbind, res)
    rownames(results.mat) = 1:nrow(results.mat)
    results = data.frame(results.mat)
    RR.mu = colMeans(results)
    RR.var = sapply(results, var) / (nrow(results) - 1)
    RR = rbind("mu"=RR.mu, "se"=sqrt(RR.var))
    apply(RR, 2, function(arg) {
      paste0(round(arg[1], 2), " (", round(100 * arg[2], 0), ")")
    })
  })
  
  save.image(paste0("Test_Run_Results/output_paper1014_1_0.", a, "_0.5", ".RData"))
  
  results.pretty = data.frame(cbind("d"=dd, Reduce(rbind, res.short)))
  
  idxvec = c(1, 3, 6, 9, 2, 4, 7)
  results.table = results.pretty[,idxvec]
  names(results.table) = names(results.pretty)[idxvec]
  xtab = xtable(results.table)
  cat("Test 3 Result - subsample.fraction = 0.", a, "\n")
  #print(xtable(results.table, caption = 'Test 3 Result - subsample.fraction = 0.a'), caption.placement = 'top', include.rownames = FALSE)
  print(xtab, include.rownames = FALSE)
}
