library(xtable)

setwd("/farmshare/user_data/thaipham/R-Simulation/causal_effect_estimation/causalForest_test_thai/")

dd = c(2, 3, 4, 5, 6, 8)
#JsamVals = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
JsamVals = c(4, 5, 6, 7, 8)

for (i in 1:length(JsamVals)) {
  Jsam = JsamVals[i]
  
  dlist = lapply(dd, function(d) {
    all.res = list()
    fnm = paste0("Test_Run_Results/output_paper1027_3_0.5_0.", Jsam, "_", d, ".RData")
    #fnm = paste0("Test_Run_Results/output_paper1027_3_0.1_0.", Jsam, "_", d, ".RData")
    #fnm = paste0("Test_Run_Results/output_paper1027_3_0.3_0.", Jsam, "_", d, ".RData")
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
  
  save.image(paste0("Test_Run_Results/output_paper1027_3_0.5_0.", Jsam, ".RData"))
  #save.image(paste0("Test_Run_Results/output_paper1027_3_0.1_0.", Jsam, ".RData"))
  #save.image(paste0("Test_Run_Results/output_paper1027_3_0.3_0.", Jsam, ".RData"))

  results.pretty = data.frame(cbind("d"=dd, Reduce(rbind, res.short)))
  
  idxvec = c(1, 6, 11, 16, 2, 7, 12)
  results.table = results.pretty[,idxvec]
  names(results.table) = names(results.pretty)[idxvec]
  #xtab = xtable(results.table, caption = 'Test 3 Result - Jsample.fraction =')
  xtab = xtable(results.table)
  cat("Test 3 Result - Jsample.fraction = 0.", Jsam, "\n")
  #print(xtab, caption.placement = 'top', include.rownames = FALSE)
  print(xtab, include.rownames = FALSE)
}
