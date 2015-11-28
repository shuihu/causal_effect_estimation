library(xtable)

setwd("/farmshare/user_data/thaipham/R-Simulation/causal_effect_estimation/causalForest_test_thai/")

n = 10000 #1000, 2000, 5000, 10000
dd = c(2, 3, 4, 5, 6, 8)
aVals = c(6, 12, 15, 20, 30, 40)

for (i in 1:length(aVals)) {
  a = aVals[i]
  
  dlist = lapply(dd, function(d) {
    all.res = list()
    fnm = paste0("Test_Two_Forests/output_paper1113_2_two_forests_case_2_", n, "_", a, "_", d, ".RData")
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
  
  save.image(paste0("Test_Two_Forests/output_paper1113_2_two_forests_case_2_", n, "_", a, ".RData"))

  results.pretty = data.frame(cbind("d"=dd, Reduce(rbind, res.short)))
  
  idxvec = c(1, 3, 2)
  results.table = results.pretty[,idxvec]
  names(results.table) = names(results.pretty)[idxvec]
  xtab = xtable(results.table)
  cat("Test 2 Two Forests Result - subsample.fraction = ", a, "\n")
  print(xtab, include.rownames = FALSE)
}
