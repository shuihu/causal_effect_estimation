library(xtable)

dd = c(2, 3, 4, 5, 6, 8)
JsamFracVals = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

for (i in 1:length(JsamFracVals)) {
  JsamFrac = JsamFracVals[i]
  
  dlist = lapply(dd, function(JsamFrac, d) {
    all.res = list()
    fnm = paste0("Test_Run_Results/output_paper1014_3_0.5_", JsamFrac, "_", d, "_", ".RData")
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
  
  save.image(paste0("Test_Run_Results/output_paper1014_3_0.5_", JsamFrac, ".RData"))
  
  results.pretty = data.frame(cbind("d"=dd, Reduce(rbind, res.short)))
  
  idxvec = c(1, 6, 11, 16, 2, 7, 12)
  results.table = results.pretty[,idxvec]
  names(results.table) = names(results.pretty)[idxvec]
  xtab = xtable(results.table)
#   print(xtab, include.rownames = FALSE)
}
