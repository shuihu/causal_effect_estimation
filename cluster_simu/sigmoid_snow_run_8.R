#setwd("~/git_local/causal_effect_estimation/causalforest_paper_simu/")

rm(list = ls())

source("sigmoid_snow_fn.R")
library(snow)
library(xtable)

NREP = 20
d = 8

cl <- makeSOCKcluster(rep("localhost", NREP)) 
clusterEvalQ(cl, source("sigmoid_snow_fn.R"))
raw_rets = clusterApply(cl, 1:NREP, simu.fun, d);
stopCluster(cl);

proc.time();

save.image(paste0("sig_results_", d, ".RData"))

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
print(xtab, include.rownames = FALSE)
