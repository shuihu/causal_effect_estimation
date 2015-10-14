library(xtable)

dd = c(2, 3, 4, 5, 6, 8)
dlist = lapply(dd, function(d) {
	all.res = list()
	fnm = paste0("step_results_", d, ".RData")
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

results.pretty = data.frame(cbind("d"=dd, Reduce(rbind, res.short)))

idxvec = c(1, 6, 11, 16, 2, 7, 12)
results.table = results.pretty[,idxvec]
names(results.table) = names(results.pretty)[idxvec]
xtab = xtable(results.table)
print(xtab, include.rownames = FALSE)