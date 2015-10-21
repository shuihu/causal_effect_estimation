library(xtable)

dd = c(2, 3, 4, 5, 6, 8)
dlist = lapply(dd, function(d) {
	all.res = list()
	for (cc in 1:8){
		fnm = paste0("cluster_simu", cc, "/sig_results_", d, ".RData")
		tryCatch({
			raw_rets = list()
			load(fnm)
			cc.list = raw_rets
		}, error = function(e) {
			cc.list = list()
		}, finally = {
			all.res = c(all.res, raw_rets)
		})
	}
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
			paste0(round(arg[1], 4), " (", round(10000 * arg[2], 0), ")")
	})
})

results.pretty = data.frame(cbind("d"=dd, Reduce(rbind, res.short)))

idxvec = c(1, 6, 11, 16, 2, 7, 12, 5, 10, 15)
results.table = results.pretty[,idxvec]
names(results.table) = names(results.pretty)[c(1, 6, 11, 16, 2, 7, 12, 5, 10, 15)]
xtab = xtable(results.table)
print(xtab, include.rownames = FALSE)