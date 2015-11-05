library(causalTree)
library(randomForestCI)
library(FNN)
library(xtable)

setwd("/farmshare/user_data/thaipham/R-Simulation/causal_effect_estimation/causalForest_test_thai/")
source("causalForest_Thai.R")

args=(commandArgs(TRUE))
d = as.numeric(args[1])
Jsam = as.numeric(args[2])
print(d)
print(Jsam)

proc.time()
start.time = as.numeric(Sys.time())
start.time

NREP = 10

n = 5000
ntree = 5000
sigma = 1

k.small = 10
k.big = 100

n.test = 1000

effect = function(x) {
  4 / ((1 + exp(-12 * (x[1] - 0.5))) * (1 + exp(-12 * (x[2] - 0.5)))) 
}

simu.fun = function(seed.idx, d, Jsam) {

set.seed(seed.idx)

X = matrix(runif(n * d, 0, 1), n, d) # features
e = 0.5
W = rbinom(n, 1, e) #treatment condition

# no main effect
Y = (W - 0.5) * apply(X, 1, effect) + sigma * rnorm(n)

X.test = matrix(runif(n.test * d, 0, 1), n.test, d)
true.eff = apply(X.test, 1, effect)

#
# random forest
#

JsamFrac = Jsam / 10

forest = causalForest_Thai(X, Y, W, num.trees = ntree, subsample.fraction = 0.5, Jsample.fraction = JsamFrac, nodesize = 1)
predictions = predict(forest, X.test)
forest.ci = randomForestInfJack(forest, X.test, calibrate = TRUE)

# true effect is 0
se.hat = sqrt(forest.ci$var.hat)
rf.cov = abs(predictions - true.eff) <= 1.96 * se.hat
rf.covered = mean(rf.cov)
rf.covered.02 = mean(rf.cov[true.eff <= 0.2])
rf.covered.1 = mean(rf.cov[true.eff <= 1])
rf.covered.2 = mean(rf.cov[true.eff <= 2])
rf.mse = mean((predictions - true.eff)^2)

knn.0.mu = knn.reg(X[W==0,], X.test, Y[W==0], k = k.small)$pred
knn.1.mu = knn.reg(X[W==1,], X.test, Y[W==1], k = k.small)$pred

knn.0.mu2 = knn.reg(X[W==0,], X.test, Y[W==0]^2, k = k.small)$pred
knn.1.mu2 = knn.reg(X[W==1,], X.test, Y[W==1]^2, k = k.small)$pred

knn.0.var = (knn.0.mu2 - knn.0.mu^2) / (k.small - 1)
knn.1.var = (knn.1.mu2 - knn.1.mu^2) / (k.small - 1)

knn.tau = knn.1.mu - knn.0.mu
knn.se = sqrt(knn.0.var + knn.1.var)
knn.cov = abs(knn.tau - true.eff) <= 1.96 * knn.se
knn.covered = mean(knn.cov)
knn.covered.02 = mean(knn.cov[true.eff <= 0.2])
knn.covered.1 = mean(knn.cov[true.eff <= 1])
knn.covered.2 = mean(knn.cov[true.eff <= 2])
knn.mse = mean((knn.tau - true.eff)^2)

knnbig.0.mu = knn.reg(X[W==0,], X.test, Y[W==0], k = k.big)$pred
knnbig.1.mu = knn.reg(X[W==1,], X.test, Y[W==1], k = k.big)$pred

knnbig.0.mu2 = knn.reg(X[W==0,], X.test, Y[W==0]^2, k = k.big)$pred
knnbig.1.mu2 = knn.reg(X[W==1,], X.test, Y[W==1]^2, k = k.big)$pred

knnbig.0.var = (knnbig.0.mu2 - knnbig.0.mu^2) / (k.big - 1)
knnbig.1.var = (knnbig.1.mu2 - knnbig.1.mu^2) / (k.big - 1)

knnbig.tau = knnbig.1.mu - knnbig.0.mu
knnbig.se = sqrt(knnbig.0.var + knnbig.1.var)
knnbig.cov = abs(knnbig.tau - true.eff) <= 1.96 * knnbig.se
knnbig.covered = mean(knnbig.cov)
knnbig.covered.02 = mean(knnbig.cov[true.eff <= 0.2])
knnbig.covered.1 = mean(knnbig.cov[true.eff <= 1])
knnbig.covered.2 = mean(knnbig.cov[true.eff <= 2])
knnbig.mse = mean((knnbig.tau - true.eff)^2)


c(rf.covered = rf.covered,
           rf.covered.02 = rf.covered.02,
           rf.covered.1 = rf.covered.1,
           rf.covered.2 = rf.covered.2,
           rf.mse = rf.mse,
	       knn.covered = knn.covered, 
           knn.covered.02 = knn.covered.02,
           knn.covered.1 = knn.covered.1,
           knn.covered.2 = knn.covered.2,
           knn.mse = knn.mse,
	       knnbig.covered = knnbig.covered, 
           knnbig.covered.02 = knnbig.covered.02,
           knnbig.covered.1 = knnbig.covered.1,
           knnbig.covered.2 = knnbig.covered.2,
           knnbig.mse = knnbig.mse)
}

raw_rets = lapply(start.time + 1:NREP, simu.fun, d, Jsam);

proc.time();

save.image(paste0("Test_Run_Results/output_paper1027_3_0.5_0.", Jsam, "_", d, ".RData"))

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

#idxvec = c(1, 6, 11, 16, 2, 7, 12, 5, 10, 15)
#results.table = results.pretty[idxvec]
#results.table = data.frame(matrix(results.table, 1, 10))
#names(results.table) = names(results.pretty)[c(1, 6, 11, 16, 2, 7, 12, 5, 10, 15)]
#xtab = xtable(results.table)
#print(xtab, include.rownames = FALSE)
