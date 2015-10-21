library(causalTree)
library(mgcv)
library(randomForestCI)
library(FNN)
library(Hmisc)
library(xtable)

rm(list = ls())
source('~/git_local/causal_effect_estimation/scripts/knn.R')

set.seed(1)

n = 10000
ntree = 1000
sigma = 1

n.test = 10000

d = 8

effect = function(x) {
	4 / ((1 + exp(-12 * (x[1] - 0.5))) * (1 + exp(-12 * (x[2] - 0.5)))) 
}


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

forest = causalForest(X, Y, W, num.trees = ntree, sample.size = n / 10, nodesize = 1)
predictions = predict(forest, X.test)

k.seq = c(seq(5, 80, by = 5))
knn.mse = sapply(k.seq, function(k) {
	tauhat = knn.cate(X, Y, W, X.test, k)
	mean((tauhat - true.eff)^2)
})

k.opt = k.seq[which.min(knn.mse)]
tau.knn = knn.cate(X, Y, W, X.test, k.opt)

k.opt

mean((predictions - true.eff)^2)
mean((tau.knn - true.eff)^2)

minp = min(true.eff, predictions, tau.knn)
maxp = max(true.eff, predictions, tau.knn)
rngp = maxp - minp

ncol = 100

true.scl = pmax(ceiling(ncol * (true.eff - minp) / rngp), 1)
fit.scl = pmax(ceiling(ncol * (predictions - minp) / rngp), 1)
knn.scl = pmax(ceiling(ncol * (tau.knn - minp) / rngp), 1)

hc = heat.colors(ncol)

pdf('~/git_local/causal_effect_estimation/causalforest_paper_simu/sigmoid_true.pdf')
plot(X.test[,1], X.test[,2], pch = 16, col = hc[true.scl], xlab = "x1", ylab = "x2")
dev.off()

pdf('~/git_local/causal_effect_estimation/causalforest_paper_simu/sigmoid_cf.pdf')
plot(X.test[,1], X.test[,2], pch = 16, col = hc[fit.scl], xlab = "x1", ylab = "x2")
dev.off()

pdf('~/git_local/causal_effect_estimation/causalforest_paper_simu/sigmoid_knn.pdf')
plot(X.test[,1], X.test[,2], pch = 16, col = hc[knn.scl], xlab = "x1", ylab = "x2")
dev.off()

plot(true.eff, predictions)
abline(0, 1)
