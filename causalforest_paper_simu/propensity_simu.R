#install.packages("~/git_local/causal_effect_estimation", type = "source", repos = NULL)
library(causalTree)
library(mgcv)
library(randomForestCI)
library(FNN)
library(Hmisc)

rm(list = ls())

n = 500
ntree = 1000
sigma = 1

n.test = 100

dvals = c(5, 10) #, 15, 20, 30, 40)
simu.reps = 2

# heterogeneous effect of treatment
effect = function(x) {
	0
}

baseline = function(x) {
	10 * (x[1] - 0.5)
}

propensity = function(x) {
	0.25 + dbeta(x[1], 2, 4)/4
}

simu.fun = function(d) {

X = matrix(runif(n * d, 0, 1), n, d) # features
e = apply(X, 1, propensity)
W = rbinom(n, 1, e) #treatment condition
Y = apply(X, 1, baseline) +  (W - 0.5) * apply(X, 1, effect) + sigma * rnorm(n)


X.test = matrix(runif(n.test * d, 0, 1), n.test, d)
true.eff = apply(X.test, 1, effect)

forest = propensityForest(X, Y, W, num.trees = ntree, sample.size = n / 10, nodesize = 1)
predictions = predict(forest, X.test)

forest.ci = randomForestInfJack(forest, X.test, calibrate = TRUE)

se.hat = sqrt(forest.ci$var.hat)
up.lim = predictions + 1.96 * se.hat
down.lim = predictions - 1.96 * se.hat

covered = (true.eff <= up.lim) & (true.eff >= down.lim)

mean(covered)

}

results = sapply(dvals, function(d) {
	replicate(simu.reps, simu.fun(d))
})


save.image("~/causal_effect_estimation/causalforest_paper_simu/propensity.RData")