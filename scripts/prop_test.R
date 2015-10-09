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
d = 5
k = 2

# heterogeneous effect of treatment
effect = function(x) {
	0
}

baseline = function(x) {
	2 * (x[1] - 0.5)
}

propensity = function(x) {
	0.25 + dbeta(x[1], 2, 4)/4
}

reps = replicate(10, {

X = matrix(runif(n * d, 0, 1), n, d) # features
e = apply(X, 1, propensity)
W = rbinom(n, 1, e) #treatment condition
Y = apply(X, 1, baseline) +  (W - 0.5) * apply(X, 1, effect) + sigma * rnorm(n)

n.test = 100
X.test = matrix(runif(n.test * d, 0, 1), n.test, d)
true.eff = apply(X.test, 1, effect)

forest = propensityForest(X, Y, W, num.trees = ntree, sample.size = n / 10, nodesize = 1)
predictions = predict(forest, X.test)

plot(X.test[,1], predictions)
forest.ci = randomForestInfJack(forest, X.test, calibrate = TRUE)

plot(X.test[,1], forest.ci$var.hat)

se.hat = sqrt(forest.ci$var.hat)
se.hat.plus = 1/(1 - mean(forest$inbag)) * se.hat

up.lim = predictions + 1.96 * se.hat.plus
down.lim = predictions - 1.96 * se.hat.plus

errbar(X.test[, 1], predictions, up.lim, down.lim, xlab = "True Treatment Effect", ylab = "Estimated Treatment Effect", pch = ".", cex = 3)
abline(0, 0, col = 2, lwd = 2)

covered = (true.eff <= up.lim) & (true.eff >= down.lim)

mean(covered)

})