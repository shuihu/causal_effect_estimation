#install.packages("~/git_local/causal_effect_estimation", type = "source", repos = NULL)
library(causalTree)
library(Hmisc)
library(mgcv)

# note: this package can be downloaded from https://github.com/swager/randomForestCI
library(randomForestCI)

rm(list = ls())

n = 1000
ntree = 1000
sigma = 1
d = 2
k = 2

# heterogeneous effect of treatment
effect = function(x) {
	4 / ((1 + exp(-6 * x[1])) * (1 + exp(-6 * x[2]))) 
}

baseline = function(x) { 0 }

X = matrix(runif(n * d, -1, 1), n, d) # features
W = rbinom(n, 1, 0.5) #treatment condition
Y = apply(X, 1, baseline) +  (W - 0.5) * apply(X, 1, effect) + sigma * rnorm(n)


n.test = 10000
X.test = matrix(runif(n.test * d, -1, 1), n.test, d)
true.eff = apply(X.test, 1, effect)

forest = causalForest(X, Y, W, num.trees = ntree, sample.size = n / 10)
predictions = predict(forest, X.test)

minp = min(true.eff, predictions)
maxp = max(true.eff, predictions)
rngp = maxp - minp

ncol = 100

true.scl = pmax(ceiling(ncol * (true.eff - minp) / rngp), 1)
fit.scl = pmax(ceiling(ncol * (predictions - minp) / rngp), 1)

hc = heat.colors(ncol)

plot(X.test[,1], X.test[,2], pch = 16, col = hc[true.scl], xlab = "x1", ylab = "x2")
plot(X.test[,1], X.test[,2], pch = 16, col = hc[fit.scl], xlab = "x1", ylab = "x2")

plot(true.eff, predictions, xlab = "True Treatment Effect", ylab = "Fitted Treatment Effect")
abline(0, 1, col = 2, lwd = 2)
forest.ci = randomForestInfJack(forest, X.test, calibrate = TRUE)
plot(forest.ci)

se.hat = sqrt(forest.ci$var.hat)
up.lim = predictions + 1.96 * se.hat
down.lim = predictions - 1.96 * se.hat

n.errbar = 200
errbar(true.eff[1:n.errbar], predictions[1:n.errbar], up.lim[1:n.errbar], down.lim[1:n.errbar], xlab = "True Treatment Effect", ylab = "Estimated Treatment Effect", pch = ".", cex = 3)
abline(0, 1, col = 2, lwd = 2)

covered = (true.eff <= up.lim) & (true.eff >= down.lim)

mean(covered)

cov.fit.smooth = gam(covered ~ s(true.eff), sp = 0.001, family = binomial())
plot(true.eff, predict(cov.fit.smooth, type = "response"))
