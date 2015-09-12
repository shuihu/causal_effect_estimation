#install.packages("~/git_local/causal_effect_estimation", type = "source", repos = NULL)
library(causalTree)
library(Hmisc)
library(mgcv)

rm(list = ls())

library(ggplot2)

n = 20000
ntree = 5000
sigma = 0.1
d = 6
k = 2

# heterogeneous effect of treatment
effect = function(x) {
	4 * prod(sign(x[1:k]) * sqrt(abs(x[1:k]))) 
}

# baseline effect of features
#baseline = function(x) {
#	2 * sum(x[1:k]^2) + 10 * sum(abs(x[1:k]))
#}

baseline = function(x) { 0 }

X = matrix(runif(n * d, -1, 1), n, d) # features
W = rbinom(n, 1, 0.5) #treatment condition
Y = apply(X, 1, baseline) +  (W - 0.5) * apply(X, 1, effect) + sigma * rnorm(n)


n.test = 10000
X.test = matrix(runif(n.test * d, -1, 1), n.test, d)
true.eff = apply(X.test, 1, effect)

cmp = comparisonForest(Y, X, W, X.test = X.test, num.trees = ntree, sample.size = n / 10)

plot(true.eff, cmp$new.tau, xlab = "True Treatment Effect", ylab = "Fitted Treatment Effect")
abline(0, 1, col = 2, lwd = 2)

minp = min(true.eff, cmp$new.tau)
maxp = max(true.eff, cmp$new.tau)
rngp = maxp - minp

ncol = 100

true.scl = pmax(ceiling(ncol * (true.eff - minp) / rngp), 1)
fit.scl = pmax(ceiling(ncol * (cmp$new.tau - minp) / rngp), 1)

hc = heat.colors(ncol)
plot(x=X.test[,1], y=X.test[,2], pch = 16, col = hc[true.scl])
plot(x=X.test[,1], y=X.test[,2], pch = 16, col = hc[fit.scl])

cmp.ci = randomForestInfJack(cmp, cmp$new.pred, calibrate = TRUE)
plot(cmp.ci)

se.hat = sqrt(cmp.ci$var.hat)
up.lim = cmp$new.tau + 1.96 * se.hat
down.lim = cmp$new.tau - 1.96 * se.hat

errbar(true.eff, cmp$new.tau, up.lim, down.lim, xlab = "True Treatment Effect", ylab = "Fitted Treatment Effect")
abline(0, 1, col = 2, lwd = 2)

covered = (true.eff <= up.lim) & (true.eff >= down.lim)

mean(covered)

save.image("./cmpforest.RData")

pdf("~/public_html/coverage.pdf")
cov.fit = gam(covered ~ s(true.eff), sp = 0.001, family = binomial())
plot(true.eff, predict(cov.fit, type = "response"))
dev.off()