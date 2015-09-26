#install.packages("~/git_local/causal_effect_estimation", type = "source", repos = NULL)
library(causalTree)
library(Hmisc)
library(mgcv)
library(ggplot2)

rm(list = ls())

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

forest = causalForest(X, Y, W, num.trees = ntree, sample.size = n / 10)
predictions <- predict(forest, X.test)

plot(true.eff, predictions, xlab = "True Treatment Effect", ylab = "Fitted Treatment Effect")
abline(0, 1, col = 2, lwd = 2)

minp = min(true.eff, predictions)
maxp = max(true.eff, predictions)
rngp = maxp - minp

ncol = 100

true.scl = pmax(ceiling(ncol * (true.eff - minp) / rngp), 1)
fit.scl = pmax(ceiling(ncol * (cmp$new.tau - minp) / rngp), 1)

hc = heat.colors(ncol)

pdf("~/public_html/cate_true.pdf")
plot(X.test[,1], X.test[,2], pch = 16, col = hc[true.scl], xlab = "x1", ylab = "x2")
dev.off()

pdf("~/public_html/cate_fit.pdf")
plot(X.test[,1], X.test[,2], pch = 16, col = hc[fit.scl], xlab = "x1", ylab = "x2")
dev.off()

cmp.ci = randomForestInfJack(cmp, cmp$new.pred, calibrate = TRUE)
plot(cmp.ci)

se.hat = sqrt(cmp.ci$var.hat)
up.lim = cmp$new.tau + 1.96 * se.hat
down.lim = cmp$new.tau - 1.96 * se.hat


pdf("~/public_html/preds_rf.pdf")
plot(true.eff, cmp$new.tau, xlab = "True Treatment Effect", ylab = "Estimated Treatment Effect")
abline(0, 1, lwd = 2, col = 2)
dev.off()

n.errbar = 200
pdf("~/public_html/preds_rf_errbar.pdf")
errbar(true.eff[1:n.errbar], cmp$new.tau[1:n.errbar], up.lim[1:n.errbar], down.lim[1:n.errbar], xlab = "True Treatment Effect", ylab = "Estimated Treatment Effect", pch = ".", cex = 3)
abline(0, 1, col = 2, lwd = 2)
dev.off()

covered = (true.eff <= up.lim) & (true.eff >= down.lim)

mean(covered)

#save.image("./cmpforest.RData")

pdf("~/public_html/coverage.pdf")
cov.fit = gam(covered ~ s(true.eff), sp = 0.001, family = binomial())
plot(true.eff, predict(cov.fit, type = "response"))
dev.off()

source("~/causal_effect_estimation/scripts/knn.R")

kk = c(1:10, seq(12, 32, by = 4))
knn.mses = sapply(kk, function(k) {
	tauhat = knn.cate(X, Y, W, X.test, k = k)
	mean((true.eff - tauhat)^2)
})

knn.tau = knn.cate(X, Y, W, X.test, k = 8)

pdf("~/public_html/preds_knn.pdf")
plot(true.eff, knn.tau, xlab = "True Treatment Effect", ylab = "Estimated Treatment Effect")
abline(0, 1, lwd = 2, col = 2)
dev.off()

pdf("~/public_html/cate_knn.pdf")
fit.knn = pmax(ceiling(ncol * (knn.tau- minp) / rngp), 1)
plot(X.test[,1], X.test[,2], pch = 16, col = hc[fit.knn], xlab = "x1", ylab = "x2")
dev.off()


rf.mse = mean((true.eff - cmp$new.tau)^2)
knn.mse = mean((true.eff - knn.tau)^2)

