#install.packages("~/git_local/causal_effect_estimation", type = "source", repos = NULL)

#setwd("~/git_local/causal_effect_estimation/scripts")

library(causalTree)
library(Hmisc)
library(mgcv)
library(ggplot2)

rm(list = ls())

n = 4000
ntree = 5000
sigma = 1
d = 2
k = 2

# heterogeneous effect of treatment
#effect = function(x) {
#	as.numeric(x[1] > (1 - sqrt(2)) & x[2] > (1 - sqrt(2)))
#}

effect = function(x) {
	20 * (pmin(pmax(0, x[1]) * pmax(0, x[2]), 1/4))
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
y.hat = cmp$new.tau

pdf("~/public_html/preds_plot.pdf")
plot(true.eff, cmp$new.tau, xlab = "True Treatment Effect", ylab = "Fitted Treatment Effect")
abline(0, 1, col = 2, lwd = 2)
dev.off()

pdf("~/public_html/prediction_boxplot.pdf")
test.df = data.frame(truth = true.eff, fit = cmp$new.tau)
boxplot(fit ~ truth, data = test.df[true.eff == 0 | true.eff == max(true.eff),], ylim = range(c(cmp$new.tau, true.eff)),  xlab = "True Treatment Effect", ylab = "Fitted Treatment Effect")
abline(0, 0, lty = 2, col = 2, lwd = 2)
abline(max(true.eff), 0, lty = 2, col = 4, lwd = 2)
dev.off()

minp = min(true.eff, cmp$new.tau)
maxp = max(true.eff, cmp$new.tau)
rngp = maxp - minp

ncol = 100

true.scl = pmax(ceiling(ncol * (true.eff - minp) / rngp), 1)
fit.scl = pmax(ceiling(ncol * (cmp$new.tau - minp) / rngp), 1)

hc = heat.colors(ncol)

pdf("~/public_html/cate_illustration.pdf")
pardef = par(mfrow=c(1, 2))
plot(X.test[,1], X.test[,2], pch = 16, col = hc[true.scl], xlab = "x1", ylab = "x2", main = "True Conditional Treatment Effect")
plot(X.test[,1], X.test[,2], pch = 16, col = hc[fit.scl], xlab = "x1", ylab = "x2", main = "Comparison Forest Estimate")
par(pardef)
dev.off()

cmp.ci = randomForestInfJack(cmp, cmp$new.pred, calibrate = TRUE)
plot(cmp.ci)

se.hat = sqrt(cmp.ci$var.hat)
up.lim = cmp$new.tau + 1.96 * se.hat
down.lim = cmp$new.tau - 1.96 * se.hat

covered = (true.eff <= up.lim) & (true.eff >= down.lim)

mean(covered)
mean(covered[true.eff == 0])
mean(covered[true.eff == max(true.eff)])


pdf("~/public_html/cov_plots.pdf")
pardef = par(mfrow=c(1, 2))
gg = gam(covered ~ s(se.hat), family = binomial(), sp = 0.01)
plot(se.hat, predict(gg, type = "response"), xlab = "SE estimate", ylab = "coverage")
gg2 = gam(covered ~ s(y.hat), family = binomial(), sp = 0.01)
plot(y.hat, predict(gg2, type = "response"), xlab = "prediction", ylab = "coverage")
dev.off()

plot(smooth.spline(y.hat, se.hat))
