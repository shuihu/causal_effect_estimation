library(randomForest)
library(randomForestCI)
library(FNN)
library(xtable)

setwd("/farmshare/user_data/thaipham/R-Simulation/causal_effect_estimation/causalForest_test_thai/")

args=(commandArgs(TRUE))
d = as.numeric(args[1])
a = as.numeric(args[2])
print(d)
print(a)

proc.time()
start.time = as.numeric(Sys.time())
start.time

NREP = 10

n = 10000
ntree = n
sigma = 1

n.test = 1000

effect = function(x) {
  (1 + 1/(1 + exp(-20 * (x[1] - 1/3)))) * (1 + 1/(1 + exp(-20 * (x[2] - 1/3))))
}

simu.fun = function(seed.idx, d, a) {

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

  alp = a/60

  forest1 = randomForest(X[W==1, ], Y[W==1], sampsize = floor(nrow(X[W==1, ]) * alp), ntree = ntree, keep.inbag = TRUE)
  predictions1 = predict(forest1, X.test)
  forest1.ci = randomForestInfJack(forest1, X.test, calibrate = TRUE)

  forest0 <- randomForest(X[W==0, ], Y[W==0], sampsize = floor(nrow(X[W==0, ]) * alp), ntree = ntree, keep.inbag = TRUE)
  predictions0 <- predict(forest0, X.test)
  forest0.ci <- randomForestInfJack(forest0, X.test, calibrate = TRUE)

  # true effect is 0
  se.hat = sqrt(forest1.ci$var.hat + forest0.ci$var.hat)
  predictions <- predictions1 - predictions0

  rf.cov = abs(predictions - true.eff) <= 1.96 * se.hat
  rf.covered = mean(rf.cov)
  rf.mse = mean((predictions - true.eff)^2)

  c(rf.covered = rf.covered,
             rf.mse = rf.mse)
}

raw_rets = lapply(start.time + 1:NREP, simu.fun, d, a);

proc.time();

save.image(paste0("Test_Two_Forests/output_paper1113_2_two_forests_case_2_", n, "_", a, "_", d, ".RData"))

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
