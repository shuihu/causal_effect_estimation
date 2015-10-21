library(causalTree)
library(mgcv)
library(randomForestCI)
library(FNN)
library(Hmisc)
library(xtable)

setwd("/Users/thaipham/Desktop/R-Simulation/causal_effect_estimation/causalForest_test_thai/")

rm(list = ls())

n = 500
ntree = 1000
sigma = 1

n.test = 100

dvals = c(2, 5, 10, 15, 20, 30)
simu.reps = 20  #500 --> true version (extremely slow)

baseline = function(x) {
  2 * (x[1] - 0.5)
}

propensity = function(x) {
  0.25 + dbeta(x[1], 2, 4)/4
}

simu.fun = function(d, alpha) {
  
  X = matrix(runif(n * d, 0, 1), n, d) # features
  e = apply(X, 1, propensity)
  W = rbinom(n, 1, e) #treatment condition
  
  # no treatment effect
  Y = apply(X, 1, baseline) + sigma * rnorm(n)
  
  X.test = matrix(runif(n.test * d, 0, 1), n.test, d)
  
  
  #
  # random forest
  #
  
  forest = propensityForest(X, Y, W, num.trees = ntree, sample.size = n * alpha, nodesize = 1)
  predictions = predict(forest, X.test)
  forest.ci = randomForestInfJack(forest, X.test, calibrate = TRUE)
  
  # true effect is 0
  se.hat = sqrt(forest.ci$var.hat)
  rf.covered = mean(abs(predictions) <= 1.96 * se.hat)
  rf.mse = mean(predictions^2)
  
  k.small = 10
  knn.0.mu = knn.reg(X[W==0,], X.test, Y[W==0], k = k.small)$pred
  knn.1.mu = knn.reg(X[W==1,], X.test, Y[W==1], k = k.small)$pred
  
  knn.0.mu2 = knn.reg(X[W==0,], X.test, Y[W==0]^2, k = k.small)$pred
  knn.1.mu2 = knn.reg(X[W==1,], X.test, Y[W==1]^2, k = k.small)$pred
  
  knn.0.var = (knn.0.mu2 - knn.0.mu^2) / (k.small - 1)
  knn.1.var = (knn.1.mu2 - knn.1.mu^2) / (k.small - 1)
  
  knn.tau = knn.1.mu - knn.0.mu
  knn.se = sqrt(knn.0.var + knn.1.var)
  knn.covered.v1 = mean(abs(knn.tau) <= 1.96 * knn.se)
  knn.covered.v2 = mean(abs(knn.tau) <= 1.96 * sqrt(mean(knn.0.var + knn.1.var)))
  knn.mse = mean(knn.tau^2)
  
  k.big = 100
  knnbig.0.mu = knn.reg(X[W==0,], X.test, Y[W==0], k = k.big)$pred
  knnbig.1.mu = knn.reg(X[W==1,], X.test, Y[W==1], k = k.big)$pred
  
  knnbig.0.mu2 = knn.reg(X[W==0,], X.test, Y[W==0]^2, k = k.big)$pred
  knnbig.1.mu2 = knn.reg(X[W==1,], X.test, Y[W==1]^2, k = k.big)$pred
  
  knnbig.0.var = (knnbig.0.mu2 - knnbig.0.mu^2) / (k.big - 1)
  knnbig.1.var = (knnbig.1.mu2 - knnbig.1.mu^2) / (k.big - 1)
  
  knnbig.tau = knnbig.1.mu - knnbig.0.mu
  knnbig.se = sqrt(knnbig.0.var + knnbig.1.var)
  knnbig.covered.v1 = mean(abs(knnbig.tau) <= 1.96 * knnbig.se)
  knnbig.covered.v2 = mean(abs(knnbig.tau) <= 1.96 * sqrt(mean(knnbig.0.var + knnbig.1.var)))
  knnbig.mse = mean(knnbig.tau^2)
  
  
  c(rf.covered = rf.covered,
    rf.mse = rf.mse,
    knn.covered.v1 = knn.covered.v1,
    knn.covered.v2 = knn.covered.v2,
    knn.mse = knn.mse,
    knnbig.covered.v1 = knnbig.covered.v1,
    knnbig.covered.v2 = knnbig.covered.v2,
    knnbig.mse = knnbig.mse)
}

for (i in 1:9) {
  alpha = i/10
  results.raw = lapply(dvals, function(d) {
    print(paste("NOW RUNNING:", d))
    res.d = sapply(1:simu.reps, function(iter) simu.fun(d, alpha))
    res.fixed = data.frame(t(res.d))
    print(paste("RESULT AT", d, "IS", colMeans(res.fixed)))
    res.fixed
  })
  
  results.condensed = lapply(results.raw, function(RR) {
    RR.mu = colMeans(RR)
    RR.var = sapply(RR, var) / (nrow(RR) - 1)
    rbind("mu"=RR.mu, "se"=sqrt(RR.var))
  })
  
  results.condensed
  
  save.image(paste0("Test_Run_Results/output_paper1014_1_", alpha, "_0.5", ".RData"))
  
  results.parsed = lapply(results.condensed, function(RR) {
    apply(RR, 2, function(arg) {
      paste0(round(arg[1], 2), " (", round(100 * arg[2], 0), ")")
    })
  })
  
  results.table = data.frame(cbind(d=dvals, Reduce(rbind, results.parsed)))
  
  results.table = results.table[,c(1, 3, 6, 9, 2, 4, 7)]
  xtab = xtable(results.table)
#   print(xtab, include.rownames = FALSE)
}





# % latex table generated in R 3.2.0 by xtable 1.7-4 package
# % Sat Oct 17 20:32:45 2015
# \begin{table}[ht]
# \centering
# \begin{tabular}{lllllll}
# \hline
# d & rf.mse & knn.mse & knnbig.mse & rf.covered & knn.covered.v1 & knnbig.covered.v1 \\ 
# \hline
# 2 & 0.02 (0) & 0.21 (0) & 0.09 (0) & 0.96 (0) & 0.93 (0) & 0.62 (1) \\ 
# 5 & 0.02 (0) & 0.25 (0) & 0.12 (0) & 0.95 (1) & 0.92 (0) & 0.51 (1) \\ 
# 10 & 0.02 (0) & 0.28 (0) & 0.12 (0) & 0.93 (1) & 0.91 (0) & 0.51 (1) \\ 
# 15 & 0.02 (0) & 0.31 (0) & 0.13 (0) & 0.91 (1) & 0.9 (0) & 0.48 (1) \\ 
# 20 & 0.02 (0) & 0.32 (0) & 0.12 (0) & 0.88 (1) & 0.89 (0) & 0.5 (1) \\ 
# 30 & 0.02 (0) & 0.33 (0) & 0.13 (0) & 0.85 (1) & 0.89 (0) & 0.48 (1) \\ 
# 40 & 0.02 (0) & 0.34 (0) & 0.13 (0) & 0.8 (1) & 0.89 (0) & 0.47 (1) \\ 
# \hline
# \end{tabular}
# \end{table}

