library(randomForest)
library(randomForestCI)
library(Hmisc)

rm(list = ls())

n = 1000
p = 20
X = matrix(rnorm(n * p), n, p)
Y = factor(rbinom(n, 1, 0.5))

n.test = 1000
X.test = matrix(rnorm(n.test * p), n.test, p)

rf = randomForest(X, Y, keep.inbag = TRUE, ntree = 1000, replace = FALSE, sampsize = n/2)
ij = randomForestInfJack(rf, X.test, calibrate = TRUE)

y.hat = ij$y.hat
se.hat = sqrt(ij$var.hat)
up = y.hat + 1.96 * se.hat
down = y.hat - 1.96 * se.hat

errbar(1:n.test, y.hat, up, down)

covered = up >= 0.5 & down <= 0.5
mean(covered)