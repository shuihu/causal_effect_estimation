### test code for matching CV method:

library(rpart)
library(rpart.plot)

# function to measure distance between samples:
measure_distance <- function(data, i, j) {
  ## y data is stored in the last column
  xdata <- data[,-ncol(data)]
  xvar <- apply(xdata, 2, var)
  nvar <- length(xvar)
  distance <- 0
  for (k in 1:nvar) {
    distance <- distance + (xdata[i, k] - xdata[j, k]) * (xdata[i, k] - xdata[j, k]) / xvar[k]   
  }
  return (distance) 
}

# find the neighbor in the same validation group && with oppossite treatment status
find_neighbor <- function(data, xgroups, w) {
  n <- nrow(data)
  neighbors<- rep(0, n)
  for (i in 1:n) {
    min = 10000
    same_group_id <- setdiff(which(xgroups == xgroups[i]), i)
    for (j in 1: length(same_group_id)) {
      if (w[same_group_id[j]] != w[i]) {
        dist <- measure_distance(data, i, same_group_id[j])
        if (dist < min) {
          min <- dist
          neighbor_id <- same_group_id[j]
        }
      }
    }
    neighbors[i] <- neighbor_id
  }
  return (neighbors)
}

# cross-validation predicting function
xerror2 <- function(fit, data, w) {
  prediction <- xpred.rpart(fit, xgroups)
  sum(is.na(prediction))
  n_cp <- nrow(fit$cptable)
  neighbor_id <- find_neighbor(data, xgroups,w)
  estimate <- (2 * w - 1) * (fit$y - fit$y[neighbor_id])
  error <- rep(0, n_cp)
  for (i in 1:n_cp) {
    yhat <- prediction[,i]
    that <- 0.5 * (yhat[neighbor_id] + yhat)
    error[i] <- sum((that - estimate)^2)
  }
  names(error) = signif(fit$cp[ ,1], digits = 6)
  opcp = fit$cp[which(error == min(error)), 1]
  newlist = list(xerror = error, optimal_cp = opcp)
  return (newlist)
}

# generate data samples and cross validation groups
NUM = 500
set.seed(1991)
x1 <- rnorm(NUM, 0, 1)
x2 <- rnorm(NUM, 0, 1)
y1 <- rnorm(NUM, x1 + x2 + 1, 1)
y0 <- rnorm(NUM, x1 - x2 + 0, 1)
X1 <- c(x1, x1)
X2 <- c(x2, x2)
y <- c(y1, y0)
w <- rep(1:0, each = NUM)
tr <- data.frame(X1, X2, y)
xgroups <- rep(0, 2 * NUM)
treat_idx <- which(w == 1)
control_idx <- which(w == 0)
xval <- 10
set.seed(1994)
xgroups[control_idx] <- sample(rep(1L:xval, length = length(control_idx)), length(control_idx), replace = F)
set.seed(1991)
xgroups[treat_idx] <- sample(rep(1L:xval, length = length(treat_idx)), length(treat_idx), replace = F)  

# compare begin
fit <- rpart(y~., data = tr, weights = w, method = "anova", cp = 0, 
             cv.option = "matching", parms = 1, minbucket = 1, xval = xgroups)
opcp <- fit$cp[which(fit$cp[,4] == min(fit$cp[,4])), 1]
opcp
pred <- xerror2(fit, tr, w)
pred_opcp <- pred$optimal_cp
pred_opcp
newlist <-list(cp1 = opcp, cp2 = pred_opcp)
newlist

# prune the tree
fit2 <- rpart(y~., data = tr, weights = w, method = "anova", cp = opcp,
              cv.option = "matching", parms = 1, minbucket = 1, xval = 0 )
rpart.plot(fit2)


