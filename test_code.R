NUM = 20
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

### test on this small sample:
library(rpart)
library(rpart.plot)
#sink("small_simulation_test.txt")


tree <- rpart(y~., data = tr, weights = w, method = "anova", cp = 0, 
              cv.option = "TOT", parms = 1, minbucket = 1, p = 0.5, xval  = xgroups)
tree
tree$cp
# find the optimal cp:
opcp <- tree$cp[which(tree$cp[,4] == min(tree$cp[,4])), 1]
opcp

# prune the tree
#prune_tree <- rpart(y~., data = tr, weights = w, method = "anova", cp = opcp, 
#              cv.option = "TOT", parms = 1, minbucket = 1, xval = 0)
#rpart.plot(prune_tree)


## validation:
xerror <- function(fit, weights) {
  p <- sum(weights) / length(weights)
  prediction <- xpred.rpart(tree, xgroups) 
  prediction
  n_cp <- nrow(fit$cptable)
  treat <- which(weights == 1)
  control <- which(weights == 0)
  y_star <- fit$y
  y_star[treat] <- y_star[treat] / p
  y_star[control] <- y_star[control] / (p - 1)
  error <- rep(0, n_cp)
  for (i in 1:n_cp) {
    yhat <- prediction[ ,i]
    error[i] <- sum((yhat - y_star)^2)
  }
  names(error) = signif(fit$cp[ ,1], digits = 6)
  opcp = fit$cp[which(error == min(error)), 1]
  newlist = list(xerror = error, optimal_cp = opcp)
  newlist
  return (newlist)
}

res <-xerror(tree, w)
res$optimal_cp
res
