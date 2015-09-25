## TODO: 
## The notion we really want is that comparison forests implement
## a random forest like interface

init.causalForest <- function(num.obs, num.trees) {
  trees <- vector("list", num.trees)
  y <- rep(0, num.obs)
  pred.matrix <- matrix(0, num.obs, num.trees)
  inbag <- matrix(0, num.obs, num.trees)
  variance <- rep(0, num.obs)
  causalForest <- list(trees = trees, y = y, ntree = num.trees, inbag = inbag, pred.matrix = pred.matrix, variance = variance)
  class(causalForest) <- "causalForest"
  causalForest
}