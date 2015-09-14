## TODO: we probably want to call this a comparisonForest
## instead of a randomForest.
##
## The notion we really want is that comparison forests implement
## a random forest like interface
##
## Also -- I removed the "variance" field, since variance is not a
## primitive of a forest (computing variance is optional post-processing)

init.randomForest <- function(num.obs, num.trees) {
  trees <- vector("list", num.trees)
  y <- rep(0, num.obs)
  pred.matrix <- matrix(0, num.obs, num.trees)
  inbag <- matrix(0, num.obs, num.trees)
  variance <- rep(0, num.obs)
  random.forest <- list(trees = trees, y = y, ntree = num.trees, inbag = inbag, pred.matrix = pred.matrix, variance = variance)
  class(random.forest) <- "randomForest"
  random.forest
}