# To be removed

predict.randomForest.shuihu <- function(forest, X) {
  ntree <- forest$ntree
  individual <- matrix(0, nrow(X), ntree)
  for (tree.index in 1:ntree) {
    individual[, tree.index] <- est.causalTree.tau(forest$trees[[tree.index]], X)
  }
  list(individual = individual)
}