predict.causalForest <- function(object, newdata, predict.all = FALSE) {
  if (any(is.na(newdata))) {
    stop("There is mising data in the input.")
  }
  if (class(newdata) == "data.frame") {
    colnames(newdata) <- 1:ncol(newdata)
  }
  ntree <- object$ntree
  individual <- matrix(0, nrow(X), ntree)
  for (tree.index in 1:ntree) {
    individual[, tree.index] <- causalTree:::est.causalTree.tau(object$trees[[tree.index]], X)
  }
  aggregate <- rowMeans(individual)
  if (predict.all) {
    list(aggregate = aggregate, individual = individual)
  } else {
    list(aggregate = aggregate)
  }
}