predict.causalForest <- function(object, newdata, predict.all = FALSE) {
  
  if (any(is.na(newdata))) {
    stop("There is mising data in the input.")
  }
  
  #
  # TODO: This is a symptom of a hack. Should be fixed.
  #
  
  if (class(newdata) == "data.frame") {
    colnames(newdata) <- 1:ncol(newdata)
  }
  
  ntree <- object$ntree
  individual <- matrix(0, nrow(newdata), ntree)
  for (tree.index in 1:ntree) {
    individual[, tree.index] <- causalTree:::est.causalTree.tau(object$trees[[tree.index]], newdata)
  }
  
  aggregate <- rowMeans(individual)
  if (predict.all) {
    list(aggregate = aggregate, individual = individual)
  } else {
    aggregate
  }
  
}