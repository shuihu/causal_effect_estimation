# Returns the predictions and covariances of the "honest" random forest
# and "standard" random forest built from the causal tree.  The "standard" random
# forest averages the predictions produced by the training sample, while the
# "honest" random forest averages the predictions produced by applying a separate estimation
# sample to the same trees produced by the training sample.
# The result is: list(pred.honest, pred.standard, pred.honest.matrix, pred.standard.matrix, use.matrix, variance.honest, variance.standard)

compare.forests <- function(Y, X, W, num.trees, sample.size, node.size, cv.option) {
  num.obs <-nrow(X)
  comparison.results <- init.compare.forests.results(num.obs, num.trees)
  sample.size <- min(sample.size, floor(num.obs / 2))
  for (tree.index in 1:num.trees) {
    sample.standard <- create.sample(Y, X, W, sample.size, replace = T)
    sample.honest <- create.sample(Y, X, W, sample.size, replace = T)
    # set use.matrix[i, j] to 1 if and only if the ith observation is in at least one of the samples
    comparison.results$use.matrix[sample.standard$indices, tree.index] = 1
    comparison.results$use.matrix[sample.honest$indices, tree.index] = 1
    
  }
}