# Returns the predictions and covariances of the "honest" random forest
# and "standard" random forest built from the causal tree.  The "standard" random
# forest averages the predictions produced by the training sample, while the
# "honest" random forest averages the predictions produced by applying a separate estimation
# sample to the same trees produced by the training sample.
# The result is: list(pred.honest, pred.standard, pred.honest.matrix, pred.standard.matrix, use.matrix, variance.honest, variance.standard)

compare.forests <- function(Y, X, W, num.trees, sample.size, node.size, cv.option, seed) {
  if (!missing(seed)) {
    set.seed(seed)
  }
  num.obs <-nrow(X)
  comparison.results <- init.compare.forests.results(num.obs, num.trees)
  sample.size <- min(sample.size, floor(num.obs / 2))
  print("Building trees ...")
  for (tree.index in 1:num.trees) {
    print(paste("Tree", as.character(tree.index)))
    sample.standard <- create.sample(Y, X, W, sample.size, replace = T)
    sample.honest <- create.sample(Y, X, W, sample.size, replace = T)
    # set use.matrix.standard[i, j] to 1 if and only if the ith observation is in the standard sample
    comparison.results$use.matrix.standard[sample.standard$indices, tree.index] = 1
    # set use.matrix.honest[i, j] to 1 if and only if the ith observation is in at least one of the samples
    comparison.results$use.matrix.honest[sample.honest$indices, tree.index] = 1
    comparison.results$use.matrix.honest[sample.standard$indices, tree.index] = 1
    tree.standard <- causalTree(Y~., data = data.frame(X = sample.standard$X, Y = sample.standard$Y), weights = sample.standard$W, method = "anova", cp = 0, parms = node.size, minbucket = 1, cv.option = cv.option)
    optimal.cp.standard <- tree.standard$cp[which.min(tree.standard$cp[,'xerror']), 'CP']
    pruned.tree.standard <- prune(tree.standard, cp = optimal.cp.standard)
    comparison.results$pred.standard.matrix[, tree.index] <- est.causalTree.tau(pruned.tree.standard, X)
    pruned.tree.honest <- reestimate.tau(pruned.tree.standard, sample.honest$Y, sample.honest$X, sample.honest$W)
    comparison.results$pred.honest.matrix[, tree.index] <- est.causalTree.tau(pruned.tree.honest, X)
  }
  print("Compute the variances")
  comparison.results$pred.standard <- rowMeans(comparison.results$pred.standard.matrix)
  comparison.results$pred.honest <- rowMeans(comparison.results$pred.honest.matrix)
  for (i in 1:num.obs) {
    for (j in 1:num.obs) {
      comparison.results$variance.standard[i] <- comparison.results$variance.standard[i] + (cov(comparison.results$pred.standard.matrix[i,], comparison.results$use.matrix.standard[j,]))^2
      comparison.results$variance.honest[i] <- comparison.results$variance.honest[i] + (cov(comparison.results$pred.honest.matrix[i,], comparison.results$use.matrix.honest[j,]))^2
    }
  }
  comparison.results
}