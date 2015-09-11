# Returns the predictions and covariances of the "honest" random forest
# and "standard" random forest built from the causal tree.  The "standard" random
# forest averages the predictions produced by the training sample, while the
# "honest" random forest averages the predictions produced by applying a separate estimation
# sample to the same trees produced by the training sample.
# The result is: list(standard = randomForest.standard, honest = randomForest.honest)
# where randomForest.standard is a randomForest object for the standard (dishonest) forest
# while randomForest.honest is a randomForest object for the honest forest.

compare.forests <- function(Y, X, W, num.trees, sample.size, node.size, cv.option, seed) {
  if (!missing(seed)) {
    set.seed(seed)
  }
  num.obs <-nrow(X)
  randomForest.standard <- init.randomForest(num.obs, num.trees)
  randomForest.honest <- init.randomForest(num.obs, num.trees)
  sample.size <- min(sample.size, floor(num.obs / 2))
  print("Building trees ...")
  for (tree.index in 1:num.trees) {
    print(paste("Tree", as.character(tree.index)))
    sample.standard <- create.sample(Y, X, W, sample.size, replace = T)
    sample.honest <- create.sample(Y, X, W, sample.size, replace = T)
    # set the standard inbag[i, j] to 1 if and only if the ith observation is in the standard sample
    randomForest.standard$inbag[sample.standard$indices, tree.index] = 1
    # set the honest inbag[i, j] to 1 if and only if the ith observation is in at least one of the samples
    randomForest.honest$inbag[sample.honest$indices, tree.index] = 1
    randomForest.honest$inbag[sample.standard$indices, tree.index] = 1
    tree.standard <- causalTree(Y~., data = data.frame(X = sample.standard$X, Y = sample.standard$Y), treatment = sample.standard$W, method = "anova", cp = 0, parms = node.size, minbucket = 1, cv.option = cv.option)
    optimal.cp.standard <- tree.standard$cp[which.min(tree.standard$cp[,'xerror']), 'CP']
    pruned.tree.standard <- prune(tree.standard, cp = optimal.cp.standard)
    randomForest.standard$trees[[tree.index]] <- pruned.tree.standard
    randomForest.standard$pred.matrix[, tree.index] <- est.causalTree.tau(pruned.tree.standard, X)
    pruned.tree.honest <- reestimate.tau(pruned.tree.standard, sample.honest$Y, sample.honest$X, sample.honest$W)
    randomForest.honest$trees[[tree.index]] <- pruned.tree.honest
    randomForest.honest$pred.matrix[, tree.index] <- est.causalTree.tau(pruned.tree.honest, X)
  }
  print("Compute the variances")
  randomForest.standard$y <- rowMeans(randomForest.standard$pred.matrix)
  randomForest.honest$y <- rowMeans(randomForest.honest$pred.matrix)
  for (i in 1:num.obs) {
    for (j in 1:num.obs) {
      randomForest.standard$variance[i] <- randomForest.standard$variance[i] + (cov(randomForest.standard$pred.matrix[i,], randomForest.standard$inbag[j,]))^2
      randomForest.honest$variance[i] <- randomForest.honest$variance[i] + (cov(randomForest.honest$pred.matrix[i,], randomForest.honest$inbag[j,]))^2
    }
  }
  list(standard = randomForest.standard, honest = randomForest.honest)
}
