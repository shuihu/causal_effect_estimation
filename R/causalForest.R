causalForest <- function(X, Y, W, num.trees, sample.size = floor(length(Y) / 10), mtry = ceiling(ncol(X)/3), nodesize = 1) {
  
  num.obs <-nrow(X)
  comparisonForest.honest <- causalTree:::init.randomForest(num.obs, num.trees)
  sample.size <- min(sample.size, floor(num.obs / 2))
  
  print("Building trees ...")
  
  for (tree.index in 1:num.trees) {
    
    print(paste("Tree", as.character(tree.index)))
    
    full.idx <- sample.int(num.obs, 2 * sample.size, replace = FALSE)
    structure.idx <- full.idx[1:sample.size]
    evaluation.idx <- full.idx[sample.size + (1:sample.size)]
    
    tree.standard <- causalTree(Y ~ ., data = data.frame(X = X[structure.idx,], Y = Y[structure.idx]), treatment = W[structure.idx], method = "anova", cp = 0, parms = 20, minbucket = nodesize, cv.option = "matching", xval = 0)
    
    tree.honest <- causalTree:::reestimate.tau(tree.standard, Y[evaluation.idx], data.frame(X = X[evaluation.idx,]), W[evaluation.idx])
    
    comparisonForest.honest$trees[[tree.index]] <- tree.honest
    comparisonForest.honest$pred.matrix[, tree.index] <- causalTree:::est.causalTree.tau(tree.honest, X)
    comparisonForest.honest$inbag[full.idx, tree.index] <- 1
  }
  
  comparisonForest.honest$tau <- rowMeans(comparisonForest.honest$pred.matrix)
  
  return(comparisonForest.honest)
}
