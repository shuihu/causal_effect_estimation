
comparisonForest <- function(Y, X, W, num.trees, X.test = NULL, sample.size = length(Y) / 10, node.size = 5, cv.option = "matching") {
  
  num.obs <-nrow(X)
  randomForest.honest <- causalTree:::init.randomForest(num.obs, num.trees)
  sample.size <- min(sample.size, floor(num.obs / 2))
  
  new.pred <- NULL
  if(!is.null(X.test)) {
  	new.pred <- matrix(0, nrow(X.test), num.trees)
  }
  
  print("Building trees ...")
  
  for (tree.index in 1:num.trees) {
    
    print(paste("Tree", as.character(tree.index)))
    full.idx <- sample.int(n, 2 * sample.size, replace = FALSE)
    structure.idx <- full.idx[1:sample.size]
    evaluation.idx <- full.idx[sample.size + (1:sample.size)]
    
    tree.standard <- causalTree(Y ~ ., data = data.frame(X = X[structure.idx,], Y = Y[structure.idx]), treatment = W[structure.idx], method = "anova", cp = 0, minsize = node.size, cv.option = cv.option)
    
    #optimal.cp.standard <- tree.standard$cp[which.min(tree.standard$cp[,'xerror']), 'CP']
    # pruned.tree.standard <- prune(tree.standard, cp = optimal.cp.standard)
     
    pruned.tree.honest <- causalTree:::reestimate.tau(tree.standard, Y[evaluation.idx], X[evaluation.idx,], W[evaluation.idx])
    
    randomForest.honest$trees[[tree.index]] <- pruned.tree.honest
    randomForest.honest$pred.matrix[, tree.index] <- causalTree:::est.causalTree.tau(pruned.tree.honest, X)
    
    if(!is.null(X.test)) {
  		new.pred[, tree.index] <- causalTree:::est.causalTree.tau(pruned.tree.honest, X.test)
  	}
  }
  
  randomForest.honest$tau <- rowMeans(randomForest.honest$pred.matrix)
  
  randomForest.honest$new.pred = new.pred
  randomForest.honest$new.tau = rowMeans(new.pred)
  
  return(randomForest.honest)
}
