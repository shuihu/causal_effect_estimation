##
## TODO: currently the code reads in private methods explicity
## to make debugging easier....
##
## TODO: make it so test points do not need to be supplied at
## training time
##
## TODO: random forests do not have pruning, so ideally
## we wouldn't need to carry around a cv.option or a cp argument
##
## TODO: implement mtry
##


comparisonForest <- function(Y, X, W, num.trees, X.test = NULL, sample.size = length(Y) / 10, mtry = ceiling(ncol(X)/3), node.size = 5, cv.option = "matching") {
  
  num.obs <-nrow(X)
  comparisonForest.honest <- causalTree:::init.randomForest(num.obs, num.trees)
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
     
    tree.honest <- causalTree:::reestimate.tau(tree.standard, Y[evaluation.idx], X[evaluation.idx,], W[evaluation.idx])
    
    comparisonForest.honest$trees[[tree.index]] <- tree.honest
    comparisonForest.honest$pred.matrix[, tree.index] <- causalTree:::est.causalTree.tau(tree.honest, X)
    comparisonForest.honest$inbag[full.idx, tree.index] <- 1
    
    ##
    ## TODO: this is a temporary hack. Eventually, comparison forests
    ## will have a "predict" method, so we won't need to operated on
    ## X.test at training time.
    ##
    
    if(!is.null(X.test)) {
  		new.pred[, tree.index] <- causalTree:::est.causalTree.tau(tree.honest, X.test)
  	}
  }
  
  comparisonForest.honest$tau <- rowMeans(comparisonForest.honest$pred.matrix)
  
  comparisonForest.honest$new.pred = new.pred
  comparisonForest.honest$new.tau = rowMeans(new.pred)
  
  return(comparisonForest.honest)
}
