causalForest_Thai <- function(X, Y, W, num.trees, subsample.fraction, Jsample.fraction = 0.5, mtry = ceiling(ncol(X)/3), nodesize = 1) {
  #sample.size = floor(length(Y) / 10)

  if (any(is.na(X)) || any(is.na(Y)) || any(is.na(W))) {
    stop("There are missing values in the input.")
  }

  num.obs <-nrow(X)
  causalForest.honest <- causalTree:::init.causalForest(X, Y, W, num.trees)

  # I sample size
  sample.size <- num.obs * subsample.fraction * (1 - Jsample.fraction)
  
  print("Building trees ...")
  
  for (tree.index in 1:num.trees) {
    
    print(paste("Tree", as.character(tree.index)))
    
    full.idx <- sample.int(num.obs, num.obs * subsample.fraction, replace = FALSE)
    train.idx <- full.idx[1:sample.size]
    reestimation.idx <- full.idx[(sample.size + 1) : num.obs * subsample.fraction]
    
    tree.standard <- causalTree(Y ~ ., data = data.frame(X = X[train.idx,], Y = Y[train.idx]), treatment = W[train.idx], method = "anova", cp = 0, minbucket = nodesize, cv.option = "matching", split.option = "CT", xval = 0)
    
    tree.honest <- causalTree:::reestimate.tau(tree.standard, Y[reestimation.idx], data.frame(X = X[reestimation.idx,]), W[reestimation.idx])
    
    causalForest.honest$trees[[tree.index]] <- tree.honest
    causalForest.honest$inbag[full.idx, tree.index] <- 1
  }
  
  return(causalForest.honest)
}
