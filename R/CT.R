# class for CT (causal tree) model
# methods: train, reestimate, predict

CT <- setClass(
  "CT",
  slots = c(tree = "causalTree", cv.option = "character")
)

setGeneric(
  name = "train",
  def = function(ct, X, W, Y) {
    standardGeneric("train")
  }
)

setMethod(
  f = "train",
  signature = "CT",
  definition = function(ct, X, W, Y) {
    ct@tree <- causalTree(Y~., data = data.frame(X = X, Y = Y), weights = W, method = "anova", parms = 1, minbucket = 1, cv.option = ct@cv.option, p = 0.5, xval = 10)
    ct
  }
)

reestimate.causalTree.matching <- function(tree, X, W, Y) {
  
}

reestimate.causalTree.TOT <- function(tree, X, W, Y) {
  
}

setGeneric(
  name = "reestimate",
  def = function(ct, X, W, Y) {
    standardGeneric("reestimate")
  }
)

setMethod(
  f = "reestimate",
  signature = "CT",
  definition = function(ct, X, W, Y) {
    estimated.leaves <- est.causalTree(tree, X)
    leaves <- which(tree$frame$var == "<leaf>")
    for (leaf in leaves) {
      obs.in.leaf <- which(estimated.leaves == leaf)
      relevant.W <- W[obs.in.leaf]
      relevant.Y <- Y[obs.in.leaf]
      tree$frame$yval[leaf] <- sum(relevant.Y * relevant.W / sum(relevant.W)) - sum(relevant.Y * (1 - relevant.W) / sum(1 - relevant.W))
    }
  }
)

setGeneric(
  name = "predict",
  def = function(ct, X) {
    standardGeneric("predict")
  }
)

setMethod(
  f = "predict",
  signature = "CT",
  definition = function(ct, X) {
    est.causalTree.tau(ct@tree, X)
  }
)