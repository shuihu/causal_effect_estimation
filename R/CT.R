# class for CT (causal tree) model
# methods: train, reestimate, predict

CT <- setClass(
  "CT",
  slots = c(tree = "causalTree", cv.option = "character", propensity = "numeric")
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
    ct@tree <- causalTree(Y~., data = data.frame(X = X, Y = Y), treatment = W, method = "anova", parms = 1, minbucket = 1, cv.option = ct@cv.option, p = 0.5, xval = 10)
    ct
  }
)

reestimate.causalTree.matching <- function(tree, X, W, Y) {
  leaf.assignments <- est.causalTree(tree, X)
  all.leaves <- get.all.leaves(tree)
  for (leaf in all.leaves) {
    obs.in.leaf <- recursive.which.in.leaf(leaf.assignments, leaf, all.leaves)
    relevant.W <- W[obs.in.leaf]
    relevant.Y <- Y[obs.in.leaf]
    tree$frame$yval[which(rownames == leaf)[1]] <- sum(relevant.Y * relevant.W / sum(relevant.W)) - sum(relevant.Y * (1 - relevant.W) / sum(1 - relevant.W))
  }
  tree
}

reestimate.causalTree.TOT <- function(tree, X, W, Y, propensity) {
  leaf.assignments <- est.causalTree(tree, X)
  all.leaves <- get.all.leaves(tree)
  transformed.Y <- Y * (W - propensity) / (propensity * (1 - propensity))
  for (leaf in all.leaves) {
    obs.in.leaf <- recursive.which.in.leaf(leaf.assignments, leaf, all.leaves)
    tree$frame$yval[which(rownames == leaf)[1]] <- mean(transformed.Y[obs.in.leaf])
  }
  tree
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
    if (ct@cv.option == "matching") {
      ct@tree <- reestimate.causalTree.matching(ct@tree, X, W, Y)
    } else if (ct@cv.option == "TOT") {
      ct@tree <- reestimate.causalTree.TOT(ct@tree, X, W, Y, ct@propensity)
    }
    ct
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

setGeneric(
  name = "count.leaves",
  def = function(ct) {
    standardGeneric("count.leaves")
  }
)

setMethod(
  f = "count.leaves",
  signature = "CT",
  definition = function(ct) {
    length(which(ct@tree$frame$var == "<leaf>"))
  }
)