setOldClass("rpart")
setOldClass("causalTree")

# class for ST (single tree) model
# methods: train.model, reestimate.model, predict.model, count.leaves

ST <- setClass(
  "ST",
  slots = c(tree = "rpart")
)

# class for TT (two tree) model
# methods: train.model, reestimate.model, predict.model, count.leaves

TT <- setClass(
  "TT",
  slots = c(tree1 = "rpart", tree0 = "rpart")
)

# class for CT (causal tree) model
# methods: train.model, reestimate.model, predict.model, count.leaves

CT <- setClass(
  "CT",
  slots = c(tree = "rpart", cv.option = "character", propensity = "numeric")
)

# helper methods

create.data.frame.for.st <- function(X, W, Y) {
  if (missing(Y)) {
    data <- data.frame(X, W)
    names(data) <- c(paste("x", as.character(1:ncol(X)), sep = ""), "w")
    data
  } else {
    data <- data.frame(X, W, Y)
    names(data) <- c(paste("x", as.character(1:ncol(X)), sep = ""), "w", "y")
    data
  }
}

create.data.frame.for.tt <- function(X, W, Y, treatment.level) {
  index <- 1:nrow(X)
  if (!missing(treatment.level) && !missing(W)) {
    index <- which(W == treatment.level)
  }
  if (missing(Y)) {
    data <- data.frame(X[index,])
    names(data) <- c(paste("x", as.character(1:ncol(X)), sep = ""))
    data
  } else {
    data <- data.frame(X[index,], Y[index])
    names(data) <- c(paste("x", as.character(1:ncol(X)), sep = ""), "y")
    data
  }
}

reestimate.causalTree.matching <- function(tree, X, W, Y) {
  leaf.assignments <- est.causalTree(tree, causalTree.matrix(create.data.frame(tree, X)))
  all.leaves <- get.all.leaves(tree)
  rownames <- as.numeric(rownames(tree$frame))
  for (leaf in all.leaves) {
    obs.in.leaf <- recursive.which.in.leaf(leaf.assignments, leaf, all.leaves)
    relevant.W <- W[obs.in.leaf]
    relevant.Y <- Y[obs.in.leaf]
    tree$frame$yval[which(rownames == leaf)[1]] <- sum(relevant.Y * relevant.W / sum(relevant.W)) - sum(relevant.Y * (1 - relevant.W) / sum(1 - relevant.W))
  }
  tree
}

reestimate.causalTree.TOT <- function(tree, X, W, Y, propensity) {
  leaf.assignments <- est.causalTree(tree, causalTree.matrix(create.data.frame(tree, X)))
  all.leaves <- get.all.leaves(tree)
  rownames <- as.numeric(rownames(tree$frame))
  transformed.Y <- Y * (W - propensity) / (propensity * (1 - propensity))
  for (leaf in all.leaves) {
    obs.in.leaf <- recursive.which.in.leaf(leaf.assignments, leaf, all.leaves)
    tree$frame$yval[which(rownames == leaf)[1]] <- mean(transformed.Y[obs.in.leaf])
  }
  tree
}

# methods for each class

setGeneric(
  name = "train.model",
  def = function(model, X, W, Y) {
    standardGeneric("train.model")
  }
)

setMethod(
  f = "train.model",
  signature("ST", "matrix", "integer", "numeric"),
  definition = function(model, X, W, Y) {
    data <- create.data.frame.for.st(X, W, Y)
    model@tree <- rpart(y ~ ., data = data, method = "anova")
    model
  }
)

setMethod(
  f = "train.model",
  signature("TT", "matrix", "integer", "numeric"),
  definition = function(model, X, W, Y) {
    data1 <- create.data.frame.for.tt(X, W, Y, 1)
    data0 <- create.data.frame.for.tt(X, W, Y, 0)
    model@tree1 <- rpart(y ~ ., data = data1, method = "anova")
    model@tree0 <- rpart(y ~ ., data = data0, method = "anova")
    model
  }
)

setMethod(
  f = "train.model",
  signature("CT", "matrix", "integer", "numeric"),
  definition = function(model, X, W, Y) {
    print("CT.train.model")
    model@tree <- causalTree(Y~., data = data.frame(X = X, Y = Y), treatment = W, method = "anova", parms = 1, minbucket = 1, cv.option = model@cv.option, p = 0.5, xval = 10)
    model
  }
)

setGeneric(
  name = "reestimate.model",
  def = function(model, X, W, Y) {
    standardGeneric("reestimate.model")
  }
)

setMethod(
  f = "reestimate.model",
  signature("ST", "matrix", "integer", "numeric"),
  definition = function(model, X, W, Y) {
    data <- create.data.frame.for.st(X, W)
    model@tree <- reestimate.rpart(model@tree, data, Y)
    model
  }
)

setMethod(
  f = "reestimate.model",
  signature("TT", "matrix", "integer", "numeric"),
  definition = function(model, X, W, Y) {
    data1 <- create.data.frame.for.tt(X, W, Y, 1)
    data0 <- create.data.frame.for.tt(X, W, Y, 0)
    model@tree1 <- reestimate.rpart(model@tree1, data1, Y)
    model@tree0 <- reestimate.rpart(model@tree0, data0, Y)
    model
  }
)

setMethod(
  f = "reestimate.model",
  signature("CT", "matrix", "integer", "numeric"),
  definition = function(model, X, W, Y) {
    if (model@cv.option == "matching") {
      model@tree <- reestimate.causalTree.matching(model@tree, X, W, Y)
    } else if (model@cv.option == "TOT") {
      model@tree <- reestimate.causalTree.TOT(model@tree, X, W, Y, model@propensity)
    }
    model
  }
)

setGeneric(
  name = "predict.model",
  def = function(model, X) {
    standardGeneric("predict.model")
  }
)

setMethod(
  f = "predict.model",
  signature("ST", "matrix"),
  definition = function(model, X) {
    W1 <- rep(1, nrow(X))
    W0 <- rep(0, nrow(X))
    data1<- create.data.frame.for.st(X, W1)
    data0 <- create.data.frame.for.st(X, W0)
    predict(model@tree, data1) - predict(model@tree, data0)
  }
)

setMethod(
  f = "predict.model",
  signature("TT", "matrix"),
  definition = function(model, X) {
    data <- create.data.frame.for.tt(X)
    predict(model@tree1, data) - predict(model@tree0, data)
  }
)

setMethod(
  f = "predict.model",
  signature("CT", "matrix"),
  definition = function(model, X) {
    est.causalTree.tau(model@tree, X)
  }
)


setGeneric(
  name = "count.leaves",
  def = function(model) {
    standardGeneric("count.leaves")
  }
)

setMethod(
  f = "count.leaves",
  signature("ST"),
  definition = function(model) {
    length(which(model@tree$frame$var == "<leaf>"))
  }
)

setMethod(
  f = "count.leaves",
  signature("TT"),
  definition = function(model) {
    length(which(model@tree1$frame$var == "<leaf>")) + length(which(model@tree0$frame$var == "<leaf>"))
  }
)

setMethod(
  f = "count.leaves",
  signature("CT"),
  definition = function(model) {
    length(which(model@tree$frame$var == "<leaf>"))
  }
)