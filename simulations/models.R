setOldClass("rpart")
source("simulations/get.all.leaves.R")
source("simulations/reestimate.rpart.R")

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

# class for TOT model using rpart
# methods: train.model, reestimate.model, predict.model, count.leaves

TOT <- setClass(
  "TOT",
  slots = c(tree = "rpart", propensity = "numeric")
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

create.data.frame.for.tot <- function(X, W, Y, propensity) {
  if (!missing(W) && !missing(Y)) {
    transformed.Y <- Y * (W - propensity) / (propensity * (1 - propensity))
    data <- data.frame(X, transformed.Y)
    names(data) <- c(paste("x", as.character(1:ncol(X)), sep = ""), "y")
    data
    data
  } else {
    data <- data.frame(X)
    names(data) <- c(paste("x", as.character(1:ncol(X)), sep = ""))
    data
  }
}

reestimate.causalTree <- function(tree, X, W, Y) {
  leaf.assignments <- est.causalTree(tree, causalTree.matrix(create.data.frame(tree, X)))
  all.leaves <- get.all.leaves(tree)
  rownames <- as.numeric(rownames(tree$frame))
  tree$where2 <- matrix(0, length(rownames), length(Y))
  for (leaf in all.leaves) {
    obs.in.leaf <- recursive.which.in.leaf(leaf.assignments, leaf, all.leaves, W)
    tree$where2[which(rownames == leaf)[1], obs.in.leaf] <- 1
    relevant.W <- W[obs.in.leaf]
    relevant.Y <- Y[obs.in.leaf]
    tree$frame$yval[which(rownames == leaf)[1]] <- sum(relevant.Y * relevant.W / sum(relevant.W)) - sum(relevant.Y * (1 - relevant.W) / sum(1 - relevant.W))
  }
  tree
}

get.optimal.cp <- function(tree) {
  max(tree$cp[which(tree$cp[,'xerror'] == min(tree$cp[,'xerror'])), 'CP'])
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
    unpruned.tree <- rpart(y ~ ., data = data, method = "anova", cp = 0, control = rpart.control())
    optimal.cp <- get.optimal.cp(unpruned.tree)
    model@tree <- prune(unpruned.tree, cp = optimal.cp)
    model
  }
)

setMethod(
  f = "train.model",
  signature("TT", "matrix", "integer", "numeric"),
  definition = function(model, X, W, Y) {
    data1 <- create.data.frame.for.tt(X, W, Y, 1)
    data0 <- create.data.frame.for.tt(X, W, Y, 0)
    unpruned.tree1 <- rpart(y ~ ., data = data1, method = "anova", cp = 0, control = rpart.control())
    optimal.cp1 <- get.optimal.cp(unpruned.tree1)
    model@tree1 <- prune(unpruned.tree1, cp = optimal.cp1)
    unpruned.tree0 <- rpart(y ~ ., data = data0, method = "anova", cp = 0, control = rpart.control())
    optimal.cp0 <- get.optimal.cp(unpruned.tree0)
    model@tree0 <- prune(unpruned.tree0, cp = optimal.cp0)
    model
  }
)

setMethod(
  f = "train.model",
  signature("TOT", "matrix", "integer", "numeric"),
  definition = function(model, X, W, Y) {
    data <- create.data.frame.for.tot(X, W, Y, model@propensity)
    unpruned.tree <- rpart(y ~ ., data = data, method = "anova", cp = 0, control = rpart.control())
    optimal.cp <- get.optimal.cp(unpruned.tree)
    model@tree <- prune(unpruned.tree, cp = optimal.cp)
    model
  }
)

setMethod(
  f = "train.model",
  signature("CT", "matrix", "integer", "numeric"),
  definition = function(model, X, W, Y) {
    unpruned.tree <- causalTree(Y~., data = data.frame(X = X, Y = Y), weights = W, method = "anova", parms = 7, cp = 0, cv.option = model@cv.option, p = 0.5)
    optimal.cp <- get.optimal.cp(unpruned.tree)
    model@tree <- prune(unpruned.tree, cp = optimal.cp)
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
    y1 <- data1$y
    y0 <- data0$y
    data1$y <- NULL
    data0$y <- NULL
    model@tree1 <- reestimate.rpart(model@tree1, data1, y1)
    model@tree0 <- reestimate.rpart(model@tree0, data0, y0)
    model
  }
)

setMethod(
  f = "reestimate.model",
  signature("TOT", "matrix", "integer", "numeric"),
  definition = function(model, X, W, Y) {
    data <- create.data.frame.for.tot(X, W, Y, model@propensity)
    model@tree <- reestimate.rpart(model@tree, data, data$y)
    model
  }
)

setMethod(
  f = "reestimate.model",
  signature("CT", "matrix", "integer", "numeric"),
  definition = function(model, X, W, Y) {
    model@tree <- reestimate.causalTree(model@tree, X, W, Y)
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
  signature("TOT", "matrix"),
  definition = function(model, X) {
    data <- create.data.frame.for.tot(X)
    predict(model@tree, data)
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
  signature("TOT"),
  definition = function(model) {
    length(which(model@tree$frame$var == "<leaf>"))
  }
)

setMethod(
  f = "count.leaves",
  signature("CT"),
  definition = function(model) {
    length(which(model@tree$frame$var == "<leaf>"))
  }
)