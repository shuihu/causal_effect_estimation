# class for ST (single tree) model
# methods: train, reestimate, predict

ST <- setClass(
  "ST",
  slots = c(tree = "rpart")
)

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

setGeneric(
  name = "train",
  def = function(st, X, W, Y) {
    standardGeneric("train")
  }
)

setMethod(
  f = "train",
  signature = "ST",
  definition = function(st, X, W, Y) {
    data <- create.data.frame.for.st(X, W, Y)
    st@tree <- rpart(y ~ ., data = data, method = "anova")
    st
  }
)

setGeneric(
  name = "reestimate",
  def = function(st, X, W, Y) {
    standardGeneric("reestimate")
  }
)

setMethod(
  f = "reestimate",
  signature = "ST",
  definition = function(st, X, W, Y) {
    data <- create.data.frame.for.st(X, W)
    st@tree <- reestimate.rpart(st@tree, data, Y)
    st
  }
)

setGeneric(
  name = "predict",
  def = function(st, X) {
    standardGeneric("predict")
  }
)

setMethod(
  f = "predict",
  signature = "ST",
  definition = function(st, X) {
    W1 <- rep(1, nrow(X))
    W0 <- rep(0, nrow(X))
    data1<- create.data.frame.for.st(X, W1)
    data0 <- create.data.frame.for.st(X, W0)
    predict(st@tree, data1) - predict(st@tree, data)
  }
)

setGeneric(
  name = "count.leaves",
  def = function(st) {
    standardGeneric("count.leaves")
  }
)

setMethod(
  f = "count.leaves",
  signature = "ST",
  definition = function(st) {
    length(which(st@tree$frame$var == "<leaf>"))
  }
)