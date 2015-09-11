# class for TT (two tree) model
# methods: train, reestimate, predict

TT <- setClass(
  "TT",
  slots = c(tree1 = "rpart", tree0 = "rpart")
)

create.data.frame.for.tt <- function(X, W, Y, treatment.level) {
  index <- 1:length(W)
  if (!missing(treatment.level)) {
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

setGeneric(
  name = "train",
  def = function(tt, X, W, Y) {
    standardGeneric("train")
  }
)

setMethod(
  f = "train",
  signature = "TT",
  definition = function(tt, X, W, Y) {
    data1 <- create.data.frame.for.tt(X, W, Y, 1)
    data0 <- create.data.frame.for.tt(X, W, Y, 0)
    tt@tree1 <- rpart(y ~ ., data = data1, method = "anova")
    tt@tree0 <- rpart(y ~ ., data = data0, method = "anova")
    tt
  }
)

setGeneric(
  name = "reestimate",
  def = function(tt, X, W, Y) {
    standardGeneric("reestimate")
  }
)

setMethod(
  f = "reestimate",
  signature = "TT",
  definition = function(tt, X, W, Y) {
    data1 <- create.data.frame.for.tt(X, W, Y, 1)
    data0 <- create.data.frame.for.tt(X, W, Y, 0)
    tt@tree1 <- reestimate(tt@tree1, data1, Y)
    tt@tree0 <- reestimate(tt@tree0, data0, Y)
    tt
  }
)

setGeneric(
  name = "predict",
  def = function(tt, X) {
    standardGeneric("predict")
  }
)

setMethod(
  f = "predict",
  signature = "TT",
  definition = function(tt, X) {
    data <- create.data.frame.for.tt(X)
    predict(tt@tree1, data) - predict(tt@tree0, data)
  }
)

setGeneric(
  name = "count.leaves",
  def = function(tt) {
    standardGeneric("count.leaves")
  }
)

setMethod(
  f = "count.leaves",
  signature = "TT",
  definition = function(tt) {
    length(which(tt@tree1$frame$var == "<leaf>")) + length(which(tt@tree0$frame$var == "<leaf>"))
  }
)