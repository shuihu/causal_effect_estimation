RandomForest <- setClass(
  "RandomForest",
  slots = c(trees = "vector"),
  prototype = list(trees = vector("list", 0))
)

setGeneric(
  name = "addTree",
  def = function(randomForest, tree) {
    standardGeneric("addTree")
  }
)

setMethod(
  f = "addTree",
  signature = "RandomForest",
  definition = function(randomForest, tree) {
    randomForest@trees <- c(randomForest@trees, tree)
    randomForest
  }
)

init.RandomForest <- function(num.trees) {
  randomForest <- RandomForest()
  randomForest@trees <- vector("list", num.trees)
  randomForest
}
