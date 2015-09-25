source("simulations/models.R")
source("simulations/compute.os.m.R")
source("simulations/compute.os.to.R")
source("simulations/compute.os.infeasible.R")

init.model <- function(model.name, propensity) {
  tree <- list()
  class(tree) <- "rpart"
  if (model.name == 'ST') {
    ST()
  } else if (model.name == 'TT') {
    TT()
  } else if (model.name == "TOT_split_xval_rpart") {
    TOT(tree = tree, propensity = propensity)
  } else if (model.name == "TOT_xval") {
    CT(tree = tree, cv.option = "TOT", propensity = propensity)
  } else if (model.name == "CT") {
    CT(tree = tree, cv.option = "matching")
  } else {
    stop("model.name must be 'ST', 'TT', 'TOT_split_xval_rpart', 'TOT_xval', or 'CT'")
  }
}

compute.tree.stats <- function(split.XW, split.Y, estimation.XW, estimation.Y, test.XW, test.Y, counterfactual.test.Y, model.name, propensity, matchIndices, is.honest) {
  initial.model <- init.model(model.name, propensity)
  trained.split.model <- train.model(initial.model, split.XW$X, split.XW$W, split.Y)
  if (is.honest) {
    trained.prediction.model <- reestimate.model(trained.split.model, estimation.XW$X, estimation.XW$W, estimation.Y)
  } else {
    trained.prediction.model <- trained.split.model
  }
  test.preds <- predict.model(trained.prediction.model, test.XW$X)
  #if (model.name == "CT") {
  #  write.table(test.preds, "test.preds.csv", sep = ",", row.names = FALSE, col.names = FALSE)
  #}
  num.leaves <- count.leaves(trained.prediction.model)
  os.to <- compute.os.to(test.XW, test.Y, test.preds, propensity)
  os.m <-compute.os.m(test.XW, test.Y, test.preds, matchIndices)
  os.infeasible <- compute.os.infeasible(test.XW, test.Y, test.preds, counterfactual.test.Y)
  list(num.leaves = num.leaves, os.to = os.to, os.m = os.m, os.infeasible = os.infeasible)
}
