init.model <- function(model.name, propensity) {
  if (model.name == 'ST') {
    ST()
  } else if (model.name == 'TT') {
    TT()
  } else if (model.name == "TOT") {
    CT(cv.option = "TOT", propensity = propensity)
  } else if (model.name == "CT") {
    CT(cv.option = "matching")
  } else {
    stop("model.name must be 'ST', 'TT', 'TOT', or 'CT'")
  }
}

compute.tree.stats <- function(split.XW, split.Y, estimation.XW, estimation.Y, test.XW, test.Y, counterfactual.test.Y, model.name, propensity, matchIndices) {
  library(causalTree)
  initial.model <- init.model(model.name, propensity)
  trained.split.model <- train(initial.model, split.XW$X, split.XW$W, split.Y)
  trained.reestimation.model <- reestimate(trained.split.model, estimation.XW$X, estimation.XW$W, estimation.Y)
  test.preds <- predict(trained.reestimation.model, test.XW$X)
  num.leaves <- count.leaves(trained.reestimation.tree)
  os.to <- compute.os.to(test.XW, test.Y, test.preds, propensity)
  os.m <-compute.os.m(test.XW, test.Y, test.preds, matchIndices)
  os.infeasible <- compute.os.infeasible(test.XW, test.Y, test.preds, counterfactual.test.Y)
  list(num.leaves = num.leaves, os.to = os.to, os.m = os.m, os.infeasible = os.infeasible)
}