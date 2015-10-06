source("simulations/models.R")
source("simulations/compute.os.m.R")
source("simulations/compute.os.to.R")
source("simulations/compute.os.infeasible.R")
source("simulations/compute.percent.in.conf.interval.R")

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

compute.tree.stats <- function(split.XW, split.Y, estimation.XW, estimation.Y, test.XW, test.Y, counterfactual.test.Y, model.name, propensity, matchIndices, is.honest, is.honest2) {
  initial.model <- init.model(model.name, propensity)
  if (!is.honest2) {
    splitx <- split.XW$X
    splitw <- split.XW$W
    splity <- split.Y
    estimationx <- estimation.XW$X
    estimationw <- estimation.XW$W
    estimationy <- estimation.Y
  } else {
    quarter.obs <- nrow(split.XW$X) / 4
    split.indices <- append(1:quarter.obs, (2*quarter.obs+1):(3*quarter.obs))
    estimation.indices <- append((quarter.obs+1):(2*quarter.obs), (3*quarter.obs+1):(4*quarter.obs))
    splitx <- split.XW$X[split.indices,]
    splitw <- split.XW$W[split.indices,]
    splity <- split.Y[split.indices]
    estimationx <- split.XW$X[estimation.indices,]
    estimationw <- split.XW$W[estimation.indices]
    estimationy <- split.Y[estimation.indices]
  }
  trained.split.model <- train.model(initial.model, splitx, splitw, splity)
  trained.reestimated.model <- reestimate.model(trained.split.model, estimationx, estimationw, estimationy)
  if (is.honest) {
    trained.prediction.model <- trained.reestimated.model
  } else {
    trained.prediction.model <- trained.split.model
  }
  if (model.name == "TOT_split_xval_rpart" || model.name == "TOT_xval" || model.name == "CT") {
    honest.in.dishonest.conf.intv.95 <- compute.percent.in.conf.interval(trained.reestimated.model@tree, trained.split.model@tree, splity, splitw, 0.95)
    honest.in.dishonest.conf.intv.90 <- compute.percent.in.conf.interval(trained.reestimated.model@tree, trained.split.model@tree, splity, splitw, 0.9)
    dishonest.in.honest.conf.intv.95 <- compute.percent.in.conf.interval(trained.split.model@tree, trained.reestimated.model@tree, estimationy, estimationw, 0.95)
    dishonest.in.honest.conf.intv.90 <- compute.percent.in.conf.interval(trained.split.model@tree, trained.reestimated.model@tree, estimationy, estimationw, 0.9)
  } else {
    honest.in.dishonest.conf.intv.95 <- NA
    honest.in.dishonest.conf.intv.90 <- NA
    dishonest.in.honest.conf.intv.95 <- NA
    dishonest.in.honest.conf.intv.90 <- NA
  }
  test.preds <- predict.model(trained.prediction.model, test.XW$X)
  num.leaves <- count.leaves(trained.prediction.model)
  os.to <- compute.os.to(test.XW, test.Y, test.preds, propensity)
  os.m <-compute.os.m(test.XW, test.Y, test.preds, matchIndices)
  os.infeasible <- compute.os.infeasible(test.XW, test.Y, test.preds, counterfactual.test.Y)
  list(honest.in.dishonest.conf.intv.95 = honest.in.dishonest.conf.intv.95, honest.in.dishonest.conf.intv.90 = honest.in.dishonest.conf.intv.90, dishonest.in.honest.conf.intv.95 = dishonest.in.honest.conf.intv.95, dishonest.in.honest.conf.intv.90 = dishonest.in.honest.conf.intv.90, num.leaves = num.leaves, os.to = os.to, os.m = os.m, os.infeasible = os.infeasible)
}
