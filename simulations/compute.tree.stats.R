source("simulations/models.R")
source("simulations/compute.os.m.R")
source("simulations/compute.os.to.R")
source("simulations/compute.os.infeasible.R")
source("simulations/compute.percent.in.conf.interval.between.trees.R")
source("simulations/compute.percent.in.conf.interval.for.data.R")
source("simulations/compute.percent.in.conf.interval.for.true.R")

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

compute.tree.stats <- function(split.XW, split.Y, estimation.XW, estimation.Y, test.XW, test.Y, counterfactual.test.Y, model.name, propensity, matchIndices, is.honest, is.honest0.5, is.dishonest2, full.tree.path, counterfactual.split.Y, counterfactual.estimation.Y) {
  if (is.dishonest2) {
    if (is.honest || is.honest0.5) {
      stop("if is.dishonest2 is TRUE, all other honest parameters must be FALSE")
    }
  }
  
  initial.model <- init.model(model.name, propensity)
  if (is.honest0.5) {
    quarter.obs <- nrow(split.XW$X) / 4
    split.indices <- append(1:quarter.obs, (2*quarter.obs+1):(3*quarter.obs))
    estimation.indices <- append((quarter.obs+1):(2*quarter.obs), (3*quarter.obs+1):(4*quarter.obs))
    splitx <- split.XW$X[split.indices,]
    splitw <- split.XW$W[split.indices]
    splity <- split.Y[split.indices]
    estimationx <- split.XW$X[estimation.indices,]
    estimationw <- split.XW$W[estimation.indices]
    estimationy <- split.Y[estimation.indices]
    counterfactual.splitw <- 1 - splitw
    counterfactual.splity <- counterfactual.split.Y[split.indices]
    counterfactual.estimationw <- 1 - estimationw
    counterfactual.estimationy <- counterfactual.split.Y[estimation.indices]
  } else if (is.dishonest2) {
    splitx <- rbind(split.XW$X, estimation.XW$X)
    splitw <- c(split.XW$W, estimation.XW$W)
    splity <- c(split.Y, estimation.Y)
    estimationx <- estimation.XW$X
    estimationw <- estimation.XW$W
    estimationy <- estimation.Y
    counterfactual.splitw <- 1 - splitw
    counterfactual.splity <- c(counterfactual.split.Y, counterfactual.estimation.Y)
    counterfactual.estimationw <- 1 - estimationw
    counterfactual.estimationy <- c(counterfactual.estimation.Y)
  } else {
    splitx <- split.XW$X
    splitw <- split.XW$W
    splity <- split.Y
    estimationx <- estimation.XW$X
    estimationw <- estimation.XW$W
    estimationy <- estimation.Y
    counterfactual.splitw <- 1 - splitw
    counterfactual.splity <- counterfactual.split.Y
    counterfactual.estimationw <- 1 - estimationw
    counterfactual.estimationy <- counterfactual.estimation.Y
  }
  trained.split.model <- train.model(initial.model, splitx, splitw, splity)
  trained.reestimated.model <- reestimate.model(trained.split.model, estimationx, estimationw, estimationy)
  if (is.honest) {
    trained.prediction.model <- trained.reestimated.model
  } else {
    trained.prediction.model <- trained.split.model
  }
  if (model.name == "TOT_split_xval_rpart" || model.name == "TOT_xval" || model.name == "CT") {
    dishonest.in.dishonest.conf.intv.95 <- compute.percent.in.conf.interval.for.true(model.name, trained.split.model@tree, splity, splitw, counterfactual.splity, counterfactual.splitw, propensity, 0.95)
    dishonest.in.dishonest.conf.intv.90 <- compute.percent.in.conf.interval.for.true(model.name, trained.split.model@tree, splity, splitw, counterfactual.splity, counterfactual.splitw, propensity, 0.9)
    honest.in.honest.conf.intv.95 <- compute.percent.in.conf.interval.for.true(model.name, trained.reestimated.model@tree, estimationy, estimationw, counterfactual.estimationy, counterfactual.estimationw, propensity, 0.95)
    honest.in.honest.conf.intv.90 <- compute.percent.in.conf.interval.for.true(model.name, trained.reestimated.model@tree, estimationy, estimationw, counterfactual.estimationy, counterfactual.estimationw, propensity, 0.9)
    honest.in.dishonest.conf.intv.95 <- compute.percent.in.conf.interval.between.trees(trained.reestimated.model@tree, trained.split.model@tree, splity, splitw, 0.95)
    honest.in.dishonest.conf.intv.90 <- compute.percent.in.conf.interval.between.trees(trained.reestimated.model@tree, trained.split.model@tree, splity, splitw, 0.9)
    dishonest.in.honest.conf.intv.95 <- compute.percent.in.conf.interval.between.trees(trained.split.model@tree, trained.reestimated.model@tree, estimationy, estimationw, 0.95)
    dishonest.in.honest.conf.intv.90 <- compute.percent.in.conf.interval.between.trees(trained.split.model@tree, trained.reestimated.model@tree, estimationy, estimationw, 0.9)
    test.in.dishonest.conf.intv.95 <- compute.percent.in.conf.interval.for.data(test.XW$X, test.XW$W, test.Y, trained.split.model, splitw, splity, 0.95)
    test.in.dishonest.conf.intv.90 <- compute.percent.in.conf.interval.for.data(test.XW$X, test.XW$W, test.Y, trained.split.model, splitw, splity, 0.9)
    test.in.honest.conf.intv.95 <- compute.percent.in.conf.interval.for.data(test.XW$X, test.XW$W, test.Y, trained.reestimated.model, estimationw, estimationy, 0.95)
    test.in.honest.conf.intv.90 <- compute.percent.in.conf.interval.for.data(test.XW$X, test.XW$W, test.Y, trained.reestimated.model, estimationw, estimationy, 0.9)
    weighted.test.in.dishonest.conf.intv.95 <- compute.percent.in.conf.interval.for.data(test.XW$X, test.XW$W, test.Y, trained.split.model, splitw, splity, 0.95, weighted = TRUE)
    weighted.test.in.dishonest.conf.intv.90 <- compute.percent.in.conf.interval.for.data(test.XW$X, test.XW$W, test.Y, trained.split.model, splitw, splity, 0.9, weighted = TRUE)
    weighted.test.in.honest.conf.intv.95 <- compute.percent.in.conf.interval.for.data(test.XW$X, test.XW$W, test.Y, trained.reestimated.model, estimationw, estimationy, 0.95, weighted = TRUE)
    weighted.test.in.honest.conf.intv.90 <- compute.percent.in.conf.interval.for.data(test.XW$X, test.XW$W, test.Y, trained.reestimated.model, estimationw, estimationy, 0.9, weighted = TRUE)
  } else {
    dishonest.in.dishonest.conf.intv.95 <- NA
    dishonest.in.dishonest.conf.intv.90 <- NA
    honest.in.honest.conf.intv.95 <- NA
    honest.in.honest.conf.intv.90 <- NA
    honest.in.dishonest.conf.intv.95 <- NA
    honest.in.dishonest.conf.intv.90 <- NA
    dishonest.in.honest.conf.intv.95 <- NA
    dishonest.in.honest.conf.intv.90 <- NA
    test.in.dishonest.conf.intv.95 <- NA
    test.in.dishonest.conf.intv.90 <- NA
    test.in.honest.conf.intv.95 <- NA
    test.in.honest.conf.intv.90 <- NA
    weighted.test.in.dishonest.conf.intv.95 <- NA
    weighted.test.in.dishonest.conf.intv.90 <- NA
    weighted.test.in.honest.conf.intv.95 <- NA
    weighted.test.in.honest.conf.intv.90 <- NA
  }
  if (trained.split.model@is.full.tree) {
    is.full.tree <- 1
  } else {
    is.full.tree <- 0
  }
  test.preds <- predict.model(trained.prediction.model, test.XW$X)
  num.leaves <- count.leaves(trained.prediction.model)
  os.to <- compute.os.to(test.XW, test.Y, test.preds, propensity)
  os.m <-compute.os.m(test.XW, test.Y, test.preds, matchIndices)
  os.infeasible <- compute.os.infeasible(test.XW, test.Y, test.preds, counterfactual.test.Y)

  list(dishonest.in.dishonest.conf.intv.95 = dishonest.in.dishonest.conf.intv.95,
       dishonest.in.dishonest.conf.intv.90 = dishonest.in.dishonest.conf.intv.90,
       honest.in.honest.conf.intv.95 = honest.in.honest.conf.intv.95,
       honest.in.honest.conf.intv.90 = honest.in.honest.conf.intv.90,
       honest.in.dishonest.conf.intv.95 = honest.in.dishonest.conf.intv.95,
       honest.in.dishonest.conf.intv.90 = honest.in.dishonest.conf.intv.90,
       dishonest.in.honest.conf.intv.95 = dishonest.in.honest.conf.intv.95,
       dishonest.in.honest.conf.intv.90 = dishonest.in.honest.conf.intv.90,
       test.in.dishonest.conf.intv.95 = test.in.dishonest.conf.intv.95,
       test.in.dishonest.conf.intv.90 = test.in.dishonest.conf.intv.90,
       test.in.honest.conf.intv.95 = test.in.honest.conf.intv.95,
       test.in.honest.conf.intv.90 = test.in.honest.conf.intv.90,
       weighted.test.in.dishonest.conf.intv.95 = weighted.test.in.dishonest.conf.intv.95,
       weighted.test.in.dishonest.conf.intv.90 = weighted.test.in.dishonest.conf.intv.90,
       weighted.test.in.honest.conf.intv.95 = weighted.test.in.honest.conf.intv.95,
       weighted.test.in.honest.conf.intv.90 = weighted.test.in.honest.conf.intv.90,
       is.full.tree = is.full.tree,
       num.leaves = num.leaves,
       os.to = os.to,
       os.m = os.m,
       os.infeasible = os.infeasible)
}
