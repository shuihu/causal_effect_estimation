compute.tree.stats <- function(split.XW, split.Y, estimation.XW, estimation.Y, test.XW, test.Y, counterfactual.test.Y, model.name, propensity, matchIndices) {
  trained.split.tree <- train.model(split.XW, split.Y, model.name)
  trained.estimation.tree <- modify.estimations(trained.split.tree, estimation.XW, estimation.Y, model.name, propensity)
  test.preds <- estimate(trained.estimation.tree, test.XW, model.name)
  num.leaves <- compute.num.leaves(trained.estimation.tree, model.name)
  os.to <- compute.os.to(test.XW, test.Y, test.preds, propensity)
  os.m <-compute.os.m(test.XW, test.Y, test.preds, matchIndices)
  os.infeasible <- compute.os.infeasible(test.XW, test.Y, test.preds, counterfactual.test.Y)
  list(num.leaves = num.leaves, os.to = os.to, os.m = os.m, os.infeasible = os.infeasible)
}