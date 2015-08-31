compute.tree.stats <- function(split.XW, split.Y, estimation.XW, estimation.Y, test.XW, test.Y, model.name) {
  trained.split.tree <- train.model(split.XW, split.Y, model.name)
  trained.estimation.tree <- modify.estimations(trained.split.tree, estimation.XW, estimation.Y, model.name)
  test.preds <- estimate(estimation.estimation.tree, test.XW, model.name)
  num.leaves <- compute.num.leaves(trained.estimation.tree, model.name)
  os.to <- compute.os.to(trained.estimation.tree$which, test.XW, test.Y, test.preds, model.name)
  os.m <-compute.os.m(trained.estimation.tree$which, test.XW, test.Y, test.preds, model.name)
  os.infeasible <- compute.os.infeasible(trained.estimation.tree$which, test.XW, test.Y, test.preds, model.name)
  list(num.leaves = num.leaves, os.to = os.to, os.m = os.m, os.infeasible = os.infeasible)
}