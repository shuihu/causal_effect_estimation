compute.os.infeasible <- function(XW, Y, tau.preds, counterfactual.Y) {
  W <- XW$W
  # E[Y | W = 1, X = x] - E[Y | W = 0, X = x] for all observations
  tau <- (2 * W - 1) * (Y - counterfactual.Y)
  mean((tau - tau.preds) ^ 2)
}