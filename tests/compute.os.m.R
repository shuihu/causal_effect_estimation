compute.os.m <- function(XW, Y, tau.preds, match.indices) {
  approx.tau <- (2 * XW$W - 1) * (Y - Y[match.indices])
  matched.tau.preds <- 0.5 * (tau.preds + tau.preds[match.indices])
  mean((approx.tau - matched.tau.preds) ^ 2)
}