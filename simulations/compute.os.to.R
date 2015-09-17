compute.os.to <- function(XW, Y, tau.preds, propensity) {
  transformed.Y <- Y * (XW$W - propensity) / (propensity * (1 - propensity))
  mean((transformed.Y - tau.preds) ^ 2)
}