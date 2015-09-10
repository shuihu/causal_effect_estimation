train.model <- function(XW, Y, model.name) {
  X <- XW$X
  W <- XW$W
  if (model.name == "CT") {
    train(CT("matching"), X, W, Y)
  } else if (model.name == "TOT") {
    train(CT("TOT"), X, W, Y)
  } else if (model.name == "TT") {
    train(TT(), X, W, Y)
  } else if (model.name == "ST") {
    train(ST(), X, W, Y)
  } else {
    stop("model.name must be ST, TT, TOT, or CT")
  }
}