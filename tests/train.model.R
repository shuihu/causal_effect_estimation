train.model <- function(XW, Y, model.name) {
  X <- XW$X
  W <- XW$W
  if (model.name == "CT") {
    causalTree(Y~X, X, W, method = "anova", parms = 1, minbucket = 1, cv.option = "matching", p = 0.5, xval = 10)
  } else if (model.name == "TOT") {
    causalTree(Y~X, X, W, method = "anova", parms = 1, minbucket = 1, cv.option = "TOT", p = 0.5, xval = 10)
  } else if (model.name == "TT") {
    
  } else if (model.name == "ST") {
    
  } else {
    stop("model.name must be ST, TT, TOT, or CT")
  }
}