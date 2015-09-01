estimate <- function(tree, X, model.name) {
  if (model.name == "CT" || model.name == "TOT") {
    estimated.leaves <- est.causalTree(tree, X)
    tree$frame$yval[estimated.leaves]
  } else if (model.name == "TT") {
    
  } else if (model.name == "ST") {
    
  } else {
    stop("model.name must be ST, TT, TOT, or CT")
  }
}