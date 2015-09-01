compute.num.leaves <- function(tree, model.name) {
  if (model.name == "TOT" || model.name == "CT" || model.name == "ST") {
    length(which(tree$frame$var == "<leaf>"))
  } else {
    -1
  }
}