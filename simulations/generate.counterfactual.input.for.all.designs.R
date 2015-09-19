generate.counterfactual.input.for.all.designs <- function(XW) {
  num.designs <- length(XW)
  counterfactual.inputs <- vector("list", num.designs)
  for (design in 1:num.designs) {
    counterfactual.inputs[[design]] <- list(X = XW[[design]]$X, W = 1 - XW[[design]]$W)
  }
  counterfactual.inputs
}