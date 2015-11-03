compute.num.full.trees <- function(designs, model.names, all.tree.stats) {
  full.trees.per.model <- vector("list", max(designs))
  for (design in designs) {
    full.trees.per.model[[design]] <- list(Proportion = 0)
  }
  full.trees <- vector("list", length(model.names))
  names(full.trees) <- model.names
  for (model.name in model.names) {
    full.trees[[model.name]] <- full.trees.per.model
  }
  for (design in designs) {
    for (model.name in model.names) {
      full.trees[[model.name]][[design]]$Proportion <- mean(all.tree.stats[[model.name]]$is.full.tree[design,])
    }
  }
  full.trees
}