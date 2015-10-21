compute.num.full.trees <- function(num.designs, model.names, all.tree.stats) {
  full.trees.per.model <- vector("list", num.designs)
  for (design in 1:num.designs) {
    full.trees.per.model[[design]] <- list(Proportion = 0)
  }
  full.trees <- vector("list", length(model.names))
  names(full.trees) <- model.names
  for (model.name in model.names) {
    full.trees[[model.name]] <- full.trees.per.model
  }
  for (design in 1:num.designs) {
    for (model.name in model.names) {
      full.trees[[model.name]][[design]]$Proportion <- mean(all.tree.stats[[model.name]]$is.full.tree[design,])
    }
  }
  full.trees
}