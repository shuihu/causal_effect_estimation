compute.leaf.comparisons <- function(num.designs, model.names, all.tree.stats) {
  leaf.comp.per.model <- rep(list(Mean = 0, Median = 0), num.designs)
  leaf.comparisons <- vector("list", length(model.names))
  names(leaf.comparisons) <- model.names
  for (model.name in model.names) {
    leaf.comparisons[[model.name]] <- leaf.comp.per.model
  }
  for (design in 1:num.designs) {
    for (model.name in model.names) {
      leaf.comparisons[[model.name]][[design]]$Mean <- mean(all.tree.stats[[model.name]]$num.leaves[design,])
      leaf.comparisons[[model.name]][[design]]$Median <- median(all.tree.stats[[model.name]]$num.leaves[design,])
    }
  }
  leaf.comparisons
}
