compute.leaf.comparisons <- function(designs, model.names, all.tree.stats) {
  leaf.comp.per.model <- vector("list", max(designs))
  for (design in designs) {
    leaf.comp.per.model[[design]] <- list(Mean = 0, Median = 0)
  }
  leaf.comparisons <- vector("list", length(model.names))
  names(leaf.comparisons) <- model.names
  for (model.name in model.names) {
    leaf.comparisons[[model.name]] <- leaf.comp.per.model
  }
  for (design in designs) {
    for (model.name in model.names) {
      leaf.comparisons[[model.name]][[design]]$Mean <- mean(all.tree.stats[[model.name]]$num.leaves[design,])
      leaf.comparisons[[model.name]][[design]]$Median <- median(all.tree.stats[[model.name]]$num.leaves[design,])
      leaf.comparisons[[model.name]][[design]]$Std <- sd(all.tree.stats[[model.name]]$num.leaves[design,])
      quantiles <- quantile(all.tree.stats[[model.name]]$num.leaves[design,])
      leaf.comparisons[[model.name]][[design]]$Spread <- quantiles[["75%"]] - quantiles[["25%"]]
    }
  }
  leaf.comparisons
}