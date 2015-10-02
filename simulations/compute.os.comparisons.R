compute.os.comparisons <- function(num.designs, model.names, os.name, all.tree.stats, all.winning.models, num.replications) {
  os.comp.per.model <- vector("list", num.designs)
  for (design in 1:num.designs) {
    os.comp.per.model[[design]] <- list(Q = 0, Share = 0)
  }
  os.comparisons <- vector("list", length(model.names))
  names(os.comparisons) <- model.names
  for (model.name in model.names) {
    os.comparisons[[model.name]] <- os.comp.per.model
  }
  for (design in 1:num.designs) {
    for (model.name in model.names) {
      os.comparisons[[model.name]][[design]]$Q <- median(all.tree.stats[[model.name]][[os.name]][design,])
      os.comparisons[[model.name]][[design]]$Q.Mean <- mean(all.tree.stats[[model.name]][[os.name]][design,])
      os.comparisons[[model.name]][[design]]$Q.Std <- sd(all.tree.stats[[model.name]][[os.name]][design,])
      quantiles <- quantile(all.tree.stats[[model.name]][[os.name]][design,])
      if (os.comparisons[[model.name]][[design]]$Q < 1) {
        print(quantiles)
      }
      os.comparisons[[model.name]][[design]]$Q.Spread <- quantiles[["75%"]] - quantiles[["25%"]]
      os.comparisons[[model.name]][[design]]$Share <- all.winning.models[[design]][[os.name]][[model.name]] / num.replications
    }
  }
  os.comparisons
}