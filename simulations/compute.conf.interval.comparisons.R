compute.conf.interval.comparisons <- function(num.designs, model.names, all.tree.stats) {
  supported.model.names <- c('TOT_split_xval_rpart', 'TOT_xval', 'CT')
  conf.interval.comp.per.model <- vector("list", num.designs)
  for (design in 1:num.designs) {
    conf.interval.comp.per.model[[design]] <- list(honest.in.dishonest.95 = NA, honest.in.dishonest.90 = NA, dishonest.in.honest.95 = NA, dishonest.in.honest.90 = NA)
  }
  conf.interval.comparisons <- vector("list", length(model.names))
  names(conf.interval.comparisons) <- model.names
  for (model.name in model.names) {
    conf.interval.comparisons[[model.name]] <- conf.interval.comp.per.model
  }
  for (design in 1:num.designs) {
    for (model.name in intersect(model.names, supported.model.names)) {
      conf.interval.comparisons[[model.name]][[design]]$honest.in.dishonest.95 <- mean(all.tree.stats[[model.name]]$honest.in.dishonest.conf.intv.95[design,])
      conf.interval.comparisons[[model.name]][[design]]$honest.in.dishonest.90 <- mean(all.tree.stats[[model.name]]$honest.in.dishonest.conf.intv.90[design,])
      conf.interval.comparisons[[model.name]][[design]]$dishonest.in.honest.95 <- mean(all.tree.stats[[model.name]]$dishonest.in.honest.conf.intv.95[design,])
      conf.interval.comparisons[[model.name]][[design]]$dishonest.in.honest.90 <- mean(all.tree.stats[[model.name]]$dishonest.in.honest.conf.intv.90[design,])
    }
  }
  conf.interval.comparisons
}