compute.conf.interval.comparisons <- function(num.designs, model.names, stats.names, all.tree.stats) {
  supported.model.names <- c('TOT_split_xval_rpart', 'TOT_xval', 'CT')
  conf.interval.comp.per.model <- vector("list", num.designs)
  for (design in 1:num.designs) {
    conf.interval.comps <- list()
    for (stats.name in stats.names) {
      conf.interval.comps[[stats.name]] <- NA
    }
    conf.interval.comp.per.model[[design]] <- conf.interval.comps
  }
  conf.interval.comparisons <- vector("list", length(model.names))
  names(conf.interval.comparisons) <- model.names
  for (model.name in model.names) {
    conf.interval.comparisons[[model.name]] <- conf.interval.comp.per.model
  }
  for (design in 1:num.designs) {
    for (model.name in intersect(model.names, supported.model.names)) {
      for (stats.name in stats.names) {
        conf.interval.comparisons[[model.name]][[design]][[stats.name]] <- mean(all.tree.stats[[model.name]][[stats.name]][design,])
      }
    }
  }
  conf.interval.comparisons
}

compute.conf.interval.comparisons.between.trees <- function(num.designs, model.names, all.tree.stats) {
  stats.names <- c('honest.in.dishonest.conf.intv.95', 'honest.in.dishonest.conf.intv.90', 'dishonest.in.honest.conf.intv.95', 'dishonest.in.honest.conf.intv.90')
  compute.conf.interval.comparisons(num.designs, model.names, stats.names, all.tree.stats)
}

compute.conf.interval.comparisons.for.test.data <- function(num.designs, model.names, all.tree.stats) {
  stats.names <- c('test.in.dishonest.conf.intv.95', 'test.in.dishonest.conf.intv.90', 'test.in.honest.conf.intv.95', 'test.in.honest.conf.intv.90')
  compute.conf.interval.comparisons(num.designs, model.names, stats.names, all.tree.stats)
}