# all.tree.stats stores for each triple (model, design, replication), the following stats:
# - num.leaves: number of leaves
# - os.to: TOT out-of-sample criterion
# - os.m: matching out-of-sample criterion
# - os.infeasible: infeasible (oracle) criterion, where we can observe each counterfactual observation

init.all.tree.stats <- function(num.replications, num.designs, model.names) {
  tree.stats <- matrix(0, num.designs, num.replications)
  tree.stats.per.model <- list(num.leaves = tree.stats, os.to = tree.stats, os.m = tree.stats, os.infeasible = tree.stats)
  all.tree.stats <- vector("list", length(model.names))
  names(all.tree.stats) <- model.names
  for (model.name in model.names) {
    all.tree.stats[[model.name]] <- tree.stats.per.model
  }
  all.tree.stats
}