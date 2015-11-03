# all.tree.stats stores for each triple (model, design, replication), the following stats:
# - num.leaves: number of leaves
# - os.to: TOT out-of-sample criterion
# - os.m: matching out-of-sample criterion
# - os.infeasible: infeasible (oracle) criterion, where we can observe each counterfactual observation
source("simulations/init.named.list.R")

init.all.tree.stats <- function(num.replications, designs, model.names, stats.names) {
  tree.stats <- matrix(0, max(designs), num.replications)
  tree.stats.per.model <- init.named.list(stats.names, tree.stats)
  all.tree.stats <- init.named.list(model.names, tree.stats.per.model)
  all.tree.stats
}