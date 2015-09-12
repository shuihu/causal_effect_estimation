# Compare the number of leaves, out-of-sample TOT criterion, out-of-sample matching criterion, and out-of-sample infeasible 
# criterion for the different models given their performance on many replications of input data across several simulation
# designs.

compare.results <- function(num.designs, model.names, simulation.results, num.replications, printOpt = TRUE) {
  all.tree.stats <- simulation.results$all.tree.stats
  all.winning.models <- simulation.results$all.winning.models
  
  # compare num.leaves
  leaf.comparisons <- compute.leaf.comparisons(num.designs, model.names, all.tree.stats)
  # compare os.to
  os.to.comparisons <- compute.os.comparisons(num.designs, model.names, "os.to", all.tree.stats, all.winning.models, num.replications)
  # compare os.m
  os.m.comparisons <- compute.os.comparisons(num.designs, model.names, "os.m", all.tree.stats, all.winning.models, num.replications)
  # compare os.infeasible
  os.infeasible.comparisons <- compute.os.comparisons(num.designs, model.names, "os.infeasible", all.tree.stats, all.winning.models, num.replications)

  if (isTRUE(printOpt)) {
    print("Leaves")
    print(leaf.comparisons)
    print("OS TO")
    print(os.to.comparisons)
    print("OS M")
    print(os.m.comparisons)
    print("OS Infeasible (oracle)")
    print(os.infeasible.comparisons)
  }
  
  list(leaves = leaf.comparisons, os.to = os.to.comparisons, os.m = os.m.comparisons, os.infeasible = os.infeasible.comparisons)
}