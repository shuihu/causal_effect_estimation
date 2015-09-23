# Run the simulations and generate the full table of comparisons between the ST, TT, TOT_split_xval_rpart, TOT_xval, and CT models as described in the paper.
# This compares the number of leaves, the TO out-of-sample criterion, the matching out-of-sample criterion,
# and the infeasible (oracle) out-of-sample criterion across three data simulation designs, each with
# 1000 replications of the data.
# By calling full.simulation with different arguments, we can generate different simulations.
source("simulations/run.full.simulation.R")
source("simulations/compare.results.R")
source("simulations/print.comparisons.to.csv.R")

full.simulation <- function(num.replications = 1000, num.designs = 5, model.names = c('ST', 'TT', 'TOT_split_xval_rpart', 'TOT_xval', 'CT'), os.names = c('os.to', 'os.m', 'os.infeasible'), num.obs.per.set = 500, num.vars.per.obs = 10, propensity = 0.5, xvals = 10, is.honest = TRUE, output.filename = "full.simulation.csv", seed) {
  if (missing(seed)) {
    seed <- 1L
  }
  simulation.results <- run.full.simulation(num.replications, num.designs, model.names, os.names, num.obs.per.set, num.vars.per.obs, propensity, xvals, is.honest, seed)
  comparisons <- compare.results(num.designs, model.names, simulation.results, num.replications, FALSE)
  print.comparisons.to.csv(comparisons, output.filename)
  comparisons
}
