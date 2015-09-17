# Simulate some data multiple times, and for each replication, use the data to train and test
# several models that estimate causal effect.  Return the results from each replication, as well
# as statistics on how often each model was the best according to various criteria.
source("simulations/init.all.tree.stats.R")
source("simulations/init.all.winning.models.R")
source("simulations/generate.input.R")
source("simulations/generate.output.R")
source("simulations/match.observations.R")
source("simulations/init.named.list.R")
source("simulations/compute.tree.stats.R")

run.full.simulation <- function(num.replications = 1000, num.designs = 3, model.names = c('ST', 'TT', 'TOT', 'CT'), os.names = c('os.to', 'os.m', 'os.infeasible'), num.obs.per.set = 500, num.vars.per.obs = 10, propensity = 0.5, xvals = 10, seed) {
  if (missing(seed)) {
    seed <- sample(1:.Machine$integer.max, 1)
  }
  
  all.tree.stats <- init.all.tree.stats(num.replications, num.designs, model.names, c('num.leaves', os.names))
  all.winning.models <- init.all.winning.models(num.designs, model.names, os.names)

  test.XW <- generate.input(num.obs.per.set, num.vars.per.obs, seed)
  match.indices <- match.observations(test.XW)
  # flip the W's
  counterfactual.test.XW <- list(X = test.XW$X, W = 1 - test.XW$W)
  for (repl in 1:num.replications) {
    print(paste("replication", as.character(repl)))
    # generate the input
    train.split.XW <- generate.input(num.obs.per.set, num.vars.per.obs, repl)
    train.restimation.XW <- generate.input(num.obs.per.set, num.vars.per.obs, repl)
    for (design in 1:num.designs) {
      train.split.Y <- generate.output(train.split.XW, design, repl)
      train.estimation.Y <- generate.output(train.restimation.XW, design, repl)
      test.Y <- generate.output(test.XW, design, seed)
      counterfactual.test.Y <- generate.output(counterfactual.test.XW, design, seed)
      
      # compute tree.stats for each model (ST, TT, TOT, CT)
      winning.models <- init.named.list(os.names, NULL)
      min.os.values <- init.named.list(os.names, -1)
      for (model.name in model.names) {
        tree.stats <- compute.tree.stats(train.split.XW, train.split.Y, train.restimation.XW, train.estimation.Y, test.XW, test.Y, counterfactual.test.Y, model.name, propensity, match.indices)
        # fill all.tree.stats with the stats for this (model, design, replication) triple
        all.tree.stats[[model.name]]$num.leaves[design, repl] <- tree.stats$num.leaves
        for (os.name in os.names) {
          all.tree.stats[[model.name]][[os.name]][design, repl] <- tree.stats[[os.name]]
        }

        for (os.name in os.names) {
          if (is.null(winning.models[[os.name]]) || tree.stats[[os.name]] <= min.os.values[[os.name]]) {
            winning.models[[os.name]] <- model.name
            min.os.values[[os.name]] <- tree.stats[[os.name]]
          }
        }
      }
      for (os.name in os.names) {
        winning.model <- winning.models[[os.name]]
        all.winning.models[[design]][[os.name]][[winning.model]] <- all.winning.models[[design]][[os.name]][[winning.model]] + 1
      }
    }
  }

  list(all.tree.stats = all.tree.stats, all.winning.models = all.winning.models)
}