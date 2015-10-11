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
source("simulations/generate.counterfactual.input.for.all.designs.R")
source("simulations/read.data.R")

run.full.simulation <- function(num.replications = 1000, num.designs = 6, model.names = c('ST', 'TT', 'TOT_split_xval_rpart', 'TOT_xval', 'CT'), os.names = c('os.to', 'os.m', 'os.infeasible'), num.obs.per.set = 500, num.vars.per.obs = 10, propensity = 0.5, xvals = 10, is.honest = TRUE, is.honest0.5 = FALSE, is.dishonest2 = FALSE, data.path, seed) {
  if (!missing(seed)) {
    set.seed(seed)
  }
  
  all.tree.stats <- init.all.tree.stats(num.replications, num.designs, model.names, c('honest.in.dishonest.conf.intv.95', 'honest.in.dishonest.conf.intv.90', 'dishonest.in.honest.conf.intv.95', 'dishonest.in.honest.conf.intv.90', 'test.in.dishonest.conf.intv.95', 'test.in.dishonest.conf.intv.90', 'test.in.honest.conf.intv.95', 'test.in.honest.conf.intv.90', 'num.leaves', os.names))
  all.winning.models <- init.all.winning.models(num.designs, model.names, os.names)

  if (missing(data.path)) {
    test.XW <- generate.input.for.all.designs(num.obs.per.set, num.vars.per.obs, num.designs)
    test.Y <- generate.output.for.all.designs(test.XW)
    match.indices <- match.observations.for.all.designs(test.XW)
    # flip the W's
    counterfactual.test.XW <- generate.counterfactual.input.for.all.designs(test.XW)
    counterfactual.test.Y <- generate.output.for.all.designs(counterfactual.test.XW)
    seeds <- sample(.Machine$integer.max, num.replications, replace = TRUE)
  } else {
    test.XW <- read.input.for.all.designs(paste(data.path, "/test_design_", sep = ""), 1:num.designs)
    test.Y <- read.output.for.all.designs(paste(data.path, "/test_design_", sep = ""), 1:num.designs)
    match.indices <- read.match.observations.for.all.designs(paste(data.path, "/test_design_", sep = ""), 1:num.designs)
    counterfactual.test.XW <- read.counterfactual.input.for.all.designs(paste(data.path, "/test_design_", sep = ""), 1:num.designs)
    counterfactual.test.Y <- read.counterfactual.output.for.all.designs(paste(data.path, "/test_design_", sep = ""), 1:num.designs)
  }
  for (repl in 1:num.replications) {
    print(paste("replication", as.character(repl)))
    if (missing(data.path)) {
      set.seed(seeds[repl])
    }
    
    # generate the input for all the designs first
    if (missing(data.path)) {
      train.split.XW <- generate.input.for.all.designs(num.obs.per.set, num.vars.per.obs, num.designs)
      train.estimation.XW <- generate.input.for.all.designs(num.obs.per.set, num.vars.per.obs, num.designs)
      train.split.Y <- generate.output.for.all.designs(train.split.XW)
      train.estimation.Y <- generate.output.for.all.designs(train.estimation.XW)
    } else {
      train.split.XW <- read.input.for.all.designs(paste(data.path, "/train_split_repl_", as.character(repl), "_design_", sep = ""), 1:num.designs)
      train.estimation.XW <- read.input.for.all.designs(paste(data.path, "/train_estimation_repl_", as.character(repl), "_design_", sep = ""), 1:num.designs)
      train.split.Y <- read.output.for.all.designs(paste(data.path, "/train_split_repl_", as.character(repl), "_design_", sep = ""), 1:num.designs)
      train.estimation.Y <- read.output.for.all.designs(paste(data.path, "/train_estimation_repl_", as.character(repl), "_design_", sep = ""), 1:num.designs)
    }

    for (design in 1:num.designs) {
      # compute tree.stats for each model (ST, TT, TOT_split_xval_rpart, TOT_xval, CT)
      winning.models <- init.named.list(os.names, NULL)
      min.os.values <- init.named.list(os.names, -1)
      for (model.name in model.names) {
        tree.stats <- compute.tree.stats(train.split.XW[[design]], train.split.Y[[design]], train.estimation.XW[[design]], train.estimation.Y[[design]], test.XW[[design]], test.Y[[design]], counterfactual.test.Y[[design]], model.name, propensity, match.indices[[design]], is.honest, is.honest0.5, is.dishonest2)
        # fill all.tree.stats with the stats for this (model, design, replication) triple
        all.tree.stats[[model.name]]$honest.in.dishonest.conf.intv.95[design, repl] <- tree.stats$honest.in.dishonest.conf.intv.95
        all.tree.stats[[model.name]]$honest.in.dishonest.conf.intv.90[design, repl] <- tree.stats$honest.in.dishonest.conf.intv.90
        all.tree.stats[[model.name]]$dishonest.in.honest.conf.intv.95[design, repl] <- tree.stats$dishonest.in.honest.conf.intv.95
        all.tree.stats[[model.name]]$dishonest.in.honest.conf.intv.90[design, repl] <- tree.stats$dishonest.in.honest.conf.intv.90
        all.tree.stats[[model.name]]$test.in.dishonest.conf.intv.95[[design, repl]] <- tree.stats$test.in.dishonest.conf.intv.95
        all.tree.stats[[model.name]]$test.in.dishonest.conf.intv.90[[design, repl]] <- tree.stats$test.in.dishonest.conf.intv.90
        all.tree.stats[[model.name]]$test.in.honest.conf.intv.95[[design, repl]] <- tree.stats$test.in.honest.conf.intv.95
        all.tree.stats[[model.name]]$test.in.honest.conf.intv.90[[design, repl]] <- tree.stats$test.in.honest.conf.intv.90
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