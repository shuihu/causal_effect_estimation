# Simulate some data multiple times, and for each replication, use the data to train and test
# several models that estimate causal effect.  Return the results from each replication, as well
# as statistics on how often each model was the best according to various criteria.

run.full.simulation <- function(num.replications = 1000, num.designs = 3, model.names = c('ST', 'TT', 'TOT', 'CT'), num.obs.per.set = 500, num.vars.per.obs = 10, xvals = 10, seed = 92827L) {
  all.tree.stats <- init.all.tree.stats(num.replications, num.designs, model.names)
  all.winning.models <- init.all.winning.models(num.designs, model.names)
  
  test.XW <- generate.input(num.obs.per.set, num.vars.per.obs, seed)
  for (repl in 1:num.replications) {
    # generate the input
    train.split.XW <- generate.input(num.obs.per.set, num.vars.per.obs, repl)
    train.estimation.XW <- generate.input(num.obs.per.set, num.vars.per.obs, repl)
    for (design in 1:num.designs) {
      train.split.Y <- generate.output(train.split.XW, design, repl)
      train.estimation.Y <- generate.output(train.estimations.XW, design, repl)
      test.Y <- generate.output(test.XW, design, seed)
      
      # compute tree.stats for each model (ST, TT, TOT, CT)
      winning.models <- list(os.to = NULL, os.m = NULL, os.infeasible = NULL)
      min.os.values <- list(os.to = -1, os.m = -1, os.infeasible = -1)
      for (model.name in model.names) {
        tree.stats <- compute.tree.stats(train.split.XW, train.split.Y, train.estimation.XW, train.estimation.Y, test.XW, model.name)
        # fill all.tree.stats with the stats for this (model, design, replication) triple
        all.tree.stats[model.name]$num.leaves[design, repl] <- tree.stats$num.leaves
        all.tree.stats[model.name]$os.to[design, repl] <- tree.stats$os.to
        all.tree.stats[model.name]$os.m[design, repl] <- tree.stats$os.m
        all.tree.stats[model.name]$os.infeasible[design, repl] <- tree.stats$os.infeasible
        
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