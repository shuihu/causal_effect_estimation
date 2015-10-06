# Generate and save all training and test data
source("simulations/generate.input.R")
source("simulations/generate.output.R")
source("simulations/match.observations.R")
source("simulations/generate.counterfactual.input.for.all.designs.R")

generate.all.data <- function(num.replications = 1000, num.designs = 6, num.obs.per.set = 500, num.vars.per.obs = 10, data.dir = "data", seed) {
  if (!missing(seed)) {
    set.seed(seed)
  }
  
  print("test.XW")
  test.XW <- generate.input.for.all.designs(num.obs.per.set, num.vars.per.obs, num.designs)
  print("test.Y")
  test.Y <- generate.output.for.all.designs(test.XW)
  print("match.indices")
  match.indices <- match.observations.for.all.designs(test.XW)
  # flip the W's
  counterfactual.test.XW <- generate.counterfactual.input.for.all.designs(test.XW)
  counterfactual.test.Y <- generate.output.for.all.designs(counterfactual.test.XW)
  for (design in 1:num.designs) {
    write.table(test.XW[[design]]$X, paste(data.dir, "/test_design_", as.character(design), "_x", ".csv", sep = ""), sep = ",", col.names = FALSE, row.names = FALSE)
    write.table(test.XW[[design]]$W, paste(data.dir, "/test_design_", as.character(design), "_w", ".csv", sep = ""), sep = ",", col.names = FALSE, row.names = FALSE)
    write.table(test.Y[[design]], paste(data.dir, "/test_design_", as.character(design), "_y", ".csv", sep = ""), sep = ",", col.names = FALSE, row.names = FALSE)
    write.table(match.indices[[design]], paste(data.dir, "/test_design_", as.character(design), "_match_indices", ".csv", sep = ""), sep = ",", col.names = FALSE, row.names = FALSE)
    write.table(counterfactual.test.XW[[design]]$X, paste(data.dir, "/test_design_", as.character(design), "_counterfactual_x", ".csv", sep = ""), sep = ",", col.names = FALSE, row.names = FALSE)
    write.table(counterfactual.test.XW[[design]]$W, paste(data.dir, "/test_design_", as.character(design), "_counterfactual_w", ".csv", sep = ""), sep = ",", col.names = FALSE, row.names = FALSE)
    write.table(counterfactual.test.Y[[design]], paste(data.dir, "/test_design_", as.character(design), "_counterfactual_y", ".csv", sep = ""), sep = ",", col.names = FALSE, row.names = FALSE)
  }
  
  print("train")
  for (repl in 1:num.replications) {
    print(paste("replication:", as.character(repl)))
    train.split.XW <- generate.input.for.all.designs(num.obs.per.set, num.vars.per.obs, num.designs)
    train.estimation.XW <- generate.input.for.all.designs(num.obs.per.set, num.vars.per.obs, num.designs)
    train.split.Y <- generate.output.for.all.designs(train.split.XW)
    train.estimation.Y <- generate.output.for.all.designs(train.estimation.XW)
    for (design in 1:num.designs) {
      write.table(train.split.XW[[design]]$X, paste(data.dir, "/train_split_repl_", as.character(repl), "_design_", as.character(design), "_x", ".csv", sep = ""), sep = ",", col.names = FALSE, row.names = FALSE)
      write.table(train.split.XW[[design]]$W, paste(data.dir, "/train_split_repl_", as.character(repl), "_design_", as.character(design), "_w", ".csv", sep = ""), sep = ",", col.names = FALSE, row.names = FALSE)
      write.table(train.estimation.XW[[design]]$X, paste(data.dir, "/train_estimation_repl_", as.character(repl), "_design_", as.character(design), "_x", ".csv", sep = ""), sep = ",", col.names = FALSE, row.names = FALSE)
      write.table(train.estimation.XW[[design]]$W, paste(data.dir, "/train_estimation_repl_", as.character(repl), "_design_", as.character(design), "_w", ".csv", sep = ""), sep = ",", col.names = FALSE, row.names = FALSE)
      write.table(train.split.Y[[design]], paste(data.dir, "/train_split_repl_", as.character(repl), "_design_", as.character(design), "_y", ".csv", sep = ""), sep = ",", col.names = FALSE, row.names = FALSE)
      write.table(train.estimation.Y[[design]], paste(data.dir, "/train_estimation_repl_", as.character(repl), "_design_", as.character(design), "_y", ".csv", sep = ""), sep = ",", col.names = FALSE, row.names = FALSE)
    }
  }
}