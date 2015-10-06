read.input.for.all.designs <- function(path.prefix, designs) {
  inputs <- vector("list", max(designs))
  for (design in designs) {
    X <- as.matrix(read.table(paste(path.prefix, as.character(design), "_x", ".csv", sep = ""), sep = ","))
    colnames(X) <- NULL
    W <- read.table(paste(path.prefix, as.character(design), "_w", ".csv", sep = ""), sep = ",")[[1]]
    inputs[[design]] <- list(X = X, W = W)
  }
  inputs
}

read.counterfactual.input.for.all.designs <- function(path.prefix, designs) {
  inputs <- vector("list", max(designs))
  for (design in designs) {
    X <- as.matrix(read.table(paste(path.prefix, as.character(design), "_counterfactual_x", ".csv", sep = ""), sep = ","))
    colnames(X) <- NULL
    W <- read.table(paste(path.prefix, as.character(design), "_counterfactual_w", ".csv", sep = ""), sep = ",")[[1]]
    inputs[[design]] <- list(X = X, W = W)
  }
  inputs
}

read.output.for.all.designs <- function(path.prefix, designs) {
  outputs <- vector("list", max(designs))
  for (design in designs) {
    outputs[[design]] <- read.table(paste(path.prefix, as.character(design), "_y", ".csv", sep = ""), sep = ",")[[1]]
  }
  outputs
}

read.counterfactual.output.for.all.designs <- function(path.prefix, designs) {
  outputs <- vector("list", max(designs))
  for (design in designs) {
    outputs[[design]] <- read.table(paste(path.prefix, as.character(design), "_counterfactual_y", ".csv", sep = ""), sep = ",")[[1]]
  }
  outputs
}

read.match.observations.for.all.designs <- function(path.prefix, designs) {
  matches <- vector("list", max(designs))
  for (design in designs) {
    matches[[design]] <- read.table(paste(path.prefix, as.character(design), "_match_indices", ".csv", sep = ""), sep = ",")[[1]]
  }
  matches
}