# generate X and W for a single replication
generate.input <- function(num.obs, num.vars, seed) {
  if (!missing(seed)) {
    set.seed(seed)
  }
  
  X <- matrix(0, num.obs, num.vars)
  for (var in 1:num.vars) {
    temp <- rnorm(num.obs, 0, 1)
    X[,var] <- temp - mean(temp)
  }
  W <- rep(0:1, each = num.obs / 2)
  list(X = X, W = W)
}

generate.input.for.all.designs <- function(num.obs, num.vars.typical, num.designs, seed) {
  inputs <- vector("list", num.designs)
  for (design in 1:num.designs) {
    if (design == 4 || design == 5 || design == 6) {
      num.vars <- 2
    } else {
      num.vars <- num.vars.typical
    }
    inputs[[design]] <- generate.input(num.obs, num.vars, seed)
  }
  inputs
}