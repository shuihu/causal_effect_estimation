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