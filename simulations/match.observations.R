# Returns matches, which is a vector such that matches[i] is the index of the observation that is closest to 
# the ith observation among all observations that had the opposite W from the ith observation.

match.observations <- function(XW) {
  X <- XW$X
  W <- XW$W
  num.obs <- nrow(X)
  matches <- rep(0, num.obs)
  for (i in 1:num.obs) {
    #print(i)
    min.dist <- -1
    match <- 0
    for (j in 1:num.obs) {
      dist <- sum((X[i,] - X[j,]) ^ 2)
      if (W[i] != W[j] && (match == 0 || dist < min.dist)) {
        min.dist <- dist
        match <- j
      }
    }
    matches[i] <- match
  }
  matches
}

match.observations.for.all.designs <- function(XW) {
  num.designs <- length(XW)
  matches <- vector("list", num.designs)
  for (design in 1:num.designs) {
    matches[[design]] <- match.observations(XW[[design]])
  }
  matches
}