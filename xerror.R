### systematic testing code:
# use xpred.rpart.R
# y* CV
# p := propensity score
xerror <- function(fit, weights) {
  p <- sum(weights) / length(weights)
  prediction <- xpred.rpart(fit) 
  n_cp <- nrow(fit$cptable)
  treat <- which(weights == 1)
  control <- which(weights == 0)
  y_star <- fit$y
  y_star[treat] <- y_star[treat] / p
  y_star[control] <- y_star[control] / (p - 1)
  error <- rep(0, n_cp)
  for (i in 1:n_cp) {
    yhat <- prediction[ ,i]
    error[i] <- sum((yhat - y_star)^2)
  }
  names(error) = signif(fit$cp[ ,1], digits = 6)
  opcp = fit$cp[which(error == min(error)), 1]
  newlist = list(xerror = error, optimal_cp = opcp)
  return (newlist)
}
  

