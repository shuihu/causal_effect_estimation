# Given the number of observations and number of trees to use in a random forest, 
# initialize list(pred.honest, pred.standard, pred.honest.matrix, pred.standard.matrix, use.matrix, variance.honest, variance.standard)
# which represents the comparison results for the "honest" and "standard" random forests trained on the same set of observationbs.
# pred.honest: vector of num.obs elements
# pred.standard: vector of num.obs elements
# pred.honest.matrix: num.obs x num.trees matrix
# pred.standard.matrix: num.obs x num.trees matrix
# use.matrix.honest: num.obs x num.trees matrix of 0s and 1s (indicating whether an observation was used for a particular tree in the honest forest)
# use.matrix.standard: num.obs x num.trees matrix of 0s and 1s (indicating whether an observation was used for a particular tree in the standard forest)
# variance.honest: vector of num.obs elements
# variance.standard: vector of num.obs elements

init.compare.forests.results <- function(num.obs, num.trees) {
  pred.honest <- rep(0, num.obs)
  pred.standard <- rep(0, num.obs)
  pred.honest.matrix <- matrix(0, num.obs, num.trees)
  pred.standard.matrix <- matrix(0, num.obs, num.trees)
  use.matrix.honest <- matrix(0, num.obs, num.trees)
  use.matrix.standard <- matrix(0, num.obs, num.trees)
  variance.honest <- rep(0, num.obs)
  variance.standard <- rep(0, num.obs)
  list(pred.honest = pred.honest, pred.standard = pred.standard, pred.honest.matrix = pred.honest.matrix, pred.standard.matrix = pred.standard.matrix, use.matrix.honest = use.matrix.honest, use.matrix.standard = use.matrix.standard, variance.honest = variance.honest, variance.standard = variance.standard)
}