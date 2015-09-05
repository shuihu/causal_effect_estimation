# This returns the output values, tau, for input X and a causal tree.

est.causalTree.tau <- function(fit, X) {
  # first find the leaves where the fit assigns the observations
  data <- causalTree.matrix(create.data.frame(fit, X))
  where <- est.causalTree(fit, data)
  nodes <- as.numeric(row.names(fit$frame))
  node.to.tau <- rep(0, max(nodes))
  for (i in 1:length(nodes)) {
    node.to.tau[nodes[i]] <- fit$frame[i, 'yval']
  }
  node.to.tau[where]
}