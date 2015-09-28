# This returns the output values, tau, for input newdata and a causal tree.

est.causalTree.tau <- function(tree.fit, newdata) {
  
  # first find the leaves where the fit assigns the observations
  data <- causalTree.matrix(create.data.frame(tree.fit, newdata))
  where <- est.causalTree(tree.fit, data)
  
  # the node field provides a unique ID for each node of the tree
  # warning: max(nodes) may be _very_ large
  nodes <- as.numeric(row.names(tree.fit$frame))
  
  # a call to factor "collapses" the unique where indices into a contiguous set
  # we need to set "levels = nodes" because not all unique nodes appear in "where"
  node.order <- order(nodes)
  where.idx <- as.numeric(factor(where, levels = nodes[node.order]))
  
  node.idx.to.tau <- tree.fit$frame$yval[node.order]
  
  node.idx.to.tau[where.idx]
}