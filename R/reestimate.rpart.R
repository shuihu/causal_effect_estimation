# reestimates the values at the leaves of an rpart tree using an honest set of observations

reestimate.rpart <- function(tree, data, Y) {
  library(rpart)
  tree2 <- tree
  prune.rpart(tree2, cp = 0)
  print("prune is fine")
  # used for finding which leaves data is assigned to
  where.tree <- tree
  where.tree$frame$yval <- 1:(length(where.tree$frame$yval))
  where <- predict(where.tree, data)
  rownames <- as.numeric(rownames(where.tree$frame))
  leaf.assignments <- rownames[where]
  all.leaves <- rownames[which(where.tree$frame$var == "<leaf>")]
  for (leaf in all.leaves) {
    in.leaf <- recursive.which.in.leaf(leaf.assignments, leaf, all.leaves)
    tree$frame$yval[which(rownames == leaf)[1]] <- mean(Y[in.leaf])
  }
  tree
}