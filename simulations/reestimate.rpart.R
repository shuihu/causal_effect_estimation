# reestimates the values at the leaves of an rpart tree using an honest set of observations

reestimate.rpart <- function(tree, data, Y, W) {
  # used for finding which leaves data is assigned to
  where.tree <- tree
  where.leaves <- which(where.tree$frame$var == "<leaf>")
  where.tree$frame$yval[where.leaves] <- where.leaves
  where <- predict(where.tree, data, type = "vector")
  rownames <- as.numeric(rownames(where.tree$frame))
  tree$where2 <- matrix(0, length(rownames), length(Y))
  leaf.assignments <- rownames[where]
  all.leaves <- rownames[where.leaves]
  for (leaf in all.leaves) {
    in.leaf <- recursive.which.in.leaf(leaf.assignments, leaf, all.leaves, W)
    tree$where2[which(rownames == leaf)[1], in.leaf] <- 1
    tree$frame$yval[which(rownames == leaf)[1]] <- mean(Y[in.leaf])
  }
  tree
}