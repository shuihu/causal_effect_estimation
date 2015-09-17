# returns a list of all the leaves in a tree

get.all.leaves <- function(tree) {
  rownames <- as.numeric(rownames(tree$frame))
  rownames[which(tree$frame$var == "<leaf>")]
}