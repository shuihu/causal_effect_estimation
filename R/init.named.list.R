init.named.list <- function(name.list, init.value) {
  l <- vector("list", length(name.list))
  names(l) <- name.list
  for (name in name.list) {
    l[[name]] <- init.value
  }
  l
}