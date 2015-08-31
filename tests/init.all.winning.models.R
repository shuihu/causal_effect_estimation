# all.winning.models stores for each triple (design, os, model), the number of replications in the design where the model achieved the lowest -Q
# on the os (out-of-sample criterion).  This is used for producing the "Share" column in the comparison table.

init.all.winning.models <- function(num.designs, model.names) {
  winning.model.per.design.and.os.name <- vector("list", length(model.names))
  names(winning.model.per.design.and.os.name) <- model.names
  for (model.name in model.names) {
    winning.model.per.design.and.os.name[[model.name]] <- 0
  }
  winning.model.per.design <- list(os.to = winning.model.per.design.and.os.name, os.m = winning.model.per.design.and.os.name, os.infeasible = winning.model.per.design.and.os.name)
  rep(winning.model.per.design, num.designs)
}
