# all.winning.models stores for each triple (design, os, model), the number of replications in the design where the model achieved the lowest -Q
# on the os (out-of-sample criterion).  This is used for producing the "Share" column in the comparison table.

init.all.winning.models <- function(num.designs, model.names, os.names) {
  winning.model.per.design.and.os.name <- init.named.list(model.names, 0) 
  winning.model.per.design <- init.named.list(os.names, winning.model.per.design.and.os.name)
  all.winning.models <- vector("list", num.designs)
  for (design in 1:num.designs) {
    all.winning.models[[design]] <- winning.model.per.design
  }
  all.winning.models
}