source("simulations/readable.labels.R")
source("simulations/round.to.hundredth.R")

print.comparisons.to.csv <- function(comparisons, filename) {
  criteria <- names(comparisons)
  num.criteria <- length(criteria)
  model.names <- names(comparisons[[1]])
  num.models <- length(model.names)
  num.designs <- length(comparisons[[1]][[1]])
  leaf.stats <- c("Mean", "Median", "Std", "Spread")
  os.stats <- c("Q", "Q.Std", "Q.Spread", "Share")
  num.stats <- length(leaf.stats)
  num.rows <- 1 + num.criteria * (2 + num.models)
  num.cols <- 1 + num.designs * num.stats
  table <- matrix("", num.rows, num.cols)
  for (design in 1:num.designs) {
    table[1, 2 + (design - 1) * num.stats] <- get.design.label(design)
  }
  for (i in 1:num.criteria) {
    if (i == 1) {
      stats <- leaf.stats
    } else {
      stats <- os.stats
    }
    # title row
    title.row <- (2 + num.models) * (i - 1) + 2
    table[title.row, 1] <- get.criterion.label(criteria[i])
    for (design in 1:num.designs) {
      for (j in 1:num.stats) {
        table[title.row, 1 + num.stats * (design - 1) + j] <- get.stats.label(criteria[i], stats[j])
      }
    }
    
    # content rows
    for (j in 1:num.models) {
      content.row <- title.row + j
      table[content.row, 1] <- get.model.label(model.names[j])
      for (design in 1:num.designs) {
        for (k in 1:num.stats) {
          criterion.label <- criteria[[i]]
          model.label <- model.names[j]
          table[content.row, 1 + (design - 1) * num.stats + k] <- as.character(round.to.hundredth(comparisons[[criterion.label]][[model.label]][[design]][[stats[k]]]))
        }
      }
    }
  }
  
  write.table(table, filename, sep = ",", row.names = FALSE, col.names = FALSE)
}