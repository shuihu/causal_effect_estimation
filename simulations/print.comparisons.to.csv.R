source("simulations/readable.labels.R")
source("simulations/round.to.hundredth.R")

print.comparisons.to.csv <- function(comparisons, filename) {
  criteria <- names(comparisons)
  num.criteria <- length(criteria)
  model.names <- names(comparisons[[1]])
  num.models <- length(model.names)
  num.designs <- length(comparisons[[1]][[1]])
  conf.interval.true.stats <- c("dishonest.in.dishonest.conf.intv.95", "dishonest.in.dishonest.conf.intv.90", "honest.in.honest.conf.intv.95", "honest.in.honest.conf.intv.90")
  conf.interval.stats.between.trees <- c("honest.in.dishonest.conf.intv.95", "honest.in.dishonest.conf.intv.90", "dishonest.in.honest.conf.intv.95", "dishonest.in.honest.conf.intv.90")
  conf.interval.stats.for.test.data <- c("test.in.dishonest.conf.intv.95", "test.in.dishonest.conf.intv.90", "test.in.honest.conf.intv.95", "test.in.honest.conf.intv.90")
  weighted.conf.interval.stats.for.test.data <- c("weighted.test.in.dishonest.conf.intv.95", "weighted.test.in.dishonest.conf.intv.90", "weighted.test.in.honest.conf.intv.95", "weighted.test.in.honest.conf.intv.90")
  full.trees.stats <- c("Proportion")
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
    if ( i == 1) {
      stats <- conf.interval.true.stats
    } else if (i == 2) {
      stats <- conf.interval.stats.between.trees
    } else if (i == 3) {
      stats <- conf.interval.stats.for.test.data
    } else if (i == 4) {
      stats <- weighted.conf.interval.stats.for.test.data
    } else if (i == 5) {
      stats <- full.trees.stats
    } else if (i == 6) {
      stats <- leaf.stats
    } else {
      stats <- os.stats
    }
    # title row
    title.row <- (2 + num.models) * (i - 1) + 2
    table[title.row, 1] <- get.criterion.label(criteria[i])
    for (design in 1:num.designs) {
      for (j in 1:num.stats) {
        if (j <= length(stats)) {
          table[title.row, 1 + num.stats * (design - 1) + j] <- get.stats.label(criteria[i], stats[j])
        } else {
          table[title.row, 1 + num.stats * (design - 1) + j] <- ""
        }
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
          if (k <= length(stats)) {
            table[content.row, 1 + (design - 1) * num.stats + k] <- as.character(round.to.hundredth(comparisons[[criterion.label]][[model.label]][[design]][[stats[k]]]))
          } else {
            table[content.row, 1 + (design - 1) * num.stats + k] <- ""
          }
        }
      }
    }
  }
  
  write.table(table, filename, sep = ",", row.names = FALSE, col.names = FALSE)
}