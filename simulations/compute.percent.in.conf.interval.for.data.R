# computes for what proportion of the leaves in a tree does the causal effect estimated from the data (difference between the average
# Y of W = 0 and W = 1 observations from the data that got assigned to the leaf) fall within the confidence interval for that leaf.

compute.percent.in.conf.interval.for.data <- function(data.X, data.W, data.Y, model, train.W, train.Y, conf.percent) {
  if (conf.percent == 0.95) {
    thresh <- 1.96
  } else if (conf.percent == 0.9) {
    thresh <- 1.645
  } else {
    stop("Only computing confidence intervals for 95% and 90%")
  }
  
  leaves <- which(model@tree$frame$var == "<leaf>")
  yval <- model@tree$frame$yval
  model@tree$frame$yval <- 1:length(model@tree$frame$yval)
  leaf.assignments <- predict.model(model, data.X)
  model@tree$frame$yval <- yval
  num.in.interval <- 0
  
  for (leaf in leaves) {
    data.in.leaf <- which(leaf.assignments == leaf)
    data1.in.leaf <- data.in.leaf[which(data.W[data.in.leaf] == 1)]
    data0.in.leaf <- data.in.leaf[which(data.W[data.in.leaf] == 0)]
    estimate.from.data <- mean(data.Y[data1.in.leaf]) - mean(data.Y[data0.in.leaf])
    
    if (!is.null(model@tree$where2)) {
      in.leaf <- which(model@tree$where2[leaf,] == 1)
    } else {
      in.leaf <- which(model@tree$where == leaf)
    }
    in.leaf.w0 <- in.leaf[which(train.W[in.leaf] == 0)]
    in.leaf.w1 <- in.leaf[which(train.W[in.leaf] == 1)]
    s1.sq <- mean((train.Y[in.leaf.w1] - mean(train.Y[in.leaf.w1]))^2)
    s0.sq <- mean((train.Y[in.leaf.w0] - mean(train.Y[in.leaf.w0]))^2)
    if (is.na(s1.sq) || is.infinite(s1.sq)) {
      avg.s1.sq <- 0
    } else {
      avg.s1.sq <- s1.sq / length(in.leaf.w1)
    }
    if (is.na(s0.sq) || is.infinite(s0.sq)) {
      avg.s0.sq <- 0
    } else {
      avg.s0.sq <- s0.sq / length(in.leaf.w0)
    }
    standard.error <- sqrt(avg.s1.sq + avg.s0.sq)
    #print(c(estimate.from.data, model@tree$frame$yval[leaf]))
    diff <- abs((estimate.from.data - model@tree$frame$yval[leaf]) / standard.error)
    if (!is.na(diff) && !is.infinite(diff) && diff <= thresh) {
      num.in.interval <- num.in.interval + 1
    }
  }
  num.in.interval / length(leaves)
}