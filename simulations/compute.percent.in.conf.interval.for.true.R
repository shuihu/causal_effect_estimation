compute.percent.in.conf.interval.for.true <- function(model.name, tree, y, w, counterfactual.y, counterfactual.w, propensity, conf.percent) {
  if (conf.percent == 0.95) {
    thresh <- 1.96
  } else if (conf.percent == 0.9) {
    thresh <- 1.645
  } else {
    stop("Only computing confidence intervals for 95% and 90%")
  }
  
  leaves <- which(tree$frame$var == "<leaf>")
  num.in.interval <- 0
  transformed.y <- y * (w - propensity) / (propensity * (1 - propensity))
  for (leaf in leaves) {
    if (!is.null(tree$where2)) {
      in.leaf <- which(tree$where2[leaf,] == 1)
    } else {
      in.leaf <- which(tree$where == leaf)
    }
    in.leaf.w0 <- in.leaf[which(w[in.leaf] == 0)]
    in.leaf.w1 <- in.leaf[which(w[in.leaf] == 1)]
    true.tau <- mean(c(y[in.leaf.w1] - y[in.leaf.w0], counterfactual.y[in.leaf.w0] - y[in.leaf.w0]))
    
    if (model.name == "TOT_split_xval_rpart") {
      standard.error <- sqrt(mean((transformed.y[in.leaf] - mean(transformed.y[in.leaf]))^2) / length(in.leaf))
    } else {
      s1.sq <- mean((y[in.leaf.w1] - mean(y[in.leaf.w1]))^2)
      s0.sq <- mean((y[in.leaf.w0] - mean(y[in.leaf.w0]))^2)
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
    }
    
    diff <- abs((tree$frame$yval[leaf] - true.tau) / standard.error)
    if (!is.na(diff) && !is.infinite(diff) && diff <= thresh) {
      num.in.interval <- num.in.interval + 1
    } 
  }
  num.in.interval / length(leaves)
}