#arr = new int[10] (sum of y, count)
#for each data:
#  data lies in where
#  while where > 0:
#    arr[x] += (y, 1)
#    where /= 2


# without considering the recursive issue temporarily:
honestTree <- function(object, data, treatment, na.action = na.causalTree) {
  if (!inherits(object, "rpart")) stop("Not a legitimate \"rpart\" object")
  
  nodes <- as.numeric(row.names(object$frame))
  num <- length(nodes)
  Terms <- object$terms
  #data <- model.frame(Terms, data, na.action = na.action,
  data <- model.frame(Terms, data, na.action = na.action, treatment = treatment, 
                      xlev = attr(object, "xlevels"))
  #print (data)
  
  if (!is.null(cl <- attr(Terms, "dataClasses")))
    .checkMFClasses(cl, data, TRUE)
  
  treatment <- data$`(treatment)`
  n <- nrow(data)
  Y <- model.response(data)
  where <- est.causalTree(object, causalTree.matrix(data))
  #wt <- model.weights(data)
  
  ## begin to compute the yval and dev:
  # initialize:
  y1 <- rep(0, num)
  y0 <- rep(0, num)
  dev1 <- rep(0,num)
  dev0 <- rep(0,num)
  count1 <- rep(0, num)
  count0 <- rep(0, num)
  
  # for loop to insert values:
  for (i in 1:n) {
    node_id <- where[i]
    while (node_id > 0) {
      index <- which(nodes == node_id)
      if (treatment[i] == 1) {
        y1[index] <- y1[index] + Y[i]
        dev1[index] <- dev1[index] + Y[i] * Y[i]  
        count1[index] <- count1[index] + 1
      } else {
        y0[index] <- y0[index] + Y[i]
        dev0[index] <- dev0[index] + Y[i] * Y[i] 
        count0[index] <- count0[index] + 1
      }
      node_id <- floor(node_id / 2)
    }
  }
  
  # get the final results for each：
  causal_effect <- y1 /count1 - y0/count0
  deviance <- dev1 - y1^2/count1 + dev0 - y0^2/count0
  
  new_object = object
  new_object$frame$dev <- deviance
  new_object$frame$yval <- causal_effect 
  new_object$frame$n <- count1 + count0
  
  # here we set weights as 1 for all data points
  new_object$frame$wt <- count1 + count0
  
  
  return (new_object)
}