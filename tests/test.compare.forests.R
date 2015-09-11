# This builds a honest and a standard random forest using causal trees, and compares their performance.

library(causalTree)
num.obs <- 500
num.vars <- 10
num.trees <- 1000
sample.size <- 200
node.size <- 1
cv.option <- "matching"
x <- matrix(0, num.obs, num.vars)
for (i in 1:num.vars) x[,i] <- rnorm(num.obs, 0, 1)
w <- rep(1:0, each = num.obs / 2)
y1 <- rnorm(num.obs / 2, 1 - x[1:num.obs/2, 1] + x[1:num.obs/2, 2], 1)
y0 <- rnorm(num.obs / 2, 0 + x[(num.obs/2 + 1):num.obs, 1] + x[(num.obs/2 + 1):num.obs, 2], 1)
y <- c(y1, y0)
forests <- compare.forests(y, x, w, num.trees, sample.size, node.size, cv.option)

print(paste("standard variance mean:", as.character(mean(forests$standard$variance))))
print(paste("honest variance mean:", as.character(mean(forests$honest$variance))))

# Uncomment any of the these lines to examine the comparison results
#print(forests$honest$y)
#print(forests$standard$y)
#print(forests$honest$pred.matrix)
#print(forests$standard$pred.matrix)
#print(forests$honest$inbag)
#print(forests$standard$inbag)
#print(forests$honest$variance)
#print(forests$standard$variance)
#print(forests$honest$trees)
#print(forests$standard$trees)
#print(forests$honest$ntree)
#print(forests$standard$ntree)

predictions.standard <- predict.randomForest.shuihu(forests$standard, x)
print(length(which((predictions.standard$individual == forests$standard$pred.matrix) == TRUE))) # should be 500,000
predictions.honest <- predict.randomForest.shuihu(forests$honest, x)
print(length(which((predictions.honest$individual == forests$honest$pred.matrix) == TRUE))) # should be 500,000
