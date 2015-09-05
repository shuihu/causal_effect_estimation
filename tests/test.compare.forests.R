# This runs builds an honest and a standard random forest using causal trees, and compares their performance.

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
results <- compare.forests(y, x, w, num.trees, sample.size, node.size, cv.option)

print(paste("variance.standard mean:", as.character(mean(results$variance.standard))))
print(paste("variance.honest mean:", as.character(mean(results$variance.honest))))

# Uncomment any of the these lines to examine the results
#print(results$pred.honest)
#print(results$pred.standard)
#print(results$pred.honest.matrix)
#print(results$pred.standard.matrix)
#print(results$use.matrix.honest)
#print(results$use.matrix.standard)
#print(results$variance.honest)
#print(results$variance.standard)
