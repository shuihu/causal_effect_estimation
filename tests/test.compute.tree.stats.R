mini <- read.table("mini.txt", header = T)
mini$black <- as.factor(mini$black)
mini$married <- as.factor(mini$married)
weight <- rep(1, 20)
mtreat <- rep(1:0, each = 10)
minstrument <- rep(1:0, each = 10)

mtree <- causalTree(income~., data = mini, weights = mtreat, method = "anova", parms = 1, cp = 0, minbucket = 1, cv.option = "TOT", p = 0.5, xval = 0)
rpart.plot(mtree)

seed <- 10
num.obs.per.set <- 20
num.vars.per.obs <- 4
design = 2

X <- matrix(0, num.obs.per.set, num.vars.per.obs)
X[,1] <- mini$age
X[,2] <- mini$education
X[,3] <- mini$black
X[,4] <- mini$married
W <- rep(1:0, each = 10)
XW <- list(X = X, W = W)
Y <- mini$income

#XW <-  generate.input(num.obs.per.set, num.vars.per.obs, seed)
#Y <- generate.output(XW, design, seed)
model <- compute.tree.stats(split.XW = XW, split.Y = Y, model.name = "ST", propensity = 0.5)
model@tree
rpart.plot(model@tree)

x1 <- rnorm(0, 100, 1)
x2 <- rnorm(0, 100, 1)
