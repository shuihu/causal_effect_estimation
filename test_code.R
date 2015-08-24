
  

  
#   

##### TEST THE findNeighbor.c ######
small <- read.table("../data/small.txt",header = T)
small$black <-as.factor(small$black)
small$married <-as.factor(small$married)
streat <-rep(1:0, each = 50)
#streat <- rep(0, 100)
stree <- rpart(income~., data = small, weights = streat, method = "anova", parms = 2, cp = 0, minbucket = 1, cv.option = "matching")
stree <- rpart(income~., data = small, weights = streat, method = "anova", parms = 2, cp = 0, minbucket = 1, cv.option = "matching")

###### TEST THE TOT method: ########

mini <- read.table("../data/mini.txt", header = T)
mini$black <- as.factor(mini$black)
mini$married <- as.factor(mini$married)
mtreat <- rep(1:0, each = 10)
#### min_node_size = 1 there is at least one control and one treatment in on leaf node:
mtree <- rpart(income~., data = mini, weights = mtreat, method = "anova", parms = 1, cp = 0, minbucket = 1, cv.option = "TOT", p = 0.5, xval = 0)
library(rpart.plot)
rpart.plot(mtree)

### test the  splitting function:
## for each variable:
## education:
sort_education <- sort(mini$education)
index1 <- which(mini$married == 0)
index2 <- which(mini$married == 1)
index1 <- which(mini$black == 1)
index2 <- which(mini$black == 0)
wt_sum1 <- sum(mini[index1[index1 < 11], 5])
wt_sum2 <- sum(mini[index1[index1 > 10], 5])
causal1 <- mean(mini[index1[index1 < 11], 5]) - mean(mini[index1[index1 > 10], 5])
causal2 <- mean(mini[index2[index2 < 11], 5]) - mean(mini[index2[index2 > 10], 5])
effect <- length(index1) * causal1^2 + length(index2) * causal2^2
root_effect<- (mean(mini[1:10, 5]) - mean(mini[11:20, 5]))^2 * 20
improve <- effect - root_effect
# at the root node: root_effect = 5.036557
improve


### test for categorical data:
mini = mini[,c(1,2,4,3,5)]


### test for first cut in root:
index1 <- which(mini$ < 40)
index2 <- which(mini$age >= 40)

#wt_sum1 <- sum(mini[index1[index1 < 11], 5])
#wt_sum2 <- sum(mini[index1[index1 > 10], 5])
causal1 <- mean(mini[index1[index1 < 11], 5]) - mean(mini[index1[index1 > 10], 5])
causal2 <- mean(mini[index2[index2 < 11], 5]) - mean(mini[index2[index2 > 10], 5])
effect <- length(index1) * causal1^2 + length(index2) * causal2^2
root_effect<- (mean(mini[1:10, 5]) - mean(mini[11:20, 5]))^2 * 20
improve <- effect - root_effect
improve

#### test for the left cut (second):

mini <- cbind(mini, rep(1:0, each = 10))
names(mini)[6] <- "wt"

index <- which(mini$education < 9.5)
#mini1 <- mini[index,]

index1 <- intersect(index, which(mini$age < 23))
index2 <- intersect(index, which(mini$age >= 23))
causal1 <- mean(mini[index1[index1 < 11], 5]) - mean(mini[index1[index1 > 10], 5])
causal2 <- mean(mini[index2[index2 < 11], 5]) - mean(mini[index2[index2 > 10], 5])
effect <- length(index1) * causal1^2 + length(index2) * causal2^2
#node_effect <- length(index) * (mean(mini[< 11,5]) - mean(mini1[rownames(mini1) > 10, 5]))^2
treat <- which(mini$wt == 1)
control <- which(mini$wt == 0)
node_effect <- length(index) * (mean(mini[intersect(index, treat),5]) - mean(mini[intersect(index, control), 5]))^2
improve <- effect - node_effect
improve

#### test for the right cut (second):

index <- which((mini$education >= 9.5 & mini$age >= 26.5) & mini$education >= 11.5)
index1 <- intersect(index, which(mini$married == 0))
index2 <- intersect(index, which(mini$married == 1))
causal1 <- mean(mini[index1[index1 < 11], 5]) - mean(mini[index1[index1 > 10], 5])
causal2 <- mean(mini[index2[index2 < 11], 5]) - mean(mini[index2[index2 > 10], 5])
effect <- length(index1) * causal1^2 + length(index2) * causal2^2
#node_effect <- length(index) * (mean(mini[< 11,5]) - mean(mini1[rownames(mini1) > 10, 5]))^2
treat <- which(mini$wt == 1)
control <- which(mini$wt == 0)
node_effect <- length(index) * (mean(mini[intersect(index, treat),5]) - mean(mini[intersect(index, control), 5]))^2
improve <- effect - node_effect
improve
#### congratulations~~~ done!! ####

mtree <- rpart(income~., data = mini, weights = mtreat, method = "anova", parms = 1, cp = 0, minbucket = 1, 
               cv.option = "matching", xval = 10)


##### test on small data set:
small<- read.table("../data/small.txt", header =T);
streat <- rep(1:0, each = 50)
small$black <- as.factor(small$black)
small$married <- as.factor(small$married)
stree <- rpart(income~., data = small, weights = streat, method = "anova", parms = 2, cp =  0, 
               minbucket = 2, cv.option = "matching", xval = 10)
stree$cp

### test on matching:
mini <- read.table("../data/mini.txt", header = T)
mini$black <- as.factor(mini$black)
mini$married <- as.factor(mini$married)
mtreat <- rep(1:0, each = 10)
#### min_node_size = 1 there is at least one control and one treatment in on leaf node:
mtree <- rpart(income~., data = mini, weights = mtreat, method = "anova", parms = 1, cp = 0, minbucket = 1, cv.option = "matching", xval = 10)
library(rpart.plot)
rpart.plot(mtree)



#### test 6 & 11:
index <-setdiff(1:20, c(6,11))
treat <- 1:10
control <- 11:20
index1 <- intersect(index, treat)
index2 <- intersect(index, control)
yval <- mean(mini[index1, 5]) - mean(mini[index2, 5])
yval
y_max <- 24.90945
tree_error <- length(index) * y_max^2 - length(index) * yval^2
tree_error
cp <- 334.710764 / tree_error
test1 <- rpart(income~., data = mini[index,], weights = mtreat[index],method = "anova", parms = 1, cp = 0, minbucket = 1, cv.option = "matching", xval = 0)

split1 <- which(mini$age < 26.5)
id <- intersect(index, split1)
id1 <- intersect(treat, id)
id2 <- intersect(control, id)
causal <- mean(mini[id1, 5]) - mean(mini[id2, 5])
causal
complexity <- length(id) * y_max^2 - length(id) *causal^2
complexity

##### simulation data:
simulation <-read.csv("../data/simulation.csv", header = T)
simu_tree <- rpart(y~., data = simulation[,-12], weights = simulation[,12], method = "anova", parms = 2, cp = 0, cv.option = "matching", xval = 10)
simu_tree


#### test the CV on matching method:
mini <- read.table("../data/mini.txt", header = T)
mini$black <- as.factor(mini$black)
mini$married <- as.factor(mini$married)
mtreat <- rep(1:0, each = 10)
#### min_node_size = 1 there is at least one control and one treatment in on leaf node:

mtree <- rpart(income~., data = mini, weights = mtreat, method = "anova", parms = 1, cp = 0, minbucket = 1, cv.option = "matching", xval = 10)
mtree$cp
library(rpart.plot)
rpart.plot(mtree)
rpart:::pred.rpart(mtree, rpart:::rpart.matrix(mini[c(6,11,5,3),]))

mtree1 <- rpart(income~., data = mini[-c(6,11,4,12),], weights = mtreat[-c(6,11,4,12)], method = "anova", parms = 1, cp = 0, minbucket = 1, cv.option = "matching", xval = 0)
rpart:::pred.rpart(mtree1, rpart:::rpart.matrix(mini[c(6,11,4,12),]))