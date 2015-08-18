# test package checking 

setwd("~/Desktop/SUMMER_JOB/data")
D <- read.table("data.txt", header = T)
#D <- read.table("../data.txt", header = T)
D$black <- as.factor(D$black)
D$married <-as.factor(D$married)
D$income <- D$income/1000
treat <- rep(1:0, each = 200)

ctree <- rpart(income~., data = D, weights = treat, method = "anova", parms = 2, cp = 0, minbucket = 1, cv.option = "matching")

# 6:ctree$where == 6
dev: 29646.98
mean = 3.88

#8: ctree$where == 4
mean=0.8150666, MSE=37203.36 
8) black=0 10   36363.82  0.81506660 *
  
  
########## USING FULL DATA: #################
Data <- read.table("large.txt", header = T)
Data$c1 = as.factor(Data$c1)
Data$c2 = as.factor(Data$c2)
Data$c3 = as.factor(Data$c3)
Data$c4 = as.factor(Data$c4)

 

ctree$cp
#3.843077e-04      0 1.0000000 0.08309191 0.01019848
  
#   

##### TEST THE findNeighbor.c ######
small <- read.table("../data/small.txt",header = T)
small$black <-as.factor(small$black)
small$married <-as.factor(small$married)
streat <-rep(1:0, each = 50)
streat <- rep(0, 100)
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
@causal1 <- mean(mini[index1[index1 < 11], 5]) - mean(mini[index1[index1 > 10], 5])
causal2 <- mean(mini[index2[index2 < 11], 5]) - mean(mini[index2[index2 > 10], 5])
effect <- length(index1) * causal1^2 + length(index2) * causal2^2
root_effect<- (mean(mini[1:10, 5]) - mean(mini[11:20, 5]))^2 * 20
improve <- effect - root_effect
# at the root node: root_effect = 5.036557
improve


### test for categorical data:
mini = mini[,c(1,2,4,3,5)]
