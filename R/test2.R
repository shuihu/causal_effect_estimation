test2 <- function() {
  age <- sample(1:50, 20, replace = T)
  education <- sample(1:12, 20, replace = T)
  black <- sample(0:1, 20, replace = T)
  married <- sample(0:1, 20, replace = T)
  income <- sample(1:30, 20, replace = T)
  mini <- data.frame(age, education, black, married, income, mini)
  
  mtree <- rpart(income~., data = mini, method = "anova", cp = 0, minbucket = 1, xval = 0)
  mtree2 <- mtree
  prune.rpart(mtree2, cp = 0.1)
  class(mtree)
  predict(mtree, mini)
}
#predict.rpart(mtree, mini)
