test2 <- function() {
  library(rpart)
  age <- sample(1:50, 20, replace = T)
  education <- sample(1:12, 20, replace = T)
  black <- sample(0:1, 20, replace = T)
  married <- sample(0:1, 20, replace = T)
  income <- sample(1:30, 20, replace = T)
  mini <- data.frame(age, education, black, married, income)
  
  mtree <- rpart(income~., data = mini, method = "anova", cp = 0, minbucket = 1, xval = 0)
  predict(mtree, mini)
}
#predict.rpart(mtree, mini)
