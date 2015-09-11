# This should be part of Splus proper -- make predict a method
predict <- function(tree, ...)  UseMethod("predict")
