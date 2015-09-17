## allow backticks in causalTree.matrix: see
## https://stat.ethz.ch/pipermail/r-help/2012-May/314081.html

library(causalTree)
Iris <- iris
names(Iris) <- sub(".", " ", names(iris), fixed=TRUE)
causalTree(Species ~ `Sepal Length`, data = Iris)
