#install.packages("devtools")
#install.packages("Rcpp")
library(devtools)
# install_github("hadley/devtools")
install_github("shuihu/causal_effect_estimation", 
               auth_token = "44ec9832e53cfbb807d1faa9fcde10f29b297491"
)
# install_github("swager/randomForestCI")
library(causalTree)
library(randomForestCI)
library(FNN)
library(xtable)
