#install.packages('devtools') 
#install.packages('devtools', repos='http://cran.us.r-project.org') --> this version runs on (yen) server

#install.packages("Rcpp")
# install.packages("Rcpp", repos='http://cran.us.r-project.org')
library(devtools)
# install_github("hadley/devtools")
# install_github("shuihu/causal_effect_estimation", 
#                auth_token = "44ec9832e53cfbb807d1faa9fcde10f29b297491"
# )
install_github("shuihu/causal_effect_estimation",  ref="master2",
               auth_token = "44ec9832e53cfbb807d1faa9fcde10f29b297491")
# install_github("swager/randomForestCI")
library(causalTree)
library(randomForestCI)

# install.packages('FNN', repos='http://cran.us.r-project.org')
# install.packages('xtable', repos='http://cran.us.r-project.org')
library(FNN)
library(xtable)
