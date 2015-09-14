library(causalTree)

#welfare = read.csv("~/Dropbox/Collaborations/TreatmentEffectsData/WelfareSurvey/ProcessedData/welfarelabel.csv")

welfare = read.csv("~/welfarelabel.csv")

X.all = welfare[, c(1, 3:60)]

X.all$polviews[X.all$polviews == "4.1220088"] = NA
X.all$polviews = factor(X.all$polviews)
polviews = sapply(X.all$polviews, function(xx) {
	if(is.na(xx)) return(NA)
	if(xx == "extrmly conservative") return(3)
	if(xx == "conservative") return(2)
	if(xx == "slghtly conservative") return(1)
	if(xx == "extremely liberal") return(-3)
	if(xx == "liberal") return(-2)
	if(xx == "slightly liberal") return(-1)
	if(xx == "moderate") return(0)
})

X.all$income[X.all$income == "10.55557"] = NA
X.all$income = factor(X.all$income)
income = sapply(X.all$income, function(xx) {
	if(is.na(xx)) return(NA)
	if(xx == "$1000 to 2999") return(1)
	if(xx == "$10000 - 14999") return(10)
	if(xx == "$15000 - 19999") return(15)
	if(xx == "$20000 - 24999") return(20)
	if(xx == "$25000 or more") return(15)
	if(xx == "$3000 to 3999") return(3)
	if(xx == "$4000 to 4999") return(4)
	if(xx == "$5000 to 5999") return(5)
	if(xx == "$6000 to 6999") return(6)
	if(xx == "$7000 to 7999") return(7)
	if(xx == "$8000 to 9999") return(8)
	if(xx == "lt $1000") return(0)
})

year = as.numeric(X.all$year)

wrkstat = X.all$wrkstat
wrkstat[wrkstat == "2.900022"] = NA
wrkstat = factor(wrkstat)
wrkstat = as.numeric(wrkstat) # because causalTree can't currently do factors

X.all$childs = as.character(X.all$childs)
X.all$childs[X.all$childs == "1.8806012"] = NA
X.all$childs[X.all$childs == "eight or more"] = "8"
X.all$childs = as.numeric(X.all$childs)

X.all$age = as.character(X.all$age)
X.all$age[X.all$age == "46.091595"] = NA
X.all$age[X.all$age == "89 or older"] = 89
X.all$age = as.numeric(X.all$age)

X = data.frame(income, polviews, year, wrkstat, X.all$hrs1, X.all$hrs2, X.all$occ, X.all$prestige, X.all$prestg80, X.all$sibs, X.all$childs, X.all$age)

ok.idx = which(!is.na(rowSums(X) + welfare$y + welfare$w))
X = X[ok.idx,]
colnames(X) = 1:ncol(X)
Y = as.numeric(welfare$y)[ok.idx]
W = as.numeric(welfare$w)[ok.idx]

cmp = comparisonForest(Y, X, W, num.trees=500)

results = data.frame(income = X[,1], polviews = X[,2], CATE = cmp$tau)

cmp.ci = randomForestInfJack(cmp, cmp$pred.matrix)

save.image("~/welfare.RData")

#boxplot(CATE ~ income, data = results)
#boxplot(CATE ~ polviews, data = results)