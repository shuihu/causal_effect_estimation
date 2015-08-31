#
# Test out the "return.all" argument of xpred
#  The data set has the virtue of continuous, categorical, and missings
#
library(causalTree)
require(survival)

fit1 <- causalTree(Surv(pgtime, pgstat) ~ age + eet + g2+grade+gleason +ploidy,
                stagec,  method='poisson')

xgct <- rep(1:3, length=nrow(stagec))  # explicitly set the xval groups

xfit1 <- xpred.causalTree(fit1, xval=xgct, return.all=T)
xfit2 <- array(0, dim=dim(xfit1))
cplist <- as.numeric(dimnames(xfit1)[[2]])

for (i in 1:3) {
    tfit <- causalTree(Surv(pgtime, pgstat) ~ age + eet + g2+grade+gleason +ploidy,
                stagec,  method='poisson', subset=(xgct !=i))
    # xvals are actually done on the absolute risk (node's risk /n), not on
    #   the rescaled risk ((node risk)/ (top node risk)) which is the basis
    #   for the printed CP.  To get the right answer we need to rescale.
    cp2 <- cplist * (fit1$frame$dev[1] / fit1$frame$n[1]) / 
                    (tfit$frame$dev[1] / tfit$frame$n[1])

    for (j in 1:length(cp2)) {
        tfit2 <- prune(tfit, cp=cp2[j])
        temp <- predict(tfit2, newdata=stagec[xgct==i,], type='matrix')
        xfit2[xgct==i, j,] <- temp
        }
    }

all.equal(xfit1, xfit2, check.attributes=FALSE)
