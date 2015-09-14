#' The infinitesimal jackknife for random forests
#'
#' @param rf A random forest trained with replace = TRUE and keep.inbag = TRUE
#' @param newdata A set of test points at which to evaluate standard errors
#' @param calibrate whether to apply calibration to mitigate Monte Carlo noise
#'        warning: if calibrate = FALSE, some variance estimates may be negative
#'                 due to Monte Carlo effects if the number of trees in rf is too small
#' @param used.trees set of trees to use for variance estimation; uses all tress if NULL

##
## TODO: currently pass in predictions directly. This is a hack.
## Eventually, comparisonForest should implement a "predict"
## method, and then it can call directly into the randomForestCI
## package for standard errors (and this file can be deleted).
##

randomForestInfJack = function(rf, predictions, calibrate = TRUE, used.trees = NULL, unif.fraction = 0.1) {
	
	#
	# Setup
	#
	
	if (is.null(rf$inbag)) {
		stop("Random forest must be trained with keep.inbag = TRUE")
	}
	
	if (length(levels(factor(colSums(rf$inbag)))) > 1) {
		stop("The keep.inbag field must store the number of times each observation was used")
	}
	
	if (is.null(used.trees)) {
		used.trees = 1:rf$ntree
	}
	
	#
	# Extract tree-wise predictions and variable counts from random forest
	#
	
	B = length(used.trees)
	n = length(rf$y)
	s = sum(rf$inbag) / rf$ntree
	
	# TEMPORARY -- now pred is just passed in. Future TODO:
	# make comparisonForest return an actual object on which we
	# can call predict
	
	#predictions = predict(rf, newdata, predict.all = TRUE)
	#pred = predictions$individual[, used.trees]
	# in case of classification, convert character labels to numeric (!)
	#class(pred) = "numeric"
	
	pred = predictions[, used.trees]
	
	y.hat = rowMeans(pred)
	pred.centered = pred - rowMeans(pred)
	
	N = Matrix::Matrix(rf$inbag[, used.trees], sparse = TRUE)
	N.avg = Matrix::rowMeans(N)
	
	#
	# Compute raw infinitesimal jackknife
	#
	
	# This is too slow for large N
	#C = N %*% t(pred.centered) - Matrix::Matrix(N.avg, nrow(N), 1) %*% Matrix::Matrix(rowSums(pred.centered), 1, nrow(pred.centered))
	#raw.IJ = Matrix::colSums(C^2) / B^2
	
	# Faster implementation. Uses the fact that colSums((A - B)^2) = T1 - 2 * T2 + T3,
	# where T1 = diag(A'A), T2 = diag(B'A), and T3 = diag(B'B)
	
	NTN = Matrix::crossprod(N, N)
	NTNPT_T = Matrix::tcrossprod(pred.centered, NTN)
	T1 = Matrix::rowSums(pred.centered * NTNPT_T)
	
	RS = rowSums(pred.centered)
	NbarTN = Matrix::crossprod(N.avg, N)
	T2 = RS * Matrix::tcrossprod(NbarTN, pred.centered)
	
	T3 = sum(N.avg^2) * RS^2
	raw.IJ = as.numeric(T1 - 2 * T2 + T3) / B^2
	
	#
	# Apply Monte Carlo bias correction
	#
	
	N.var = mean(Matrix::rowMeans(N^2) - Matrix::rowMeans(N)^2)
	boot.var = rowSums(pred.centered^2) / B
	bias.correction = n * N.var * boot.var / B
	vars = raw.IJ - bias.correction

	results = data.frame(y.hat=y.hat, var.hat=vars)
	
	if (nrow(results) <= 20) {
		calibrate = FALSE
		warning("No calibration with n <= 20")
	}
	
	#
	# If appropriate, calibrate variance estimates; this step in particular
	# ensures that all variance estimates wil be positive.
	#
	
	if (calibrate) {
		
		# Compute variance estimates using half the trees
		calibration.ratio = 2
		n.sample = ceiling(B / calibration.ratio)
		results.ss = randomForestInfJack(rf, pred, calibrate = FALSE, used.trees = sample(used.trees, n.sample))
		
		# Use this second set of variance estimates to estimate scale of Monte Carlo noise
		sigma2.ss = mean((results.ss$var.hat - results$var.hat)^2)
		delta = n.sample / B
		sigma2 = (delta^2 + (1 - delta)^2) / (2 * (1 - delta)^2) * sigma2.ss
		
		# Use Monte Carlo noise scale estimate for empirical Bayes calibration
		vars.calibrated = causalTree:::calibrateEB(vars, sigma2, unif.fraction = unif.fraction)
		results$var.hat = vars.calibrated
	}
	
	return (results)
}
