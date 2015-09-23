\name{causalTree}
\alias{causalTree}
%\alias{causalTreecallback}
\title{
  Causal Effect Regression and Estimation Trees
}
\description{
  Fit a \code{causalTree} model to get an \code{rpart} object
}
\usage{
causalTree(formula, data, weights, treatment, subset, na.action = na.rpart, split.option, cv.option, minsize = 2L, model = FALSE, x = FALSE, y = TRUE, parms, p, control, cost, \dots)
}
\arguments{
  \item{formula}{a \link{formula}, with a response but no interaction
    terms.  If this a a data frome, that is taken as the model frame
    (see \code{\link{model.frame}).}
  }
  
  \item{data}{an optional data frame in which to interact the variables
    named in the formula.} 
  
  \item{weights}{optional case weights.}
  
  \item{treatment}{a vector indicates the treatment status of the data, 1 represents treated and 0 represents controlled. }
  
  \item{subset}{optional expression saying that only a subset of the
    rows of the data should be used in the fit.}
  
  \item{na.action}{the default action deletes all observations for which
    \code{y} is missing, but keeps those in which one or more predictors
    are missing.}
  
  \item{method}{one of \code{"anova"}, \code{"poisson"}, \code{"class"}
    or \code{"exp"}, same as the options of \code{method} in \pkg{rpart}. In this version, only \code{"anova"} is avaible for \code{method}, so we set it defaultly.}
    
    \item{split.option}{causal tree split options, one of \code{"CT"} and \code{"TOT"}, two splitting rules in the paper.}
    
    \item{cv.option}{cross validation options, one of \code{"TOT"} and \code{"matching"}, two cross validation rules in the paper.}
    
    \item{minsize}{the minimum number of both treated cases and controlled cases in the terminal nodes of the tree, and the default value is 2.}
  
  \item{model}{if logical: keep a copy of the model frame in the result?
    If the input value for \code{model} is a model frame (likely from an
    earlier call to the \code{causalTree} function), then this frame is used
    rather than constructing new data.}
  \item{x}{keep a copy of the \code{x} matrix in the result.}
  
  \item{y}{keep a copy of the dependent variable in the result.  If
    missing and \code{model} is supplied this defaults to \code{FALSE}.}
  
  \item{parms}{optional parameters for the splitting function.
  same as the parms in \code{rpart}.}
    
    \item{p}{propensity scores used in \code{"TOT"} splitting method and \code{"TOT"} cross validation. The default value is the proportion of treated cases in all cases.}
    
  \item{control}{a list of options that control details of the
    \code{rpart} algorithm.  See \code{\link{rpart.control}}.}
  
  \item{cost}{a vector of non-negative costs, one for each variable in
    the model. Defaults to one for all variables.  These are scalings to
    be applied when considering splits, so the improvement on splitting
    on a variable is divided by its cost in deciding which split to
    choose.}
  
  \item{\dots}{arguments to \code{\link{rpart.control}} may also be
    specified in the call to \code{causalTree}.  They are checked against the
    list of valid arguments.}
}

\details{
  CausalTree differs from \code{rpart} function in \pkg{rpart} package in splitting rules and cross validation methods. Please check Susan and Guido's paper \emph{Machine Learning Methods for Estimating Heterogeneous Causal Effects} (2015) for detials.
}

\value{
  An object of class \code{rpart}.  See \code{\link{rpart.object}}.
}

\references{
  Breiman L., Friedman J. H., Olshen R. A., and Stone, C. J. (1984)
  \emph{Classification and Regression Trees.}
  Wadsworth.
  
  Susan A., Guido I. (2015)  \emph{Machine Learning Methods for Estimating Heterogeneous Causal Effects}.
  
}

\seealso{
  \code{\link{rpart.control}}, \code{\link{rpart.object}},
  \code{\link{summary.rpart}}, \code{\link{print.rpart}}
}

\examples{
fit <- causalTree(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, 
data = simulation.1, treatment = simulation.1$treatment, split.option = "CT", 
cv.option = "matching", cp = 0, minsize = 5, minbucket = 5)

opcp <- fit$cptable[,1][which.min(fit$cptable[,4])]

opfit <- prune(fit, opcp)

rpart.plot(opfit)
}
\keyword{tree, causal effects?}
