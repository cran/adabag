\name{plot.margins}
\alias{plot.margins}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots the margins of the ensemble
}
\description{ Plots the previously calculated margins of an AdaBoost.M1, AdaBoost-SAMME or Bagging classifier for a data frame}

\usage{
\method{plot}{margins}(x, y = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{margins}. This is assumed to be the result
	 of some function that produces an object with a component named \code{margins} as that 
	returned by the \code{margins} function. 
}
  \item{y}{
 This argument can be used to represent in the same plot
the margins in the test and train sets, \code{x} and \code{y}, respectively.
Should be \code{NULL} (by default) or an object of class \code{margins}.

}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
Intuitively, the margin for an observation is related to the certainty of its classification. It is calculated as the difference between
the support of the correct class and the maximum support of an incorrect class
}
\value{
A labeled plot is produced on the current graphics device (one being opened if needed). 

}
\references{
Alfaro, E., Gamez, M. and Garcia, N. (2013): ``adabag: An R Package for Classification with Boosting and Bagging''. Journal of Statistical Software, Vol 54, 2, pp. 1--35.

  Alfaro, E., Garcia, N., Gamez, M. and Elizondo, D. (2008): ``Bankruptcy forecasting: An empirical comparison of AdaBoost and neural networks''. Decision Support Systems, 45, pp. 110--122.

  Schapire, R.E., Freund, Y., Bartlett, P. and Lee, W.S. (1998): ``Boosting the margin: A new explanation for the effectiveness of voting methods''. The Annals of Statistics, vol 26, 5, pp. 1651--1686.
}
\author{Esteban Alfaro-Cortes \email{Esteban.Alfaro@uclm.es}, Matias Gamez-Martinez \email{Matias.Gamez@uclm.es} and Noelia Garcia-Rubio \email{Noelia.Garcia@uclm.es} }



%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ 
	\code{\link{margins}},
       \code{\link{boosting}},
       \code{\link{predict.boosting}},
    	\code{\link{bagging}},
	\code{\link{predict.bagging}}
}
\examples{
library(mlbench)
data(BreastCancer)
l <- length(BreastCancer[,1])
sub <- sample(1:l,2*l/3)
cntrl <- rpart.control(maxdepth = 3, minsplit = 0,  cp = -1)

BC.adaboost <- boosting(Class ~.,data=BreastCancer[sub,-1],mfinal=5, control=cntrl)
BC.adaboost.pred <- predict.boosting(BC.adaboost,newdata=BreastCancer[-sub,-1])

BC.margins<-margins(BC.adaboost,BreastCancer[sub,-1]) # training set
BC.predmargins<-margins(BC.adaboost.pred,BreastCancer[-sub,-1]) # test set
plot.margins(BC.predmargins,BC.margins)


}

\keyword{tree }% at least one, from doc/KEYWORDS
\keyword{classif}