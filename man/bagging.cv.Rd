\name{bagging.cv}
\alias{bagging.cv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Runs v-fold cross validation with Bagging}

\description{
	The data are divided into \code{v} non-overlapping subsets of roughly equal size. Then, \code{bagging}
	is applied on \code{(v-1)} of the subsets. Finally, predictions are made for the left out subsets,
	and the process is repeated for each of the \code{v} subsets.
}

\usage{
bagging.cv(formula, data, v = 10, mfinal = 100, control, par=FALSE)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{formula}{ a formula, as in the \code{lm} function. }
  \item{data}{a data frame in which to interpret the variables named in \code{formula} }
  \item{v}{An integer, specifying the type of v-fold cross validation. Defaults to 10.
	If \code{v} is set as the number of observations, leave-one-out cross validation is carried out. 
	Besides this, every value between two and the number of observations is valid and means
	that roughly every v-th observation is left out.}
  \item{mfinal}{an integer, the number of iterations for which boosting is run 
	or the number of trees to use. Defaults to \code{mfinal=100} iterations.}
  \item{control}{options that control details of the rpart algorithm. See rpart.control for more details.  }
  \item{par}{ if \code{TRUE}, the cross validation process is runned in parallel. If \code{FALSE} (by default),
 the function runs without parallelization.  }

}

\value{
   An object of class \code{bagging.cv}, which is a list with the following components:
  \item{class }{the class predicted by the ensemble classifier.}
  \item{confusion }{the confusion matrix which compares the real class with the predicted one.}
  \item{error }{returns the average error.}
}

\references{Alfaro, E., Gamez, M. and Garcia, N. (2013): ``adabag: An R Package for Classification with Boosting and Bagging''. Journal of Statistical Software, Vol 54, 2, pp. 1--35.

	Alfaro, E., Garcia, N., Gamez, M. and Elizondo, D. (2008): ``Bankruptcy forecasting: An empirical comparison of AdaBoost and neural networks''. Decision Support Systems, 45, pp. 110--122.

	Breiman, L. (1996): "Bagging predictors". Machine Learning, Vol 24, 2, pp. 123--140.

	Breiman, L. (1998). "Arcing classifiers". The Annals of Statistics, Vol 26, 3, pp. 801--849. }

\author{Esteban Alfaro-Cortes \email{Esteban.Alfaro@uclm.es}, Matias Gamez-Martinez \email{Matias.Gamez@uclm.es} and Noelia Garcia-Rubio \email{Noelia.Garcia@uclm.es} }





 %~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ 
	\code{\link{bagging}}, 
	\code{\link{predict.bagging}} }

\examples{
## rpart library should be loaded
library(rpart)
data(iris)
iris.baggingcv <- bagging.cv(Species ~ ., v=2, data=iris, mfinal=3,
control=rpart.control(cp=0.01))
iris.baggingcv[-1]


## rpart and mlbench libraries should be loaded
## Data Vehicle (four classes) 
#This example has been hidden to keep execution time <5s
#data(Vehicle)
#Vehicle.bagging.cv <- bagging.cv(Class ~.,data=Vehicle,v=5,mfinal=10, 
#control=rpart.control(maxdepth=5))
#Vehicle.bagging.cv[-1]

}



\keyword{tree }% at least one, from doc/KEYWORDS
\keyword{classif}
