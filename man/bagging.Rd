\name{bagging}
\alias{bagging}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Applies the Bagging algorithm to a data set }

\description{
	Fits the Bagging algorithm proposed by Breiman 
	in 1996 using classification trees as single classifiers.
}
\usage{
bagging(formula, data, mfinal = 100, control, par=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{formula}{ a formula, as in the \code{lm} function.  }
  \item{data}{a data frame in which to interpret the variables named in the \code{formula} }
  \item{mfinal}{an integer, the number of iterations for which boosting is run 
	or the number of trees to use. Defaults to \code{mfinal=100} iterations.}
  \item{control}{options that control details of the rpart algorithm. See rpart.control for more details.  }
  \item{par}{ if \code{TRUE}, the cross validation process is runned in parallel. If \code{FALSE} (by default),
 the function runs without parallelization.  }
  \item{...}{ further arguments passed to or from other methods.}
}
\details{
  Unlike boosting, individual classifiers are independent among them in bagging

}
\value{

  An object of class \code{bagging}, which is a list with the following components:
  \item{formula }{the formula used.}
  \item{trees }{the trees grown along the iterations.}
  \item{votes }{a matrix describing, for each observation, the number of trees that assigned it to each class.}
  \item{prob }{a matrix describing, for each observation, the posterior probability or degree of support of each class. 
	These probabilities are calculated using the proportion of votes in the final ensemble.}
  \item{class }{the class predicted by the ensemble classifier.}
  \item{samples }{ the bootstrap samples used along the iterations.}
  \item{importance }{returns the relative importance of each variable in the classification task.
   This measure takes into account the gain of the Gini index given by a variable in each tree.}

}
\references{Alfaro, E., Gamez, M. and Garcia, N. (2013): ``\code{adabag}: An R Package for Classification with Boosting and Bagging''. Journal of Statistical Software, Vol 54, 2, pp. 1--35.

	Alfaro, E., Garcia, N., Gamez, M. and Elizondo, D. (2008): ``Bankruptcy forecasting: An empirical comparison of AdaBoost and neural networks''. Decision Support Systems, 45, pp. 110--122.

	Breiman, L. (1996): "Bagging predictors". Machine Learning, Vol 24, 2, pp.123--140.

	Breiman, L. (1998): "Arcing classifiers". The Annals of Statistics, Vol 26, 3, pp. 801--849. }

\author{Esteban Alfaro-Cortes \email{Esteban.Alfaro@uclm.es}, Matias Gamez-Martinez \email{Matias.Gamez@uclm.es} and Noelia Garcia-Rubio \email{Noelia.Garcia@uclm.es} }



\seealso{ 
	\code{\link{predict.bagging}},
	\code{\link{bagging.cv}} }

\examples{
## rpart library should be loaded
#This example has been hidden to fulfill execution time <5s
#library(rpart)
#data(iris)
#iris.bagging <- bagging(Species~., data=iris, mfinal=10)

# Data Vehicle (four classes)
library(rpart)
library(mlbench)
data(Vehicle)
l <- length(Vehicle[,1])
sub <- sample(1:l,2*l/3)
Vehicle.bagging <- bagging(Class ~.,data=Vehicle[sub, ],mfinal=5, 
	control=rpart.control(maxdepth=5, minsplit=15))
#Using the pruning option
Vehicle.bagging.pred <- predict.bagging(Vehicle.bagging,newdata=Vehicle[-sub, ], newmfinal=3)
Vehicle.bagging.pred$confusion
Vehicle.bagging.pred$error


}


\keyword{tree }% at least one, from doc/KEYWORDS
\keyword{classif}
