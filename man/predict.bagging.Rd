\name{predict.bagging}
\alias{predict.bagging}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Predicts from a fitted bagging object.}

\description{
  Classifies a dataframe using a fitted bagging object.
}
\usage{
\method{predict}{bagging}(object, newdata, ...)}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{object}{fitted model object of class \code{bagging}. This is assumed to be the result
	 of some function that produces an object with the same named components as that 
	returned by the \code{bagging} function. }

  \item{newdata}{data frame containing the values at which predictions are required. The predictors referred 
	to in the right side of \code{formula(object)} must be present by name in \code{newdata}.}

  \item{...}{ further arguments passed to or from other methods.}
}


\value{
   An object of class \code{predict.bagging}, which is a list with the following components:

  \item{class }{the class predicted by the ensemble classifier.}
  \item{confusion }{the confusion matrix which compares the real class with the predicted one.}
  \item{error }{returns the average error.}

}
\references{Alfaro, E., Gamez, M. and Garcia, N. (2007): ``Multiclass corporate failure prediction by Adaboost.M1''. International Advances in Economic Research, Vol 13, 3, pp. 301--312.

Breiman, L. (1996): "Bagging predictors". Machine Learning, Vol 24, 2, pp. 123--140.

Breiman, L. (1998). "Arcing classifiers". The Annals of Statistics, Vol 26, 3, pp. 801--849. }

\author{Esteban Alfaro Cortes \email{Esteban.Alfaro@uclm.es}, Matias Gamez Martinez \email{Matias.Gamez@uclm.es} and Noelia Garcia Rubio \email{Noelia.Garcia@uclm.es} }


 %~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ 
	\code{\link{bagging}},
	\code{\link{bagging.cv}} }

\examples{
library(rpart)
data(iris)
names(iris)<-c("LS","AS","LP","AP","Especies")
sub <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
iris.bagging <- bagging(Especies ~ ., data=iris[sub,], mfinal=10)
iris.predbagging<- predict.bagging(iris.bagging, newdata=iris[-sub,])

## rpart and mlbench libraries should be loaded
library(rpart)
library(mlbench)
data(BreastCancer)
l <- length(BreastCancer[,1])
sub <- sample(1:l,2*l/3)
BC.bagging <- bagging(Class ~.,data=BreastCancer[,-1],mfinal=25, maxdepth=3)
BC.bagging.pred <- predict.bagging(BC.bagging,newdata=BreastCancer[-sub,-1])
BC.bagging.pred[-1]

# Data Vehicle (four classes)
library(rpart)
library(mlbench)
data(Vehicle)
l <- length(Vehicle[,1])
sub <- sample(1:l,2*l/3)
Vehicle.bagging <- bagging(Class ~.,data=Vehicle[sub, ],mfinal=50, maxdepth=5)
Vehicle.bagging.pred <- predict.bagging(Vehicle.bagging,newdata=Vehicle[-sub, ])
Vehicle.bagging.pred[-1]

}


\keyword{tree }% at least one, from doc/KEYWORDS
\keyword{classif}
