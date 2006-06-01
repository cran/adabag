\name{predict.boosting}
\alias{predict.boosting}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Predicts from a fitted Adaboost.M1 object.  }



\description{
  Classifies a dataframe using a fitted adaboost.M1 object.

}
\usage{
\method{predict}{boosting}(object, newdata, ...)

}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{fitted model object of class \code{adaboost.M1}. This is assumed to be the result
	 of some function that produces an object with the same named components as that 
	returned by the \code{adaboost.M1} function. }

  \item{newdata}{data frame containing the values at which predictions are required. The predictors referred 
	to in the right side of \code{formula(object)} must be present by name in newdata.}

  \item{...}{ further arguments passed to or from other methods.}
}



\value{
	An object of class predict.boosting, which is a list with the following components:

  \item{class }{the class predicted by the ensemble classifier.}
  \item{confusion }{the confusion matrix which compares the real class with the predicted one.}
  \item{error }{returns the average error.}
}

\references{Alfaro, E.; Gámez, M. and García, N. (2006): "Multiclass corporate failure prediction by Adaboost.M1". To appear in International Advances in Economic Research.

	Freund, Y. and Schapire, R.E. (1996): "Experiments with a New Boosting Algorithm". En Proceedings of the Thirteenth International Conference on Machine Learning, pp. 148--156, Morgan Kaufmann. 
	
	Breiman, L. (1998): "Arcing classifiers". The Annals of Statistics, Vol 26, 3, pp. 801--849. }
\author{Esteban Alfaro Cortés \email{Esteban.Alfaro@uclm.es}, Matías Gámez Martínez \email{Matias.Gamez@uclm.es} and Noelia García Rubio \email{Noelia.Garcia@uclm.es}} 


% ~Make other sections like Warning with \section{Warning }{....} ~


\seealso{ 
	\code{\link{adaboost.M1}},
	\code{\link{boosting.cv}} }

\examples{
## rpart library should be loaded
library(rpart)
data(iris)
names(iris)<-c("LS","AS","LP","AP","Especies")
sub <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
iris.adaboost <- adaboost.M1(Especies ~ ., data=iris[sub,], mfinal=10)
iris.predboosting<- predict.boosting(iris.adaboost, newdata=iris[-sub,])

## rpart and mlbench libraries should be loaded
## Comparing the test error of rpart and adaboost.M1
library(rpart)
library(mlbench)
data(BreastCancer)
l <- length(BreastCancer[,1])
sub <- sample(1:l,2*l/3)

BC.rpart <- rpart(Class~.,data=BreastCancer[sub,-1], maxdepth=3)
BC.rpart.pred <- predict(BC.rpart,newdata=BreastCancer[-sub,-1],type="class")
tb <-table(BC.rpart.pred,BreastCancer$Class[-sub])
error.rpart <- 1-(sum(diag(tb))/sum(tb))
tb
error.rpart

BC.adaboost <- adaboost.M1(Class ~.,data=BreastCancer[,-1],mfinal=25, maxdepth=3)
BC.adaboost.pred <- predict.boosting(BC.adaboost,newdata=BreastCancer[-sub,-1])
BC.adaboost.pred[-1]

## Data Vehicle (four classes) 
library(rpart)
library(mlbench)
data(Vehicle)
l <- length(Vehicle[,1])
sub <- sample(1:l,2*l/3)
mfinal <- 25
maxdepth <- 5

Vehicle.rpart <- rpart(Class~.,data=Vehicle[sub,],maxdepth=maxdepth)
Vehicle.rpart.pred <- predict(Vehicle.rpart,newdata=Vehicle[-sub, ],type="class")
tb <- table(Vehicle.rpart.pred,Vehicle$Class[-sub])
error.rpart <- 1-(sum(diag(tb))/sum(tb))
tb
error.rpart

Vehicle.adaboost <- adaboost.M1(Class ~.,data=Vehicle[sub, ],mfinal=mfinal, maxdepth=maxdepth)
Vehicle.adaboost.pred <- predict.boosting(Vehicle.adaboost,newdata=Vehicle[-sub, ])
Vehicle.adaboost.pred[-1]


}


\keyword{tree }% at least one, from doc/KEYWORDS
\keyword{classif}
