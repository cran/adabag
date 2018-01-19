\name{predict.boosting}
\alias{predict.boosting}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Predicts from a fitted boosting object }



\description{
  Classifies a dataframe using a fitted boosting object.

}
\usage{
\method{predict}{boosting}(object, newdata, newmfinal=length(object$trees), ...)

}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{fitted model object of class \code{boosting}. This is assumed to be the result
	 of some function that produces an object with the same named components as that 
	returned by the \code{boosting} function. }

  \item{newdata}{data frame containing the values at which predictions are required. The predictors referred 
	to in the right side of \code{formula(object)} must be present by name in newdata.}

  \item{newmfinal}{The number of trees of the boosting object to be used in the prediction.
	This argument allows the user to prune the ensemble. By default all the trees 
	in \code{object} are used }

  \item{...}{ further arguments passed to or from other methods.}
}



\value{
	An object of class predict.boosting, which is a list with the following components:

  \item{formula}{the formula used.  }
  \item{votes}{a matrix describing, for each observation, the number of trees that assigned it to each class, weighting each tree by its \code{alpha} coefficient.  }
  \item{prob }{a matrix describing, for each observation, the posterior probability or degree of support of each class. 
	These probabilities are calculated using the proportion of votes in the final ensemble.}
  \item{class}{the class predicted by the ensemble classifier.  }
  \item{confusion }{the confusion matrix which compares the real class with the predicted one.}
  \item{error }{returns the average error.}
}

\references{Alfaro, E., Gamez, M. and Garcia, N. (2013): ``adabag: An R Package for Classification with Boosting and Bagging''. Journal of Statistical Software, Vol 54, 2, pp. 1--35.

	Alfaro, E., Garcia, N., Gamez, M. and Elizondo, D. (2008): ``Bankruptcy forecasting: An empirical comparison of AdaBoost and neural networks''. Decision Support Systems, 45, pp. 110--122.

	Breiman, L. (1998): "Arcing classifiers". The Annals of Statistics, Vol 26, 3, pp. 801--849. 

	Freund, Y. and Schapire, R.E. (1996): "Experiments with a new boosting algorithm". En Proceedings of the Thirteenth International Conference on Machine Learning, pp. 148--156, Morgan Kaufmann. 
	
	Zhu, J., Zou, H., Rosset, S. and Hastie, T. (2009): ``Multi-class AdaBoost''. Statistics and Its Interface, 2, pp. 349--360. 
}


\author{Esteban Alfaro-Cortes \email{Esteban.Alfaro@uclm.es}, Matias Gamez-Martinez \email{Matias.Gamez@uclm.es} and Noelia Garcia-Rubio \email{Noelia.Garcia@uclm.es} }


% ~Make other sections like Warning with \section{Warning }{....} ~


\seealso{ 
	\code{\link{boosting}},
	\code{\link{boosting.cv}} }

\examples{
## rpart library should be loaded
#This example has been hidden to fulfill execution time <5s 
#library(rpart)
#data(iris)
#sub <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
#iris.adaboost <- boosting(Species ~ ., data=iris[sub,], mfinal=10)
#iris.predboosting<- predict.boosting(iris.adaboost, newdata=iris[-sub,])
#iris.predboosting$prob

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

BC.adaboost <- boosting(Class ~.,data=BreastCancer[,-1],mfinal=10, coeflearn="Freund", 
boos=FALSE , control=rpart.control(maxdepth=3))

#Using the pruning option
BC.adaboost.pred <- predict.boosting(BC.adaboost,newdata=BreastCancer[-sub,-1], newmfinal=10)
BC.adaboost.pred$confusion
BC.adaboost.pred$error




}


\keyword{tree }% at least one, from doc/KEYWORDS
\keyword{classif}
