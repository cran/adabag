\name{boosting}
\alias{boosting}
\alias{adaboost.M1}

\title{ Applies the AdaBoost.M1 and SAMME algorithms to a data set }

\description{ Fits the AdaBoost.M1 (Freund and Schapire, 1996) and SAMME (Zhu et al., 2009) algorithms 
 using classification trees as single classifiers.  }

\usage{
  boosting(formula, data, boos = TRUE, mfinal = 100, coeflearn = 'Breiman', 
	control)
}

\arguments{
  \item{formula}{ a formula, as in the \code{lm} function.  }
  \item{data}{a data frame in which to interpret the variables named in \code{formula}.  }
  \item{boos}{ if \code{TRUE} (by default), a bootstrap sample of the training set is drawn using 
   the weights for each observation on that iteration. If \code{FALSE}, every observation
   is used with its weights.  }
  \item{mfinal}{an integer, the number of iterations for which boosting is run 
   or the number of trees to use. Defaults to \code{mfinal=100} iterations.  }
  \item{coeflearn}{ if 'Breiman'(by default), \code{alpha=1/2ln((1-err)/err)} is used. 
   If 'Freund' \code{alpha=ln((1-err)/err)} is used. In both cases the AdaBoost.M1 algorithm is used
   and \code{alpha} is the weight updating coefficient. On the other hand, if coeflearn is 'Zhu' the SAMME algorithm 
   is implemented with \code{alpha=ln((1-err)/err)+ln(nclasses-1)}}
  \item{control}{options that control details of the rpart algorithm. See rpart.control for more details.  }

}

\details{
  AdaBoost.M1 and SAMME are simple generalizations of AdaBoost for more than two classes. In AdaBoost-SAMME 
  the individual trees are required to have an error lower than 1-1/nclasses instead of 1/2 of the AdaBoost.M1
  
}

\value{
   An object of class \code{boosting}, which is a list with the following components:  
  \item{formula}{the formula used.  }
  \item{trees}{the trees grown along the iterations.  }
  \item{weights}{a vector with the weighting of the trees of all iterations.  }
  \item{votes}{a matrix describing, for each observation, the number of trees that assigned it to each class, weighting each tree by its \code{alpha} coefficient.  }
  \item{prob}{a matrix describing, for each observation, the posterior probability of each class. 
	These probabilities are calculated using the proportion of votes in the final ensemble.}
  \item{class}{the class predicted by the ensemble classifier.  }
  \item{importance}{returns the relative importance of each variable in the classification task.
   This measure is the number of times each variable is selected to split.  }
}

\references{Alfaro, E., Gamez, M. and Garcia, N. (2007): ``Multiclass corporate failure prediction by Adaboost.M1''. International Advances in Economic Research, Vol 13, 3, pp. 301--312.

  Alfaro, E., Garcia, N., Gamez, M. and Elizondo, D. (2008): ``Bankruptcy forecasting: An empirical comparison of AdaBoost and neural networks''. Decision Support Systems, 45, pp. 110--122.

  Breiman, L. (1998): ``Arcing classifiers''. The Annals of Statistics, Vol 26, 3, pp. 801--849.

  Freund, Y. and Schapire, R.E. (1996): ``Experiments with a new boosting algorithm''. In Proceedings of the Thirteenth International Conference on Machine Learning, pp. 148--156, Morgan Kaufmann. 

  Zhu, J., Zou, H., Rosset, S. and Hastie, T. (2009): ``Multi-class AdaBoost''. Statistics and Its Interface, 2, pp. 349--360. 

 }

\author{Esteban Alfaro-Cortes \email{Esteban.Alfaro@uclm.es}, Matias Gamez-Martinez \email{Matias.Gamez@uclm.es} and Noelia Garcia-Rubio \email{Noelia.Garcia@uclm.es} }


\seealso{ 
       \code{\link{predict.boosting}},
       \code{\link{boosting.cv}} }

\examples{

## rpart library should be loaded
library(rpart)
data(iris)
names(iris)<-c("LS","AS","LP","AP","Especies")
iris.adaboost <- boosting(Especies~LS +AS +LP+ AP, data=iris, boos=TRUE, 
	mfinal=10)

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

BC.adaboost <- boosting(Class ~.,data=BreastCancer[,-1],mfinal=25, coeflearn="Freund", boos=FALSE , control=rpart.control(maxdepth=3))
BC.adaboost.pred <- predict.boosting(BC.adaboost,newdata=BreastCancer[-sub,-1])
BC.adaboost.pred$confusion
BC.adaboost.pred$error


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

Vehicle.adaboost <- boosting(Class ~.,data=Vehicle[sub, ],mfinal=mfinal, coeflearn="Zhu",
	control=rpart.control(maxdepth=maxdepth))
Vehicle.adaboost.pred <- predict.boosting(Vehicle.adaboost,newdata=Vehicle[-sub, ])
Vehicle.adaboost.pred$confusion
Vehicle.adaboost.pred$error


}


\keyword{tree }% at least one, from doc/KEYWORDS
\keyword{classif}