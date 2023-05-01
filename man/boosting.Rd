\name{boosting}
\alias{boosting}
\alias{adaboost.M1}

\title{ Applies the AdaBoost.M1 and SAMME algorithms to a data set }

\description{ Fits the AdaBoost.M1 (Freund and Schapire, 1996) and SAMME (Zhu et al., 2009) algorithms 
 using classification trees as single classifiers.  }

\usage{
  boosting(formula, data, boos = TRUE, mfinal = 100, coeflearn = 'Breiman', 
	control,...)
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
   is implemented with \code{alpha=ln((1-err)/err)+} \code{ln(nclasses-1)}.}
  \item{control}{options that control details of the rpart algorithm. See rpart.control for more details.  }
  \item{...}{ further arguments passed to or from other methods.}

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
  \item{prob}{a matrix describing, for each observation, the posterior probability or degree of support of each class. 
	These probabilities are calculated using the proportion of votes in the final ensemble.}
  \item{class}{the class predicted by the ensemble classifier.  }
  \item{importance}{returns the relative importance of each variable in the classification task.
   This measure takes into account the gain of the Gini index given by a variable in a tree and the weight of this tree.  }
}

\references{Alfaro, E., Gamez, M. and Garcia, N. (2013): ``adabag: An R Package for Classification with Boosting and Bagging''. Journal of Statistical Software, Vol 54, 2, pp. 1--35.

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
data(iris)
iris.adaboost <- boosting(Species~., data=iris, boos=TRUE, mfinal=3)
iris.adaboost


## Data Vehicle (four classes) 
library(mlbench)
data(Vehicle)
l <- length(Vehicle[,1])
sub <- sample(1:l,2*l/3)
mfinal <- 3 
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

#comparing error evolution in training and test set
errorevol(Vehicle.adaboost,newdata=Vehicle[sub, ])->evol.train
errorevol(Vehicle.adaboost,newdata=Vehicle[-sub, ])->evol.test

plot.errorevol(evol.test,evol.train)


}


\keyword{tree }% at least one, from doc/KEYWORDS
\keyword{classif}
