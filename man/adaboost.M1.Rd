\name{adaboost.M1}
\alias{adaboost.M1}

\title{ Applies the Adaboost.M1 algorithm to a data set }

\description{ Fits the Adaboost.M1 algorithm proposed by Freund and Schapire 
  in 1996 using classification trees as single classifiers.  }

\usage{
  adaboost.M1(formula, data, boos = TRUE, mfinal = 100, coeflearn = 'Breiman', minsplit = 5, cp = 0.01, maxdepth = nlevels(vardep))
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
   If 'Freund' \code{alpha=ln((1-err)/err)} is used. Where \code{alpha} is the weight updating coefficient.  }
  \item{minsplit}{the minimum number of observations that must exist in a node in order for a split to be attempted.  }
  \item{cp}{complexity parameter. Any split that does not decrease the overall 
   lack of fit by a factor of \code{cp} is not attempted.  }
  \item{maxdepth}{set the maximum depth of any node of the final tree, with the root node counted as depth 0 
   (past 30 rpart will give nonsense results on 32-bit machines).  Defaults to the number of classes.  }
}

\details{
  Adaboost.M1 is a simple generalization of Adaboost for more than two classes  
}

\value{
   An object of class \code{adaboost.M1}, which is a list with the following components:  
  \item{formula}{the formula used.  }
  \item{trees}{the trees grown along the iterations.  }
  \item{weights}{a vector with the weighting of the trees of all iterations.  }
  \item{votes}{a matrix describing, for each observation, the number of trees that assigned it to each class, weighting each tree by its \code{alpha} coefficient.  }
  \item{class}{the class predicted by the ensemble classifier.  }
  \item{importance}{returns the relative importance of each variable in the classification task.
   This measure is the number of times each variable is selected to split.  }
}

\references{Alfaro, E., Gamez, M. and Garcia, N. (2006): ``Multiclass corporate failure prediction by Adaboost.M1''. To appear in International Advances in Economic Research.

  Freund, Y. and Schapire, R.E. (1996): ``Experiments with a New Boosting Algorithm''. In Proceedings of the Thirteenth International Conference on Machine Learning, pp. 148--156, Morgan Kaufmann. 

  Breiman, L. (1998): ``Arcing classifiers''. The Annals of Statistics, Vol 26, 3, pp. 801--849. }

\author{Esteban Alfaro Cortés \email{Esteban.Alfaro@uclm.es}, Matías Gámez Martínez \email{Matias.Gamez@uclm.es} and Noelia García Rubio \email{Noelia.Garcia@uclm.es} }


\seealso{ 
       \code{\link{predict.boosting}},
       \code{\link{boosting.cv}} }

\examples{

## rpart library should be loaded
library(rpart)
data(iris)
names(iris)<-c("LS","AS","LP","AP","Especies")
iris.adaboost <- adaboost.M1(Especies~LS +AS +LP+ AP, data=iris, boos=TRUE, mfinal=10)

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
