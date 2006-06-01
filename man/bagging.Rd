\name{bagging}
\alias{bagging}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Applies the Bagging algorithm to a data set. }

\description{
	Fits the Bagging algorithm proposed by Breiman 
	in 1996 using classification trees as single classifiers.
}
\usage{
bagging(formula, data, mfinal = 100, minsplit = 5, cp = 0.01, maxdepth = nlevels(vardep))
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{formula}{ a formula, as in the \code{lm} function.  }
  \item{data}{a data frame in which to interpret the variables named in the \code{formula} }
  \item{mfinal}{an integer, the number of iterations for which boosting is run 
	or the number of trees to use. Defaults to \code{mfinal=100} iterations.}
  \item{minsplit}{the minimum number of observations that must exist in a node, in order for a split to be attempted.}
  \item{cp}{complexity parameter. Any split that does not decrease the overall 
	lack of fit by a factor of \code{cp} is not attempted. }
  \item{maxdepth}{set the maximum depth of any node of the final tree, with the root node counted as depth 0 
	(past 30 rpart will give nonsense results on 32-bit machines).  Defaults to the number of classes.}

}
\details{
  Unlike boosting, individual classifiers are independent among them in bagging

}
\value{

  An object of class \code{bagging}, which is a list with the following components:
  \item{formula }{the formula used.}
  \item{trees }{the trees grown along the iterations.}
  \item{votes }{a matrix describing, for each observation, the number of trees that assigned it to each class.}
  \item{class }{the class predicted by the ensemble classifier.}
  \item{samples }{ the bootstrap samples used along the iterations.}
  \item{importance }{returns the relative importance of each variable in the classification task.
	This measure is the number of times each variable is selected to split.}

}
\references{Alfaro, E.; Gámez, M. and García, N. (2006): "Multiclass corporate failure prediction by Adaboost.M1". To appear in International Advances in Economic Research.

	Breiman, L. (1996): "Bagging predictors". Machine Learning, Vol 24, 2, pp.123--140.

	Breiman, L. (1998). "Arcing classifiers". The Annals of Statistics, Vol 26, 3, pp. 801--849. }
\author{Esteban Alfaro Cortés \email{Esteban.Alfaro@uclm.es}, Matías Gámez Martínez \email{Matias.Gamez@uclm.es} and Noelia García Rubio \email{Noelia.Garcia@uclm.es}} 



\seealso{ 
	\code{\link{predict.bagging}},
	\code{\link{bagging.cv}} }

\examples{
## rpart library should be loaded
library(rpart)
data(iris)
names(iris)<-c("LS","AS","LP","AP","Especies")
lirios.bagging <- bagging(Especies~LS +AS +LP+ AP, data=iris, mfinal=10)

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
