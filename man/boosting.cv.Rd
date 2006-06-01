\name{boosting.cv}
\alias{boosting.cv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Runs v-fold cross validation with adaboost.M1 }

\description{
	The data are divided into \code{v} non-overlapping subsets of roughly equal size. Then, \code{adaboost.M1}
	is applied on \code{(v-1)} of the subsets. Finally, predictions are made for the left out subsets,
	and the process is repeated for each of the \code{v} subsets.
}

\usage{
boosting.cv(formula, data, v = 10, boos = TRUE, mfinal = 100, coeflearn = "Breiman", minsplit = 5, cp = 0.01, maxdepth = nlevels(vardep))
}
%- maybe also 'usage' for other objects documented here.
\arguments{  

  \item{formula}{ a formula, as in the \code{lm} function.  }
  \item{data}{a data frame in which to interpret the variables named in \code{formula} }
  \item{boos}{ if \code{TRUE} (by default), a bootstrap sample of the training set is drawn using 
	the weights for each observation on that iteration. If \code{FALSE}, every observation
	is used with its weights. }
  \item{v}{An integer, specifying the type of v-fold cross validation. Defaults to 10.
	If \code{v} is set as the number of observations, leave-one-out cross validation is carried out. 
	Besides this, every value between two and the number of observations is valid and means
	that roughly every v-th observation is left out.}
  \item{mfinal}{an integer, the number of iterations for which boosting is run 
	or the number of trees to use. Defaults to \code{mfinal=100} iterations.}
  \item{coeflearn}{ if "Breiman"(by default), \code{alpha=1/2ln((1-err)/err)} is used. 
	If "Freund" \code{alpha=ln((1-err)/err)} is used. Where \code{alpha} is the weight updating coefficient. }
  \item{minsplit}{the minimum number of observations that must exist in a node, in order for a split to be attempted.}
  \item{cp}{complexity parameter. Any split that does not decrease the overall 
	lack of fit by a factor of \code{cp} is not attempted. }
  \item{maxdepth}{set the maximum depth of any node of the final tree, with the root node counted as depth 0 
	(past 30 rpart will give nonsense results on 32-bit machines).  Defaults to the number of classes.}

}


\value{
   An object of class \code{boosting.cv}, which is a list with the following components:
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
	\code{\link{predict.boosting}} }


\examples{
## rpart library should be loaded
library(rpart)
data(iris)
names(iris)<-c("LS","AS","LP","AP","Especies")
iris.boostcv <- boosting.cv(Especies ~ ., v=10, data=iris, mfinal=10, maxdepth=3)

data(kyphosis)
kyphosis.boostcv <- boosting.cv(Kyphosis ~ Age + Number + Start, data=kyphosis, mfinal=15)

## rpart and mlbench libraries should be loaded
## Data Vehicle (four classes) 
library(rpart)
library(mlbench)
data(Vehicle)
Vehicle.boost.cv <- boosting.cv(Class ~.,data=Vehicle,mfinal=25, maxdepth=5)
Vehicle.boost.cv[-1]

}




\keyword{tree }% at least one, from doc/KEYWORDS
\keyword{classif}
