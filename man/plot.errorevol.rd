\name{plot.errorevol}
\alias{plot.errorevol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots the error evolution of the ensemble
}
\description{
Plots the previously calculated error evolution of an AdaBoost.M1, AdaBoost-SAMME or Bagging classifier for a data frame
as the ensemble size grows  
}
\usage{
\method{plot}{errorevol}(x, y = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class \code{errorevol}. This is assumed to be the result
	 of some function that produces an object with a component named \code{error} as that 
	returned by the \code{errorevol} function. }
  \item{y}{
 This argument can be used to represent in the same plot
the evolution of the test and train errors, \code{x} and \code{y}, respectively.
Should be \code{NULL} (by default) or an object of class \code{errorevol}.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
This can be useful to see how fast \code{bagging} or  \code{boosting} reduce the error of the ensemble. in addition,
it can detect the presence of overfitting and, therefore, the convenience of pruning the ensemble using \code{predict.bagging} or \code{predict.boosting}.
}
\value{
A labeled plot is produced on the current graphics device (one being opened if needed). 
}
\references{
Alfaro, E., Gamez, M. and Garcia, N. (2013): ``adabag: An R Package for Classification with Boosting and Bagging''. Journal of Statistical Software, Vol 54, 2, pp. 1--35.

	Alfaro, E., Garcia, N., Gamez, M. and Elizondo, D. (2008): ``Bankruptcy forecasting: An empirical comparison of AdaBoost and neural networks''. Decision Support Systems, 45, pp. 110--122.

	Breiman, L. (1996): ``Bagging predictors''. Machine Learning, Vol 24, 2, pp.123--140.

	Freund, Y. and Schapire, R.E. (1996): ``Experiments with a new boosting algorithm''. In Proceedings of the Thirteenth International Conference on Machine Learning, pp. 148--156, Morgan Kaufmann. 

	Zhu, J., Zou, H., Rosset, S. and Hastie, T. (2009): ``Multi-class AdaBoost''. Statistics and Its Interface, 2, pp. 349--360. 
}
\author{Esteban Alfaro-Cortes \email{Esteban.Alfaro@uclm.es}, Matias Gamez-Martinez \email{Matias.Gamez@uclm.es} and Noelia Garcia-Rubio \email{Noelia.Garcia@uclm.es} }


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
       \code{\link{boosting}},
       \code{\link{predict.boosting}},
  	\code{\link{bagging}},
	\code{\link{predict.bagging}},
	\code{\link{errorevol}}
}
\examples{
data(iris)
train <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))

cntrl<-rpart.control(maxdepth=1)
#increase mfinal in your own execution of this example to see 
#the real usefulness of this function
iris.adaboost <- boosting(Species ~ ., data=iris[train,], mfinal=10, control=cntrl)

#Error evolution along the iterations in training set 
errorevol(iris.adaboost,iris[train,])->evol.train
plot.errorevol(evol.train)

#comparing error evolution in training and test set
errorevol(iris.adaboost,iris[-train,])->evol.test
plot.errorevol(evol.test, evol.train)

# See the help of the functions error evolution and boosting 
# for more examples of the use of the error evolution

}

\keyword{tree }% at least one, from doc/KEYWORDS
\keyword{classif}
