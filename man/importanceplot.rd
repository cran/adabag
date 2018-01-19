\name{importanceplot}
\alias{importanceplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots the variables relative importance 
}
\description{
Plots the relative importance of each variable in the classification task.
   This measure takes into account the gain of the Gini index given by a variable in a tree and, in the boosting case, the weight of this tree. 
}
\usage{
importanceplot(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
fitted model object of class \code{boosting} or \code{bagging}. This is assumed to be the result
	 of some function that produces an object with a component named \code{importance} as that 
	returned by the \code{boosting} and \code{bagging} functions.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
For this goal, the \code{varImp} function of the \code{caret} package is used to get 
the gain of the Gini index of the variables in each tree.
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
  	\code{\link{bagging}},
}
\examples{
#Examples
#Iris example
library(rpart)
data(iris)
sub <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
iris.adaboost <- boosting(Species ~ ., data=iris[sub,], mfinal=3)
importanceplot(iris.adaboost)

#Examples with bagging
#iris.bagging <- bagging(Species ~ ., data=iris[sub,], mfinal=5)
#importanceplot(iris.bagging, horiz=TRUE, cex.names=.6)

}

\keyword{tree }% at least one, from doc/KEYWORDS
\keyword{classif}
