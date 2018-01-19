\name{margins}
\alias{margins}

\title{ Calculates the margins }

\description{ Calculates the margins of an AdaBoost.M1, AdaBoost-SAMME or Bagging classifier for a data frame}


\usage{ margins(object, newdata)
}

\arguments{

	\item{object}{This object must be the output of one of the functions \code{bagging}, \code{boosting}, \code{predict.bagging} or \code{predict.boosting}.
	This is assumed to be the result of some function that produces an object with two components named \code{formula}  and \code{class},  as those 
	returned for instance by the \code{bagging} function. }

	\item{newdata}{ The same data frame used for building the \code{object}}

}

\details{
Intuitively, the margin for an observation is related to the certainty of its classification. It is calculated as the difference between
the support of the correct class and the maximum support of an incorrect class}

\value{
   An object of class \code{margins}, which is a list with only one component:  

	\item{margins}{a vector with the margins. }
 
}

\references{Alfaro, E., Gamez, M. and Garcia, N. (2013): ``adabag: An R Package for Classification with Boosting and Bagging''. Journal of Statistical Software, Vol 54, 2, pp. 1--35.

  Alfaro, E., Garcia, N., Gamez, M. and Elizondo, D. (2008): ``Bankruptcy forecasting: An empirical comparison of AdaBoost and neural networks''. Decision Support Systems, 45, pp. 110--122.

  Schapire, R.E., Freund, Y., Bartlett, P. and Lee, W.S. (1998): ``Boosting the margin: A new explanation for the effectiveness of voting methods''. The Annals of Statistics, vol 26, 5, pp. 1651--1686.
}

\author{Esteban Alfaro-Cortes \email{Esteban.Alfaro@uclm.es}, Matias Gamez-Martinez \email{Matias.Gamez@uclm.es} and Noelia Garcia-Rubio \email{Noelia.Garcia@uclm.es} }


\seealso{ 
	\code{\link{bagging}},
	\code{\link{boosting}},
	\code{\link{plot.margins}},
	\code{\link{predict.boosting}},
	\code{\link{predict.bagging}}
}

\examples{

#Iris example
library(rpart)
data(iris)
sub <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
iris.adaboost <- boosting(Species ~ ., data=iris[sub,], mfinal=3)
margins(iris.adaboost,iris[sub,])->iris.margins # training set
plot.margins(iris.margins)

# test set
iris.predboosting<- predict.boosting(iris.adaboost, newdata=iris[-sub,])
margins(iris.predboosting,iris[-sub,])->iris.predmargins 
plot.margins(iris.predmargins,iris.margins)

#Examples with bagging
iris.bagging <- bagging(Species ~ ., data=iris[sub,], mfinal=3)
margins(iris.bagging,iris[sub,])->iris.bagging.margins # training set

iris.predbagging<- predict.bagging(iris.bagging, newdata=iris[-sub,])
margins(iris.predbagging,iris[-sub,])->iris.bagging.predmargins # test set
par(bg="lightyellow")
plot.margins(iris.bagging.predmargins,iris.bagging.margins)


}


\keyword{tree }% at least one, from doc/KEYWORDS
\keyword{classif}