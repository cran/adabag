\name{adabag-package}
\alias{adabag-package}
\alias{adabag}
\docType{package}
\title{
Applies Adaboost.M1 and Bagging
}
\description{
This package implements Freund and Schapire's Adaboost.M1 algorithm and Breiman's Bagging algorithm 
	using classification trees as individual classifiers. Once these classifiers have been trained, 
	they can be used to predict on new data. Also, cross validation predictions can be done. 
	This version 2.0 adds a new function "margins" to calculate the margins for these classifiers. 
	Also a higher flexibility is achieved giving access to the "rpart.control" argument of "rpart".
}
\details{
\tabular{ll}{
Package: \tab adabag\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2011-07-08\cr
License: \tab GPL(>= 2)\cr
LazyLoad: \tab yes\cr
}
%~~ An overview of how to use the package, including the most important ~~
%~~ functions ~~
}
\author{
Author: Esteban Alfaro Cortes, Matias Gamez Martinez and Noelia Garcia Rubio\cr
Maintainer: Esteban Alfaro Cortes <Esteban.Alfaro@uclm.es>

}
\references{
Alfaro, E., Gamez, M. and Garcia, N. (2007): ``Multiclass corporate failure prediction by Adaboost.M1''. International Advances in Economic Research, Vol 13, 3, pp. 301--312.

  Freund, Y. and Schapire, R.E. (1996): ``Experiments with a new boosting algorithm''. In Proceedings of the Thirteenth International Conference on Machine Learning, pp. 148--156, Morgan Kaufmann. 

  Breiman, L. (1998): ``Arcing classifiers''. The Annals of Statistics, Vol 26, 3, pp. 801--849.
}
\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
       \code{\link{adaboost.M1}},
       \code{\link{predict.boosting}},
       \code{\link{boosting.cv}},
	\code{\link{bagging}},
	\code{\link{predict.bagging}},
	\code{\link{bagging.cv}},
	\code{\link{margins}}
}

\examples{
## rpart library should be loaded
library(rpart)
data(iris)
names(iris)<-c("LS","AS","LP","AP","Especies")
iris.adaboost <- adaboost.M1(Especies~LS +AS +LP+ AP, data=iris, boos=TRUE, 
	mfinal=10)

library(rpart)
data(iris)
names(iris)<-c("LS","AS","LP","AP","Especies")
sub <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
iris.bagging <- bagging(Especies ~ ., data=iris[sub,], mfinal=10)
iris.predbagging<- predict.bagging(iris.bagging, newdata=iris[-sub,])

}

\keyword{tree }% at least one, from doc/KEYWORDS
\keyword{classif}