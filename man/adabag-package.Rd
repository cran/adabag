\name{adabag-package}
\alias{adabag-package}
\alias{adabag}
\docType{package}
\title{
Applies AdaBoost.M1, AdaBoost-SAMME and Bagging
}
\description{
This package implements Freund and Schapire's Adaboost.M1 algorithm and Breiman's Bagging algorithm 
	using classification trees as individual classifiers. Once these classifiers have been trained, 
	they can be used to predict on new data. Also, cross validation predictions can be done. 
	Since version 2.0 a new function "margins" is available to calculate the margins for these classifiers. 
	Also a higher flexibility is achieved giving access to the "rpart.control" argument of "rpart".
	Four important new features have been introduced on version 3.0, AdaBoost-SAMME (Zhu et al., 2009) is implemented 
	and a new function "errorevol" shows the error of the ensembles as a function of the number of iterations. 
	In addition, the ensembles can be pruned using the option "newmfinal" in the predict.bagging and predict.boosting
	functions and the posterior probability of each class for observations can be obtained.
}
\details{
\tabular{ll}{
Package: \tab adabag\cr
Type: \tab Package\cr
Version: \tab 3.0\cr
Date: \tab 2011-12-14\cr
License: \tab GPL(>= 2)\cr
LazyLoad: \tab yes\cr
}
%~~ An overview of how to use the package, including the most important ~~
%~~ functions ~~
}
\author{
Author: Esteban Alfaro-Cortes, Matias Gamez-Martinez and Noelia Garcia-Rubio\cr
Maintainer: Esteban Alfaro-Cortes <Esteban.Alfaro@uclm.es>

}
\references{
  Alfaro, E., Gamez, M. and Garcia, N. (2007): ``Multiclass corporate failure prediction by Adaboost.M1''. International Advances in Economic Research, Vol 13, 3, pp. 301--312.

  Alfaro, E., Garcia, N., Gamez, M. and Elizondo, D. (2008): ``Bankruptcy forecasting: An empirical comparison of AdaBoost and neural networks''. Decision Support Systems, 45, pp. 110--122.

  Breiman, L. (1998): ``Arcing classifiers''. The Annals of Statistics, Vol 26, 3, pp. 801--849.

  Freund, Y. and Schapire, R.E. (1996): ``Experiments with a new boosting algorithm''. In Proceedings of the Thirteenth International Conference on Machine Learning, pp. 148--156, Morgan Kaufmann. 

  Zhu, J., Zou, H., Rosset, S. and Hastie, T. (2009): ``Multi-class AdaBoost''. Statistics and Its Interface, 2, pp. 349--360. 
}

\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
       \code{\link{boosting}},
       \code{\link{predict.boosting}},
       \code{\link{boosting.cv}},
	\code{\link{bagging}},
	\code{\link{predict.bagging}},
	\code{\link{bagging.cv}},
	\code{\link{margins}}
	\code{\link{errorevol}}
}

\examples{
## rpart library should be loaded
library(rpart)
data(iris)
names(iris)<-c("LS","AS","LP","AP","Especies")
iris.adaboost <- boosting(Especies~LS +AS +LP+ AP, data=iris, boos=TRUE, 
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