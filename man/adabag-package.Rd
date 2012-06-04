\name{adabag-package}
\alias{adabag-package}
\alias{adabag}
\docType{package}
\title{
Applies multiclass AdaBoost.M1, AdaBoost-SAMME and Bagging
}
\description{
This package implements Freund and Schapire's Adaboost.M1 algorithm and Breiman's Bagging algorithm 
	using classification trees as individual classifiers. Once these classifiers have been trained, 
	they can be used to predict on new data. Also, cross validation predictions can be done. 
	Since version 2.0 the function "margins" is available to calculate the margins for these classifiers. 
	Also a higher flexibility is achieved giving access to the "rpart.control" argument of "rpart".
	Four important new features were introduced on version 3.0, AdaBoost-SAMME (Zhu et al., 2009) is implemented 
	and a new function "errorevol" shows the error of the ensembles as a function of the number of iterations. 
	In addition, the ensembles can be pruned using the option "newmfinal" in the predict.bagging and predict.boosting
	functions and the posterior probability of each class for observations can be obtained. Version 3.1 modifies 
	the relative importance measure to take into account the gain of the Gini index given by a variable in each tree 
	and the weights of these trees. 
}
\details{
\tabular{ll}{
Package: \tab adabag\cr
Type: \tab Package\cr
Version: \tab 3.1\cr
Date: \tab 2012-07-04\cr
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

\subsection{Reverse cites}{To the best of our knowledge this package has been cited by:


	Chrzanowska, M., Alfaro, E., and Witkowska, D. (2009). The individual borrowers recognition: Single and ensemble trees.
	Expert Systems with Applications, 36(2), pp. 6409--6414.

	De Bock, K. W., Coussement, K., and Van den Poel, D. (2010). Ensemble classification based on generalized additive models. 
	Computational Statistics & Data Analysis, 54(6), pp. 1535--1546.
	
	De Bock, K. W. and Van den Poel, D. (2011). An empirical evaluation of rotation-based ensemble classifiers for customer churn prediction. 
	Expert Systems with Applications, Volume 38, Issue 10, pp. 12293--12301.

	Fan, Y., Murphy, T.B., William, R. and Watson G. (2009). digeR: GUI tool for analyzing 2D DIGE data.
	R package version 1.2.

	Krempl, G. and Hofer, V. (2008). Partitioner trees: combining boosting and arbitrating. 
	In: Okun, O., Valentini, G. (eds.) Proc. 2nd Workshop Supervised and Unsupervised Ensemble Methods and Their Applications, Patras, Greece, pp. 61--66.

	Maindonald, J. and Braun, J. (2010). Data Analysis and Graphics Using R - An Example-Based Approach.  
	3rd ed, Cambridge University Press (p. 373)

	Stewart, B.M. and Zhukov, Y.M. (2009). Use of force and civil-military relations in Russia: An automated content analysis. 
	Small Wars & Insurgencies, 20(2), pp. 319--343.

	Torgo, L.  (2010). Data Mining with R: Learning with Case Studies. Series: Chapman & Hall/CRC Data Mining 
	and Knowledge Discovery.

	If you know any other work where this package is cited, please send us an email

}

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
data(iris)
iris.adaboost <- boosting(Species~., data=iris, boos=TRUE, 
	mfinal=10)

sub <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
iris.bagging <- bagging(Species ~ ., data=iris[sub,], mfinal=10)
iris.predbagging<- predict.bagging(iris.bagging, newdata=iris[-sub,])

}

\keyword{tree }% at least one, from doc/KEYWORDS
\keyword{classif}