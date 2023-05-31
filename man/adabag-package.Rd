\name{adabag-package}
\alias{adabag-package}
\alias{adabag}
\docType{package}
\title{
 Applies Multiclass AdaBoost.M1, SAMME and Bagging
}
\description{

It implements Freund and Schapire's Adaboost.M1 algorithm and Breiman's Bagging
algorithm using classification trees as individual classifiers. Once these classifiers have been trained, they can be used to predict on new data. Also, cross validation estimation of the error can 	be done. Since version 2.0 the function margins() is available to calculate the margins for these classifiers. Also a higher flexibility is achieved giving access to the rpart.control() argument of 'rpart'. Four important new features were introduced on version 3.0, AdaBoost-SAMME (Zhu et al., 2009) is implemented and a new function errorevol() shows the error of the ensembles as a function of the number of iterations. In addition, the ensembles can be pruned using the option 'newmfinal' in the predict.bagging() and predict.boosting() functions and the posterior probability of
each class for observations can be obtained. Version 3.1 modifies the relative importance measure to take into account the gain of the Gini index given by a variable in each tree and the weights of these trees. Version 4.0 includes the margin-based ordered aggregation for Bagging pruning (Guo and Boukir, 2013) and a function to auto prune the 'rpart' tree. Moreover, three new plots are also
available importanceplot(), plot.errorevol() and plot.margins(). Version 4.1 allows to predict on unlabeled data. Version 4.2 includes the parallel computation option for some of the functions. Version 5.0 includes the Boosting and Bagging algorithms for label ranking (Albano, Sciandra and Plaia, 2023).

}
\details{
\tabular{ll}{
Package: \tab adabag\cr
Type: \tab Package\cr
Version: \tab 5.0\cr
Date: \tab 2023-05-4\cr
License: \tab GPL(>= 2)\cr
LazyLoad: \tab yes\cr
}
%~~ An overview of how to use the package, including the most important ~~
%~~ functions ~~
}
\author{
Author: Esteban Alfaro-Cortes, Matias Gamez-Martinez and Noelia Garcia-Rubio,\cr
with contributions from L. Guo, A. Albano, M. Sciandra and A. Plaia \cr
Maintainer: Esteban Alfaro-Cortes <Esteban.Alfaro@uclm.es>
}

\references{
  Albano, A., Sciandra, M., and Plaia, A. (2023): ``A weighted distance-based approach with boosted decision trees for label ranking''. Expert Systems with Applications.


  Alfaro, E., Gamez, M. and Garcia, N. (2013): ``adabag: An R Package for Classification with Boosting and Bagging''. Journal of Statistical Software, 54(2), 1--35.

  Alfaro, E., Garcia, N., Gamez, M. and Elizondo, D. (2008): ``Bankruptcy forecasting: An empirical comparison of AdaBoost and neural networks''. Decision Support Systems, 45, 110--122.

  Breiman, L. (1998): ``Arcing classifiers''. The Annals of Statistics, 26(3), 801--849.

  Freund, Y. and Schapire, R.E. (1996): ``Experiments with a new boosting algorithm''. In Proceedings of the Thirteenth International Conference on Machine Learning, 148--156, Morgan Kaufmann.

  Guo, L. and Boukir, S. (2013): "Margin-based ordered aggregation for ensemble pruning". Pattern Recognition Letters, 34(6), 603-609.

  Zhu, J., Zou, H., Rosset, S. and Hastie, T. (2009): ``Multi-class AdaBoost''. Statistics and Its Interface, 2, 349--360.

\subsection{Reverse cites}{To the best of our knowledge this package has been cited by:

	Andriyas, S. and McKee, M. (2013). Recursive partitioning techniques for modeling irrigation behavior.
	Environmental Modelling & Software, 47, 207--217.

	Chan, J. C. W. and Paelinckx, D. (2008). Evaluation of Random Forest and Adaboost tree-based ensemble classification and spectral band selection for ecotope mapping using airborne hyperspectral imagery.
	Remote Sensing of Environment, 112(6), 2999--3011.

	Chrzanowska, M., Alfaro, E., and Witkowska, D. (2009). The individual borrowers recognition: Single and ensemble trees.
	Expert Systems with Applications, 36(2), 6409--6414.

	De Bock, K. W., Coussement, K., and Van den Poel, D. (2010). Ensemble classification based on generalized additive models.
	Computational Statistics & Data Analysis, 54(6), 1535--1546.

	De Bock, K. W. and Van den Poel, D. (2011). An empirical evaluation of rotation-based ensemble classifiers for customer churn prediction.
	Expert Systems with Applications, 38(10), 12293--12301.

	Fan, Y., Murphy, T.B., William, R. and Watson G. (2009). digeR: GUI tool for analyzing 2D DIGE data.
	R package version 1.2.

	Garcia-Perez-de-Lema, D., Alfaro-Cortes, E., Manzaneque-Lizano, M. and Banegas-Ochovo, R. (2012). Strategy, competitive factors and performance in small and medium enterprise (SMEs).
	African Journal of Business Management, 6(26), 7714--7726.

	Gonzalez-Rufino, E., Carrion, P., Cernadas, E., Fernandez-Delgado, M. and Dominguez-Petit, R. (2013). Exhaustive comparison of colour texture
	features and classification methods to discriminate cells categories in histological images of fish ovary. Pattern Recognition, 46, 2391--2407.

	Krempl, G. and Hofer, V. (2008). Partitioner trees: combining boosting and arbitrating.
	In: Okun, O., Valentini, G. (eds.) Proc. 2nd Workshop Supervised and Unsupervised Ensemble Methods and Their Applications, Patras, Greece, 61--66.

	Maindonald, J. and Braun, J. (2010). Data Analysis and Graphics Using R - An Example-Based Approach.
	3rd ed, Cambridge University Press (p. 373)

	Murphy, T. B., Dean, N. and Raftery, A. E. (2010). Variable selection and updating in model-based discriminant analysis for high dimensional data with food authenticity applications.
	The annals of applied statistics, 4(1), 396--421.

	Stewart, B.M. and Zhukov, Y.M. (2009). Use of force and civil-military relations in Russia: An automated content analysis.
	Small Wars & Insurgencies, 20(2), 319--343.

	Torgo, L.  (2010). Data Mining with R: Learning with Case Studies. Series: Chapman & Hall/CRC Data Mining
	and Knowledge Discovery.

	If you know any other work where this package is cited, please send us an email

}

}



\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~

	\code{\link{autoprune}},
	\code{\link{bagging}},
	\code{\link{bagging.cv}},
	\code{\link{boosting}},
	\code{\link{boosting.cv}},
	\code{\link{errorevol}},
	\code{\link{importanceplot}},
	\code{\link{margins}},
	\code{\link{MarginOrderedPruning.Bagging}},
	\code{\link{plot.errorevol}},
	\code{\link{plot.margins}},
	\code{\link{predict.bagging}},
	\code{\link{predict.boosting}},
	\code{\link{Ensemble_ranking_IW}},
	\code{\link{errorevol_ranking_vector_IW}},
	\code{\link{prep_data}}
}

\examples{
## rpart library should be loaded
data(iris)
iris.adaboost <- boosting(Species~., data=iris, boos=TRUE,
	mfinal=3)
importanceplot(iris.adaboost)

sub <- c(sample(1:50, 35), sample(51:100, 35), sample(101:150, 35))
iris.bagging <- bagging(Species ~ ., data=iris[sub,], mfinal=3)
#Predicting with labeled data
iris.predbagging<-predict.bagging(iris.bagging, newdata=iris[-sub,])
iris.predbagging
#Predicting with unlabeled data
iris.predbagging<- predict.bagging(iris.bagging, newdata=iris[-sub,-5])
iris.predbagging
}

\keyword{tree }% at least one, from doc/KEYWORDS
\keyword{classif}
