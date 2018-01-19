\name{errorevol}
\alias{errorevol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Shows the error evolution of the ensemble }

\description{
Calculates the error evolution of an AdaBoost.M1, AdaBoost-SAMME or Bagging classifier for a data frame
as the ensemble size grows  
}

\usage{ errorevol(object, newdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
This object must be the output of one of the functions \code{bagging} or \code{boosting}.
	This is assumed to be the result of some function that produces an object with two components named \code{formula}  and \code{trees},  as those 
	returned for instance by the \code{bagging} function.
}
  \item{newdata}{ Could be the same data frame used in \code{object} or a new one}
}
\details{
This can be useful to see how fast \code{Bagging},  \code{boosting} reduce the error of the ensemble. in addition,
it can detect the presence of overfitting and, therefore, the convenience of pruning the ensemble using \code{predict.bagging} or \code{predict.boosting}.
}
\value{
   An object of class \code{errorevol}, which is a list with only one component:  

	\item{error}{a vector with the error evolution.}

}
\references{Alfaro, E., Gamez, M. and Garcia, N. (2013): ``adabag: An R Package for Classification with Boosting and Bagging''. Journal of Statistical Software, Vol 54, 2, pp. 1--35.

	Alfaro, E., Garcia, N., Gamez, M. and Elizondo, D. (2008): ``Bankruptcy forecasting: An empirical comparison of AdaBoost and neural networks''. Decision Support Systems, 45, pp. 110--122.

	Breiman, L. (1996): ``Bagging predictors''. Machine Learning, Vol 24, 2, pp.123--140.

	Freund, Y. and Schapire, R.E. (1996): ``Experiments with a new boosting algorithm''. In Proceedings of the Thirteenth International Conference on Machine Learning, pp. 148--156, Morgan Kaufmann. 

	Zhu, J., Zou, H., Rosset, S. and Hastie, T. (2009): ``Multi-class AdaBoost''. Statistics and Its Interface, 2, pp. 349--360. 
}


\author{Esteban Alfaro-Cortes \email{Esteban.Alfaro@uclm.es}, Matias Gamez-Martinez \email{Matias.Gamez@uclm.es} and Noelia Garcia-Rubio \email{Noelia.Garcia@uclm.es} }

%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
       \code{\link{boosting}},
       \code{\link{predict.boosting}},
  	\code{\link{bagging}},
	\code{\link{predict.bagging}}
}
\examples{

library(mlbench)
data(BreastCancer)
l <- length(BreastCancer[,1])
sub <- sample(1:l,2*l/3)
cntrl <- rpart.control(maxdepth = 3, minsplit = 0,  cp = -1)

BC.adaboost <- boosting(Class ~.,data=BreastCancer[sub,-1],mfinal=5, control=cntrl)
BC.adaboost.pred <- predict.boosting(BC.adaboost,newdata=BreastCancer[-sub,-1])

errorevol(BC.adaboost,newdata=BreastCancer[-sub,-1])->evol.test
errorevol(BC.adaboost,newdata=BreastCancer[sub,-1])->evol.train

plot.errorevol(evol.test,evol.train)
abline(h=min(evol.test[[1]]), col="red",lty=2,lwd=2)
abline(h=min(evol.train[[1]]), col="blue",lty=2,lwd=2)


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tree }% at least one, from doc/KEYWORDS
\keyword{classif}