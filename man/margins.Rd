\name{margins}
\alias{margins}

\title{ Calculates the margins }

\description{ Calculates the margins of an AdaBoost.M1, AdaBoost-SAMME or Bagging classifier for a data frame}

%no se si es margins or margin
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

\references{Alfaro, E., Gamez, M. and Garcia, N. (2007): ``A Boosting approach for corporate failure prediction''. Applied Intelligence. 27, pp. 29--37.

  Alfaro, E., Garcia, N., Gamez, M. and Elizondo, D. (2008): ``Bankruptcy forecasting: An empirical comparison of AdaBoost and neural networks''. Decision Support Systems, 45, pp. 110--122.

  Schapire, R.E., Freund, Y., Bartlett, P. and Lee, W.S. (1998): ``Boosting the margin: A new explanation for the effectiveness of voting methods''. The Annals of Statistics, vol 26, 5, pp. 1651--1686.
}

\author{Esteban Alfaro-Cortes \email{Esteban.Alfaro@uclm.es}, Matias Gamez-Martinez \email{Matias.Gamez@uclm.es} and Noelia Garcia-Rubio \email{Noelia.Garcia@uclm.es} }


\seealso{ 
       \code{\link{boosting}},
       \code{\link{predict.boosting}},
    	\code{\link{bagging}},
	\code{\link{predict.bagging}},
}

\examples{

#Iris example
library(rpart)
data(iris)
names(iris)<-c("LS","AS","LP","AP","Especies")
sub <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
iris.adaboost <- boosting(Especies ~ ., data=iris[sub,], mfinal=10)
margins(iris.adaboost,iris[sub,])->iris.margins # training set

# test set
iris.predboosting<- predict.boosting(iris.adaboost, newdata=iris[-sub,])
margins(iris.predboosting,iris[-sub,])->iris.predmargins 


#Examples with bagging
iris.bagging <- bagging(Especies ~ ., data=iris[sub,], mfinal=10)
margins(iris.bagging,iris[sub,])->iris.baggging.margins # training set


iris.predbagging<- predict.bagging(iris.bagging, newdata=iris[-sub,])
margins(iris.predbagging,iris[-sub,])->iris.baggging.predmargins # test set

# We change the name to the margins object to save space 
# and use the same code for the graph
#iris.margins<-iris.predmargins 
#iris.margins<-iris.baggging.margins
#iris.margins<-iris.baggging.predmargins

# Example of a graph using the margins
par(bg="lightyellow")

# 1 Rotulos en castellano, con disculpas por la falta de acentos por el codigo
plot(sort(iris.margins[[1]]), (1:length(iris.margins[[1]]))/length(iris.margins[[1]]),
type="l", xlim=c(-1,1),main="Grafico de la distribucion de los margenes", xlab="m", 
ylab="\% conjunto entrenamiento", col="blue3")
abline(v=0, col="red",lty=2)

# 2 English headings
plot(sort(iris.margins[[1]]), (1:length(iris.margins[[1]]))/length(iris.margins[[1]]), 
type="l", xlim=c(-1,1),main="Margin cumulative distribution graph", xlab="m", 
ylab="\% training set", col="blue3")
abline(v=0, col="red",lty=2)


## rpart and mlbench libraries should be loaded
library(rpart)
library(mlbench)
data(BreastCancer)
l <- length(BreastCancer[,1])
sub <- sample(1:l,2*l/3)

BC.adaboost <- boosting(Class ~.,data=BreastCancer[sub,-1],mfinal=25, control=rpart.control(maxdepth=3))
BC.adaboost.pred <- predict.boosting(BC.adaboost,newdata=BreastCancer[-sub,-1])

BC.margins<-margins(BC.adaboost,BreastCancer[sub,-1]) # training set
BC.predmargins<-margins(BC.adaboost.pred,BreastCancer[-sub,-1]) # test set


}


\keyword{tree }% at least one, from doc/KEYWORDS
\keyword{classif}