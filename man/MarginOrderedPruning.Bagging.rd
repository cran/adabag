\name{MarginOrderedPruning.Bagging}
\alias{MarginOrderedPruning.Bagging}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{MarginOrderedPruning.Bagging}
\description{Margin-based ordered aggregation for bagging pruning}
\usage{
MarginOrderedPruning.Bagging(baggingObject, trainingset, pruningset, 
	marginType = "unsupervised", doTrace = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{baggingObject}{
fitted model object of class \code{bagging} 
}
  \item{trainingset}{
the training set of the \code{bagging} object
}
  \item{pruningset}{
a set aside dataset for \code{bagging} pruning
}
  \item{marginType}{
if "unsupervised" (by default) the margin is the difference between the proportions of votes
of the first and second most popular classes. Else the margin is calculated as the 
difference between the proportion of votes of the correct class and the most popular among the other classes
}
  \item{doTrace}{
If set to \code{TRUE}, give a more verbose output as \code{MarginOrderedPruning.Bagging} is running
}
}
\value{
Returns a list with the following components:
  \item{prunedBagging }{a pruned \code{bagging} object}
  \item{AccuracyOrderedEnsemblePruningSet }{Accuracy of each ordered ensemble 
	on pruning set}
}
\references{
Guo, L. and Boukir, S. (2013): "Margin-based ordered aggregation for ensemble pruning". Pattern Recognition
Letters, 34(6), 603-609.
}
\author{
Li Guo \email{guoli84@hotmail.com}
}
\note{
Questions about this function should be sent to Li Guo
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{bagging}},
\code{\link{predict.bagging}}
}
\examples{
## mlbench package should be loaded
library(mlbench)
data(Satellite)
## Separate data into 3 parts: training set, pruning set and test set
ind <- sample(3, nrow(Satellite), replace = TRUE, prob=c(0.3, 0.2,0.5))

## create bagging with training set
#increase mfinal in your own execution of this example to see 
#the real usefulness of this function
Satellite.bagging<-bagging(classes~.,data=Satellite[ind==1,],mfinal=3)
#Satellite.bagging.pred<-predict(Satellite.bagging,Satellite[ind==3,])

##pruning bagging
Satellite.bagging.pruning<-MarginOrderedPruning.Bagging(Satellite.bagging,
Satellite[ind==1,],Satellite[ind==2,])
#Satellite.bagging.pruning.pred<-predict(Satellite.bagging.pruning$prunedBagging,
#Satellite[ind==3,])

## create bagging with training and pruning set
#This example has been hidden to fulfill execution time <5s 
#Satellite.bagging2<-bagging(classes~.,data=Satellite[ind!=3,],25)
#Satellite.bagging2.pred<-predict(Satellite.bagging2,Satellite[ind==3,])

}

\keyword{tree}% at least one, from doc/KEYWORDS
\keyword{classif}
