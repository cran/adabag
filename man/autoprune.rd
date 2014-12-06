\name{autoprune}
\alias{autoprune}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Builds automatically a pruned tree of class \code{rpart}
}
\description{
Builds automatically a pruned tree of class \code{rpart} looking in the
cptable for the minimum cross validation error plus a standard deviation 
}
\usage{
autoprune(formula, data, subset=1:length(data[,1]), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{ a formula, as in the \code{lm} function.  }
  \item{data}{a data frame in which to interpret the variables named in the \code{formula}. }
\item{subset}{   
optional expression saying that only a subset of the rows of the data should be used in the fit, as in the \code{rpart} function.}

  \item{...}{ further arguments passed to or from other methods.}
}
\details{
The cross validation estimation of the error (xerror) has a random component. To avoid this randomness
the 1-SE rule (or 1-SD rule) selects the simplest model with a xerror equal or less than
the minimum xerror plus the standard deviation of the minimum xerror.
}
\value{
An object of class \code{rpart}
}
\references{
%% ~put references to the literature/web site here ~
Breiman, L., Friedman, J.H., Olshen, R. and Stone, C.J.  (1984): "Classification and Regression Trees". Wadsworth International Group. Belmont

Therneau, T., Atkinson, B. and Ripley, B. (2014). rpart: Recursive Partitioning and Regression Trees. R package version 4.1-5
}

\author{Esteban Alfaro-Cortes \email{Esteban.Alfaro@uclm.es}, Matias Gamez-Martinez \email{Matias.Gamez@uclm.es} and Noelia Garcia-Rubio \email{Noelia.Garcia@uclm.es} }


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{rpart}}
}
\examples{
## rpart library should be loaded
library(rpart)
data(iris)
iris.prune<-autoprune(Species~., data=iris)
iris.prune

## Comparing the test error of rpart and autoprune
library(mlbench)
data(BreastCancer)
l <- length(BreastCancer[,1])
sub <- sample(1:l,2*l/3)

BC.rpart <- rpart(Class~.,data=BreastCancer[sub,-1],cp=-1, maxdepth=5)
BC.rpart.pred <- predict(BC.rpart,newdata=BreastCancer[-sub,-1],type="class")
tb <-table(BC.rpart.pred,BreastCancer$Class[-sub])
tb
1-(sum(diag(tb))/sum(tb))


BC.prune<-autoprune(Class~.,data=BreastCancer[,-1],subset=sub)
BC.rpart.pred <- predict(BC.prune,newdata=BreastCancer[-sub,-1],type="class")
tb <-table(BC.rpart.pred,BreastCancer$Class[-sub])
tb
1-(sum(diag(tb))/sum(tb))



}

\keyword{tree }% at least one, from doc/KEYWORDS
\keyword{classif}
