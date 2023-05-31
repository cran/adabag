\name{adabag-internal}
\alias{adabag-internal}
\alias{entropyEachTree.bagging}
\alias{Margin.vote}
\alias{OOBIndex}
\alias{predictOrderedAggregation.bagging}
\alias{vote.bagging}
\alias{select}
\alias{internal1}
\alias{internal2}
\alias{itemp_IW}
\alias{stemp_IW}
\alias{etemp_IW}


\title{Internal \code{adabag} functions}
\description{Internal \code{adabag} functions}

\usage{
entropyEachTree.bagging(object, trainingdata, OOB=TRUE, marginType="unsupervised",

	newmfinal=length(object$trees), doTrace=TRUE)

Margin.vote(vote,type="unsupervised",observed)

OOBIndex(mySample)

predictOrderedAggregation.bagging(object, newdata, myorder,doTrace=TRUE)

vote.bagging(object, newdata,OOB=TRUE, myTreeIndex=seq(1,length(object$trees)))

select(fila, vardep.summary, ...)

internal1(x,item)

internal2(riga, namecol, item=item)

itemp_IW(y, offset, parms, wt)

stemp_IW(y, wt, x, parms, continuous)

etemp_IW(y, wt, parms)


}

\details{These are not to be called by the user}

\keyword{internal }% at least one, from doc/KEYWORDS
