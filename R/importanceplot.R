importanceplot <-
function(object,...){
  #if(!((class(object)=="bagging")|(class(object)=="boosting")))
  if(!(inherits(object,"bagging")|inherits(object,"boosting")))
         stop("object class should be bagging or boosting")

  barplot(object$imp[order(object$imp,decreasing=TRUE)], main="Variable relative importance", col="lightblue",las=1,xaxs="r",...)
}
