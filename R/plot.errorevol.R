plot.errorevol <- function(x, y=NULL, ...){
	#if(!((class(x)=="errorevol")))
  if(!(inherits(x,"errorevol")))
	 		stop("x class should be errorevol")
	#if(!((class(y)=="errorevol")|is.null(y)))
	  if(!(inherits(y,"errorevol")|is.null(y)))
		stop("y class should be errorevol or NULL")

	plot(x$error, type="l", ylim=c(0,max(x$error)+0.05), main="Ensemble error vs number of trees",  xlab="Iterations", ylab="Error", col = "red",...)

	if(!is.null(y)) {
	lines(y$error, cex = .5 ,col="blue3", lty=2)


	legend("topright", c("test","train"), col = c("red", "blue"), lty=1:2)
}
}
