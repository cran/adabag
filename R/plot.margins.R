plot.margins <-
function(x, y=NULL, ...){
  #if(!((class(x)=="margins")))
    if(!(inherits(x,"margins")))
      stop("x class should be margins")
  #if(!((class(y)=="margins")|is.null(y)))
    if(!(inherits(y,"margins")|is.null(y)))
      stop("y class should be margins or NULL")

  plot(sort(x$margins), (1:length(x$margins))/length(x$margins),
       type="l", xlim=c(-1,1),main="Margin cumulative distribution graph", xlab="m", ylab="Cumulative relative frequency", col="darkblue", lwd=3, cex.main=2)

  abline(v=0, col="red",lty=3, lwd=3 )
  if(!is.null(y)) {
    lines(sort(y$margins), (1:length(y$margins))/length(y$margins), type="l", cex = .5 ,col="green", lwd=3, lty=2)
    legend("topleft", c("test","train"), col = c("darkblue", "green"), lty=1:2, lwd=3)
  }
}

