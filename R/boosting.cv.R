boosting.cv <-
function ( formula, data,v=10,boos=TRUE ,mfinal=100, coeflearn="Breiman", control, par=FALSE) 
{

#Exigimos que coeflearn sea uno de esos tres valores
if (!(as.character(coeflearn) %in% c("Freund","Breiman","Zhu"))){
stop("coeflearn must be 'Freund', 'Breiman' or 'Zhu' ")
}

vardep<-data[,as.character(formula[[2]])]
n <- length(vardep)
#para validacion cruzada 2<v<n
if(v>n) stop(" v should be in [2, n]")
if(v<2) stop(" v should be in [2, n]")




  
if (par==TRUE) { 
  
  # Calculate the number of cores
    no_cores <- detectCores() - 1
    
    # Initiate cluster
    cl <- makeCluster(no_cores)
    
    #Para el foreach
    registerDoParallel(cl)
#    clusterExport(cl, "n")
    clusterEvalQ(cl, library(adabag)) 



#    for (i in 1:v) {
    kk<-foreach(i = 1:v, .combine = rbind, .packages='adabag')  %dopar% 
    {

     n <- length(vardep)
        test <- v * (0:floor(n/v)) + i
        test <- test[test < n + 1]
        fit <- boosting(formula, data[-test,],boos ,mfinal,coeflearn,control=control)
#	predclass[test] <- predict.boosting(fit, data[test,])$class
      predclass <- predict.boosting(fit, data[test,])$class

      x<-data.frame(test, predclass)
      
      return(x)


#cat("i: ", c(i, date()), "\n")

    }
    stopCluster(cl)
    predclass<-kk$predclass[order(kk$test)]
}


if (par==FALSE) {
  predclass <- rep("O",n)
  for (i in 1:v) {
    test <- v * (0:floor(n/v)) + i
    test <- test[test < n + 1]
    fit <- boosting(formula, data[-test,],boos ,mfinal,coeflearn,control=control)
    predclass[test] <- predict.boosting(fit, data[test,])$class
    
    cat("i: ", c(i, date()), "\n")
  }  
    
}

   # para que devuelva la matriz de confusion
tabla <- table(predclass, vardep, dnn=c("Predicted Class", "Observed Class")) 

# Para que devuelva el error en newdata
error<- 1- sum(predclass== vardep)/n

output<- list(class=predclass, confusion=tabla, error=error )

}