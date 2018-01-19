bagging.cv <-
  function ( formula, data,v=10, mfinal=100,control, par=FALSE) 
  {
    vardep<-data[,as.character(formula[[2]])]
#    n <<- length(vardep)
    n <- length(vardep)
    #para validacion cruzada 2<v<n
    if(v>n) stop(" v should be in [2, n]")
    if(v<2) stop(" v should be in [2, n]")


    if (par==TRUE) {    
    
#    no_cores <- detectCores() - 1 # Calculate the number of cores
    no_cores <- 2  # para el check de CRAN
    cl <- makeCluster(no_cores)     # Initiate cluster
    registerDoParallel(cl) #PAra el foreach
 #   clusterExport(cl, "vardep")
    clusterEvalQ(cl, library(adabag)) 
    
        
#    for (i in 1:v) {

    kk<-foreach(i = 1:v, .combine = rbind, .packages='adabag')  %dopar% 
    {
    
    n <- length(vardep)
  
      test <- v * (0:floor(n/v)) + i
      test <- test[test < n + 1]
      fit <- bagging(formula, data[-test,],mfinal=mfinal, control=control)
#      predclass[test] <- predict.bagging(fit, data[test,])$class
      predclass <- predict.bagging(fit, data[test,])$class

      x<-data.frame(test, predclass)
      
      return(x)
      }
    stopCluster(cl)
    
    predclass<-kk$predclass[order(kk$test)]
    
    }   
    
    if (par==FALSE) {
          predclass <- rep("O",n)
          for (i in 1:v) {
            test <- v * (0:floor(n/v)) + i
            test <- test[test < n + 1]
            fit <- bagging(formula, data[-test,],mfinal, control=control)
            predclass[test] <- predict.bagging(fit, data[test,])$class
          }
          
              
    }
            
    # para que devuelva la matriz de confusion
    tabla <- table(predclass, vardep, dnn=c("Predicted Class", "Observed Class")) 
    
    # Para que devuelva el error en newdata
    error<- 1- sum(predclass== vardep)/n
    
    output<- list(class=predclass, confusion=tabla, error=error)

  }
