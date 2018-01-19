bagging <-
  function(formula, data, mfinal=100, control, par=FALSE,...) {
    
    formula<- as.formula(formula)
    vardep <- data[,as.character(formula[[2]])]
    n <- length(data[,1])
    nclases <- nlevels(vardep)
    pred<- data.frame(rep(0,n)) # Dataframe para guardar las pred, al inicio esta vacio, pero luego se va agnadiendo
    
    arboles <- list() #Creamos una lista para guardar los arboles
    replicas <- array(0, c(n,mfinal))
    #2012-05-16 nueva medida de importancia
    #sustituye a acum
    arboles[[1]] <- rpart(formula, data = data, control = rpart.control(minsplit=1, cp=-1, maxdepth=30) ) 
    
    #Para sacar el n de variables, este luego lo sustituye en el bucle
    
    #if( is.numeric(nrow(arboles[[1]]$splits))=="FALSE" ) stop("change rpart.control to avoid empty trees")
    nvar<-dim(varImp(arboles[[1]], surrogates = FALSE, competes = FALSE))[1]
    
    data1<-data
    
    #2017-04-02 Prueba para parallel con TRUE/FALSE
    if (par==TRUE) { 
    
    #2017-02-14 Prueba para parallel
    no_cores <- detectCores() - 1 # Calculate the number of cores
    cl <- makeCluster(no_cores)     # Initiate cluster
    registerDoParallel(cl) #PAra el foreach
    #clusterExport(cl, "n")
    clusterEvalQ(cl, library(adabag)) 
    
#Esta parte no se, si quedo o lo quite al final    
     comb <- function(x, ...) {   lapply(seq_along(x),
             function(i) c(x[[i]], lapply(list(...), function(y) y[[i]]),lapply(list(...), function(z) z[[i]]))) #saca las 3 cosas pero duplica los arboles

    }
            oper <- foreach(m=1:mfinal, .combine='comb', .multicombine=TRUE,
            .init=list(list(), list(), list())) %dopar% {
    
    #for (m in 1:mfinal) {
      #boostrap<- sample(1:n,replace=TRUE)
      #fit <- rpart(formula,data=data[boostrap,], control=control)

	 n <- length(data[,1])      
      k2 <- 1 			#Los autores agradecen su sugerencia a Ignacio Medina
      while (k2 == 1){
        

        boostrap <- sample(1:n, replace = TRUE)
        #            fit <- rpart(formula, data = data[boostrap, ], control = control)
        fit <- rpart(formula, data = data1[boostrap, ], control = control)
        k2 <- length(fit$frame$var)
      }
      
      
      
  #    arboles[[m]] <- fit #Guardamos los arboles
   #   replicas[,m]<-boostrap

      
#      k <- varImp(arboles[[m]], surrogates = FALSE, competes = FALSE)
#     imp[m,] <-k[sort(row.names(k)), ]
          k <- varImp(fit, surrogates = FALSE, competes = FALSE)
          imp <-k[sort(row.names(k)), ]
      
      #Parallel
         list(fit, boostrap, imp)
            }
    stopCluster(cl)

      

    arboles <- oper[[1]][1:mfinal] #Guardamos los arboles de 1:mfinal porque estan duplicados
    replicas<-matrix(unlist(oper[[2]], use.names=FALSE ), ncol = mfinal, byrow = FALSE)
    imp<-matrix(unlist(oper[[3]][1:mfinal], use.names=FALSE ), ncol = nvar, byrow = TRUE)

 
    }   
    
    #A partir de aqui igual sin parallel
    if (par==FALSE) {
      
      imp<- array(0, c(mfinal,nvar))  #Creo una matriz para guardar el "improve" de cada variable conforme evoluciona boosting/bagging
      
      
      for (m in 1:mfinal) {
      
      k2 <- 1 			#Los autores agradecen su sugerencia a Ignacio Medina
      while (k2 == 1){
        
        boostrap <- sample(1:n, replace = TRUE)
        fit <- rpart(formula, data = data1[boostrap, ], control = control)
        k2 <- length(fit$frame$var)
      }
      
      
      
      arboles[[m]] <- fit #Guardamos los arboles
      replicas[,m]<-boostrap
      k <- varImp(arboles[[m]], surrogates = FALSE, competes = FALSE)
      imp[m,] <-k[sort(row.names(k)), ]
      }  
    }
        
    
    #pred<-as.data.frame(sapply (arboles, predict, data=data, type="class"))
    #Lo cambio porque da problemas el data=data
    pred<-as.data.frame(sapply (arboles, predict, data1, type="class"))


        
    
    classfinal <- array(0, c(n,nlevels(vardep)))
    for (i in 1:nlevels(vardep)){
      classfinal[,i] <- matrix(as.numeric(pred==levels(vardep)[i]),nrow=n)%*%rep(1,mfinal)
    }
    
    predclass <- rep("O",n)	
    #2014-11-12 Se puede hacer esto usando apply para evitar el bucle? 
    #Creo la funcion "select" que en caso de empate devuelva la clase mayoritaria de entre las empatadas
    #2015-07-25 modifico la funcion select para poder usar predict con unlabeled data
    predclass[]<-apply(classfinal,1,FUN=select, vardep.summary=summary(vardep))
    
    #normalizar la importancia de las variables, las ponderaciones son todas iguales en bagging
    
    pond<-rep(1,mfinal)

    k <- varImp(arboles[[1]], surrogates = FALSE, competes = FALSE)
    
    imppond<-as.vector(as.vector(pond)%*%imp)
    imppond<-imppond/sum(imppond)*100
    names(imppond)<-sort(row.names(k))
    
    
    #Para que devuelva las probabilidades a posteriori
    classfinal/apply(classfinal,1,sum)->votosporc
    
    
    ans<- list(formula=formula,trees=arboles,votes=classfinal,prob=votosporc,class=predclass, samples=replicas, importance=imppond)
    
    #2015-07-25 pruebo a meter las clases de vardep como atributo de la salida
    attr(ans, "vardep.summary") <- summary(vardep, maxsum=700)
    
    mf <- model.frame(formula=formula, data=data1) 
    terms <- attr(mf, "terms") 
    ans$terms <- terms 
    ans$call <- match.call()
    
    
    
    
    class(ans) <- "bagging"
    ans
    
  }
