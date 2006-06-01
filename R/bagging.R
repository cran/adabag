"bagging" <-
function(formula, data, mfinal=100, minsplit=5, cp=0.01,maxdepth=nlevels(vardep)) {
	
	formula<- as.formula(formula)
	vardep <- data[,as.character(formula[[2]])]
        n <- length(data[,1])
	nclases <- nlevels(vardep)
	
	#pesos <- rep(1/n,n)
	#data<-data.frame(data,pesos)#creo que en bagging no hace falta
     	arboles <- list() #Creamos una lista para guardar los arboles
	replicas <- array(0, c(n,mfinal))
	
	for (m in 1:mfinal) {
		
		boostrap<- sample(1:n,replace=TRUE)
		fit <- rpart(formula,data=data[boostrap,], minsplit=minsplit,cp=cp,maxdepth=maxdepth)
	        flearn <- predict(fit,newdata=data,type="class")
		ind<-as.numeric(vardep != flearn) 	#crear un vector indicador
		err<- sum(ind)/n                       #calcula el error en esa iteración

		arboles[[m]] <- fit			#Guardamos los arboles
		replicas[,m]<-boostrap

	#para conocer la importancia de las variables
		if(m==1){summary(fit$fram[,1])->acum} 
		else{summary(fit$fram[,1])->acum1
		acum<-acum+acum1
		} 
		
	}

	pred<- data.frame(rep(0,n)) 
	# Crea un dataframe para guardar las pred, al 1º está vacío, pero luego se va añadiendo
	for (m in 1:mfinal) {
		if(m==1){pred <- predict(arboles[[m]],data,type="class")} 
		else{pred <- data.frame(pred,predict(arboles[[m]],data,type="class"))} 
	}

	classfinal <- array(0, c(n,nlevels(vardep)))
	for (i in 1:nlevels(vardep)){
 		classfinal[,i] <- matrix(as.numeric(pred==levels(vardep)[i]),nrow=n)%*%rep(1,mfinal)
	}
	
	predclass <- rep("O",n)
	for(i in 1:n){
		predclass[i] <- 
as.character(levels(vardep)[(order(classfinal[i,],decreasing=TRUE)[1])])
	}

	#normalizar la importancia de las variables
	acum<-acum[-1]/sum(acum[-1])*100
	ans<- list(formula=formula,trees=arboles,votes=classfinal,class=predclass, samples=replicas, importance=acum)
	class(ans) <- "bagging"
	ans
}

