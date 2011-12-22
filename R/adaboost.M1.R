boosting <-
function(formula, data,boos=TRUE, mfinal=100, coeflearn="Breiman", control) {


#Exigimos que coeflearn sea uno de esos dos valores
if (!(as.character(coeflearn) %in% c("Freund","Breiman","Zhu"))){
stop("coeflearn must be 'Freund', 'Breiman' or 'Zhu' ")
}
formula<- as.formula(formula)
vardep <- data[,as.character(formula[[2]])]
        n <- length(data[,1])
nclases <- nlevels(vardep)

pesos <- rep(1/n,n)
guardarpesos <- array(0, c(n,mfinal)) #para ver los pesos de las observaciones

w <- rep(1/n,n) # desaparece el not visible binding for "<<-" que se usa en boos=F
data<-data.frame(pesos, data) #Los pesos en rpart deben ser una columna del dataframe

     arboles <- list() #Creamos una lista para guardar los arboles
pond <- rep(0,mfinal) # Un vector donde guardaremos la ponderacion de cada arbol.

for (m in 1:mfinal) {
#Creamos muestras boostrap utilizando los pesos

if (boos==TRUE) {
boostrap<- sample(1:n,replace=TRUE,prob=pesos)
fit <- rpart(formula,data=data[boostrap,-1], control=control)
        flearn <- predict(fit,newdata=data[,-1],type="class")
ind<-as.numeric(vardep != flearn) #crear un vector indicador
err<- sum(pesos*ind)         #Calcula el error ponderado en esa iteracion

}

#limitamos el tamaño del arbol para que sean distintos?
if (boos==FALSE) {
	w<<- pesos
	fit <- rpart(formula=formula, data=data[,-1], weights=w, control=control) 

        flearn <- predict(fit,data=data[,-1], type="class")
ind<-as.numeric(vardep != flearn) #Crear un vector indicador
err<- sum(pesos*ind)         #Calcula el error ponderado en esa iteracion


}
# Diferenciamos entre Freund, Breiman y Zhu. 
c<- log((1-err)/err)

	if (coeflearn=="Breiman"){
	c<- (1/2)*c
	}

	if (coeflearn=="Zhu"){
	c<- c+log(nclases-1)
	}

		guardarpesos[,m]<-pesos
pesos <- pesos*exp(c*ind)
pesos<- pesos/sum(pesos)

#Si el error no es menor que la regla por defecto los pesos se inicializan
# Segun Opitz y Maclin si no 0<err<0.5 se ajustan los valores de a 3 y 0.001


maxerror<-0.5
eac<-0.001 # minimum fraction of error above 0 or under maxerror 
#maxerror<-min(1-max(summary(vardep))/sum(summary(vardep)), 0.5)
#maxerror<-1-max(summary(vardep))/sum(summary(vardep))

		if (coeflearn=="Zhu"){
		maxerror<-1-1/nclases
		}



if (err>=maxerror) {
pesos <- rep(1/n,n)
#c<-0.001
maxerror<-maxerror-eac
c<- log((1-maxerror)/maxerror)

	if (coeflearn=="Breiman"){
	c<- (1/2)*c
	}

	if (coeflearn=="Zhu"){
	c<- c+log(nclases-1)
	}

} 

if (err==0) {
pesos <- rep(1/n,n)
#c<-3
c<- log((1-eac)/eac)

	if (coeflearn=="Breiman"){
	c<- (1/2)*c
	}

	if (coeflearn=="Zhu"){
	c<- c+log(nclases-1)
	}

}

arboles[[m]] <- fit#Guardamos los arboles
pond[m]<- c #Guardamos las ponderaciones

#para conocer la importancia de las variables
if(m==1){summary(fit$fram[,1])->acum} 
else{summary(fit$fram[,1])->acum1
acum<-acum+acum1
} 

}

pred<- data.frame(rep(0,n))

for (m in 1:mfinal) {
if(m==1){pred <- predict(arboles[[m]],data[,-1],type="class")}
else{pred <- data.frame(pred,predict(arboles[[m]],data[,-1],type="class"))}
}

classfinal <- array(0, c(n,nlevels(vardep)))
for (i in 1:nlevels(vardep)){
 classfinal[,i] <- matrix(as.numeric(pred==levels(vardep)[i]),nrow=n)%*%as.vector(pond)
}

predclass <- rep("O",n)
for(i in 1:n){
predclass[i] <- as.character(levels(vardep)[(order(classfinal[i,],decreasing=TRUE)[1])])
}
#normalizar la importancia de las variables
acum<-acum[-1]/sum(acum[-1])*100


#Para que devuelva las probabilidades a posteriori
classfinal/apply(classfinal,1,sum)->votosporc



ans<- list(formula=formula, trees=arboles, weights=pond, votes=classfinal,prob=votosporc,class=predclass, importance=acum)
class(ans)<-"boosting"
ans
}
