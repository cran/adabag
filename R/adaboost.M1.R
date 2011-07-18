adaboost.M1 <-
function(formula, data,boos=TRUE, mfinal=100, coeflearn="Breiman", control) {


#Exigimos que coeflearn sea uno de esos dos valores
if (!(as.character(coeflearn) %in% c("Freund","Breiman"))){
stop("coeflearn must be 'Freund' or 'Breiman' ")
}
formula<- as.formula(formula)
vardep <- data[,as.character(formula[[2]])]
        n <- length(data[,1])
nclases <- nlevels(vardep)

pesos <- rep(1/n,n)
w <- rep(1/n,n) # desaparece el not visible binding for "<<-" que se usa en boos=F
data<-data.frame(pesos, data) #Los pesos en rpart deben ser una columna del dataframe

     arboles <- list() #Creamos una lista para guardar los arboles
pond <- rep(0,mfinal) # Un vector donde guardaremos la ponderaci<f3>n de cada <e1>rbol.

for (m in 1:mfinal) {
#Creamos muestras boostrap utilizando los pesos

if (boos==TRUE) {
boostrap<- sample(1:n,replace=TRUE,prob=pesos)
fit <- rpart(formula,data=data[boostrap,-1], control=control)
        flearn <- predict(fit,newdata=data[,-1],type="class")
ind<-as.numeric(vardep != flearn) #crear un vector indicador
err<- sum(ind)/n                       #calcula el error en esa iteraci<f3>n
}

#<bf>limitamos el tama<f1>o del arbol para que sean distintos?
if (boos==FALSE) {
	w<<- pesos
	fit <- rpart(formula=formula, data=data[,-1], weights=w, control=control) 

        flearn <- predict(fit,data=data[,-1], type="class")
ind<-as.numeric(vardep != flearn) #Crear un vector indicador
err<- sum(pesos*ind)         #Calcula el error ponderado en esa iteraci<f3>n


}
# Diferenciamos entre Freund y Breiman. 
c<- log((1-err)/err)
if (coeflearn=="Breiman"){
c<- (1/2)*c
}

pesos <- pesos*exp(c*ind)
pesos<- pesos/sum(pesos)

#Si el error no es menor que la regla por defecto los pesos se inicializan
# Seg<fa>n Opitz y Maclin si no 0<err<0.5 se ajustan los valores de c.

maxerror<-min(1-max(summary(vardep))/sum(summary(vardep)), 0.5)

if (err>=maxerror) {
pesos <- rep(1/n,n)
c<-0.001
} 
if (err==0) {
pesos <- rep(1/n,n)
c<-3
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

ans<- list(formula=formula, trees=arboles, weights=pond,votes=classfinal,class=predclass, importance=acum)
class(ans)<-"boosting"
ans
}

