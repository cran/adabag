errorevol <-
function(object, newdata) {

#newdata could be the same used in object or a new one

#Exigimos que class(object) sea uno de esos dos valores
if (!(class(object) %in% c("bagging","boosting"))){
stop("object must of class 'bagging' or 'boosting' ")
}

vardep <- newdata[,as.character(object$formula[[2]])]
mfinal<-length(object$trees)
n <- length(newdata[,1])
nclases <- nlevels(vardep)
#pesos <- rep(1/n,n)
#newdata<-data.frame(newdata,pesos)

# para poder hacerlo con bagging o con boosting
if(class(object)=="bagging"){ponderacion<-rep(1,mfinal)}
else{ponderacion<- object$weights}

pred<- data.frame(rep(0,n)) # Crea un dataframe para guardar las pred, al 1� est� vac�o, pero luego se va a�adiendo
erroracum<-rep(0,mfinal)#Creo un vector para guardar los errores conforme evoluciona boosting

for (m in 1:mfinal) {

if(m==1){pred <- predict(object$trees[[m]],newdata,type="class")} 
else{pred <- data.frame(pred,predict(object$trees[[m]],newdata,type="class"))} 
}



mvotos <- list() #Creamos una lista para guardar una matriz para cada clase con sus votos (matrizvotos) 

classfinal <- array(0, c(n,nlevels(vardep)))

for (i in 1:nlevels(vardep)){
 mvotos[[i]]<-matrix(as.numeric(pred==levels(vardep)[i]),nrow=n)%*%diag(ponderacion)
}


for (j in 1:mfinal) {

if(j==1){
for (i in 1:nlevels(vardep)) {classfinal[,i]<-mvotos[[i]][,1]}

}
else{
for (i in 1:nlevels(vardep)) {classfinal[,i] <- apply(cbind(classfinal[,i],mvotos[[i]][,j]), 1, sum)
}
}


predclass <- rep("O",n)
for(i in 1:n){
predclass[i] <- as.character(levels(vardep)[(order(classfinal[i,],decreasing=TRUE)[1])])
}


# Para que devuelva el error en newdata
error<- 1- sum(predclass== vardep)/n

erroracum[j]<-error


}
output<- list( error=erroracum)
class(output) <- "errorevol"
output
}
