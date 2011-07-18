predict.bagging <-
function(object, newdata, ...) {

# devuelva la formula para la funcion margin
formula <- object[[1]]
vardep <- newdata[,as.character(object[[1]][[2]])]
mfinal<-length(object[[2]])
n <- length(newdata[,1])
nclases <- nlevels(vardep)

pred<- data.frame(rep(0,n)) 

# Crea un dataframe para guardar las pred, al 1<ba> est<e1> vac<ed>o, pero luego se va a<f1>adiendo
for (m in 1:mfinal) {
if(m==1){pred <- predict(object[[2]][[m]],newdata,type="class")} 
else{pred <- data.frame(pred,predict(object[[2]][[m]],newdata,type="class"))} 
}

classfinal <- array(0, c(n,nlevels(vardep)))
for (i in 1:nlevels(vardep)){
 classfinal[,i]<- matrix(as.numeric(pred==levels(vardep)[i]),nrow=n)%*%rep(1,mfinal)
}

predclass <- rep("O",n)
for(i in 1:n){
predclass[i] <- as.character(levels(vardep)[(order(classfinal[i,],decreasing=TRUE)[1])])
}
# para que devuelva la matriz de confusi<f3>n
tabla <- table(predclass, vardep, dnn=c("Predicted Class", "Observed Class")) 

# Para que devuelva el error en newdata
error<- 1- sum(predclass== vardep)/n

output<- list(formula=formula, votes=classfinal,class=predclass, confusion=tabla, error=error)
}

