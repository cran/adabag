bagging.cv <-
function ( formula, data,v=10, mfinal=100,control) 
{
vardep<-data[,as.character(formula[[2]])]
n <- length(vardep)
#para validaci<f3>n cruzada 2<v<n
if(v>n) stop(" el valor de v no es adecuado")
if(v<2) stop(" el valor de v no es adecuado")

predclass <- rep("O",n)
#	con<-control #prueba

    for (i in 1:v) {
        test <- v * (0:floor(n/v)) + i
        test <- test[test < n + 1]
       fit <- bagging(formula, data[-test,],mfinal, control=control)
#	con<<-control
#        fit <- bagging(formula, data[-test,],mfinal, control=con)
	fit.predict<-predict.bagging(fit, data[test,])
        predclass[test] <- fit.predict$class
    }

   # para que devuelva la matriz de confusi<f3>n
tabla <- table(predclass, vardep, dnn=c("Clase estimada", "Clase real")) 

# Para que devuelva el error en newdata
error<- 1- sum(predclass== vardep)/n

output<- list(class=predclass, confusion=tabla, error=error)

}

