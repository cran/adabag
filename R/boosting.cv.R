"boosting.cv" <-
function ( formula, data,v=10,boos=TRUE ,mfinal=100, coeflearn="Breiman", minsplit=5, cp=0.01, maxdepth=nlevels(vardep)) 
{
	vardep<-data[,as.character(formula[[2]])]
	n <- length(vardep)
	#para validación cruzada 2<v<n
	if(v>n) stop(" v should be 2<v<n")
	if(v<2) stop(" v should be 2<v<n")

	predclass <- rep("O",n)

    for (i in 1:v) {
        test <- v * (0:floor(n/v)) + i
        test <- test[test < n + 1]
        fit <- adaboost.M1(formula, data[-test,],boos ,mfinal,coeflearn=coeflearn, minsplit=minsplit,cp=cp,maxdepth=maxdepth)
	fit.predict<-predict.boosting(fit, data[test,])
        predclass[test] <- fit.predict[[1]]
    }

   # para que devuelva la matriz de confusión
	tabla <- table(predclass, vardep, dnn=c("Predicted Class", "Observed Class")) 

	# Para que devuelva el error en newdata
	error<- 1- sum(predclass== vardep)/n

	output<- list(class=predclass, confusion=tabla, error=error)

}

