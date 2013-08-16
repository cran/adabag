
predict.bagging<- function (object, newdata, newmfinal=length(object$trees), ...) 

{
    if (newmfinal > length(object$trees) | newmfinal < 1) 
        stop("newmfinal must be 1<newmfinal<mfinal")

    formula <- object$formula
    vardep <- newdata[, as.character(object$formula[[2]])]
 #   mfinal <- length(object$trees)
    n <- length(newdata[, 1])
    nclases <- nlevels(vardep)
    pred <- data.frame(rep(0, n))
    for (m in 1:newmfinal) {
        if (m == 1) {
            pred <- predict(object$trees[[m]], newdata, type = "class")
        }
        else {
            pred <- data.frame(pred, predict(object$trees[[m]], 
                newdata, type = "class"))
        }
    }
    classfinal <- array(0, c(n, nlevels(vardep)))
    for (i in 1:nlevels(vardep)) {
        classfinal[, i] <- matrix(as.numeric(pred == levels(vardep)[i]), 
            nrow = n) %*% rep(1, newmfinal)
    }
    predclass <- rep("O", n)
    for (i in 1:n) {
        predclass[i] <- as.character(levels(vardep)[(order(classfinal[i, 
            ], decreasing = TRUE)[1])])
    }
    tabla <- table(predclass, vardep, dnn = c("Predicted Class", 
        "Observed Class"))
    error <- 1 - sum(predclass == vardep)/n

#Para que devuelva las probabilidades a posteriori
classfinal/apply(classfinal,1,sum)->votosporc

output<- list(formula=formula, votes=classfinal, prob=votosporc, class=predclass, confusion=tabla, error=error)
}