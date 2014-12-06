select <-
function(fila, vardep, ...) {
    
    if(length(which(fila==max(fila)))>1)
    {predclass <-names(summary(vardep)[which(fila==max(fila))])[
      order(summary(vardep)[which(fila==max(fila))],decreasing=TRUE)[1]]
    }
    else{predclass<- as.character(levels(vardep)[(order(fila,decreasing=TRUE)[1])])} 
    
   # ans<-predclass
   predclass
  }
