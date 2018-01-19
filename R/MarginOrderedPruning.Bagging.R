MarginOrderedPruning.Bagging <-
function(baggingObject,trainingset,pruningset,marginType="unsupervised",doTrace=TRUE)
{
       MyentropyEachTree<-entropyEachTree.bagging(baggingObject,trainingdata=trainingset,marginType=marginType,doTrace=doTrace)
       MyentropyEachTree.order.pred<-predictOrderedAggregation.bagging(object=baggingObject,newdata=pruningset,myorder=MyentropyEachTree$order,doTrace=doTrace)

       myBestTreeIndex<-MyentropyEachTree$order[1:MyentropyEachTree.order.pred$BestEnsembleSize]
      
    prunedBagging<- list(formula=baggingObject$formula,trees=baggingObject$trees[myBestTreeIndex])

#2016-11-15 pruebo a meter las clases de vardep como atributo de la salida porque lo uso en predict.bagging
attr(prunedBagging, "vardep.summary") <- attributes(baggingObject)$vardep.summary 

   class(prunedBagging) <- "bagging"
   
      output<-list(prunedBagging=prunedBagging,AccuracyOrderedEnsemblePruningSet=MyentropyEachTree.order.pred$AccuracyOrderedEnsemble)
}
