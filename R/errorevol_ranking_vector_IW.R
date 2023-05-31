errorevol_ranking_vector_IW <- function (object, newdata,iw,squared=FALSE){
  if (!(class(object) %in% c("bagging", "boosting"))) {
    stop("object must of class 'bagging'or 'boosting'")
  }
  item <- object$args$item
  mat.dist <- object$args$mat.dist
  perm_tab_complete_up <- object$args$perm_tab_complete_up
  perm <- object$args$perm
  namecol <- object$args$namecol
  if(missing(iw))iw <- object$args$iw

  if (is(dim(iw), "NULL")) {
    if(length(iw)!= (ncol(perm_tab_complete_up)-1)){
      stop("The dimension of the weighting vector is incorrect.")}
  }
  else{
    if(ncol(iw) !=  (ncol(perm_tab_complete_up)-1)){
      stop("The dimension of the weighting vector is incorrect.")}
  }

  vardep <- newdata[, as.character(object$formula[[2]])]
  mfinal <- length(object$trees)
  cat("\n n of trees =",mfinal)
  n <- length(newdata[, 1])
  nclases <- nlevels(as.factor(vardep))
  #if (class(object) != "boosting") {
if (inherits(object,"boosting")==FALSE){
    ponderacion <- rep(1, mfinal)
  } else {
    ponderacion <- object$weights
  }
  erroracum <- rep(0, mfinal)
  #1print("weights")
  #1print(ponderacion)

  pred<-matrix(0,ncol=length(object$trees),nrow=dim(newdata)[1])
  #print(pred)
  for(j in 1: length(object$trees))
    pred[,j]<-predict(object$trees[j],newdata = newdata)[[1]]
  #head((pred))
  #1print(length(object$trees))

  previsione_finale<-matrix(0,nrow=dim(newdata)[1],ncol=length(object$trees))#dim(newdata)[1]
  previsione_finale[,1]<-pred[,1]
  # the k-th final forecast column contains the aggregate forecast up to the k-th tree

  j=2
  # the matrix X has # of columns equal to items per number of trees,
  #each block of item values will contain the expected ranking for
  #that unit from the k-th tree

  X<-array(dim=c(dim(newdata)[1],length(object$trees)*item))
  X<-data.frame(X)
  while(j<=length(object$trees))
  {
    t<-matrix(0,nrow=dim(newdata)[1],ncol=item)
    for(i in 1:dim(newdata)[1])#dim(newdata)[1]
    {
      #X<-matrix(0,nrow=j,ncol=item)
      for(k in 1:j)
      {
        #X[k,]<-perm_tab_complete_up[pred[i,k],1:item]
        X[i,(k-1)*item+1:item]<-perm_tab_complete_up[pred[i,k],1:item]
      }

    }
    j=j+1
    #1print(j)
  }


  #no parallel
  if(squared==TRUE){options(rlib_message_verbosity = "quiet")
    previsione_finale[,2:length(object$trees)]<-t(sapply(1:dim(newdata)[1],
                                                         function(i) sapply(2:length(object$trees),
                                                                            function(s) semi_join(data.frame(perm_tab_complete_up[,c(1:item,item+1)]),
                                                                                                  internal2(riga=internal1(iwquickcons(matrix(unlist(X[i,1:(s*item)]),s,item, byrow = T),iw,Wk=ponderacion[1:(s)]^2)$Consensus[1,],item), item=item, namecol=namecol))[,item+1])))
  }
  else{options(rlib_message_verbosity = "quiet")
    previsione_finale[,2:length(object$trees)]<-t(sapply(1:dim(newdata)[1],
                                                         function(i) sapply(2:length(object$trees),
                                                                            function(s) semi_join(data.frame(perm_tab_complete_up[,c(1:item,item+1)]),
                                                                                                  internal2(riga=internal1(iwquickcons(matrix(unlist(X[i,1:(s*item)]),s,item, byrow = T),iw,Wk=ponderacion[1:(s)])$Consensus[1,],item), item=item, namecol=namecol))[,item+1])))
  }



  cat("\nWeighted tau_x =\n")


  Tau_singolo<-t(sapply(1:dim(newdata)[1], function(i) sapply(1:length(object$trees),
                                                              function(s) ifelse((vardep[i])==(previsione_finale[i,s]),1,iw_tau_x(perm_tab_complete_up[vardep[i],1:item],perm_tab_complete_up[previsione_finale[i,s],1:item],iw)))))



  tau_medio<-apply(Tau_singolo,2,mean,na.rm=T)
  print(tau_medio)
  error<-1-(tau_medio+1)/2

  pr<-data.frame(previsione_finale[,mfinal])
  names(pr)<-"Label"
  options(rlib_message_verbosity = "quiet")
  pred_rank<-left_join(pr,perm_tab_complete_up)[,-1]

  #print(erroracum)
  output <- list(error = error, final_prediction=pred_rank)
  class(output) <- "errorevol"
  output
}

