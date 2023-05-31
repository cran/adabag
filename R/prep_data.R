prep_data <- function (y, x, iw){
  if (is(dim(iw), "NULL")) {
    if(length(iw)!=  ncol(y)){
      stop("The dimension of the weighting vector is incorrect.")}
  }
  else{
    if(ncol(iw) !=  ncol(y)){
      stop("The dimension of the weighting vector is incorrect.")}
  }
  y=t(apply(y, 1, rank, ties.method = "min"))
  y=data.frame(y)
  x=data.frame(x)
  item<-ncol(y)
  universo=t(apply(univranks(item)$Cuniv$R, 1, rank, ties.method = "min"))
  perm_tab_complete_up<-data.frame(universo, Label=1:nrow(universo))
  names(perm_tab_complete_up)[1:item]<-colnames(y)
  perm_tab_complete_up<-perm_tab_complete_up
  perm<-unique(y)
  mat.dist<-as.matrix(iw_kemenyd(x=unique(y),w=iw))
  Label <- suppressMessages(left_join(data.frame(y),
                                      data.frame(perm_tab_complete_up))[, item + 1])
  dati <- cbind(Label,x )
  dati <- dati[complete.cases(dati), ]
  attr(dati,"item")<-item
  attr(dati,"mat.dist")<-mat.dist
  attr(dati,"perm_tab_complete_up")<-perm_tab_complete_up
  attr(dati,"perm")<-perm
  attr(dati,"namecol") <- colnames(y)
  return(dati)
}

