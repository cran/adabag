#Internal functions

internal1 <- function(x,item){
  if(is.null(x)){
    return(rep(1,item))
  }
  else{ rank(x,ties.method = "min")}
}

internal2 <- function(riga, namecol, item=item){
  riga<-data.frame(t(riga))
  names(riga)<-namecol
  #1print(riga)
  return(riga)
}

itemp_IW <- function(y, offset, parms, wt) {
  # print("itemp")
  # print(environment())
  # print(parent.frame())
  # print(parent.frame(n=2))
  # print(env$item)
  #env3<<-environment()
  if (is.matrix(y) && ncol(y) > 1)
    stop("Matrix response not allowed")
  if (!missing(parms) && length(parms) > 0)
    warning("parameter argument ignored")
  if (length(offset)) y <- y - offset
  sfun <- function(yval, dev, wt, ylevel, digits ) {
    paste(" mean=", format(signif(yval, digits)),
          ", MSE=" , format(signif(dev/wt, digits)),
          sep = '')
  }
  environment(sfun) <- .GlobalEnv
  list(y = c(y), parms = NULL, numresp = 1, numy = 1, summary = sfun)
}
stemp_IW <- function(y, wt, x, parms, continuous) {
  # print("stemp_IW")
  # print(environment())
  # print(parent.frame())
  # print(parent.frame(n=2))
  # env2<<-environment()
  mat.dist <-  get("env", envir = globalenv())$mat.dist
  mat = mat.dist[y,y]
  massima_distanza<-sum(mat.dist[y,y])/2
  n <- length(y)
  if (continuous) {
    # continuous x variable
    goodness = vector("double",n-1)
    direction = vector("double",n-1)
    left.wt <- cumsum(wt)[-n]
    right.wt <- sum(wt) - left.wt

    for(i in 1:(n-1)){
      matl=mat[1:left.wt[i],1:left.wt[i]]
      if((sum(matl)/2) > massima_distanza)
      {
      }
      matr=mat[(left.wt[i]+1):n,(left.wt[i]+1):n]
      if((sum(matr)/2) >massima_distanza)
      {
      }
      if( length(1:left.wt[i]) == 1){
        lmean <- massima_distanza-sum(matl)
      }else{
        lmean <- massima_distanza-sum(matl)/2
      }

      if( length((left.wt[i]+1):n) == 1){
        rmean <- massima_distanza-sum(matr)
      }else{
        rmean <- massima_distanza-sum(matr)/2
      }

      goodness[i] <- (lmean+rmean)
      direction[i] <- sign(lmean)
    }

    list(goodness = goodness, direction = direction)
  } else {
    # Categorical X variable
    ux <- sort(unique(x))

    # For anova splits, we can order the categories by their means
    # then use the same code as for a non-categorical

    ind1 = which(x == ux[1])
    ind2 = which(x == ux[2])

    matl = mat[ind1,ind1]
    matr = mat[ind2,ind2]

    if( length(ind1) == 1){
      lmean <- sum(matl)
    }else{
      lmean <- sum(matl)/2
    }

    if( length(ind2) == 1){
      rmean <- sum(matr)
    }else{
      rmean <- sum(matr)/2
    }

    list(goodness=lmean+rmean, direction = ux)
  }

  #print(wt)
  #print(x)
}
etemp_IW <- function(y, wt, parms) {
  # print("etemp_IW")
  # print(environment())
  # print(parent.frame())
  # print(parent.frame(n=2))

  perm<- get("env", envir = globalenv())$perm
  mat.dist<- get("env", envir = globalenv())$mat.dist
  item<- get("env", envir = globalenv())$item
  iw <- get("env", envir = globalenv())$iw
  # env4 <<- environment()
  a<-100
  if(length(y)>1)
  {
    TR <- tabulaterows(perm[y,1:item])
    if(nrow(TR$X) ==1)
    {x  <- TR$X[1,]
    }
    else
    {
      CR<-iwquickcons(perm[y,1:item],iw)
      x  <- internal1(CR$Consensus[1,])
      if(is.null(x)) {x<-rep(1, item)
      names(x)<-c(paste0("V", 1:item))

      }

    }
    for(i in 1:dim(perm)[1])
      if(identical( as.numeric(perm[i,1:item]),as.numeric(x)))
      {y.median=i
      dev.good <- sum(mat.dist[y.median,y])
      a<-99
      break
      }
    if (a==100)
    {
      y.median<-00000000
      a<-perm
      names(a)<-NULL
      a<-as.matrix(a)


      mat=iw_kemenyd(x=a[y,1:item], y= x[1:item],iw)


      dev.good1 = sum(mat)/2/length(y)
      dev.good <- sum(mat)
    }
  }
  if(length(y)<2)
  {y.median=0
  dev.good=0
  }

  list(label = y.median, deviance = dev.good)
}



