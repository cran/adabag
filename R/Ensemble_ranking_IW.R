Ensemble_ranking_IW <- function(formula, data,iw, algo = "boosting", mfinal = 100, coeflearn = "Breiman",
                                control, bin=FALSE, trace= TRUE, ...){

  if (!(as.character(coeflearn) %in% c("Freund", "Breiman",
                                       "Zhu"))) {
    stop("coeflearn must be 'Freund', 'Breiman' or 'Zhu' ")
  }

  if (!(as.character(algo) %in% c("bagging", "boosting",
                                  "random forest"))) {
    stop("algo must be 'bagging' or 'boosting' ")
  }
  item <- attr(data,"item")
  mat.dist <- attr(data,"mat.dist")
  perm_tab_complete_up  <- attr(data,"perm_tab_complete_up")
  perm <- attr(data,"perm")
  namecol <- attr(data,"namecol")
  env <- environment()
  .GlobalEnv$env <- env

  if (is(dim(iw), "NULL")) {
    if(length(iw)!= (ncol(perm_tab_complete_up)-1)){
      stop("The dimension of the weighting vector is incorrect.")}
  }
  else{
    if(ncol(iw) !=  (ncol(perm_tab_complete_up)-1)){
      stop("The dimension of the weighting vector is incorrect.")}
  }

  data_rank<-suppressMessages(left_join(data[,1:2],
                                        perm_tab_complete_up,
                                        by = "Label")[, -c(1:2)])

  formula <- as.formula(formula)
  vardep <- data[, as.character(formula[[2]])]
  n <- length(data[, 1])
  nclases <- nlevels(as.factor(vardep))
  ylevels<-levels(as.factor(vardep))
  pesos <- rep(1/n, n)
  guardarpesos <- array(0, c(n, mfinal))
  w <- rep(1/n, n)
  data <- cbind(pesos, data)
  arboles <- list()
  pond <- rep(0, mfinal)
  iw<-iw
  maxdepth=control$maxdepth
  arboles[[1]] <-
    rpart::rpart(
      as.factor(Label) ~ .,
      data = data[,-1],
      method = list(eval = etemp_IW, split = stemp_IW, init = itemp_IW),
      control = rpart.control(cp = -1, maxdepth = maxdepth),
    )

  ###########################################################################ulist

  nvar <- dim(varImp(arboles[[1]], surrogates = FALSE, competes = FALSE))[1]
  imp <- array(0, c(mfinal, nvar))

  paste("selected method:",algo)

  if( algo=="boosting"){
    cp=control$cp
    minsplit=control$minsplit
    for (m in 1:mfinal) {
      k <- 1
      while (k == 1) {
        boostrap <- sample(1:n, replace = TRUE, prob = pesos)
        data_new<-data[boostrap, ]
        data_rank2<-data_rank[boostrap,]
        fit <- rpart::rpart(as.factor(Label)~., data = data[boostrap, -1], method=list(eval = etemp_IW, split = stemp_IW, init = itemp_IW),
                            control = rpart.control(cp = cp, maxdepth = maxdepth, minsplit=minsplit))
        fit2<-fit
        ylab=NULL
        foglie <- unique(fit2$where)
        foglie<-sort(foglie)
        #print(foglie)
        if(length(foglie)>0)
        {
          for (z in 1:length(foglie))
          {
            TR <- tabulaterows(data_rank2[which(fit2$where==foglie[z]),1:item])
            if(nrow(TR$X) ==1)
            {x  <- TR$X[1,]
            tau=1
            ylab<-rbind(ylab,x)
            }
            else
            {
              CR <- iwquickcons(data_rank2[which(fit2$where==foglie[z]),1:item],iw)
              x <- internal1(CR$Consensus[1,1:item],item)
              ylab<-rbind(ylab,x)
            }
          }}
        ylab<-data.frame(ylab)
        names(ylab)<-names(perm_tab_complete_up)[1:item]
        nodi_etich<-suppressMessages(left_join(data.frame(ylab),data.frame(perm_tab_complete_up))[,item+1])
        if(length(foglie)!=length(nodi_etich))
          stop()

        fit2$frame$yval[fit2$frame$var=="<leaf>"]<-nodi_etich
        fit$frame$yval[fit$frame$var=="<leaf>"]<-nodi_etich

        k <- length(fit2$frame$var)

      }


      flearn <- predict(fit2, newdata = data[, -1])
      Tau_pesato<-vector()

      Tau_pesato<-sapply(1:length(vardep), function(h) {ifelse((vardep[h])==(flearn[h]) ,1,iw_tau_x(data_rank[h,],perm_tab_complete_up[flearn[h],1:item],iw))})

      #print(Tau_pesato)
      #print(paste("Average weighted tau_x=", round(mean(Tau_pesato, na.rm = T),4)))
      if(trace) cat("\nn tree =", m,
                    "\nAverage weighted tau_x =",round(mean(Tau_pesato, na.rm = T),4),"\n")

      ind<-(1-(Tau_pesato+1)/2)
      err <- sum(pesos * ind, na.rm = T)
      c <- log((1 - err)/err)
      if (coeflearn == "Breiman") {
        c <- (1/2) * c
      }
      if (coeflearn == "Zhu") {
        c <- c + log(nclases - 1)
      }
      #print(c)
      guardarpesos[, m] <- pesos
      if(bin==FALSE){
        pesos <- pesos * exp(c * ind)}
      else{
        pesos <- pesos * log2(1+exp(c * ind))
      }
      pesos <- pesos/sum(pesos, na.rm = T)
      maxerror <- 0.5
      eac <- 0.001
      if (coeflearn == "Zhu") {
        maxerror <- 1 - 1/nclases
      }
      if (err >= maxerror) {
        pesos <- rep(1/n, n)
        #print(pesos)
        maxerror <- maxerror - eac
        c <- log((1 - maxerror)/maxerror)
        if (coeflearn == "Breiman") {
          c <- (1/2) * c
        }
        if (coeflearn == "Zhu") {
          c <- c + log(nclases - 1)
        }
      }
      if (err == 0) {
        pesos <- rep(1/n, n)
        #print(pesos)
        c <- log((1 - eac)/eac)
        if (coeflearn == "Breiman") {
          c <- (1/2) * c
        }
        if (coeflearn == "Zhu") {
          c <- c + log(nclases - 1)
        }
      }
      arboles[[m]] <- fit
      # print("n tree")
      # print(m)
      pond[m] <- c
      if (m == 1) {
        pred <- flearn
      }
      else {
        pred <- data.frame(pred, flearn)
      }




      if (length(fit$frame$var) > 1) {
        k <- varImp(fit, surrogates = FALSE, competes = FALSE)
        imp[m, ] <- k[sort(row.names(k)), ]
      }
      else {
        imp[m, ] <- rep(0, nvar)
      }
    }#end for boosting
  } #end if boosting

  if( algo=="bagging"){
    cp=control$cp
    minsplit=control$minsplit
    for (m in 1:mfinal) {
      k <- 1
      while (k == 1) {
        boostrap <- sample(1:n, replace = TRUE, prob = rep(1/n,n))
        data_new<-data[boostrap, ]
        data_rank2<-data_rank[boostrap,]
        fit <- rpart::rpart(as.factor(Label)~., data = data[boostrap, -1], method=list(eval = etemp_IW, split = stemp_IW, init = itemp_IW),
                            control = rpart.control( cp = cp, maxdepth = maxdepth, minsplit=minsplit))
        fit2<-fit
        ylab=NULL
        foglie <- unique(fit2$where)
        foglie<-sort(foglie)
        if(length(foglie)>0)
        {
          for (z in 1:length(foglie))
          {
            TR <- tabulaterows(data_rank2[which(fit2$where==foglie[z]),1:item])
            if(nrow(TR$X) ==1)
            {x  <- TR$X[1,]
            tau=1
            ylab<-rbind(ylab,x)
            }
            else
            {
              CR <- iwquickcons(data_rank2[which(fit2$where==foglie[z]),1:item],iw)
              x <- internal1(CR$Consensus[1,1:item],item)
              ylab<-rbind(ylab,x)
            }
          }}
        ylab<-data.frame(ylab)
        names(ylab)<-names(perm_tab_complete_up)[1:item]
        nodi_etich<-suppressMessages(left_join(data.frame(ylab),data.frame(perm_tab_complete_up))[,item+1])
        if(length(foglie)!=length(nodi_etich))
          stop()

        fit2$frame$yval[fit2$frame$var=="<leaf>"]<-nodi_etich
        fit$frame$yval[fit$frame$var=="<leaf>"]<-nodi_etich

        k <- length(fit2$frame$var)

      }


      flearn <- predict(fit2, newdata = data[, -1])
      Tau_pesato<-vector()

      Tau_pesato<-sapply(1:length(vardep), function(h) {ifelse((vardep[h])==(flearn[h]) ,1,iw_tau_x(data_rank[h,],perm_tab_complete_up[flearn[h],1:item],iw))})
      print(paste("Average weighted tau_x =", round(mean(Tau_pesato, na.rm = T),4)))
      arboles[[m]] <- fit
      pond[m] <- 1
      if (m == 1) {
        pred <- flearn
      }
      else {
        pred <- data.frame(pred, flearn)
      }


      if (length(fit$frame$var) > 1) {
        k <- varImp(fit, surrogates = FALSE, competes = FALSE)
        imp[m, ] <- k[sort(row.names(k)), ]
      }
      else {
        imp[m, ] <- rep(0, nvar)
      }
    }#end for bagging
  } #end if bagging

  imppond <- as.vector(as.vector(pond) %*% imp)
  imppond <- imppond/sum(imppond) * 100
  names(imppond) <- sort(row.names(k))

  ans <- list(formula = formula, trees = arboles, weights = pond,
              importance = imppond, args=list(item=item,mat.dist=mat.dist,
                                              perm_tab_complete_up=perm_tab_complete_up,
                                              perm=perm,namecol=namecol,iw=iw))

  mf <- model.frame(formula = formula, data = data[, -1])
  terms <- attr(mf, "terms")
  ans$terms <- terms
  ans$call <- match.call()
  class(ans) <- algo
  rm(list=c("env"),envir=.GlobalEnv)
  ans

}# end
