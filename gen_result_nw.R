# Generate result for test

# F1. evaluate of prediction performance
gen_result_thre_perf <- function(df,need.result = 0){
  result.pred <- melt(tapply(df$pred,df$sn,function(x)as.numeric(any(x == 1))))
  names(result.pred) <- c('sn','overThre')
  result.pred$bilabel <- 0
  result.pred$bilabel[result.pred$sn %in% id.pos$sn] <- 1
  
  TP <- sum((result.pred$bilabel == 1 & result.pred$overThre == 1))
  TN <- sum((result.pred$bilabel == 0 & result.pred$overThre == 0))
  FP <- sum((result.pred$bilabel == 0 & result.pred$overThre == 1))
  FN <- sum((result.pred$bilabel == 1 & result.pred$overThre == 0))
  FDR = TP/(TP + FN)
  FAR = FP/(TN + FP)
  
  ifelse(need.result == 0,return(list(TP,TN,FP,FN,FDR,FAR)),return(list(TP,TN,FP,FN,FDR,FAR,result.pred)))
}

# F2. generate lead time.
gen_result_thre_leadTime <- function(df,lt.order = 1){
  tmp2 <- by(df,df$sn,function(x){
    tmp <- subset(x,pred == 1)
    if(nrow(tmp) == 0)
      return(rep(-1,(length(lt.order) + 5)))
    else{
      tmp1 <- sort(tmp$dist.fail,decreasing = T)
      lt.order[lt.order > length(tmp1)] <- length(tmp1)
      return(c(tmp1[lt.order],nrow(x),nrow(tmp),
               max(tmp$result),mean(tmp$result),min(tmp$result)))
    }
  })
  do.call(rbind,tmp2)
}

# F3. evaluate chaos region
gen_result_thre_chaos <- function(df){
  by(df[,c('sn','pred','dist.fail')],df$sn,function(x){
    max.pos <- max(x$dist.fail[x$pred == 1],-1)
    min.neg <- min(x$dist.fail[x$pred == 0],1e10)
    
    tmp <- data.frame(exist.chaos = 0,len.chaos = 0,
                      num.chaos.pos = 0,num.chaos.neg = 0,
                      rate.chaos.pos = 0)
    if(max.pos == -1){
      return(tmp)
    }else if(abs(min.neg) == 1e10){
      tmp$rate.chaos.pos <- 1
    }else if(max.pos < min.neg){
      tmp$rate.chaos.pos <- -1
    }else {
      tmp1 <- subset(x,dist.fail >= min.neg & dist.fail <= max.pos)
      tmp$exist.chaos <- 1
      tmp$len.chaos <- max.pos - min.neg
      tmp$num.chaos.neg <- nrow(tmp1[tmp1$pred == 0,])
      tmp$num.chaos.pos <- nrow(tmp1[tmp1$pred == 1,])
      tmp$rate.chaos.pos <- sum(tmp1$pred)/nrow(tmp1)
    }
    tmp
  })
}

# Generate the threSet for performance test
gen_threSet_onresult <- function(df,num = 1000){
  tmp <- sort(unique(df$result))
  idx <- seq(1,length(tmp),length.out = min(length(tmp),num))
  thre.predict <- tmp[idx]
}

# F4. threshold evaluation
gen_result_thre <- function(df,rate.pos = NULL,thre.predict = NULL,thre.lt = NULL){
  
  # generate thre.predict by roc of smart prediction
  if(is.null(thre.predict)){
    thre.predict <- gen_threSet_onresult(df,1000)
  }
  
  # For all
  perf.all <- lapply(thre.predict,function(x){
    df$pred <- as.numeric(df$result > x)
    gen_result_thre_perf(df,0)
  })
  perf.all <- data.frame(matrix(unlist(perf.all),byrow = T,nrow = length(perf.all)))
  names(perf.all) <- c('TP','TN','FP','FN','FDR','FAR')
  if(is.null(rate.pos)){
    perf.all$F <- perf.all$FP + perf.all$FN
  }else{
    perf.all$F <- perf.all$FP + perf.all$FN*(1-rate.pos)/rate.pos
  }
  
  perf.all$thre <- thre.predict
  
  # choose best thre.lt via accuracy
  if(is.null(thre.lt)){
    thre.lt <- perf.all$thre[which(perf.all$F == min(perf.all$F))[1]]
  }

  # Lead time for disk
  df$pred <- as.numeric(df$result > thre.lt)
  list[x,x,x,x,x,x,result.pred] <- gen_result_thre_perf(df,1)
  lt.order <- seq(1)
  tmp.lt <- data.frame(gen_result_thre_leadTime(df,lt.order))
  names(tmp.lt) <- c('leadTime','num.After.pos','num.pos','max.pred','mean.pred','min.pred')
  result.pred <- cbind(result.pred,tmp.lt)
  
  # Chaos For disk
  chaos <- gen_result_thre_chaos(df)
  chaos <- do.call(rbind,chaos)
  perf.disk <- cbind(result.pred,chaos)
  
  # Generate return
  perf.disk$thre <- thre.lt
  df$thre <- thre.lt
  
  list(perf.all = perf.all,
       perf.disk = perf.disk,
       perf.smart = df,
       thre = thre.lt)
}

# F5.result evaluation
gen_result <- function(df,rate.pos = NULL,thre.predict = NULL,thre = NULL){
  df <- df[,-which(names(df) %in% col.smart)]
  df.ahead_of_fail <- factorX(subset(df,dist.fail > 0))
  tmp <- gen_result_thre(df.ahead_of_fail,rate.pos,thre.predict,thre)
}

