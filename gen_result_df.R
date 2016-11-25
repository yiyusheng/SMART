# Generate result for test

# F1. evaluate of prediction performance
gen_result_thre_perf <- function(df,need.result = 0){
  result.pred <- melt(tapply(df$pred,df$sn,function(x)as.numeric(any(x == 1))))
  names(result.pred) <- c('sn','overThre')
  result.pred$bilabel <- 0
  result.pred$bilabel[result.pred$sn %in% id.pos$sn] <- 1
  be <- base_eval(result.pred$bilabel,result.pred$overThre)
  ifelse(need.result == 0,return(be),return(list(a = be,b = result.pred)))
}

# F2. generate lead time.
gen_result_thre_leadTime <- function(df,lt.order = 1){
  tmp2 <- by(df,df$sn,function(x){
    tmp <- subset(x,pred == 1)
    if(nrow(tmp) == 0)
      return(rep(-1,2))
    else{
      return(c(max(tmp$dist.fail),nrow(tmp)/nrow(x)))
    }
  })
  list2df(as.list(tmp2),n = c('leadTime','pos.rate','sn'))
}


# F3. threshold evaluation
gen_result_thre <- function(df){
  
  # generate thre.predict by roc of smart prediction
  thre.predict <- as.numeric(quantile(df$result,seq(0,1,0.001)))

  
  # For all
  perf.all <- lapply(thre.predict,function(x){
    df$pred <- as.numeric(df$result > x)
    gen_result_thre_perf(df,0)
  })
  perf.all <- list2df(perf.all,n = c('TP','FN','TN','FP','P','N','FDR','FAR','acc','F1'))
  perf.all$thre <- thre.predict
  
  # choose best thre.lt via accuracy
  thre.lt <- perf.all$thre[which.max(perf.all$F1)[1]]

  # Lead time for disk
  df$pred <- as.numeric(df$result > thre.lt)
  list[x,perf.disk] <- gen_result_thre_perf(df,1)
  tmp.lt <- gen_result_thre_leadTime(df)
  perf.disk <- merge(perf.disk,tmp.lt,by = 'sn')
  
  # Generate return
  perf.disk$thre <- thre.lt
  df$thre <- thre.lt
  list(pa = perf.all,pd = perf.disk)
}

# F4.result evaluation
gen_result <- function(df){
  df <- df[,-which(names(df) %in% col.smart)]
  df.ahead_of_fail <- factorX(subset(df,dist.fail > 0))
  tmp <- gen_result_thre(df.ahead_of_fail)
}

