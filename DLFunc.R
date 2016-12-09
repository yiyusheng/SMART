
# F1. convert pred to binary
bipred <- function(pred,quan = 0.5){
  median.pred <- quantile(pred,quan)
  pred[pred > median.pred] <- 1
  pred[pred <= median.pred] <- 0
  pred
}

# F2A. convert result of observation to disk and generate FAR,FDR and acc for type1
cost_type1 <- function(sn,label,pred){
  data <- data.frame(sn = factor(sn),lb = label,pd = pred)
  
  predDisk <- data.frame(sn = levels(data$sn),
                         pred = as.numeric(tapply(data$pd,data$sn,function(x)any(x == 1))),
                         label = 0)
  predDisk$label[predDisk$sn %in% id.pos$sn] <- 1
  
  return(base_eval(predDisk$label,predDisk$pred))
}

# F2B.convert result of observation to disk and generate cost for type3. For type3, FAR and FDR is not enough to evalute the model
cost_type3 <- function(sn,label,pred){
  tmp <- data.frame(sn = sn,lb = label,pd = pred)
  
  cost <- list2df(by(tmp,tmp$sn,function(df){
    if(all(df$lb == 1)){
      return(all(df$pd[df$lb == 1] == 0))
    }else if(all(df$lb == 0)){
      return(any(df$pd[df$lb == 0] == 1))
    }else{
      return(all(df$pd[df$lb == 1] == 0) + any(df$pd[df$lb == 0] == 1))
    }
  }),n = c('cost','sn'))
  
  return(mean(cost$cost))
}

# F3.generate custom loss function of disk for mxnet
disk_clr <- mx.metric.custom('disk_clr',function(label,pred){
  pred <- bipred(t(pred))

  if(type == 'type1'){
    be <- cost_type1(train$sn,label,pred)
    cost <- be$acc
    append(costSet,list(be$far,be$fdr,be$acc))
  }else if(type == 'type3'){
    cost <- cost_type3(train$sn,label,pred)
    append(costSet,cost)
  }
  append(costSet,cost)
  return(cost)
})

# F4. generate custom loss function of observation to calculate the accuracy
obsv_clr <- mx.metric.custom('obsv_clr',function(label,pred){
  pred <- bipred(t(pred))
  
  be_obsv <- base_eval(label,pred)
  if(type == 'type1'){
    be_disk <- cost_type1(train$sn,label,pred)
  }
  append(costSet,list(be_obsv$far,be_obsv$fdr,be_obsv$acc,be_disk$far,be_disk$fdr,be_disk$acc))
  
  return(mean(pred == label))
})

# F4.normalize data and seperate data and label
nor_sep <- function(train,test){
  train$class <- 'train'
  test$class <- 'test'
  data <- rbind(train,test)
  data[,col.smart] <- normalize(data[,col.smart])
  train <- factorX(subset(data,class == 'train'))
  test <- factorX(subset(data,class == 'test'))
  train$class <- NULL;test$class <- NULL
  
  train_data <- as.matrix(train[,col.smart])
  train_label <- as.array(train$bilabel)
  test_data <- as.matrix(test[,col.smart])
  test_label <- as.array(test$bilabel)
  list(train_data,train_label,test_data,test_label)
}
