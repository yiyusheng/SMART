# Build model and test model

# F1. a framework to fit different model and generate predicted result
group.model <- function(smart.pos,smart.neg,id.pos,id.neg,func,group,rate.pos){
  t1 <- Sys.time()
  f <- get(func)

  # Data sample and set time window
  # gen_diff(smart.pos)
  load(file.path(dir_data,'smart_lag.Rda'))
  new.pos.lag <- cbind(new.pos,r1,r2,r3)
  train.frame <- factorX(subset(new.pos.lag,sn %in% id.pos$sn[id.pos$group != group]))
  test.frame <- factorX(subset(new.pos.lag,sn %in% id.pos$sn[id.pos$group == group]))
  
  list[train.frame,test.frame] <- gen_weekly_pred(smart.pos,smart.neg,id.pos,id.neg,group)
  
  # Build Model and generate result
  # testF <- f(train.frame,test.frame)
  
  # Save data
  fn <- paste('Group',group,'_',func,sep='')
  # save(perf.all,perf.disk,perf.smart,thre,file = file.path(dir_data,paste(fn,'.Rda',sep='')))
  
  # return data
  t2 <- Sys.time()
  elap <- difftime(t2,t1,units = 'mins')
  cat(sprintf('[%s]%s Done\telapsed:%0.2f mins\n',Sys.time(),fn,elap))
}

# trainF <- train.frame;testF <- test.frame

# F2.Model logic regression
multinomial.logic.regression <- function(trainF,testF){
  # Train 
  trainF <- filter_sn(trainF,0.01)
  trainF$multilabel <- as.factor(trainF$multilabel)
  model <- multinom(multilabel ~ .,data = trainF[,c('multilabel',col.smart,col.lag)],MaxNWts = 1e6)
  # model <- nnet(multilabel ~ ., data = trainF[,c('multilabel',col.smart,col.lag)],size = 100,MaxNWts = 1e6)
  
  # predict train
  pred <- predict(model,trainF[,c(col.smart,col.lag)],'probs')
  # evaluate smart of train
  available.opt <- as.numeric(attr(pred,'dimnames')[[2]])
  pred <- available.opt[apply(pred,1,which.max)]
  trainF$result <- pred
  trainF$difflt <- abs(trainF$multilabel - trainF$result)
  trainF <- sort_col(trainF,n = c(col.smart,col.lag))
  # evaluate disk of train
  pred.disk <- list2df(tapply(trainF$difflt,trainF$sn,function(x)list(sum(x == 0)/length(x),length(x))),n = c('rate','len'))
  
  a <- subset(trainF,sn == '9QK2MLSN')
  b <- table(a$multilabel,a$result)
  # Predict
  pred <- predict(model,testF[,col.smart],type = 'response')
  testF$result <- pred
  testF
}

# F2. Logistic regression
logic.regression <- function(trainF,testF){
  # Train 
  trainF <- filter_sn(trainF,1)
  model <- glm(bilabel ~ .,data = trainF[,c('bilabel',col.smart)],family = 'binomial')
  x <- eval_train(trainF,model)
  # Predict
  pred <- predict(model,testF[,col.smart],type = 'response')
  testF$result <- pred
  testF
}

# F3.Model Support Vector Machine
support.vector.machine <- function(trainF,testF,group){
  # Train 
  trainF <- filter_sn(trainF,0.5)
  model <- svm(bilabel ~ .,data = trainF[,c('bilabel',col.smart)])
  # Predict
  pred <- predict(model,testF[,col.smart],type = 'response')
  testF$result <- pred
  testF
}

# F4.Model: naive.bayes
naive.bayes <- function(trainF,testF,group){
  # Train 
  trainF <- filter_sn(trainF,1)
  trainF$bilabel <- factor(trainF$bilabel)
  model <- naiveBayes(bilabel ~ .,data = trainF[,c('bilabel',col.smart)])
  # Predict
  pred <- predict(model,testF[,col.smart])
  testF$result <- as.numeric(fct2ori(pred))
  testF
}

# F4.Model: hidden.markov
hidden.markov <- function(trainF,testF,group){
  # Train 
  trainF <- filter_sn(trainF,1)
  model <- naiveBayes(bilabel ~ .,data = trainF[,c('bilabel',col.smart)])
  # Predict
  pred <- predict(model,testF[,col.smart])
  testF$result <- pred
  testF
}