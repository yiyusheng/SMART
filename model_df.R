# Build model and test model

# F1. a framework to fit different model and generate predicted result
group.model <- function(smart.pos,smart.neg,id.pos,id.neg,group,tw,func,rate.pos){
  t1 <- Sys.time()
  f <- get(func)

  # Data sample and set time window
  list[train,test] <- extract_train_test(smart.pos,smart.neg,id.pos,id.neg,group)
  list[train.frame,test.frame] <- change_label_tw(train,test,id.pos,id.neg,tw)
  
  # Build Model and generate result
  testF <- f(train.frame,test.frame)
  
  # Generate result of perforamance and lead time
  list[perf.all,perf.disk,perf.smart,thre] <- gen_result(testF,rate.pos)
  
  # Evaluate performance and lead time and visualize them
  # eval_result(list(perf.all,perf.disk,perf.smart),group,func)
  
  # Save data
  fn <- paste('Group',group,'_',func,sep='')
  save(perf.all,perf.disk,perf.smart,thre,file = file.path(dir_data,paste(fn,'.Rda',sep='')))
  
  # return data
  t2 <- Sys.time()
  elap <- difftime(t2,t1,units = 'mins')
  cat(sprintf('[%s]\t%s Done\telapsed:%0.2f mins\n',Sys.time(),fn,elap))
  list(perf.all,perf.disk,perf.smart)
}

# trainF <- train.frame;testF <- test.frame

# F2.Model logic regression
logic.regression <- function(trainF,testF){
  # Train 
  trainF <- filter_sn(trainF,1)
  model <- glm(bilabel ~ .,data = trainF[,c('bilabel',col.smart)],family = 'binomial')
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