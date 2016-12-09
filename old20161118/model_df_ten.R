# Build model and test model
# F1. a framework to fit different model and generate predicted result
group.model <- function(smart.pos,smart.neg,id.pos,id.neg,
                        group,func,rate.pos,
                        pos.tw,neg.count){
  t1 <- Sys.time()
  f <- get(func)

  # Data sample and set time window
  list[smart.pos.dedup,smart.neg.dedup] <- list(deduplcate_smart(smart.pos),deduplcate_smart(smart.neg))
  list[train,test] <- extract_train_test(smart.pos.dedup,smart.neg.dedup,id.pos,id.neg,group)
  train.frame <- limit_smart(train,pos.tw,neg.count)
  test.frame <- test
  list[train.frame,test.frame] <- change_label_tw(train,test,id.pos,id.neg,pos.tw)
  
  # Build Model and generate result
  list[trainF,testF] <- f(train.frame,test.frame)
  
  # Generate result of perforamance and lead time
  # list[pa.train,pd.train] <- gen_result(trainF,rate.pos)
  list[pa.test,pd.test] <- gen_result(testF,rate.pos)
  # list[perf.all,perf.disk] <- gen_result(testF,rate.pos)
  
  # Evaluate performance and lead time and visualize them
  # eval_result(list(perf.all,perf.disk,perf.smart),group,func)
  
  # Save data
  fn <- paste('Group',group,func,pos.tw,neg.count,sep='_')
  # save(perf.all,perf.disk,perf.smart,thre,file = file.path(dir_data,paste(fn,'.Rda',sep='')))
  
  # return data
  t2 <- Sys.time()
  elap <- difftime(t2,t1,units = 'mins')
  cat(sprintf('[%s]\t%s Done\telapsed:%0.2f mins\n',Sys.time(),fn,elap))
  list(pa.train,pd.train,pa.test,pd.test)
}

# trainF <- train.frame;testF <- test.frame

# F2.Model logic regression
logic.regression <- function(trainF,testF){
  # Train 
  trainF <- filter_sn(trainF,1)
  model <- glm(bilabel ~ .,data = trainF[,c('bilabel',col.smart)],family = 'binomial')
  
  # Predict train
  pred.train <- predict(model,trainF[,col.smart],type = 'response')
  trainF$result <- pred.train
  
  # Predict test
  pred.test <- predict(model,testF[,col.smart],type = 'response')
  testF$result <- pred.test
  
  list(trainF,testF)
}

# F3.Model Support Vector Machine
support.vector.machine <- function(trainF,testF){
  # Train 
  trainF <- filter_sn(trainF,0.3)
  model <- svm(bilabel ~ .,data = trainF[,c('bilabel',col.smart)])
  
  # Predict train
  pred.train <- predict(model,trainF[,col.smart],type = 'response')
  trainF$result <- pred.train
  
  # Predict test
  pred.test <- predict(model,testF[,col.smart],type = 'response')
  testF$result <- pred.test
  
  list(trainF,testF)
}

# # F4.Model: naive.bayes
# naive.bayes <- function(trainF,testF,group){
#   # Train 
#   trainF <- filter_sn(trainF,1)
#   trainF$bilabel <- factor(trainF$bilabel)
#   model <- naiveBayes(bilabel ~ .,data = trainF[,c('bilabel',col.smart)])
#   # Predict
#   pred <- predict(model,testF[,col.smart])
#   testF$result <- as.numeric(fct2ori(pred))
#   testF
# }
# 
# # F4.Model: hidden.markov
# hidden.markov <- function(trainF,testF,group){
#   # Train 
#   trainF <- filter_sn(trainF,1)
#   model <- naiveBayes(bilabel ~ .,data = trainF[,c('bilabel',col.smart)])
#   # Predict
#   pred <- predict(model,testF[,col.smart])
#   testF$result <- pred
#   testF
# }