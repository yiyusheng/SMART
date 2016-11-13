# Build model and test model

# F1. a framework to fit different model and generate predicted result
group.model <- function(smart.pos,smart.neg,id.pos,id.neg,
                        group,func,nw1,nw2,
                        use.neg,use.af,use.res){
  t1 <- Sys.time()
  f <- get(func)

  # Data sample and set time window
  list[smart.pos.nw,smart.neg.nw] <- gen_weekly_pred(smart.pos,smart.neg,nw1,nw2,use.neg,use.af,use.res)
  list[train,test] <- extract_train_test(smart.pos.nw,smart.neg.nw,id.pos,id.neg,group)
  list[train.frame,test.frame] <- list(train,test)
  
  # Build Model and generate result
  list[trainF,testF] <- f(train.frame,test.frame)
  list[pa.train,pd.train,p.train] <- gen_result(trainF,nw1,nw2)
  list[pa.test,pd.test,p.test] <- gen_result(testF,nw1,nw2)
  
  # Save data
  fn <- paste('Group',group,func,nw1,nw2,use.res,sep='_')
  ggsave(file=file.path(dir_data,'figure',paste(fn,'_train.jpg',sep='')),
         plot=p.train, width = 8, height = 6, dpi = 100)
  ggsave(file=file.path(dir_data,'figure',paste(fn,'_test.jpg',sep='')),
         plot=p.test, width = 8, height = 6, dpi = 100)
  save(pa.train,pd.train,p.train,pa.test,pd.test,p.test,file = file.path(dir_data,paste(fn,'.Rda',sep='')))
  
  # display
  t2 <- Sys.time()
  elap <- difftime(t2,t1,units = 'mins')
  cat(sprintf('[%s]%s\telapsed:%0.2f mins\n',Sys.time(),fn,elap))
  
  list(pa.train,p.train,pa.test,p.test)
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
  pred <- predict(model,testF[,col.smart],type = 'response')
  testF$result <- pred
  
  list(trainF,testF)
}

# F3.Model Support Vector Machine
support.vector.machine <- function(trainF,testF,group){
  # Train 
  trainF <- filter_sn(trainF,0.01)
  model <- svm(bilabel ~ .,data = trainF[,c('bilabel',col.smart)])
  
  # Predict train
  pred.train <- predict(model,trainF[,col.smart],type = 'response')
  trainF$result <- pred.train
  
  # Predict test
  pred <- predict(model,testF[,col.smart],type = 'response')
  testF$result <- pred
  
  list(trainF,testF)
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