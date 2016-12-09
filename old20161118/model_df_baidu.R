# group.model for disk failure prediction for baidu's dataset
group.model <- function(smart.pos,smart.neg,id.pos,id.neg,func,
                        collect.time = 5,group = 1,
                        pos.tw = 3,neg.count = 20,
                        units.perday = 24,dir_plot = 'pdf_lt_baidu'){
  t1 <- Sys.time()
  f <- get(func)
  fn <- paste(func,group,collect.time,pos.tw,neg.count,sep='_')
  
  # Filter data to simulate collect time
  smart.pos <- subset(smart.pos,dist.fail <= collect.time*units.perday)
  smart.neg <- subset(smart.neg,dist.fail <= collect.time*units.perday)
  
  # Generate train and test based on group
  list[train,test] <- extract_train_test(smart.pos,smart.neg,id.pos,id.neg,group)
  
  # limit number of smart for training(time window,neg.count) and testing(neg.count)
  train <- limit_smart(train,pos.tw*units.perday,neg.count)
  test <- limit_smart(test,1e5,neg.count)
  
  # Model building and prediction
  list[trainF,testF] <- f(train,test,'bilabel')
  
  # Generate result for testF
  list[pa.test,pd.test] <- gen_result(testF)
  
  # plot of pdf of lead time and save
  pl.test <- plot.pdf_leadTime(pd.test,units.perday)
  ggsave(file=file.path(dir_data,'figure',dir_plot,paste(fn,'_test.jpg',sep='')),
         plot=pl.test, width = 8, height = 6, dpi = 100)
  
  # select the best performance and give a overview
  pd.test.TP <- subset(pd.test,overThre == 1& bilabel == 1)
  pa.test.best <- pa.test[which.max(pa.test$F1),]
  pa.test.best <- list(pa.test.best$FDR,pa.test.best$FAR,pa.test.best$F1,pa.test.best$thre,
                   nrow(pd.test.TP[pd.test.TP$pos.rate == 1,])/nrow(pd.test.TP),
                   func,collect.time,group,pos.tw,neg.count)
  

  # timing and return data
  t2 <- Sys.time()
  elap <- difftime(t2,t1,units = 'mins')
  cat(sprintf('[%s]\t%s Done\telapsed:%0.2f mins\n',Sys.time(),fn,elap))
  list(pat = pa.test,patb = pa.test.best,
       pdt = pd.test,plt = pl.test)
}


# F2.Model logic regression
logic.regression <- function(trainF,testF,attr_label){
  # locate label
  trainF <- change_name(trainF,attr_label,'label')
  testF <- change_name(testF,attr_label,'label')
  
  # Train 
  trainF <- filter_sn(trainF,1)
  model <- glm(label ~ .,data = trainF[,c('label',col.smart)],family = 'binomial')
  
  # Predict train
  pred.train <- predict(model,trainF[,col.smart],type = 'response')
  trainF$result <- pred.train
  
  # Predict test
  pred.test <- predict(model,testF[,col.smart],type = 'response')
  testF$result <- pred.test
  
  # recovery label
  trainF <- change_name(trainF,'label',attr_label)
  testF <- change_name(testF,'label',attr_label)
  
  list(trainF,testF)
}

# F3.Model Support Vector Machine
support.vector.machine <- function(trainF,testF){
  # locate label
  trainF <- change_name(trainF,attr_label,'label')
  testF <- change_name(testF,attr_label,'label')
  
  # Train 
  trainF <- filter_sn(trainF,0.3)
  model <- svm(label ~ .,data = trainF[,c('label',col.smart)])
  
  # Predict train
  pred.train <- predict(model,trainF[,col.smart],type = 'response')
  trainF$result <- pred.train
  
  # Predict test
  pred.test <- predict(model,testF[,col.smart],type = 'response')
  testF$result <- pred.test
  
  # recovery label
  trainF <- change_name(trainF,'label',attr_label)
  testF <- change_name(testF,'label',attr_label)
  list(trainF,testF)
}
