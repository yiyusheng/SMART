# Build model and test model

# F1. a framework to fit different model and generate predicted result
group.model <- function(smart.pos,smart.neg,id.pos,id.neg,
                        group,func,collect.time = 10,units.perday = 24,
                        nw1 = 3,nw2 = 7,pos.tw = 1e3,neg.count = 5,
                        use.neg = 1,use.af = 1,use.res = 1){
  t1 <- Sys.time()
  f <- get(func)
  fn <- paste(func,group,collect.time,units.perday,
              nw1,nw2,pos.tw,neg.count,sep='_')
  
  # Filter data to simulate collect time
  smart.pos <- subset(smart.pos,dist.fail <= collect.time*units.perday)
  smart.neg <- subset(smart.neg,dist.fail <= collect.time*units.perday)

  # Data sample and set time window
  list[smart.pos,smart.neg] <- gen_weekly_pred(smart.pos,smart.neg,nw1,nw2,use.neg,use.af,use.res)
  list[train,test] <- extract_train_test(smart.pos,smart.neg,id.pos,id.neg,group)
  
  train <- limit_smart(train,pos.tw,neg.count)
  test <- limit_smart(test,1e5,neg.count)
  
  # Build Model and generate result
  list[trainF,testF] <- f(train,test)
  # list[pa.train,pd.train,pdm.train] <- gen_result(trainF,nw1)
  list[pa.test,pp.test] <- gen_result(testF,nw1,nw2)
  
  # display
  t2 <- Sys.time()
  elap <- difftime(t2,t1,units = 'mins')
  cat(sprintf('[%s]%s\telapsed:%0.2f mins\n',Sys.time(),fn,elap))
  
  list(pa.test,pp.test)
}

# trainF <- train.frame;testF <- test.frame

# F2.Model logic regression
logic.regression <- function(trainF,testF){
  # Train 
  trainF <- filter_sn(trainF,1)
  model <- glm(weeklabel ~ .,data = trainF[,c('weeklabel',col.smart)],family = 'binomial')
  
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
  model <- svm(weeklabel ~ .,data = trainF[,c('weeklabel',col.smart)])
  
  # Predict train
  pred.train <- predict(model,trainF[,col.smart],type = 'response')
  trainF$result <- pred.train
  
  # Predict test
  pred.test <- predict(model,testF[,col.smart],type = 'response')
  testF$result <- pred.test
  
  list(trainF,testF)
}

# F4.Model: naive.bayes
# naive.bayes <- function(trainF,testF,group){
#   # Train 
#   trainF <- filter_sn(trainF,1)
#   trainF$bilabel <- factor(trainF$bilabel)
#   model <- naiveBayes(bilabel ~ .,data = trainF[,c('bilabel',col.smart)])
#   # Predict
#   pred <- predict(model,testF[,col.smart])
#   testF$result <- as.numeric(fct2ori(pred))
#   testF
#   
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