# Disk Failure Predict on Tencent's dataset
rm(list = ls())
#@@@ CONFIGURE @@@#
osFlag = Sys.info()[1] == 'Windows'
if (osFlag){
  source('configWindows.R')
}else{
  source('configLinux.R')
  library(doMC)
  registerDoMC(cores = 30)
}

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'SMARTMerge_Int_2h.Rda'))
load(file.path(dir_data,'smartName.Rda'))

smartName <- smartName[smartName != 'Power_Cycle_Count_Value' &
                         smartName != 'Unsafe_Shutdown_Count_Value' &
                         smartName != 'PowerOnHours_Count_Value']

####################################
# S1. Label item for lead time prediction
# smartL <- subset(smartInt,(label == 1 & group == 'ykliu') | (label == 0 & group == 'york'))
smartL <- rbind(smartF,smartN)
smartL$restTime <- as.numeric(difftime(smartL$failed_time,smartL$time,tz = 'UTC',units = 'hours'))
smartL$restTime[smartL$label == 0] <- -10000
smartL$label <- paste('L',smartL$label,sep='')
smartL <- factorX(smartL[,c('sn','model','label','restTime',smartName)])

# S2. Extract sn and label for data partition
snLabel <- data.frame(sn = levels(smartL$sn),
                      label = as.character(tapply(smartL$label,smartL$sn,function(x)x[1])))

# S3. Predict and testing
smartLred <- function(smartL,tw = 12){
  p1 <- proc.time()
  # 3.1 Data partition with time window
  inTrain <- createDataPartition(y = snLabel$label, p = .7, list = FALSE)
  training <- factorX(subset(smartL, sn %in% snLabel$sn[inTrain] & restTime <= tw))
  smp <- sample(1:nrow(training),min(10000,nrow(training)))
  testing <- factorX(subset(smartL, sn %in% snLabel$sn[-inTrain]))
  # testing <- factorX(subset(smartL, sn %in% snLabel$sn[-inTrain] & restTime <= tw))
  
  # 3.2.1 Model Training
  mod <- svm(training[smp,smartName],training$label[smp],
             type = 'C', kernel = 'radial', gamma = 0.1, cost = 10)
  
  # 3.2.2 Model Training via caret
  svmGrid <- expand.grid(sigma = 2^c(-3:3),
                         C = 2^c(0:6))
  
  ctrl <- trainControl(method = 'cv',number = 3,
                       classProb = T,
                       summaryFunction = twoClassSummary)
  
  svmFit <- train(x = training[smp,5:ncol(training)],
                  y = factor(training$label[smp]),
                  method = 'svmRadial',
                  preProc = c('center','scale'),
                  tuneGrid = svmGrid,
                  metric = 'ROC',
                  trControl = ctrl)
  print(svmFit)
  print(varImp(svmFit))
  save(svmFit,file = file.path(dir_data,'svmFit.Rda'))
  svmFit
  
  # 3.3 Model Testing for items
  pred <- predict(mod,testing[,smartName])
  testing$pred <- pred
  
  # 3.4 Map testing result to disk
  staPred <- by(testing,testing$sn,function(x){
    leadTime <- x$restTime[min(which(x$pred == 1))]
    posRate <- sum(x == 1)/length(x)
    pred <- as.numeric(posRate > 0)
    list(as.character(x$sn[1]),leadTime,posRate,pred)
  })
  
  predDisk <- data.frame(matrix(unlist(staPred),byrow = T,nrow = nrow(staPred)))
  names(predDisk) <- c('sn','leadTime','posRate','pred')
  predDisk$label <- snLabel$label[-inTrain]
  c <- confusionMatrix(predDisk$pred,predDisk$label)
  FDR <- c$byClass['Specificity']
  FAR <- 1 - c$byClass['Sensitivity']
  
  p2 <- proc.time()
  
  # 3.6 Result print
  print(sprintf('tw:%.0f FDR:%.3f FAR:%.3f time:%fs',tw,FDR*100,FAR*100,p2[3]-p1[3]))
}
sapply(seq(12,192,12),function(x)smartLred(smartL,x))
