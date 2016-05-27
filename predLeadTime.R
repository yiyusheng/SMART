# Lead time Predict on Tencent's dataset
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
load(file.path(dir_data,'SMARTMerge_Int_2hold.Rda'))
load(file.path(dir_data,'smartName.Rda'))

smartName <- smartName[smartName != 'Power_Cycle_Count_Value' &
                       smartName != 'Unsafe_Shutdown_Count_Value' &
                       smartName != 'PowerOnHours_Count_Value']

####################################
# S1. Label item for lead time prediction
smartL <- subset(smartInt,label == 1 & group == 'ykliu' & seqTime <= failed_time)
smartL$restTime <- as.numeric(difftime(smartL$failed_time,smartL$seqTime,tz = 'UTC',units = 'hours'))
tw = 7*24
smartL$ltLabel <- smartL$restTime < tw
smartL <- factorX(subset(smartL,restTime < tw*2))
balance <- tapply(smartL$restTime,smartL$sn,function(x)sum(x>tw)/length(x))
smartL <- factorX(subset(smartL,sn %in% names(balance)[balance > 0.4 & balance < 0.6]))

# S2. Add time point and new feature
smartL <- smartL[,c('sn','seqTime','model','failed_time','restTime','ltLabel',smartName)]

for (i in 1:length(smartName)){
  n <- smartName[i]
  tmp <- tapply(smartL[[n]],smartL$sn,function(x){
    len <- length(x)
    diff <- x[2:len] - x[1:(len-1)]
    c(0,diff)
  })
  smartL[[paste(n,'Diff',sep='')]] <- unlist(tmp)
}
smartL <- factorX(subset(smartL,model == 'ST31000524NS'))

# S3. Data Partition
smartL$ltLabel <- factor(paste('A',as.numeric(smartL$ltLabel),sep=''))
inTrain <- createDataPartition(y = smartL$ltLabel, p = .7, list = FALSE)
training <- factorX(smartL[inTrain,])
testing <- factorX(smartL[-inTrain,])

# S4. Model Training
smpA <- subset(training,ltLabel == 'A0')
smpB <- subset(training,ltLabel == 'A1')
smpB <- smpB[sample(1:nrow(smpB),min(nrow(smpA),nrow(smpB))),]
smp <- rbind(smpA,smpB)
smp <- smp[,!(names(smp) %in% c('Spin_Up_Time_ValueDiff', 
                                'Seek_Error_Rate_ValueDiff', 
                                'Offline_Uncorrectable_ValueDiff', 
                                'Current_Pending_Sector_ValueDiff',
                                'PowerOnHours_Count_ValueDiff',
                                'Current_Pending_Sector_RawDiff'))]
if(osFlag){
  idx <- sample(1:nrow(smp),min(2000,nrow(smp)))
}else{
  idx <- 1:nrow(smp)
}

colPred <- c(smartName,paste(smartName,'Diff',sep=''))

predSVM <- function(){
  svmGrid <- expand.grid(sigma = 2^c(0,1,2,3,4),
                         C = 2^c(4:9))
  
  ctrl <- trainControl(method = 'cv',number = 3,
                       classProb = T,
                       summaryFunction = twoClassSummary)
  
  svmFit <- train(x = smp[idx,7:ncol(smp)],
                  y = factor(smp$ltLabel[idx]),
                  method = 'svmRadial',
                  preProc = c('center','scale'),
                  tuneGrid = svmGrid,
                  metric = 'ROC',
                  trControl = ctrl)
  print(svmFit)
  print(varImp(svmFit))
  save(svmFit,file = file.path(dir_data,'svmFit.Rda'))
  svmFit
}

predGLM <- function(){
  glmGrid <- expand.grid(sigma = 2^c(0,1,2,3,4),
                         C = 2^c(4:9))
  
  ctrl <- trainControl(method = 'cv',number = 3,
                       classProb = T,
                       summaryFunction = twoClassSummary)
  
  glmFit <- train(x = smp[idx,7:ncol(smp)],
                  y = factor(smp$ltLabel[idx]),
                  tuneLength = 10,
                  method = 'glmStepAIC',
                  family = 'binomial',
                  metric = 'ROC',
                  trControl = ctrl)
  print(glmFit)
  print(varImp(glmFit))
  save(glmFit,file = file.path(dir_data,'glmFit.Rda'))
  glmFit
}
fit <- predGLM()
