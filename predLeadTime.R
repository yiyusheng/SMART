# Disk Failure Predict on Tencent's dataset
rm(list = ls())
#@@@ CONFIGURE @@@#
source(paste('config',Sys.info()[1],'.R',sep=''))

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'SMARTMerge_Int_2h.Rda'))
load(file.path(dir_data,'smartName.Rda'))
smartName <- smartName[smartName != 'Power_Cycle_Count_Value' &  smartName != 'Unsafe_Shutdown_Count_Value']
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
colPred <- c(smartName,paste(smartName,'Diff',sep=''))
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
                                'Current_Pending_Sector_ValueDiff'))]
idx <- sample(1:nrow(smp),min(1000,nrow(smp)))

ctrl <- trainControl(method = 'cv',
                     classProb = T,
                     summaryFunction = twoClassSummary)
svmGrid <- expand.grid(sigma = 2^c(-30,-20,-10,-5,-1,0,1,2),C = 2^c(0:5))

svmFit <- train(x = smp[idx,colPred],
                y = factor(smp$ltLabel[idx]),
                data = training,
                method = 'svmRadial',
                preProc = c('center','scale'),
                tuneGrid = svmGrid,
                metric = 'ROC',
                trControl = ctrl)





# load(file.path(dir_data,'predTencent_smartL.Rda'))
# load(file.path(dir_data,'ykliu_smart.Rda'))
# t1 <- unlist(tapply(smartL$sn,smartL$sn,function(x)(1:length(x))/length(x)))
# t2 <- unlist(tapply(smartL$sn,smartL$sn,function(x)1:length(x)))
# t3 <- unlist(tapply(smartL$sn,smartL$sn,function(x)length(x):1))
# smartL$timeP1 <- t1 # standard time point [0,1]
# smartL$timeP2 <- t2 # ordered time point
# smartL$timeP3 <- t3 # descend ordered time point
# mod <- svm(smp[idx,colPred],smp$ltLabel[idx],type = 'C', 
#            kernel = c('linear','polynomial','radial','sigmoid')[3], 
#            gamma = 0.1, cost = 1)

# S5. A novel matric to evaluate prediction performance
# trainingA <- training
# trainingA$pred <- predict(mod,trainingA[,colPred])
# table(training$ltLabel,trainingA$pred)
# 
# majority <- function(data){
#   A <- subset(data,restTime > tw)
#   B <- subset(data,restTime <= tw)
#   m1 <- sum(A$pred == T)/nrow(A)
#   m2 <- sum(B$pred == T)/nrow(B)
#   list(m1,m2)
# }
# p1 <- by(trainingA[,c('sn','restTime','pred')],trainingA$sn,majority)
# predCDF <- data.frame(sn = levels(trainingA$sn),
#                       matrix(unlist(p1),byrow = T,nrow = length(p1)))
# names(predCDF) <- c('sn','m1','m2')
# predCDF <- subset(predCDF,!is.na(m1) & !is.na(m2))
# plot <- ggplot(predCDF,aes(m2)) + stat_ecdf() + stat_ecdf(aes(predCDF$m1))
# print(plot)

# S6. Model Testing for items
# pred <- predict(mod,testing[,smartName])
# testing$pred <- pred
# Rtest <- table(testing$ltLabel,testing$pred)
# FDR <- Rtest[2,2]/(Rtest[2,2] + Rtest[1,2])
# FAR <- Rtest[2,1]/(Rtest[2,1] + Rtest[1,1])