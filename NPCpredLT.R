# Lead time Predict on Tencent's dataset
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
#@@@ LOAD DATA @@@#
# load(file.path(dir_data,'SMARTMerge_Int_2hold.Rda'))
# load(file.path(dir_data,'SMARTMerge_Int2h.Rda'))
# load(file.path(dir_data,'NPCdfpForlt.Rda'))
#@@@ FUNCTION @###


####################################
# S1. Add predictors for lead time prediction
# load(file.path(dir_data,'SMARTprepF24h.Rda'))
# smartL <- smartF
# smartL <- smartL[,c(1:4,20,21,5:19)]
# smartName <- names(smartL)[7:21]
# for (i in 1:length(smartName)){
#   n <- smartName[i]
#   tmp <- tapply(smartL[[n]],smartL$sn,function(x){
#     len <- length(x)
#     if(len >= 2){
#       diff <- x[2:len] - x[1:(len-1)]
#       r <- c(0,diff)
#     }else{
#       r <- 0
#     }
#     r
#   })
#   smartL[[paste(n,'Diff',sep='')]] <- unlist(tmp)
# }
# smartL$restTime <- as.numeric(difftime(smartL$failed_time,smartL$seqTime,tz = 'UTC',units = 'hours'))
# smartL <- subset(smartL,label == 0 | restTime > 0)
# smartL <- smartL[order(smartL$sn,smartL$time),]
# smartL <- smartL[,c(1:6,ncol(smartL),7:(ncol(smartL) - 1))]
# snCount <- melt(table(smartL$sn))
# names(snCount) <- c('sn','count')
# snNeed <- factorX(subset(snCount,count > 10)) #modify for different hours (24h)
# smartL <- factorX(subset(smartL,sn %in% snNeed$sn))
# save(snNeed,snCount,smartL,file = file.path(dir_data,'predLeadTime_smartL24h.Rda'))

# S2. load data for lead time prediction and delete unrelated column
load(file.path(dir_data,'predLeadTime_smartL24h.Rda'))
smartL$sector <- smartL$Reallocated_Sector_Ct_Raw + smartL$Current_Pending_Sector_Raw
smartL$sectorDiff <- smartL$Reallocated_Sector_Ct_RawDiff + smartL$Current_Pending_Sector_RawDiff
smartName <- c('Raw_Read_Error_Rate_Value','Spin_Up_Time_Value',
               'Reallocated_Sector_Ct_Value','sector','Seek_Error_Rate_Value')
smartName <- c(smartName,paste(smartName,'Diff',sep=''))
smartL <- smartL[,c(names(smartL)[1:7],smartName)]
smartLbup <- smartL

# S3. label for each iterate window
smartL <- smartLbup
iterWin <- 5
iw <- seq(iterWin,30,iterWin)
labName <- paste('lab',iw,sep='')
for (i in 1:length(iw)){
  smartL[[labName[i]]] <- as.numeric(smartL$restTime <= iw[i]*24)
}

# S4. Model for each iterate window and testing
inTrain <- list()
mod <- list()
rset <- list()

inTrain <- sample(1:nrow(smartL),0.7*nrow(smartL))
training <- factorX(smartL[inTrain,])
testing <- factorX(smartL[-inTrain,])

thresTest <- function(thres,labn,predn){
  predBool <- factorX(as.numeric(tmpTest[[predn]] > thres))
  c <- confusionMatrix(predBool,tmpTest[[labn]])
  FDR <- as.numeric(c$byClass['Specificity'])*100
  FAR <- (1 - as.numeric(c$byClass['Sensitivity']))*100
  cat(sprintf('iw:%s\tThreshold:%.2f\tFDR:%.2f\tFAR:%.2f\n',predn,thres,FDR,FAR))
  r <- list(predn,thres,FDR,FAR)
}

predName <- paste('pred',iw,sep='')
tmpTest <- testing
#delete tmpTest with all 0 in labname
testDel <- apply(tmpTest[,labName],MARGIN = 1, sum)
tmpTest <- tmpTest[testDel > 0 & testDel < 6,]
library(LiblineaR)
for (i in 2:(length(iw)-1)){
  tmp <- subset(training,restTime < iw[i]*24*2 & restTime > iw[i]*24*0.1)
  tmpMod <- tmp[,c(smartName,labName[i])]
  names(tmpMod) <- c(smartName,'label')
  mod[[predName[i]]] <- LiblineaR(data = tmpMod[,smartName],target = tmpMod$label,
                            type = 0,cost = 1,epsilon = 0.1)
  
  p <- predict(mod[[predName[i]]],tmpTest[,smartName],proba = T,decisionValues = T)
  p <- p$probabilities[,2]
  
  tmpTest[[predName[i]]] <- p
  meanP <- round(mean(p),digits = 1)
  rset[[predName[i]]] <- sapply(seq(max(0,meanP-0.1),min(1,meanP+0.1),0.01),
                                thresTest,labn = labName[i],predn = predName[i])
}


####################################
# 'Reallocated_Sector_Ct_Raw','Current_Pending_Sector_Raw',,'Spin_Retry_Count_Value'
# smartName <- names(smartL)[8:37]
# smartName <- smartName[smartName != 'Power_Cycle_Count_Value' &
#                          smartName != 'Unsafe_Shutdown_Count_Value' &
#                          smartName != 'PowerOnHours_Count_Value' &
#                          smartName != 'Spin_Retry_Count_Value' &
#                          smartName != 'Udma_CRC_Error_Count_Value' &
#                          smartName != 'Power_Cycle_Count_ValueDiff' &
#                          smartName != 'Unsafe_Shutdown_Count_ValueDiff' &
#                          smartName != 'PowerOnHours_Count_ValueDiff' &
#                          smartName != 'Spin_Retry_Count_ValueDiff' &
#                          smartName != 'Udma_CRC_Error_Count_ValueDiff']
# # tmp <- subset(training,restTime < iw[i]*24*1.5 & restTime > iw[i]*24*0.5)
# tmp <- training
# tmp1 <- tmp[,c(smartName,paste('labIW',iw[i],sep=''))]
# names(tmp1) <- c(smartName,'label')
# library(LiblineaR)
# # mod <- glm(label~.,data = tmp1,family = binomial())
# mod <- LiblineaR(data = tmp1[,1:10],target = tmp1$label,
#                  type = 0,
#                  cost = 1,
#                  epsilon = 0.1)
# 
# # S5.Model testing
# p <- predict(mod,testing[,smartName],proba = T,decisionValues = T)
# p <- p$probabilities[,2]
# meanP <- round(mean(p))
# r <- sapply(seq(max(0,meanP-0.1),min(1,meanP+0.1),0.01),thresTest)

# inTrain <- createDataPartition(y = colSums(smartL[,28:ncol(smartL)]), p = .7, list = FALSE)
# p <- predict.glm(mod,tmp[,smartName],type = 'response')
# for (i in 1:length(iw)){
# for ( i in 1:1){
#   cat(sprintf('Modeling: iterate window %.0f\n',iw[i]))
#   # tmp <- subset(training,restTime < iw[i]*24*1.5 & restTime > iw[i]*24*0.5)
#   tmp <- training
#   tmp1 <- tmp[,c(smartName,paste('labIW',iw[i],sep=''))]
#   names(tmp1) <- c(smartName,'label')
#   model[[paste('iw',iw[i],sep='')]] <- glm(label~.,data = tmp1,family = binomial())
#   modelSVM[[paste('iw',iw[i],sep='')]] <- svm(label~.,data = tmp1)
# }


# p <- melt(quantileX(p))
# plot <- data.frame(xx = 0:100,
#                    yy = p)
# ggplot(plot,aes(x = xx,y = yy)) + geom_point()










# smartL$ltLabel <- smartL$restTime < tw
# # adjust
# N <- 10
# smartL <- factorX(subset(smartL,restTime < tw*N))
# balance <- tapply(smartL$restTime,smartL$sn,function(x)sum(x>tw)/length(x))
# smartL <- factorX(subset(smartL,sn %in% names(balance)[balance > 0.5]))
# 
# # S2. Add time point and new feature
# smartL <- factorX(smartL[,c('sn','model','restTime','ltLabel',smartName)])
# 
# 
# 
# # S3. Data Partition
# smartL$ltLabel <- as.numeric(smartL$ltLabel)
# inTrain <- createDataPartition(y = smartL$ltLabel, p = .7, list = FALSE)
# training <- factorX(smartL[inTrain,])
# testing <- factorX(smartL[-inTrain,])
# 
# glmFit <- glm.fit(training[,smartName],training$ltLabel,family = binomial())
# predTrain <- predict(glmFit,traing[,smartName],type = 'response')


# smartL <- factorX(subset(smartL,model == 'ST31000524NS'))

# # S3. Data Partition
# smartL$ltLabel <- factor(paste('A',as.numeric(smartL$ltLabel),sep=''))
# inTrain <- createDataPartition(y = smartL$ltLabel, p = .7, list = FALSE)
# training <- factorX(smartL[inTrain,])
# testing <- factorX(smartL[-inTrain,])
# 
# # S4. Model Training
# smpA <- subset(training,ltLabel == 'A0')
# smpB <- subset(training,ltLabel == 'A1')
# smpB <- smpB[sample(1:nrow(smpB),min(nrow(smpA),nrow(smpB))),]
# smp <- rbind(smpA,smpB)
# smp <- smp[,!(names(smp) %in% c('Spin_Up_Time_ValueDiff', 
#                                 'Seek_Error_Rate_ValueDiff', 
#                                 'Offline_Uncorrectable_ValueDiff', 
#                                 'Current_Pending_Sector_ValueDiff',
#                                 'PowerOnHours_Count_ValueDiff',
#                                 'Current_Pending_Sector_RawDiff'))]
# if(osFlag){
#   idx <- sample(1:nrow(smp),min(2000,nrow(smp)))
# }else{
#   idx <- 1:nrow(smp)
# }
# 
# colPred <- c(smartName,paste(smartName,'Diff',sep=''))
# 
# predSVM <- function(){
#   svmGrid <- expand.grid(sigma = 2^c(0,1,2,3,4),
#                          C = 2^c(4:9))
#   
#   ctrl <- trainControl(method = 'cv',number = 3,
#                        classProb = T,
#                        summaryFunction = twoClassSummary)
#   
#   svmFit <- train(x = smp[idx,7:ncol(smp)],
#                   y = factor(smp$ltLabel[idx]),
#                   method = 'svmRadial',
#                   preProc = c('center','scale'),
#                   tuneGrid = svmGrid,
#                   metric = 'ROC',
#                   trControl = ctrl)
#   print(svmFit)
#   print(varImp(svmFit))
#   save(svmFit,file = file.path(dir_data,'svmFit.Rda'))
#   svmFit
# }
# 
# predGLM <- function(){
#   glmGrid <- expand.grid(sigma = 2^c(0,1,2,3,4),
#                          C = 2^c(4:9))
#   
#   ctrl <- trainControl(method = 'cv',number = 3,
#                        classProb = T,
#                        summaryFunction = twoClassSummary)
#   
#   glmFit <- train(x = smp[idx,7:ncol(smp)],
#                   y = factor(smp$ltLabel[idx]),
#                   tuneLength = 10,
#                   method = 'glmStepAIC',
#                   family = 'binomial',
#                   metric = 'ROC',
#                   trControl = ctrl)
#   print(glmFit)
#   print(varImp(glmFit))
#   save(glmFit,file = file.path(dir_data,'glmFit.Rda'))
#   glmFit
# }
# fit <- predGLM()
