# Disk Failure Predict on Tencent's dataset
rm(list = ls())
#@@@ CONFIGURE @@@#
source(file.path('D:/Git/SMART','SMARTConfig.R'))
library(caret)
library(e1071)

#@@@ Function @@@#
source('D:/Git/R_Function/Rfun.R')
source(file.path(dir_code,'SMARTFunc.R'))

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'smartMerge_smart.Rda'))

####################################
# # S1. Filter failed item whose time is far behind failed_time
# # smartM <- smartM[,c(2:5,1,6:22)]
# smartP <- smartM
# smartP <- subset(smartP,group == 'ykliu' | label == 0)  #remove failed disk from york and use exclusively use ykliu's failed disk
# smartP$Udma_CRC_Error_Count_Value <- NULL
# smartP$Spin_Retry_Count_Value <- NULL
# smartP <- subset(smartP,label == 0|difftime(failed_time,time,units = 'days') >= -3)
# smartP <- smartP[order(smartP$sn,smartP$time),]
# smartP <- factorX(smartP)
# smartName <- names(smartP[,5:17])
# save(smartName,smartP,file = file.path(dir_data,'predTencent_smartP.Rda'))
load(file.path(dir_data,'predTencent_smartP.Rda'))

# S2. Add time point
t1 <- unlist(tapply(smartP$sn,smartP$sn,function(x)(1:length(x))/length(x)))
t2 <- unlist(tapply(smartP$sn,smartP$sn,function(x)1:length(x)))
t3 <- unlist(tapply(smartP$sn,smartP$sn,function(x)length(x):1))
smartP$timeP1 <- t1 # standard time point [0,1]
smartP$timeP2 <- t2 # ordered time point
smartP$timeP3 <- t3 # descend ordered time point
smartP <- smartP[,c('sn','label','timeP1','timeP2','timeP3',smartName)]

# S3. Extract sn and label for data partition
snLabel <- data.frame(sn = levels(smartP$sn),
                      label = as.numeric(tapply(smartP$label,smartP$sn,function(x)x[1])))

# S4. Predict and testing
smartPred <- function(smartP,tw = 12){
  p1 <- proc.time()
  # 4.1 Data partition with time window
  inTrain <- createDataPartition(y = snLabel$label, p = .7, list = FALSE)
  training <- factorX(subset(smartP, sn %in% snLabel$sn[inTrain] & timeP3 <= tw))
  testing <- factorX(subset(smartP, sn %in% snLabel$sn[-inTrain] & timeP3 <= tw))
  
  # 4.2 Model Training
  smp <- sample(1:nrow(training),min(100000,nrow(training)))
  mod <- svm(training[smp,smartName],training$label[smp],
             type = 'C', kernel = 'radial', gamma = 0.1, cost = 10)
  predsmp <- predict(mod,training[smp,smartName])
  Rsmp <- table(training$label[smp],predsmp)
  
  # 4.3 Model Testing for items
  pred <- predict(mod,testing[,smartName])
  testing$pred <- pred
  
  # 4.4 Map testing result to disk
  predDisk <- data.frame(sn = levels(testing$sn),
                         pred = as.numeric(tapply(testing$pred,testing$sn,function(x){
                           sum(x == 1)/length(x) > 0})))
  predDisk$label <- snLabel$label[-inTrain]
  predDisk$correct <- predDisk$label == predDisk$pred
  a <- testing[testing$sn %in% predDisk$sn[predDisk$correct == F],]
  
  
  r <- table(predDisk[,c('pred','label')])
  FDR <- r[2,2]/(r[2,2] + r[1,2])
  FAR <- r[2,1]/(r[2,1] + r[1,1])
  r
  
  # # 4.5 Result visualization
  predVisual <- factorX(testing[testing$sn %in% snLabel$sn[snLabel$label == 1 | testing$pred == 1],
                                c('sn','timeP1','pred')])
  posInfo <- data.frame(sn = levels(predVisual$sn),
                        posRate = as.numeric(tapply(predVisual$pred,predVisual$sn,
                                                    function(x)sum(x == 1)/length(x))),
                        firstTime = as.numeric(by(predVisual[c('timeP1','pred')],predVisual$sn,
                                                  function(x){
                                                    if (any(x$pred == 1))
                                                      min(x$timeP1[x$pred == 1])
                                                    else 1
                                                  })))
  posInfo <- posInfo[order(posInfo$posRate,posInfo$firstTime),]
  predVisual$sn <- factor(predVisual$sn,levels = posInfo$sn)
  p <- ggplot(predVisual,aes(x = timeP1,y = factor(sn),color = pred)) + geom_point(size = 0.1) +
    xlab('time to failure (1 = failure)') + ylab('SN')
  ggsave(p,file = file.path(dir_data,'Figure','LeadTimeDist_tw_Tencent',paste('tw',tw,'.jpg',sep='')),
         width = 8,height = 6,dpi = 100)
  p2 <- proc.time()
  
  # 4.6 Result print
  print(sprintf('tw:%.0f FDR:%.3f FAR:%.3f time:%fs',tw,FDR*100,FAR*100,p2[3]-p1[3]))
}
sapply(seq(12,192,12),function(x)smartPred(smartP,x))
