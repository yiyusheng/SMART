# Disk Failure Predict on Tencent's dataset
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')

#@@@ LOAD DATA @@@#
#consist of 300k normal disk from york and 2100 bad disk from ykliu
load(file.path(dir_data,'SMARTMerge_Int2h.Rda'))  
smartName <- names(smartN)[5:19]
smartName <- smartName[smartName != 'Power_Cycle_Count_Value' &
                         smartName != 'Unsafe_Shutdown_Count_Value' &
                         smartName != 'PowerOnHours_Count_Value' &
                         smartName != 'Spin_Retry_Count_Value' &
                         smartName != 'Udma_CRC_Error_Count_Value']

####################################
# S1. Label item for lead time prediction
smartN <- subset(smartN,!(sn %in% smartF$sn))
smartF <- subset(smartF,!(sn %in% smartN$sn))
smartL <- rbind(smartF,smartN)
smartL$restTime <- as.numeric(difftime(smartL$failed_time,smartL$time,tz = 'UTC',units = 'hours'))
smartL$restTime[smartL$label == 0] <- -10000
smartL <- subset(smartL,label == 0 | restTime > 0)
smartL <- smartL[order(smartL$sn,smartL$time),]
smartL <- factorX(smartL[,c('sn','model','label','restTime',smartName)])

# S2. Extract sn and label for data partition
snLabel <- data.frame(sn = levels(smartL$sn),
                      label = as.character(tapply(smartL$label,smartL$sn,function(x)x[1])))

# S3. Predict and testing
#tw = 48;ga = 0.1;co = 10;w1 = 1; w2 = 1;countTrain = 30000
smartPred <- function(tw = 48,ga = 0.1,co = 10,w1 = 1, w2 = 1,countTrain = 30000){
  p1 <- proc.time()
  # 3.1 Data partition with time window
  inTrain <- createDataPartition(y = snLabel$label, p = .7, list = FALSE)
  training <- factorX(subset(smartL, sn %in% snLabel$sn[inTrain] & restTime <= tw))
  smp <- sample(1:nrow(training),min(countTrain,nrow(training)))
  # testing <- factorX(subset(smartL, sn %in% snLabel$sn[-inTrain]))
  # testing <- factorX(subset(smartL, sn %in% snLabel$sn[-inTrain] & restTime <= tw))
  testing <- factorX(subset(smartL,label == 1))
  
  # 3.2 Model Training
  w <- c(204/7,1)
  names(w) <- c(0,1)
  mod <- svm(training[smp,smartName],training$label[smp],
             type = 'C', kernel = 'radial', 
             gamma = ga, cost = co,class.weights = w)
  
  # 3.3 Model Testing for items
  pred <- predict(mod,testing[,smartName])
  testing$pred <- pred
  
  # 3.4 Map testing result to disk
  staPred <- by(testing,testing$sn,function(x){
    leadTime <- ifelse(any(x$pred == 1),x$restTime[min(which(x$pred == 1))],0)
    posRate <- sum(x$pred == 1)/nrow(x)
    pred <- as.numeric(posRate > 0)
    list(as.character(x$sn[1]),leadTime,posRate,pred)
  })
  predDisk <- data.frame(matrix(unlist(staPred),byrow = T,nrow = nrow(staPred)))
  names(predDisk) <- c('sn','leadTime','posRate','pred')
  predDisk$label <- snLabel$label[match(predDisk$sn,snLabel$sn)]
  c <- confusionMatrix(predDisk$pred,predDisk$label)
  #FDR <- c$byClass['Specificity']
  #FAR <- 1 - c$byClass['Sensitivity']
  
  r <- table(predDisk[,c('pred','label')])
  FDR <- r[2,2]/(r[2,2] + r[1,2])
  FAR <- r[2,1]/(r[2,1] + r[1,1])
  
  p2 <- proc.time()
  
  # 3.5 Result print
  cat(sprintf('tw:%.0f\tgamma:%.3f\tcost:%.3f\tw1:%.0f\tw2:%.0f\tFDR:%.3f\tFAR:%.3f\ttime:%fs\tcountTR:%f',
              tw,ga,co,w1,w2,FDR*100,FAR*100,p2[3]-p1[3],countTrain))
  r <- list(tw,ga,co,w1,w2,countTrain,FDR*100,FAR*100,c,predDisk)
  r
}

# a <- sapply(seq(10000,100000,10000),function(x){smartPred(countTrain = x)})
a <- smartPred()
save(a,file = file.path(dir_data,'NPCdfpForlt.Rda'))
