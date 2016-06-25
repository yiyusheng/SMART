# Disk Failure Predict on Tencent's dataset
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
require('caret')
require('e1071')

#@@@ LOAD DATA @@@#
#consist of 300k normal disk(each 20 days) from york and 2100 bad disk from ykliu
#load(file.path(dir_data,'SMARTMerge_int2h20d30k.Rda'))  
load(file.path(dir_data,'SMARTprepF4hFtr1.Rda'))
load(file.path(dir_data,'SMARTprepN0.5wFtr1.Rda'))
load(file.path(dir_data,'smartName.Rda'))

attrNeed <- smartName[c(1,2,3,4,10,12,15)]
attrFinal <- names(smartF)[names(smartF) %in% c(attrNeed,paste(attrNeed,'amp'),paste(attrNeed,'int'))]
sNameFinal <- c(names(smartF)[c(1:4,20,21)],attrFinal)

####################################
# S1. Label item for lead time prediction
# For normal disk
smartN <- factorX(subset(smartN,!(sn %in% smartF$sn),sNameFinal))
# For failed disk
maxFtime <- as.POSIXct(tapply(smartF$failed_time,smartF$sn,max),origin = '1970-01-01',tz = 'UTC')
smartF$failed_time <- maxFtime[match(smartF$sn,names(maxFtime))]
multiFtime <- tapply(smartF$failed_time,smartF$sn,function(x)length(unique(x)))
smartF <- subset(smartF, !(sn %in% names(multiFtime)[multiFtime > 1]))
smartF <- factorX(subset(smartF,!(sn %in% smartN$sn),sNameFinal))
# Merge
smartL <- rbind(smartF,smartN)
smartL$restTime <- as.numeric(difftime(smartL$failed_time,smartL$time,tz = 'UTC',units = 'hours'))
smartL$restTime[smartL$label == 0] <- -10000
smartL <- subset(smartL,label == 0 | restTime > 0)
smartL <- smartL[order(smartL$sn,smartL$time),]
smartL <- factorX(smartL[,c('sn','model','label','restTime','time',attrFinal)])

# S2. Extract sn and label for data partition
snLabel <- data.frame(sn = levels(smartL$sn),
                      label = as.character(tapply(smartL$label,smartL$sn,function(x)x[1])),
                      count = as.numeric(tapply(smartL$sn,smartL$sn,length)))
smartL <- subset(smartL,sn %in% snLabel$sn[snLabel$count > 100])

# S3. Predict and testing
#tw = 48;ga = 0.01;co = 100;wP = 0.05; countTrain = -1; countNeg = 2000
smartPred <- function(tw = 48,ga = 0.01,co = 100,wP = 0.05, countTrain = -1, countNeg = 2000){
  p1 <- proc.time()
  # 3.0 control of negative server number
  if ( countNeg == -1){
      smartL1 <- smartL
  }else{
      normalSN <- subset(snLabel,label == 0)
      normalSN <- normalSN[sample(1:nrow(normalSN),countNeg),]
      smartL1 <- factorX(subset(smartL,label == 1 | sn %in% normalSN$sn))
  }
  # 3.1 Data partition with time window
  inTrain <- createDataPartition(y = snLabel$label, p = .7, list = FALSE)
  training <- factorX(subset(smartL1, sn %in% snLabel$sn[inTrain] & (label == 0 | restTime <= tw)))
  if (countTrain == -1){
      smp <- 1:nrow(training)
  }else{
      smp <- sample(1:nrow(training),min(countTrain,nrow(training)))
  }
  testing <- factorX(subset(smartL1, sn %in% snLabel$sn[-inTrain]))

  # 3.15 weight
  nfRate <- table(training$label)
  nfRate <- nfRate[1]/nfRate[2]
  w <- c(1,nfRate*wP)
  #w <- c(1,1)
  names(w) <- c(0,1)
  
  # 3.2 Model Training
  mod <- svm(training[smp,attrFinal],training$label[smp],
             type = 'C', 
             kernel = 'radial', 
             cross = 3,
             scale = T,
             gamma = ga, 
             cost = co,
             class.weights = w)
  
  # 3.3 Model Testing for items
  pred <- predict(mod,testing[,attrFinal])
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

  # 3.5 Summary
  c <- confusionMatrix(predDisk$pred,predDisk$label)
  FDR <- c$byClass['Specificity']
  FAR <- 1 - c$byClass['Sensitivity']
  p2 <- proc.time()
  
  # 3.6 Result print
  cat(sprintf('tw:%.0f\tgamma:%.3f\tcost:%.0f\twp:%.3f\tw1:%.3f\tcountTr:%.0f\tcountNeg:%.0f\tFDR:%.3f\tFAR:%.3f\ttime:%.0fs\n',
              tw,ga,co,
              wP,w[['1']],countTrain,countNeg,
              FDR*100,FAR*100,p2[3]-p1[3]))
  r <- list(tw,ga,co,wP,w[['1']],countTrain,countNeg,FDR*100,FAR*100,c,predDisk)
  r
}

# 4. Parameter setting
# tw ga co wP countTrain countNeg
para <- expand.grid(c(12,24,48,72,96),
                    c(0.005,0.01,0.05),
                    c(50,100,200,500),
                    c(0.05,0.001),
                    -1,c(2000))
para <- para[order(para$Var1,para$Var2,para$Var3,para$Var4,para$Var5,para$Var6),]
# r <- smartPred()

# 5. Launch parallel tool
require(doParallel)
ck <- makeCluster(min(40,nrow(para)), outfile = '')
registerDoParallel(ck)
r <- foreach(i = 1:nrow(para),
             .combine = rbind,
             .verbose = T,
             .packages = c('caret','e1071')) %dopar% smartPred(para$Var1[i],
             para$Var2[i],para$Var3[i],para$Var4[i],para$Var5[i],para$Var6[i])
save(para,r,file = file.path(dir_data,'NPCdfp07.Rda'))
stopCluster(ck)
