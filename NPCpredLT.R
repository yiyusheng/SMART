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