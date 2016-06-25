# Lead time Predict on Tencent's dataset
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
require('caret')
require('e1071')
require('LiblineaR')
options('width' = 120)

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'SMARTprepF4hFtr.Rda'))
load(file.path(dir_data,'smartName.Rda'))
load(file.path(dir_data,'diskInfoForEach0.5kN.Rda'))

#@@@ FUNCTION @###
thresTest <- function(data,thres){
  predBool <- as.numeric(data$pred > thres)
  # c <- confusionMatrix(factor(predBool),factor(data$label))
  # FDR <- as.numeric(c$byClass['Specificity'])*100
  # FAR <- (1 - as.numeric(c$byClass['Sensitivity']))*100
  FDR <- sum(data$label == 1 & predBool == 1)/sum(data$label == 1)*100
  FAR <- sum(data$label == 0 & predBool == 1)/sum(data$label == 0)*100
  # cat(sprintf('Threshold:%.2f\tFDR:%.2f\tFAR:%.2f\n',thres,FDR,FAR))
  r <- list(thres,FDR,FAR)
  r
}

####################################
# S1. For failed disk
# attrNeed <- smartName[c(1,2,3,4,10,12,15)]
smartFBackup <- smartF
attrNeed <- smartName[c(1:5,7,8,10:12,14,15)]
attrFinal <- names(smartFBackup)[names(smartFBackup) %in% 
                             c(attrNeed,paste(attrNeed,'amp'),paste(attrNeed,'int'),paste(attrNeed,'cont'))]
sNameFinal <- c(names(smartFBackup)[c(1:4,20,21)],attrFinal)


smartF <- subset(smartFBackup, sn %in% diskInfoF$sn[diskInfoF$countF == 1])
smartF$restTime <- as.numeric(difftime(smartF$failed_time,smartF$time,tz = 'UTC',units = 'hours'))
smartF <- subset(smartF,restTime > 0)
smartF <- smartF[order(smartF$sn,smartF$time),]
smartF <- factorX(smartF[,c('sn','model','label','restTime','time',attrFinal)])
smartF[,attrFinal] <- scale(smartF[,attrFinal])

# S2. label for each iterate window (days)
smartL <- smartF
iterWin <- 10
maxIterWin <- 10
iw <- seq(iterWin,maxIterWin,iterWin)
labName <- paste('lab',iw,sep='')
for (i in 1:length(iw)){
  smartL[[labName[i]]] <- as.numeric(smartL$restTime <= iw[i]*24)
}

# S3. Model for each iterate window and testing
inTrain <- list()
mod <- list()
rset <- list()

inTrain <- sample(1:nrow(smartL),0.7*nrow(smartL))
training <- factorX(smartL[inTrain,])
testing <- factorX(smartL[-inTrain,])

predName <- paste('pred',iw,sep='')
tmpTest <- testing

paraAttr <- lapply(1:length(attrNeed),function(i)combn(attrNeed,i,simplify = F))
paraAttr <- do.call(c,paraAttr)

at <- smartName[c(4,7)]
leadTimePrep <- function(at){
  attrN <- c(at,paste(at,'amp'),paste(at,'int'),paste(at,'cont'))
  labN <- paste('lab',iw,sep='')
  tmp <- subset(training,restTime < iw*24*2 & restTime > iw*24*0.2)
  tmpMod <- tmp[,c(attrN,labN)]
  names(tmpMod) <- c(attrN,'label')
  
  mod <- LiblineaR(data = tmpMod[,attrN],target = tmpMod$label,
                            type = 0,cost = 1,epsilon = 0.1)
  
  p <- predict(mod,tmpTest[,attrN],proba = T,decisionValues = T)
  p <- p$probabilities[,2]
  
  meanP <- round(mean(p),digits = 1)
  ttest <- data.frame(label = tmpTest[[labN]],pred = p)
  r <- sapply(seq(max(0,meanP-0.1),min(1,meanP+0.4),0.03),thresTest,data = ttest)
  r1 <- matrix(unlist(r),byrow = F,nrow = 3)
  r1 <- round(r1,digits = 3)
  print(r1[2:3,])
  r1 <- data.frame(t(r1[2:3,]))
  r1
}

require(doParallel)
ck <- makeCluster(min(40,length(paraAttr)), outfile = '')
registerDoParallel(ck)
r <- foreach(i = paraAttr,
             .combine = rbind,
             .verbose = T,
             .packages = c('LiblineaR')) %dopar% leadTimePrep(i)
save(paraAttr,r,file = file.path(dir_data,'NPCltp01.Rda'))
stopCluster(ck)




# maxFtime <- as.POSIXct(tapply(smartF$failed_time,smartF$sn,max),origin = '1970-01-01',tz = 'UTC')
# smartF$failed_time <- maxFtime[match(smartF$sn,names(maxFtime))]
# multiFtime <- tapply(smartF$failed_time,smartF$sn,function(x)length(unique(x)))
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
# smartL$sector <- smartL$Reallocated_Sector_Ct_Raw + smartL$Current_Pending_Sector_Raw
# smartL$sectorDiff <- smartL$Reallocated_Sector_Ct_RawDiff + smartL$Current_Pending_Sector_RawDiff
# smartName <- c('Raw_Read_Error_Rate_Value','Spin_Up_Time_Value',
#                'Reallocated_Sector_Ct_Value','sector','Seek_Error_Rate_Value')
# smartName <- c(smartName,paste(smartName,'Diff',sep=''))
# smartL <- smartL[,c(names(smartL)[1:7],smartName)]
# smartLbup <- smartL

#delete tmpTest with all 0 in labname
# testDel <- apply(tmpTest[,labName],MARGIN = 1, sum)
# tmpTest <- tmpTest[testDel > 0 & testDel < 6,]

# for (i in 1:length(iw)){
#   tmp <- subset(training,restTime < iw[i]*24*2 & restTime > iw[i]*24*0.4)
#   tmpMod <- tmp[,c(attrFinal,labName[i])]
#   names(tmpMod) <- c(attrFinal,'label')
#   mod[[predName[i]]] <- LiblineaR(data = tmpMod[,attrFinal],target = tmpMod$label,
#                                   type = 0,cost = 1,epsilon = 0.1)
#   
#   p <- predict(mod[[predName[i]]],tmpTest[,attrFinal],proba = T,decisionValues = T)
#   p <- p$probabilities[,2]
#   
#   tmpTest[[predName[i]]] <- p
#   meanP <- round(mean(p),digits = 1)
#   rset[[predName[i]]] <- sapply(seq(max(0,meanP-0.1),min(1,meanP+0.1),0.01),
#                                 thresTest,labn = labName[i],predn = predName[i])
# }