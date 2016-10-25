# Select features from SMART for lead time prediction
# We try to use method of feature selection to decide contribution of each feature for lead time responce value
# Date: 2016-06-20
# Author: Pezy

rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
library(ggplot2)
load(file.path(dir_data,'SMARTprepF4hFtr.Rda'))
load(file.path(dir_data,'SMARTprepN0.5wFtr.Rda'))
load(file.path(dir_data,'smartName.Rda'))
load(file.path(dir_data,'diskInfoForEach0.5kN.Rda'))

# Add restTime
smartF$restTime <- as.numeric(difftime(smartF$failed_time,smartF$time,tz = 'UTC',units = 'hours'))

# select attr to evaluate
# attrNeed <- smartName[c(1,2,3,4,10,12,15)]
# attrNeed <- smartName
attrNeed <- smartName[c(3,4,7,8,10)]
attrFinal <- names(smartF)[names(smartF) %in% c(attrNeed,
                                                paste(attrNeed,'amp'),
                                                paste(attrNeed,'int'),
                                                paste(attrNeed,'cont'))]
sNameFinal <- c('sn','model','failed_time','time','label','seqTime','restTime',attrFinal)

# filter smartF
smartF <- subset(smartF,restTime > 0 & !(sn %in% diskInfoF$sn[diskInfoF$countF > 1]),sNameFinal)
smartF <- factorX(smartF[order(smartF$sn,smartF$time),])
####################################
# S1. Label smart items as NPCltp.R
smartL <- smartF
iterWin <- 5
maxIterWin <- 5
iw <- seq(iterWin,maxIterWin,iterWin)
labName <- paste('lab',iw,sep='')
for (i in 1:length(iw)){
  smartL[[labName[i]]] <- as.numeric(smartL$restTime <= iw[i]*24)
}
smartL[,attrFinal] <- scale(smartL[,attrFinal])

# S2. pearson coefficient
pearCov <- matrix(0,nrow = length(labName),ncol = length(attrFinal))
for (i in 1:length(labName)){
  for (j in 1:length(attrFinal)){
    tmp <- cor.test(smartL[[labName[i]]],smartL[[attrFinal[j]]])
    pearCov[i,j] <- tmp$estimate
  }
}
pearCov <- pearCov * 100

# S3. Random Forest
# library(randomForest)
# rPred <- list()
# fitSet <- list()
# for (i in 1:length(labName)){
#   RFfit <- randomForest(x = smartL[,attrFinal],
#                         y = smartL[[labName[i]]],
#                         ntree = 500)
#   fitSet[i] <- RFfit
#   rPred[i] <- predict(RFfit,smartL[,attrFinal])
# }

# S4.Decision Tree
library(rpart)
library(rpart.plot)
ct <- rpart.control(xval = 3, minsplit = 20, cp = 0.01)
tmp <- smartL[,c(attrFinal,labName[i])]
fit <- rpart(lab5~.,
             data = tmp,
             method = 'class',
             # control = ct,
             parms = list(prior = c(0.65,0.35), split = "information"))
a <- predict(fit,tmp[,attrFinal])
pred <- as.numeric(a[,1] > 0.5)
confusionMatrix(factor(pred),factor(tmp$lab5))
rpart.plot(fit, branch=1, branch.type=2, type=1, extra=102,
           shadow.col="gray", box.col="green",
           border.col="blue", split.col="red",
           split.cex=1.2, main="Decision Tree")
