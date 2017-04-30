# Lead time Predict on Tencent's dataset
# Date: 2016-06-15
# Author: Pezy

rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
source(file.path(dir_code,'modelLabel.R'))
source(file.path(dir_code,'NPCltpFunc.R'))

require('caret')
require('e1071')
require('LiblineaR')
library('plyr')

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'SMARTprepF24hFtr.Rda'))
load(file.path(dir_data,'smartNameNew.Rda'))
load(file.path(dir_data,'diskInfoForEach0.5kN.Rda'))

####################################
###@@@ MAIN @@@###
iwProc <- function(iwSet = c(5,8,10,15,20,40),flagP = F){
  # S1. diskGen
  smartL <- smartF[!duplicated(smartF[,c('sn','time')]),]
  attrNeed <- smartName[c(1:5,7,8,10:12,14,15,16)]
  smartL <- dataGen(attrNeed,data = smartL, T)
  diskInfoF$modelLtp <- modelLabel(diskInfoF$model)
  
  # S2A. label for each iterate window (days)
  # smartL <- subset(smartL,modelLtp == '750G2')
  labName <- paste('lab',iwSet,sep='')
  predName <- paste('pred',iwSet,sep='')
  for (i in 1:length(iwSet)){
    smartL[[labName[i]]] <- as.numeric(smartL$restTime <= iwSet[i]*24)
    smartL[[labName[i]]][smartL[[labName[i]]] == 0] <- -1
  }
  
  # S2B. perc set
  percSets <- list(per25 = c(0,0.25,1.75,2),
                   per50 = c(0,0.5,1.5,2),
                   per75 = c(0,0.75,1.25,2),
                   per100 = c(0,1,1,2),
                   per200 = c(0,1,1,3),
                   per300 = c(0,1,1,4),
                   per400 = c(0,1,1,5))
  
  # S3. Model for each iterate window and testing
  snLevel <- levels(smartL$sn)
  inTrain <- sample(1:length(snLevel),0.7*length(snLevel))
  # save(inTrain,file = file.path(dir_data,'inTrain.Rda'))
  load(file.path(dir_data,'inTrain.Rda'))
  training <- factorX(smartL[smartL$sn %in% snLevel[inTrain],])
  testing <- factorX(smartL[!(smartL$sn %in% snLevel[inTrain]),])
  
  f <- 'iw'
  iwD <- iwSet[1]
  # Train and testing
  testR <- leadTimePrep(at = attrNeed[c(1:12)],dataTr = training,dataTe = testing,
                        flag = f,percSets = percSets,iwSet = iwSet)
  # Parse results for single model
  if (f == 'perc'){
    testE <- sapply(names(percSets),evalTestSPerc,dataP = testR$t,dataT = testing,iw = iwD)
    testE <- do.call(rbind,testE)
  }else if (f == 'iw'){
    testE <- sapply(iwSet,evalTestS,dataP = testR$t,dataT = testing)
    testE <- do.call(rbind,testE)
    testE$class <- paste(gsub('lab','',testE$class),'days',sep=' ')
    testE$class <- factor(testE$class,levels = paste(iwSet,'days',sep=' '))
  }
  
  # S4. Plot and Save for single model
  ifelse(flagP,plotTestE(testE),NULL)
  
  # S5. Parse result for iterate models
  testPred <- data.frame(t(do.call(rbind,testR$t)))
  # for scale
  # mins <- apply(testPred,2,min)
  # maxs <- apply(testPred,2,max)
  # center = apply(testPred,2,mean),scale = maxs - mins)
  
  
  # generate the best and merge
  iterPred <- cbind(testing[,c('sn','restTime',labName)],testPred)
  iterPred <- subset(iterPred,restTime < 1*24*max(iwSet))
  iterPred[,names(testPred)] <- scale(iterPred[,names(testPred)])

  bestPred <- procIterR(iterPred[,names(testPred)],iwSet)
  iterPred <- cbind(iterPred,bestPred)
  
  iterPred$restTime <- iterPred$restTime/24
  iterPred$accu <- sapply(1:nrow(iterPred),function(i)iterAccu(iterPred$restTime[i],iterPred$bestPred[i],iwSet))
  randPred <- sample(iwSet,length(iterPred$restTime),replace = T)
  iterPred$accuRandom <- sapply(1:nrow(iterPred),function(i)iterAccu(iterPred$restTime[i],randPred[i],iwSet))
  iterPred
}
iwProc(flagP = T)
iwSingle <- c(2,5,8,10,20)
# predN <- paste('predSet',c(iwSingle,11),sep='')
# iwSet <- c(iwSet,list(c(10,20,30,40)))
predN <- paste('predSet',iwSingle,sep='')
iwSet <- lapply(iwSingle,function(x)unique(c(seq(x,40,x),40)))


r.iwProc <- lapply(iwSet,iwProc,F)
accuSet <- data.frame(sapply(r.iwProc,'[[',c('accu')))
accuRandomSet <- data.frame(sapply(r.iwProc,'[[',c('accuRandom')))
bestPredSet <- data.frame(sapply(r.iwProc,'[[',c('bestPred')))
names(accuSet) <- predN
names(accuRandomSet) <- paste(predN,'R',sep='')
names(bestPredSet) <- paste(predN,'P',sep='')

r.merge <- data.frame(do.call(rbind,lapply(1:ncol(accuSet),
                                     function(i)cbind(accuSet[,i],accuRandomSet[,i],bestPredSet[,i],names(accuSet)[i]))))
names(r.merge) <- c('Accu','AccuR','leadTime','iterateWindow')
li <- melt(table(r.merge[,names(r.merge)]))
r.rate <- data.frame(do.call(rbind,lapply(1:(nrow(li)/2),function(i){
  cur <- li[(2*i-1):(2*i),]
  list(cur$leadTime[1],as.character(cur$iterateWindow[1]),
       cur$value[cur$Accu == T]/(cur$value[cur$Accu == T] + cur$value[cur$Accu == F]),
       cur$value[cur$Accu == T] + cur$value[cur$Accu == F])
})))
names(r.rate) <- c('leadTime','iterateWindow','Rate','count')
r.rate <- subset(r.rate,!is.na(r.rate$Rate))
r.rate$iterateWindow <- factor(r.rate$iterateWindow,levels = predN)
r.rate$leadTime <- as.numeric(r.rate$leadTime)
r.rate$Rate <- as.numeric(r.rate$Rate) * 100
r.rate$count <- as.numeric(r.rate$count)
r.rate <- r.rate[order(r.rate$iterateWindow,r.rate$leadTime),]


r.rate$iterateWindow <- mapvalues(r.rate$iterateWindow,
                                  from = levels(r.rate$iterateWindow), 
                                  to = sapply(iwSet,length))
plotrRate(r.rate)

# Calculate for each iwSet
r.rate$fail <- r.rate$count*r.rate$Rate*0.01
by(r.rate,r.rate$iterateWindow,function(x){
  sum(x$fail)/sum(x$count)
})















# iterAccuracy <- data.frame(t(apply(accu,2,table)))
# iterAccuracy$rate <- iterAccuracy$TRUE./(iterAccuracy$TRUE. + iterAccuracy$FALSE.) * 100
# iterAccuracy$name <- row.names(iterAccuracy)
# iterAccuracy$name <- factor(iterAccuracy$name,levels = predN)
# ggplot(iterAccuracy,aes(x = name,y = rate)) + geom_bar(stat = 'identity')
# inTrain <- sample(1:nrow(smartL),0.7*nrow(smartL))
# training <- factorX(smartL[inTrain,])
# testing <- factorX(smartL[-inTrain,])
# iterPred <- data.frame(sn = testing$sn,
#                 restTime = testing$restTime/24,
#                 proRestTime =  procIterR(testPredScale))

# numChange <- data.frame(sn = levels(testing$sn),
#                         count = as.numeric(tapply(iterPred$sn,iterPred$sn,length)),
#                         countC = as.numeric(tapply(iterPred$proRestTime,iterPred$sn,function(x)length(unique(x)))))
# numChange <- subset(numChange,count > 20*6)
# quantileX(numChange$countC)
# a <- list()
# for (i in 1:10){
#   inTrain <- sample(1:nrow(smartL),0.7*nrow(smartL))
#   training <- factorX(smartL[inTrain,])
#   testing <- factorX(smartL[-inTrain,])
#   # sapply(paste('lab',iwSet,sep=''),function(x)table(training[[x]]))
#   # sapply(paste('lab',iwSet,sep=''),function(x)table(testing[[x]]))
#   # load(file.path(dir_data,'GreatSet.Rda'))
#   # load(file.path(dir_data,'BadSet.Rda'))
#   
#   # testR <- leadTimePrep(attrNeed)
#   testR <- leadTimePrep(at = attrNeed[c(1:5)],dataTr = training,dataTe = testing)
#   testE <- sapply(iwSet,evalTestS,dataP = testR$t,dataT = testing)
#   testE <- do.call(rbind,testE)
#   testE$class <- factor(testE$class,levels = labName)
#   p <- ggplot(testE,aes(x = FAR, y = FDR, color = class, shape = class )) + 
#     geom_line() + geom_point() +
#     theme(legend.key.width = unit(4,units = 'line'))
#   print(p)
#   
#   a[[i]] <- list(inTrain,training,testing,testR,p)
# }
# pSet <- lapply(a,'[[',5)
# testRSet <- lapply(lapply(lapply(lapply(a,'[[',4),'[[','m'),'[[',1),'[[','W')
# paraSet <- matrix(unlist(testRSet),byrow = T,nrow = 10)
# print(pSet[[9]])
# 
# inTrain <- a[[4]]
# summary(testR)
# testR1 <- testR
# testR1$pred1 <- 1- testR1$pred1
# testR1$pred3 <- 1 - testR1$pred3
# testR1$pred5 <- 1- testR1$pred5
# testR1$pred7 <- 1- testR1$pred7
# testR1$pred9 <- 1- testR1$pred9
# testR1$pred12 <- 1- testR1$pred12
# 
# a <- melt(testR1)
# ggplot(a,aes(x = value,color = variable)) + stat_ecdf()
# test



# S4. Process iterate result
# procIterR <- function(data){
#   r <- apply(data,MARGIN = 1,function(x){
#     iwSet[which(x == max(x))]
#   })
# }
# b <- data.frame(restTime = testing$restTime/24,
#                 proRestTime =  procIterR(scale(a)))

# S4. Attrubutes Selection Test.
# paraAttr <- lapply(1:length(attrNeed),function(i)combn(attrNeed,i,simplify = F))
# paraAttr <- do.call(c,paraAttr)
# 
# # threshold analysis
# meanP <- round(mean(p),digits = 1)
# r <- sapply(seq(max(0,meanP-0.1),min(1,meanP+0.4),0.03),thresTest,data = ttest)
# r1 <- matrix(unlist(r),byrow = F,nrow = 3)
# r1 <- round(r1,digits = 3)
# print(r1[2:3,])
# r1 <- data.frame(t(r1[2:3,]))
# r1
# 
# require(doParallel)
# ck <- makeCluster(min(40,length(paraAttr)), outfile = '')
# registerDoParallel(ck)
# r <- foreach(i = paraAttr,
#              .combine = rbind,
#              .verbose = T,
#              .packages = c('LiblineaR')) %dopar% leadTimePrep(i)
# save(paraAttr,r,file = file.path(dir_data,'NPCltp01.Rda'))
# stopCluster(ck)




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
