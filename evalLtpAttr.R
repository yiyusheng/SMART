# Evaluate attributes of SMART for lead time prediction
# Date: 2016-06-15
# Author: Pezy

rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
source(file.path(dir_code,'modelLabel.R'))
library(ggplot2)
load(file.path(dir_data,'ykliu_smart.Rda'))
load(file.path(dir_data,'SMARTprepF4h.Rda'))
load(file.path(dir_data,'SMARTprepN0.5w.Rda'))
load(file.path(dir_data,'diskInfoForEach0.5kN.Rda'))
load(file.path(dir_data,'smartName.Rda'))

names(smart)[5:19] <- smartName
smartFBack <- smartF
attrNeed <- smartName[c(1:5,7,8,10:12,14,15)]
sNameFinal <- c(names(smartFBack)[c(1:4,20,21)],attrNeed)
smartF <- subset(smartFBack, sn %in% diskInfoF$sn[diskInfoF$countF == 1],sNameFinal)

smartF$restTime <- as.numeric(difftime(smartF$failed_time,smartF$time,tz = 'UTC',units = 'hours'))
smartF <- subset(smartF,restTime > -24*20)
smartF$modelLtp <- modelLabel(smartF$model)
smartF <- smartF[order(smartF$sn,smartF$time),]
smartF <- factorX(smartF[,c('sn','modelLtp','label','failed_time','restTime','time',attrNeed)])
smartF[,attrNeed] <- scale(smartF[,attrNeed])

diskInfoF$modelLtp <- modelLabel(diskInfoF$model)

###@@@ FUNCTION @@@###
plotAttrValue <- function(data,attr){
  tmp <- factorX(subset(data,1==1,c('sn','time','restTime',attr)))
  names(tmp)[4] <- 'attr'
  p <- ggplot(tmp,aes(x = time,y = attr,group = sn,color = factor(round(restTime/24/7)))) + geom_line() +
    ylab(attr)
  print(p)
}

plotAttrDistImedi <- function(data,attr){
  tmp <- factorX(subset(data,1==1,c('sn','time','class',attr)))
  names(tmp)[4] <- 'attr'
  p <- ggplot(tmp,aes(x = attr,color = class)) + stat_ecdf() +
    ylab(attr)
  ggsave(file = file.path(dir_data,'figure','evalLtpAttr',paste('padi',attr,'.jpg',sep='')),
         plot=p, width = 8, height = 6, dpi = 100)
  p
}
####################################
# S1. disk statistc and choose disks to evaluate
snEval <- subset(diskInfoF,count > 800 & countF == 1 & model == 'ST31000524NS' &
                   failed_time > as.POSIXct('2014-07-15') & 
                   failed_time < as.POSIXct('2014-10-25'))

# smart Eval for F
smartEvalF <- subset(smartF,sn %in% snEval$sn[sample(1:nrow(snEval),5)])
smartEvalF <- factorX(smartEvalF)

# smart Eval for N
snN <- levels(smartN$sn)
smartEvalN <- subset(smartN,sn %in% snN[sample(1:length(snN),10)])
names(smartEvalN)[5:19] <- paste('a',1:15,sep='')

# S2. Plot for each attributes

# for (i in 1:length(attrNeed)){
#   pSetB[[i]] <- plotAttrValue(smartEvalF,attrNeed[i])
#   pSetA[[i]] <- plotAttrDistImedi(data = smartAttrDist,attr = attrNeed[i])
# }

# S3. distribution of smart attr with immediately failing
# smartDist <- subset(smart,1==1,c('sn','model','failed_time','time','restTime',attrNeed))
# smartDist[,attrNeed] <- scale(smartDist[,attrNeed])
# 
# upT <- 20*24;brokeT <- 10*24
# smartAttrDist <- factorX(subset(smartDist,restTime < upT))
# smartAttrDist$class <- 'Bf'
# smartAttrDist$class[smartAttrDist$restTime < brokeT] <- 'ime'

# S4. Plot for each attributes of each disk
plotDiskAttr <- function(df){
  # df <- subset(smartF,sn == sn)
  tmp <- subset(df,1==1,c('time',attrNeed))
  m <- melt(tmp,id.vars = 'time')
  p <- ggplot(m,aes(x = time,y = value,color = variable,linetype = variable)) + 
    geom_line() + geom_vline(aes(xintercept=as.numeric(df$failed_time[1])),
                             color = 'red',size = 1,linetype = 'dashed') +
    xlab(df$sn[1]) + ylab(df$modelLtp[1]) +
    theme(legend.key.width = unit(4,units = 'line'))
  # print(p)
  ggsave(file = file.path(dir_data,'figure','plotDiskAttr',paste(df$modelLtp[1],df$sn[1],'.jpg',sep='')),
         plot=p, width = 16, height = 12, dpi = 100)
}

by(smartF,smartF$sn,plotDiskAttr)
# sn <- levels(smartF$sn)
# require(doParallel)
# ck <- makeCluster(min(40,length(sn)), outfile = '')
# registerDoParallel(ck)
# r <- foreach(i = 1:length(sn),
#              .combine = rbind,
#              .verbose = T,
#              .packages = c('ggplot2','reshape2')) %dopar% plotDiskAttr(sn)
# stopCluster(ck)
