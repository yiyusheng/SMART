# Evaluate attributes of SMART for lead time prediction
# Date: 2016-06-15
# Author: Pezy

rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
library(ggplot2)
load(file.path(dir_data,'ykliu_smart.Rda'))
load(file.path(dir_data,'SMARTprepN1w.Rda'))
load(file.path(dir_data,'smartName.Rda'))

####################################
# S1. disk statistc and choose disks to evaluate
disk <- data.frame(sn = levels(smart$sn),
                   model = factor(tapply(as.character(smart$model),smart$sn,unique)),
                   count = as.numeric(tapply(smart$sn,smart$sn,length)),
                   countF = as.numeric(tapply(smart$failed_time,smart$sn,function(x)length(unique(x)))),
                   firstTime = as.POSIXct(tapply(smart$time,smart$sn,min),origin = '1970-01-01',tz = 'UTC'),
                   lastTime = as.POSIXct(tapply(smart$time,smart$sn,max),origin = '1970-01-01',tz = 'UTC'),
                   failed_time = as.POSIXct(tapply(smart$failed_time,smart$sn,max),origin = '1970-01-01',tz = 'UTC'))
row.names(disk) <- NULL
disk <- unnameX(disk)
disk <- factorX(disk)
snEval <- subset(disk,count > 800 & 
                   countF == 1 & 
                   failed_time > as.POSIXct('2014-08-15') & 
                   failed_time < as.POSIXct('2014-09-15') & 
                   model == 'ST31000524NS')

# smart Eval for F
smartEvalF <- subset(smart,sn %in% snEval$sn[sample(1:nrow(snEval),10)])
smartEvalF$beforeFail <- as.numeric(smartEvalF$time < smartEvalF$failed_time)
smartEvalF <- factorX(smartEvalF)

# smart Eval for N
snN <- levels(smartN$sn)
smartEvalN <- subset(smartN,sn %in% snN[sample(1:length(snN),10)])
names(smartEvalN)[5:19] <- paste('a',1:15,sep='')

# S2. Plot for each attributes
attrNames <- paste('a',c(1,2,3,5,10,12,15),sep='')

plotForLtpAttr <- function(data,attr){
  # tmp <- factorX(subset(data,1==1,c('sn','failed_time','time',attr,'beforeFail')))
  tmp <- factorX(subset(data,1==1,c('sn','time',attr)))
  names(tmp)[3] <- 'attr'
  p <- ggplot(tmp,aes(x = time,y = attr,group = sn,color = sn)) + geom_line() +
    ylab(attr)
  print(p)
}

plotForLtpAttr(smartEvalF,attrNames[3])
