# Compare attr for normal smart and failed smart to decide attrs being used
# We estimate attrs by number of changing and amplitude of changing
# Date: 2016-06-17
# Author: Pezy

rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
library(ggplot2)
load(file.path(dir_data,'ykliu_smart.Rda'))
load(file.path(dir_data,'SMARTprepN1w.Rda'))
load(file.path(dir_data,'smartName.Rda'))

####################################
# S1. extract disks with similar item numbers
smartF <- smart
smartF$restTime <- difftime(smartF$failed_time,smartF$time,units = 'days',tz = 'UTC')
smartF <- subset(smartF,restTime < 16 & restTime > 0)
diskF <- data.frame(sn = levels(smartF$sn),
                   model = factor(tapply(as.character(smartF$model),smartF$sn,unique)),
                   count = as.numeric(tapply(smartF$sn,smartF$sn,length)),
                   firstTime = as.POSIXct(tapply(smartF$time,smartF$sn,min),origin = '1970-01-01',tz = 'UTC'),
                   lastTime = as.POSIXct(tapply(smartF$time,smartF$sn,max),origin = '1970-01-01',tz = 'UTC'))
diskN <- data.frame(sn = levels(smartN$sn),
                    model = factor(tapply(as.character(smartN$model),smartN$sn,unique)),
                    count = as.numeric(tapply(smartN$sn,smartN$sn,length)),
                    firstTime = as.POSIXct(tapply(smartN$time,smartN$sn,min),origin = '1970-01-01',tz = 'UTC'),
                    lastTime = as.POSIXct(tapply(smartN$time,smartN$sn,max),origin = '1970-01-01',tz = 'UTC'))
diskF1 <- subset(diskF,count > 110 & count < 130)
diskF1 <- factorX(diskF1[sample(1:nrow(diskF1),1000),])
diskN1 <- factorX(diskN[sample(1:nrow(diskN),1000),])
smartF1 <- subset(smartF,sn %in% diskF1$sn)
smartN1 <- subset(smartN,sn %in% diskN1$sn)
names(smartN1)[5:19] <- paste('a',1:15,sep='')
n <- paste('a',1:15,sep='')

# S1. calculate number and amplitude of changing
chaNumAmp <- function(arr){
  len <- length(arr)
  tmp <- arr[2:len] - arr[1:(len-1)]
  data.frame(posNum = sum(tmp > 0),
       negNum = sum(tmp < 0),
       posAmp = mean(tmp[tmp > 0]),
       negAmp = mean(tmp[tmp < 0]))
}

chaAttr <- function(df){
  r <- lapply(n,function(x){
    r1 <- do.call(rbind,tapply(df[[x]],df$sn,chaNumAmp))
    names(r1) <- paste(x,names(r1))
    r1
  })
  r <- do.call(cbind,r)
}

rF <- chaAttr(smartF1)
rN <- chaAttr(smartN1)
sumFN <- rbind(summary(rF),summary(rN))

# we can delete attr a6,a9,a13 absolustely
# we can delete attr a2,a8
# we can consider to delete attr a5,a7,a11,a14