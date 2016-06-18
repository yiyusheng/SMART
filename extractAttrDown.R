# Extract features for lead time prediction
# We extract value decreation for attr of SMART 
# Date: 2016-06-17
# Author: Pezy

rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
library(ggplot2)
load(file.path(dir_data,'ykliu_smart.Rda'))
load(file.path(dir_data,'SMARTprepN1w.Rda'))
load(file.path(dir_data,'smartName.Rda'))
load(file.path(dir_data,'diskInfoForEach.Rda'))

####################################
# S1. extract decreasing smart items
smartF <- factorX(subset(smart,time < failed_time & sn %in% diskInfoF$sn[diskInfoF$countBeforeF > 100]))
nameAttr <- paste('a',1:15,sep='')

chgAttr <- function(df,at){
  len <- nrow(df)
  tmp <- df[2:len,at] - df[1:(len-1),at]
  tmp <- c(0,tmp)
  idx <- which(tmp != 0)
  leni <- length(idx)
  
  if(leni == 0){
    return(NULL)
  }else{
    tmpt <- difftime(df$time[idx[2:leni]],df$time[idx[1:(leni-1)]],units = 'hours',tz = 'UTC')
    tmpt <- c(0,tmpt)
    r <- data.frame(sn = df$sn[1],
               value = df[idx,at],
               amp = tmp[idx],
               time = df[idx,'time'],
               internal = tmpt)
    return(r)
  }

}

r <- by(smartF,smartF$sn,chgAttr,at = nameAttr[3])
r <- do.call(rbind,r)

# calcAmp <- function(df){
#   m <- df[2:nrow(df),nameAttr] - df[1:(nrow(df)-1),nameAttr]
#   m <- rbind(rep(0,15),m)
#   m
# }
# amp <- by(smartF,smartF$sn,calcAmp)
# amp <- do.call(rbind,amp)
# amp <- cbind(smartF[,1:4],amp)
# 
# # S2. extract changed item and remove unchanged item
# nameAttr1 <- nameAttr[c(1,3,10,15)]
# idxamp <- apply(amp[,nameAttr1],1,function(x){all(x == 0)})
# # idxamp <- apply(amp[,nameAttr],1,function(x){all(x == 0)})
# ampEffect <- amp[!idxamp,]
# count <- tapply(ampEffect$sn,ampEffect$sn,length)
# ampEffect <- factorX(subset(ampEffect,sn %in% names(count)[count > 10]))
# ampEffect <- ampEffect[order(ampEffect$sn,ampEffect$time),]
# calcTime <- function(df){
#   t <- difftime(df[2:nrow(df)])
#   m <- df[2:nrow(df),nameAttr1] - df[1:(nrow(df)-1),nameAttr1]
#   m <- rbind(rep(0,15),m)
#   m
# }