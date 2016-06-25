# Extract features for lead time prediction
# We extract value decreation for attr of SMART 
# Date: 2016-06-17
# Author: Pezy

rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
library(ggplot2)
# load(file.path(dir_data,'ykliu_smart.Rda'))
load(file.path(dir_data,'SMARTprepF4h.Rda'))
load(file.path(dir_data,'SMARTprepN0.5w.Rda'))
load(file.path(dir_data,'smartName.Rda'))
load(file.path(dir_data,'diskInfoForEach0.5kN.Rda'))

###@@@ FUNCTION @@@###
# F1. calculate number of continuous true in a array
contTrue <- function(arr){
  len <- length(arr)
  r <- 1:len
  for (i in r){
    if (arr[i] == T){
      next
    }else{
      r[i:len] <- r[i:len] - r[i]
    }
  }
  r
}

attrChgAnal <- function(df,at,timeBnd = 24*60){
  len <- nrow(df)
  # Diff
  tmp <- df[2:len,at] - df[1:(len-1),at]
  tmp <- c(0,tmp)
  # Diff idx
  idx <- which(tmp != 0)
  leni <- length(idx)
  # Generate result
  r <- data.frame(amp = rep(0,nrow(df)),inte = 0,cont = 0)
  
  if(leni > 0){
    # Append Diff of value as amp
    r$amp[idx] = tmp[idx]
  }
  tmpt <- 0
  if(leni > 1){
    # Append Diff of time as internal
    tmpt <- difftime(df$time[idx[2:leni]],df$time[idx[1:(leni-1)]],units = 'hours',tz = 'UTC')
    tmpt <- c(0,tmpt)
    r$inte[idx] = tmpt
    # Append number of continuous time internal
    r$cont[idx] <- contTrue(timeBnd-tmpt > 0)
  }
  return(r)
}
####################################
# S1. filter and name columns
smartF1 <- factorX(subset(smartF,time < failed_time & sn %in% diskInfoF$sn[diskInfoF$countBeforeF > 100]))
smartN1<- factorX(subset(smartN, sn %in% diskInfoN$sn[diskInfoN$count > 100]))
smartF1$Sector_Sum <- smartF1$Reallocated_Sector_Ct_Raw + smartF1$Current_Pending_Sector_Raw
smartN1$Sector_Sum <- smartN1$Reallocated_Sector_Ct_Raw + smartN1$Current_Pending_Sector_Raw
nameAttr <- names(smartF1)[c(5:19,22)]
names(smartN1)[c(5:19,22)] <- nameAttr
na <- nameAttr[c(1,3,4,10,15,16)]
# na <- nameAttr

# S2. Processing
for (i in 1:length(na)){
  r1 <- do.call(rbind,by(smartF1,smartF1$sn,attrChgAnal,at = na[i]))
  names(r1) <- c(paste(na[i],'amp',sep=' '),
                paste(na[i],'int',sep=' '),
                paste(na[i],'cont',sep=' '))
  smartF1 <- cbind(smartF1,r1)
  
  # r2 <- do.call(rbind,by(smartN1,smartN1$sn,attrChgAnal,at = na[i]))
  # names(r2) <- c(paste(na[i],'amp',sep=' '),
  #               paste(na[i],'int',sep=' '))
  # smartN1 <- cbind(smartN1,r2)
}

# S3. Saving
smartF <- smartF1
save(smartF,file = file.path(dir_data,'SMARTprepF4hFtr.Rda'))
# smartN <- smartN1
# save(smartN,file = file.path(dir_data,'SMARTprepN0.5wFtr.Rda'))