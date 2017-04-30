# Sequenize smart from york and ykliu and merge them.
# Because smart from ykliu are all from failed disk.
# I extract smart from 300000 disks, 10 smart samples per disk, to be the negative set.
# Here I merge them and adjust their sampling interval to a const value.

rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'diskInfo.Rda'))
load(file.path(dir_data,'ykliu_smart.Rda'))   #from ykliu
load(file.path(dir_data,'extractSMART.Rda'))  #from york New,containing 300k disks
load(file.path(dir_data,'load_ftr_attridOld.Rda'))
source(file.path(dir_code,'dataPrepareOld.R'))

####################################
# # S1. process smart respectively and Merge
# 
# S1.1 For york's smart(smartN).
# add label + remove item with value out of range
smartNeed <- mchAttr(smartNeed,diskInfoSmp,'class','sn')
smartN <- subset(smartNeed,1==1,c('sn','model','class','time',
                                        names(smartNeed)[c(7:9,21,10:20)]))
names(smartN)[5:19] <- paste('a',1:15,sep='')
smartN$label <- 0
smartN$label[smartN$class == 'Failure'] <- 1
smartN <- factorX(subset(smartN,
                       a1 >= 0 & a1 <= 253 & a2 >= 0 & a2 <= 253 & a3 >= 0 & a3 <= 253 &
                       a5 >= 0 & a5 <= 253 & a6 >= 0 & a6 <= 253 & a9 >= 0 & a9 <= 253 &
                       a10 >= 0 & a10 <= 253 & a11 >= 0 & a11 <= 253 & a12 >= 0 & a12 <= 253 &
                       a13 >= 0 & a13 <= 253 & a14 >= 0 & a14 <= 253 & !is.na(a15)))

# S1.2 Add failed time to york's smart
diskInfoSmp$f_time <- data.f$f_time[match(diskInfoSmp$svrid,data.f$svrid)]
diskInfoSmp$f_time[is.na(diskInfoSmp$f_time)] <- as.POSIXct('1970-01-01',tz='UTC')
smartN$class <- diskInfoSmp$f_time[match(smartN$sn,diskInfoSmp$sn)]
names(smartN)[3] <- 'failed_time'
# smartN$group <- 'york'

# S1.3 Convert failed_time to POSIXct for ykliu's smart
smart$failed_time <- as.POSIXct(smart$failed_time,tz = 'UTC')
smart$time <- as.POSIXct(smart$time,tz = 'UTC')
attr(smartN$time,'tzone') <- 'UTC'
smart$label <- 1
# smart$group <- 'ykliu'

# S1.4 Save
save(smartN,smart,file = file.path(dir_data,'smartMerge_smart.Rda'))

# S2. Adjust time interval for failure disk
timeseq <- seq.POSIXt(as.POSIXct('2014-06-01',tz = 'UTC'),as.POSIXct('2014-11-01',tz='UTC'),by = 3600*2)
sn <- levels(smart$sn)

adjTime <- function(data){
  t1 <- proc.time()
  idx <- rep(0,length(timeseq))
  for (i in 1:length(timeseq)){
    tmp <- abs(difftime(timeseq[i],data$time,units = 'day'))
    idx[i] <- which(tmp == min(tmp))
  }
  idxA <- which(idx>1 & idx < max(idx))
  s <- cbind(data[idx[idxA],],seqTime = timeseq[idxA])
  t2 <- proc.time()
  print(sprintf('No.%f SN:%s Duration:%f nrowOri:%f nrowAdd:%f rate:%f',
                which(data$sn[1] == sn),data$sn[1],t2[3]-t1[3],nrow(data),nrow(s),nrow(s)/nrow(data)))
  s
}

smartN$seqTime <- smartN$time
#smart <- by(smart,factor(smart$sn),adjTime)
#save(smart,file = file.path(dir_data,'SMARTMerge_list.Rda'))
#smartF <- do.call(rbind,smart)
names(smartN)[5:19] <- names(smartNeed)[c(7:9,21,10:20)]
#names(smartF)[5:19] <- names(smartNeed)[c(7:9,21,10:20)]
smartN1 <- smartN
load(file.path(dir_data,'SMARTMerge_Int2h.Rda'))
smartN <- smartN1
save(smartF,smartN,file = file.path(dir_data,'SMARTMerge_Int2h20d.Rda'))

 


# S2. statistic time interval.
# load(file.path(dir_data,'smartMerge_smart.Rda'))
# staSmpItv <- data.frame(sn = levels(smartM$sn),
#                         count = as.numeric(tapply(smartM$sn,smartM$sn,length)))
# tmp <- tapply(smartM$time,smartM$sn,function(x){
#   len <- length(x)
#   x <- sort(x)
#   diff <- difftime(x[2:len], x[1:(len-1)], units = 'hours')
#   summary(as.numeric(diff))[c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")]
# })
# tmp <- data.frame(matrix(unlist(tmp),byrow = T,nrow = length(tmp)))
# names(tmp) <- c("min","1st","median","mean","3rd","max")
# staSmpItv <- cbind(staSmpItv,tmp)
# ggplot(staSmpItv,aes(x = count)) + stat_ecdf()

