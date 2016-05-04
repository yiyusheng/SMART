# Statistic data getting from ykliu
rm(list = ls())
#@@@ CONFIGURE @@@#
source(file.path('D:/Git/SMART','SMARTConfig.R'))

#@@@ Function @@@#
source('D:/Git/R_Function/Rfun.R')
source(file.path(dir_code,'SMARTFunc.R'))

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'diskInfo.Rda'))
load(file.path(dir_data,'ykliu_smart.Rda'))   #from ykliu
load(file.path(dir_data,'extractSMART.Rda'))  #from york
load(file.path(dir_data,'load_ftr_attridOld.Rda'))
source(file.path(dir_code,'dataPrepareOld.R'))

######
#S1. check dev_class_id and business of disk from ykliu
smartM <- smart
smartM <- mchAttr(smartM,diskInfo,'svrid','sn')
smartM <- mchAttr(smartM,cmdb,'dev_class_id','svrid')
smartM <- factorX(smartM)
cmdbM <- factorX(subset(cmdb,svrid %in% smartM$svrid))
#extract sn
sti$diSn <- gsub('.*SN:(\\w{8})[,;].*','\\1',sti$hwinfo)

#S2. generate train and test set.
# clean smart from york
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
# Add failed time to york's smart
diskInfoSmp$f_time <- data.f$f_time[match(diskInfoSmp$svrid,data.f$svrid)]
diskInfoSmp$f_time[is.na(diskInfoSmp$f_time)] <- as.POSIXct('1970-01-01',tz='UTC')
smartN$class <- diskInfoSmp$f_time[match(smartN$sn,diskInfoSmp$sn)]
names(smartN)[3] <- 'failed_time'
smartN$group <- 'york'
# convert failed_time to POSIXct for ykliu's smart
smart$failed_time <- as.POSIXct(smart$failed_time,tz = 'UTC')
smart$time <- as.POSIXct(smart$time,tz = 'UTC')
smart$group <- 'ykliu'
smart$label <- 1
# merge two classes
smartM <- rbind(smart,smartN)
# train set
# a <- smartM[duplicated(smartM$sn,smartM$time),]
