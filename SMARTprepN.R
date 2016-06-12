# Sequenize smart from york 
# I extract smart from 300000 disks, 20 days' smart samples per disk, to be the negative set.
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'diskInfo.Rda'))
load(file.path(dir_data,'extractSMART20d.Rda'))  #from york New,containing 300k disks
load(file.path(dir_data,'load_ftr_attridOld.Rda'))
source(file.path(dir_code,'dataPrepareOld.R'))

####################################
# S1.1 For york's smart(smartN).
# add label + remove item with value out of range
smartNeed$class <- diskInfoSmp$class[match(smartNeed$sn,diskInfoSmp$sn)]
smartN <- subset(smartNeed,1==1,c('sn','model','class','time',
                                        names(smartNeed)[c(7:9,21,10:20)]))
names(smartN)[5:19] <- paste('a',1:15,sep='')
smartN$label <- 0
smartN$label[smartN$class == 'Failure'] <- 1
smartN <- factorX(subset(smartN,
                       a1 >= 0 & a1 <= 200 & a2 >= 0 & a2 <= 200 & a3 >= 0 & a3 <= 200 &
                       a5 >= 0 & a5 <= 200 & a6 >= 0 & a6 <= 200 & a9 >= 0 & a9 <= 200 &
                       a10 >= 0 & a10 <= 200 & a11 >= 0 & a11 <= 200 & a12 >= 0 & a12 <= 200 &
                       a13 >= 0 & a13 <= 200 & a14 >= 0 & a14 <= 200 & !is.na(a15)))

# S1.2 Add failed time to york's smart
diskInfoSmp$f_time <- data.f$f_time[match(diskInfoSmp$svrid,data.f$svrid)]
diskInfoSmp$f_time[is.na(diskInfoSmp$f_time)] <- as.POSIXct('1970-01-01',tz='UTC')
smartN$class <- diskInfoSmp$f_time[match(smartN$sn,diskInfoSmp$sn)]
names(smartN)[3] <- 'failed_time'

# S1.3 decrease number of normal disk
numSN <- 10
snCount <- melt(table(smartN$sn))
names(snCount) <- c('sn','count')
snCountCond <- subset(snCount,count >= 128 & count <= 130)
selectSN <- factorX(snCountCond[sample(1:nrow(snCountCond),numSN*10000),])

# S2. Adjust time interval for failure disk
smartN$seqTime <- smartN$time
names(smartN)[5:19] <- names(smartNeed)[c(7:9,21,10:20)]
smartN <- factorX(subset(smartN,sn %in% selectSN$sn))
row.names(smartN) <- NULL
save(smartN,selectSN,snCount,file = file.path(dir_data,
                                              paste('SMARTprepN',numSN,'w.Rda',sep='')))
