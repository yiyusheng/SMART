# Compare york's failure record and failed_time in ykliu's smart
# Date: 2016-06-14
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'diskInfo.Rda'))
load(file.path(dir_data,'ykliu_smart.Rda'))   #from ykliu
load(file.path(dir_data,'load_ftr_attridOld.Rda'))
source(file.path(dir_code,'dataPrepareOld.R'))

####################################
# S1. sn and failed_time in smart
smartIntro <- smart[!duplicated(smart[,c('sn','failed_time')]),c('sn','failed_time')]
row.names(smartIntro) <- NULL
smartIntro$svrid <- diskInfo$svrid[match(smartIntro$sn,diskInfo$sn)]

# S2. svrid and failed_time in tmp.f
cmdbIntro <- tmp.f[!duplicated(tmp.f[,c('svrid','f_time')]),c('svrid','f_time')]
row.names(cmdbIntro) <- NULL

# S3. merge
compFtime <- merge(cmdbIntro,smartIntro,by = 'svrid',all = T)
compFtime$failed_time <- as.POSIXct(compFtime$failed_time,tz = 'UTC')
compFtime <- subset(compFtime,failed_time < as.POSIXct('2014-08-01') & failed_time > as.POSIXct('2014-06-01'))
compFtime$diff <- difftime(compFtime$f_time,compFtime$failed_time,units = 'days',tz = 'UTC')
ggplot(compFtime,aes(x = diff)) + geom_histogram(binwidth = 7)
