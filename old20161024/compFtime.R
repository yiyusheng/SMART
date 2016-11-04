# Compare york's failure record and failed_time in ykliu's smart
# Date: 2016-06-14
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'diskInfo.Rda'))
load(file.path(dir_data,'ykliu_smart.Rda'))   #from ykliu
load(file.path(dir_data,'..','attrid','dataPrepareAFR10-15.Rda'))

####################################
# S1. sn and failed_time in smart
ykliuF <- smart[!duplicated(smart[,c('sn','failed_time')]),c('sn','failed_time')]
sti$sn <- factor(gsub('SN:|,.*','',sti$hwinfo))
# ykliuF$svrid <- sti$svr_asset_id[match(ykliuF$sn,sti$sn)]
ykliuF$svrid <- diskInfo$svrid[match(ykliuF$sn,diskInfo$sn)]

yorkF <- factorX(subset(tmp.f,f_time >= as.p('2014-07-01') & f_time < as.p('2014-11-01')))

compFtime <- merge(ykliuF,yorkF[,c('svr_id','f_time')],by.y = 'svr_id',by.x = 'svrid')
compFtime$diff.time <- abs(as.numeric(difftime(compFtime$failed_time,compFtime$f_time,units = 'days')))










row.names(smartIntro) <- NULL
smartIntro$svrid <- diskInfo$svrid[match(smartIntro$sn,diskInfo$sn)]

# S2. svrid and failed_time in tmp.f
cmdbIntro <- tmp.f[!duplicated(tmp.f[,c('svr_id','f_time')]),c('svr_id','f_time')]
row.names(cmdbIntro) <- NULL

# S3. merge
compFtime <- merge(cmdbIntro,smartIntro,by.y = 'svrid',by.x = 'svr_id',all = T)
compFtime <- subset(compFtime,f_time < as.POSIXct('2014-05-01') & f_time > as.POSIXct('2014-11-01'))

compFtime$diff <- difftime(compFtime$f_time,compFtime$failed_time,units = 'days',tz = 'UTC')
ggplot(compFtime,aes(x = diff)) + geom_histogram(binwidth = 7)
