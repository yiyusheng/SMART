# Statistic smart start time and end time
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
library(ggplot2)

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'diskInfoTime.Rda'))
load(file.path(dir_data,'diskInfo.Rda'))
load(file.path(dir_data,'ykliu_smart.Rda')) 
load(file.path(dir_data,'load_ftr_attridOld.Rda'))
source(file.path(dir_code,'dataPrepareOld.R'))

# S1.plot last recording time from york's smart 
# Since most disks are normal, we filter last recording time behind 2014-07-31 to find failed disk.
lastYork <- diskInfoTime
# lastYork <- subset(diskInfoTime,max < as.POSIXct('2014-07-31'))
# p1 <- ggplot(dit,aes(x = max)) + geom_histogram(binwidth = 86400)
# print(p1)

# S2. calculate time from ykliu's smart
smart$time <- as.POSIXct(smart$time,tz = 'UTC')
tmp <- tapply(smart$time,smart$sn,function(data){
  data.frame(min = min(data),max = max(data),count = length(data))
})
tmp <- do.call(rbind,tmp)
tmp$sn <- levels(smart$sn)
tmp <- tmp[,c('sn','min','max','count')]
row.names(tmp) <- NULL
lastYkl <- tmp
# lastYkl <- subset(tmp,max < as.POSIXct('2014-10-20'))
# p3 <- ggplot(tmp1,aes(x = max)) + geom_histogram(binwidth = 86400)
# print(p3)

# S3. check table sn_to_ip
sn_to_ip <- read.table(file = file.path(dir_data,'sn_to_ip'),
                       fill = T,sep='\t',header = F,fileEncoding = 'utf8')
names(sn_to_ip) <- names(sti)
sn_to_ip$create_time <- as.POSIXct(as.character(sn_to_ip$create_time),tz = 'UTC')
sn_to_ip$last_time <- as.POSIXct(as.character(sn_to_ip$last_time),tz = 'UTC')
# sn_to_ip$recover_time <- as.POSIXct(as.character(sn_to_ip$recover_time),tz = 'UTC')

strAc <- 'SN: .*,'
idxAc <- grep(strAc,sn_to_ip$alarm_content)
ruleSN <- regexpr(strAc,sn_to_ip$alarm_content[idxAc])
SN <- regmatches(sn_to_ip$alarm_content[idxAc],ruleSN)
SN <- gsub('SN: |,','',SN)
sn_to_ip$snAc <- ''
sn_to_ip$snAc[idxAc] <- SN
sn_to_ip$snAc[sn_to_ip$snAc == '未知'] <- ''

strHw <- 'SN:.*[,;]'
idxHw <- grep(strHw,sn_to_ip$hwinfo)
ruleHw <- regexpr(strHw,sn_to_ip$hwinfo[idxHw])
SN <- regmatches(sn_to_ip$hwinfo[idxHw],ruleHw)
SN <- gsub('SN:|,|;|Category.*','',SN)
sn_to_ip$snHw <- ''
sn_to_ip$snHw[idxHw] <- SN

sn_to_ip$SN <- ''
sn_to_ip$SN[sn_to_ip$snAc == sn_to_ip$snHw] <- sn_to_ip$snAc[sn_to_ip$snAc == sn_to_ip$snHw]
sn_to_ip$SN[sn_to_ip$SN == '' & sn_to_ip$snAc != ''] <- sn_to_ip$snAc[sn_to_ip$SN == '' & sn_to_ip$snAc != '']
sn_to_ip$SN[sn_to_ip$SN == '' & sn_to_ip$snHw != ''] <- sn_to_ip$snHw[sn_to_ip$SN == '' & sn_to_ip$snHw != '']
sn_to_ip$SN[sn_to_ip$snHw != sn_to_ip$snAc & sn_to_ip$snHw != '' & sn_to_ip$snAc != ''] <- 
  sn_to_ip$snHw[sn_to_ip$snHw != sn_to_ip$snAc & sn_to_ip$snHw != '' & sn_to_ip$snAc != '']

noSN <- subset(sn_to_ip,SN == '')
sn_to_ip <- subset(sn_to_ip,SN != '')
names(sn_to_ip)[names(sn_to_ip) == 'SN'] <- 'sn'
sn_to_ip$sn <- factor(sn_to_ip$sn)
sn_to_ip <- factorX(sn_to_ip)
ftimeSni <- data.frame(sn = levels(sn_to_ip$sn),
                       create_time = as.POSIXct.numeric(tapply(sn_to_ip$create_time,sn_to_ip$sn,max),
                                                        origin = '1970-01-01',tz = 'UTC'))

# S4. checked table statistic
ct$failed_time <- as.POSIXct(ct$failed_time,tz = 'UTC')
ctYkl <- ct

# S5. check failed time in ykliu's smart
load(file.path(dir_data,'SMARTprepF4h.Rda'))
smart$failed_time <- as.POSIXct(smart$failed_time,tz = 'UTC')
ftimeYkl <- data.frame(sn = levels(smart$sn),
                     failed_time = as.POSIXct.numeric(tapply(smart$failed_time,smart$sn,max),
                                                      origin = '1970-01-01',tz = 'UTC'),
                     count = as.numeric(tapply(smart$failed_time,
                                                       smart$sn,function(x)length(unique(x)))))
ftimeYkl$failed_time <- unname(ftimeYkl$failed_time)


# S6. check whether dis(from ykliu's smart) is a subset of smart(from ykliu's smart)
dis$failed_time <- as.POSIXct(dis$failed_time,tz = 'UTC')
dis$time <- as.POSIXct(dis$failed_time,tz = 'UTC')
diff <- difftime(dis$time,dis$failed_time,units = c('days'),tz = 'UTC')

# S7. merge and compare all times
smartTime <- ftimeYkl
row.names(smartTime) <- NULL
smartTime <- merge(smartTime,ct,by = 'sn',all.x = T)
smartTime <- merge(smartTime,ftimeSni[,c('sn','create_time')],by = 'sn',all.x = T)
smartTime <- merge(smartTime,lastYkl[,c('sn','max')],by = 'sn',all.x = T)
smartTime <- merge(smartTime,lastYork[,c('sn','max')],by = 'sn',all.x = T)

names(smartTime) <- c('sn','ftimeSMART','count','ftimeCT','reason','createTimeSTI','lastYkl','lastYork')
smartTime <- smartTime[,c(1,2,4,6:8,3,5)]

# S8. check disk with more than one failed time in smart
fcount <- tapply(smart$failed_time,smart$sn,function(x)length(unique(x)))
multiFailSn <- names(fcount)[fcount > 1]
tmp <- subset(smart,sn %in% multiFailSn[])
tmp <- factorX(tmp[!duplicated(tmp[,c('sn','failed_time')]),])

# ct$svrid <- diskInfo$svrid[match(ct$sn,diskInfo$sn)]
# ct$fsTime <- diskInfo$fsTime[match(ct$sn,diskInfo$sn)]
# ct$f_time <- data.f$f_time[match(ct$svrid,data.f$svrid)]
# ct$f_timeSMART <- smart$failed_time[match(ct$sn,smart$sn)]
# ct$diffT1 <- difftime(ct$f_timeSMART,ct$failed_time,units = 'days',tz = 'UTC')
# ct <- ct[order(abs(ct$diffT1),decreasing = T),]
# df <- subset(data.fMore,svr_id %in% ct$svrid & f_time > as.POSIXct('2014-01-01'))
# ctDf <- subset(ct,svrid %in% df$svr_id)
# df <- df[order(df$svr_id),]
# ctDf <- ctDf[order(ctDf$svrid),]
# 
# ct1 <- subset(ct,!is.na(f_time))
# ct1$diffTime <- difftime(ct1$f_time,ct1$failed_time,units = 'days',tz = 'UTC')
# ct1 <- ct1[order(ct1$diffTime),]
# data.f1 <- data.frame(svrid = levels(data.f$svrid),
#                       count = as.numeric(tapply(data.f$svrid,data.f$svrid,length)))
# ct1$fCount <- data.f1$count[match(ct1$svrid,data.f1$svrid)]
# ct2 <- subset(ct,!is.na(svrid))
# ct2$dev_class_id <- cmdb$dev_class_id[match(ct2$svrid,cmdb$svrid)]
# ct2$bs3 <- cmdb$bs3[match(ct2$svrid,cmdb$svrid)]
# ct2 <- factorX(ct2)