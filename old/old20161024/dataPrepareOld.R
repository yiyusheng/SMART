# data prepare for AFR_io
names(cmdb)[3] <- 'svrid'
names(data.f)[2] <- 'svrid'
names(data.fAllDev)[2] <- 'svrid'
data.fFewDev <- subset(data.f,ip %in% cmdb$ip)

data.f <- subset(data.fAllDev,ip %in% cmdb$ip)
data.f$use_time <- cmdb$use_time[match(data.f$svrid,cmdb$svrid)]
data.f$failShiptime <- floor(data.f$f_time - data.f$use_time)
units(data.f$failShiptime) <- 'days'
data.f$failShiptime <- as.numeric(data.f$failShiptime)/365
data.f$fsTime <- floor(data.f$failShiptime)
data.f$fsTimeN <- cut(data.f$failShiptime,c(0,1/2,1:7),include.lowest = T)
data.f$fsTimeN <- gsub('^\\[|^\\(|,.*$','',data.f$fsTimeN)

cmdb$shiptimeToLeft <- floor(as.POSIXct('2014-06-01') - cmdb$use_time)
cmdb$shiptimeToRight <- floor(as.POSIXct('2014-08-01') - cmdb$use_time)
units(cmdb$shiptimeToLeft) <- 'days'
units(cmdb$shiptimeToRight) <- 'days'
cmdb$shiptimeToLeft <- as.numeric(cmdb$shiptimeToLeft)/365
cmdb$shTime <- floor(cmdb$shiptimeToLeft + (1/12))
cmdb$shTimeN <- cut(cmdb$shiptimeToLeft,c(0,1/2,1:7),include.lowest = T)
cmdb$shTimeN <- gsub('^\\[|^\\(|,.*$','',cmdb$shTimeN)
cmdb$dClass <- ''

class_C <- 'C1'
class_B <- c('B5','B6','B1')
# class_TS <- c('TS3','TS4','TS5','TS6')
class_TS <- c('TS1','TS3','TS4','TS5','TS6')
cmdb$dClass[cmdb$dev_class_id %in% class_C] <- 'C'
cmdb$dClass[cmdb$dev_class_id %in% class_B] <- 'B'
cmdb$dClass[cmdb$dev_class_id %in% class_TS] <- 'TS'

cmdbio <- subset(cmdb,svrid %in% mean_io$svrid & dev_class_id %in% c(class_C,class_TS))
cmdbio$total <- disk_ip$total[match(cmdbio$ip,disk_ip$ip)]
cmdbio <- subset(cmdbio,!is.na(total) & (dClass != 'C' | total %in% c(500,250,1000)))
cmdbio$totalMerge <- cmdbio$total
cmdbio$totalMerge[cmdbio$totalMerge <= 18000] <- 12000
cmdbio$totalMerge[cmdbio$totalMerge > 18000] <- 24000
cmdbio$dClass[cmdbio$dClass == 'TS' & cmdbio$totalMerge == 12000] <- 'TS1T'
cmdbio$dClass[cmdbio$dClass == 'TS' & cmdbio$totalMerge == 24000] <- 'TS2T'
mean_io <- subset(mean_io,svrid %in% factor(cmdbio$svrid))

tmp.cmdb <- cmdbio
tmp.f <- subset(data.f,svrid %in% cmdbio$svrid)
tmp.f$total <- tmp.cmdb$total[match(tmp.f$ip,tmp.cmdb$ip)]
tmp.f$shTime <- tmp.cmdb$shTime[match(tmp.f$ip,tmp.cmdb$ip)]
tmp.f$shTimeN <- tmp.cmdb$shTimeN[match(tmp.f$ip,tmp.cmdb$ip)]
tmp.f <- factorX(tmp.f)
tmp.io <- mean_io
tmp.io$dev_class_id <- cmdb$dev_class_id[match(tmp.io$svrid,cmdb$svrid)]
# lastYears <- 7

disk_ip$dClass <- cmdbio$dClass[match(disk_ip$ip,cmdbio$ip)]
tmp.io$dClass <- cmdbio$dClass[match(tmp.io$svrid,cmdbio$svrid)]
tmp.f$dClass <- cmdbio$dClass[match(tmp.f$svrid,cmdbio$svrid)]
tmp.io$shTime <- cmdbio$shTime[match(tmp.io$svrid,cmdbio$svrid)]
tmp.io$shTimeN <- cmdbio$shTimeN[match(tmp.io$svrid,cmdbio$svrid)]
tmp.io$ip <- cmdbio$ip[match(tmp.io$svrid,cmdbio$svrid)]
names(cmdb)[3] <- 'svrid'
