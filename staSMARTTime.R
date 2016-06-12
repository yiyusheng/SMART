# Statistic smart start time and end time
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'diskInfoTime.Rda'))
load(file.path(dir_data,'diskInfo.Rda'))
load(file.path(dir_data,'ykliu_smart.Rda')) 
load(file.path(dir_data,'load_ftr_attridOld.Rda'))
source(file.path(dir_code,'dataPrepareOld.R'))

# S1.plot smart from york
dit <- subset(diskInfoTime,max < as.POSIXct('2014-07-31'))
p1 <- ggplot(dit,aes(x = max)) + geom_histogram(binwidth = 86400)
print(p1)

# S2. calculate time from ykliu's smart
smart$time <- as.POSIXct(smart$time,tz = 'UTC')
tmp <- tapply(smart$time,smart$sn,function(data){
  data.frame(min = min(data),max = max(data),count = length(data))
})
tmp <- do.call(rbind,tmp)
tmp$sn <- levels(smart$sn)
tmp <- tmp[,c('sn','min','max','count')]
row.names(tmp) <- NULL
p2 <- ggplot(tmp,aes(x = count)) + geom_histogram(binwidth = 100)
print(p2)
tmp1 <- subset(tmp,max < as.POSIXct('2014-10-20'))
p3 <- ggplot(tmp1,aes(x = max)) + geom_histogram(binwidth = 86400)
print(p3)

# S3. check table sn_to_ip
sn_to_ip <- read.table(file = file.path(dir_data,'stiTest'),fill = T,sep='\t',header = F,fileEncoding = 'utf8')


# S4. checked table statistic
ct$failed_time <- as.POSIXct(ct$failed_time,tz = 'UTC')
ct$svrid <- diskInfo$svrid[match(ct$sn,diskInfo$sn)]
ct$fsTime <- diskInfo$fsTime[match(ct$sn,diskInfo$sn)]
ct$f_time <- data.f$f_time[match(ct$svrid,data.f$svrid)]
ct$f_timeSMART <- smart$failed_time[match(ct$sn,smart$sn)]
ct$diffT1 <- difftime(ct$f_timeSMART,ct$failed_time,units = 'days',tz = 'UTC')
ct <- ct[order(abs(ct$diffT1),decreasing = T),]
df <- subset(data.fMore,svr_id %in% ct$svrid & f_time > as.POSIXct('2014-01-01'))
ctDf <- subset(ct,svrid %in% df$svr_id)
df <- df[order(df$svr_id),]
ctDf <- ctDf[order(ctDf$svrid),]

ct1 <- subset(ct,!is.na(f_time))
ct1$diffTime <- difftime(ct1$f_time,ct1$failed_time,units = 'days',tz = 'UTC')
ct1 <- ct1[order(ct1$diffTime),]
data.f1 <- data.frame(svrid = levels(data.f$svrid),
                      count = as.numeric(tapply(data.f$svrid,data.f$svrid,length)))
ct1$fCount <- data.f1$count[match(ct1$svrid,data.f1$svrid)]

ct2 <- subset(ct,!is.na(svrid))
ct2$dev_class_id <- cmdb$dev_class_id[match(ct2$svrid,cmdb$svrid)]
ct2$bs3 <- cmdb$bs3[match(ct2$svrid,cmdb$svrid)]
ct2 <- factorX(ct2)

# S5. check failed time and max time
load(file.path(dir_data,'SMARTprepF4h.Rda'))
dfTime <- data.frame(sn = levels(smartF$sn),
                     failed_time = as.POSIXct.numeric(tapply(smartF$failed_time,smartF$sn,max),
                                                      origin = '1970-01-01',tz = 'UTC'),
                     last_time = as.POSIXct.numeric(tapply(smartF$time,smartF$sn,max),
                                                    origin = '1970-01-01',tz = 'UTC'))
dfTime$diff <- difftime(dfTime$last_time,dfTime$failed_time,units = 'days',tz = 'UTC')
ct1 <- subset(ct)

# S6. sn_to_ip
sti$create_time <- as.POSIXct(sti$create_time,tz = 'UTC')
sti$last_time <- as.POSIXct(sti$last_time,tz = 'UTC')
