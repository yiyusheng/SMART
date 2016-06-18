# Extract data from files export from mysql into Rda. 
# We have done this in 2016-05-03 with some mistakes
# Date: 2016-06-14
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'old','ykliu','ykliu_smart_old.Rda'))

####################################
# S1. get names
name.dis <- names(dis)
name.ct <- names(ct)
name.smart <- names(smart)
name.sn <- names(sn)
name.sti <- names(sti)

# S2. read table
dis <- read.table(file = file.path(dir_data,'old','ykliu','distinctbad'),fill = T,header = F,fileEncoding = 'utf8',sep = '\t')
ct <- read.table(file = file.path(dir_data,'old','ykliu','checked_table'),fill = T,header = F,fileEncoding = 'utf8',sep = '\t')
smart <- read.table(file = file.path(dir_data,'old','ykliu','bad_disk_smart_7_10'),fill = T,header = F,fileEncoding = 'utf8',sep = '\t')
sn <- read.table(file = file.path(dir_data,'old','ykliu','selected_sn'),fill = T,header = F,fileEncoding = 'utf8',sep = '\t')
sti <- read.table(file = file.path(dir_data,'old','ykliu','sn_to_ip'),fill = T,header = F,fileEncoding = 'utf8',sep = '\t')
# backup is the same as bad_disk_smart_7_10 except witthout time on failed_time and create_time
# smartA <- read.table(file = file.path(dir_data,'old','ykliu','backup'),fill = T,header = F,fileEncoding = 'utf8',sep = '\t')

names(dis) <- name.dis
names(ct) <- name.ct
names(smart) <- name.smart
names(sn) <- name.sn
names(sti) <- name.sti
smart <- smart[!duplicated(smart[,c('sn','time')]),]
smart$failed_time <- as.POSIXct(smart$failed_time,tz = 'UTC')
smart$time <- as.POSIXct(smart$time,tz = 'UTC')
smart <- factorX(smart[order(smart$sn,smart$time),])
row.names(smart) <- NULL
save(dis,ct,smart,sn,sti,file = file.path(dir_data,'ykliu_smart.Rda'))
