# Info of each disk for their SMART
# Date: 2016-06-17
# Author: Pezy

rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
library(ggplot2)
load(file.path(dir_data,'ykliu_smart.Rda'))
load(file.path(dir_data,'SMARTprepN1w.Rda'))
load(file.path(dir_data,'smartName.Rda'))

####################################
# S1. extract info for each disk
smart$beforeF <- difftime(smart$failed_time,smart$time,tz = 'UTC')
diskInfoF <- data.frame(sn = levels(smart$sn),
                   model = factor(tapply(as.character(smart$model),smart$sn,unique)),
                   count = as.numeric(tapply(smart$sn,smart$sn,length)),
                   countBeforeF = as.numeric(tapply(smart$beforeF,smart$sn,function(x)sum(x > 0))),
                   countF = as.numeric(tapply(smart$failed_time,smart$sn,function(x)length(unique(x)))),
                   firstTime = as.POSIXct(tapply(smart$time,smart$sn,min),origin = '1970-01-01',tz = 'UTC'),
                   lastTime = as.POSIXct(tapply(smart$time,smart$sn,max),origin = '1970-01-01',tz = 'UTC'),
                   failed_time = as.POSIXct(tapply(smart$failed_time,smart$sn,max),origin = '1970-01-01',tz = 'UTC'))

diskInfoN <- data.frame(sn = levels(smart$sn),
                    model = factor(tapply(as.character(smart$model),smart$sn,unique)),
                    count = as.numeric(tapply(smart$sn,smart$sn,length)),
                    firstTime = as.POSIXct(tapply(smart$time,smart$sn,min),origin = '1970-01-01',tz = 'UTC'),
                    lastTime = as.POSIXct(tapply(smart$time,smart$sn,max),origin = '1970-01-01',tz = 'UTC'))
save(diskInfoF,diskInfoN,file = file.path(dir_data,'diskInfoForEach.Rda'))