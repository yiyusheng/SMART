#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_smart_vibration_day.R
#
# Description: To propose a SMART data requirement. We need the vibration of SMART attributes in one day.
# If the vibration is not large, we need one value per day. If not, we need more value per day.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-05-03 17:13:45
#
# Last   modified: 2017-05-03 17:13:46
#
#
#

rm(list = ls());setwd('~/Code/R/SMART/');source('~/rhead')
source('~/Code/R/SMART/Statistic_SMART/locate_fail_diskFunc.R')

sta_smart_vibration_day <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\tfile:%s\tSTART!!!\n',date(),fn))
  load(file.path(dir_dataSMART14,fn))
  
  # staD <- factorX(subset(dt,fn == fn,c('sn','date','count')))
  staD <- factorX(splitSDM[[fn]][,c('sn','date','count')])
  staD$st <- paste(staD$sn,staD$date,sep = '-')
  
  DT <- factorX(subset(smart,time > dayS & time < dayE))
  DT <- DT[,c('sn','time',col_smart)]
  DT$time <- as.Date(DT$time)
  DT$st <- factor(paste(DT$sn,DT$time,sep = '-'))
  DT <- factorX(subset(DT,st %in% staD$st))
  
  r_mean <- aggregate(DT[,col_smart],by = list(DT$st),mean)
  r_sd <- aggregate(DT[,col_smart],by = list(DT$st),sd)
  r_count <- aggregate(DT[,col_smart],by = list(DT$st),function(x)length(unique(x)))
  return(list(r_mean,r_sd,r_count))
}

###### MAIN ######
load(file.path(dir_data,'locate_fail_disk.Rda'))
dayS <- as.p('2014-07-10');dayE <- as.p('2014-08-01');seqDay <- seq.Date(as.Date(dayS),as.Date(dayE),by = 1)
col_day <- intersect(names(sta_date),as.character(seqDay))
sta_date <- sta_date[,c('sn','svrid','fn',col_day)]
sta_date_melt <- melt(sta_date,id.vars = c('sn','svrid','fn'))
sta_date_melt <- subsetX(sta_date_melt,value == 8)
names(sta_date_melt) <- c('sn','svrid','fn','date','count')
splitSDM <- split(sta_date_melt,sta_date_melt$fn)


fname <- list.files(dir_dataSMART14)
idx <- seq_len(length(fname))
r <- foreachX(idx,'sta_smart_vibration_day')
save(r,file = file.path(dir_data,'sta_smart_vibration_day.Rda'))
mean_sn_date <- lapplyX(r,'[[',1)
sd_sn_date <- lapplyX(r,'[[',2)
count_sn_date <- lapplyX(r,'[[',3)
save(mean_sn_date,sd_sn_date,count_sn_date,file = file.path(dir_data,'sta_smart_vibration_day.Rda'))

