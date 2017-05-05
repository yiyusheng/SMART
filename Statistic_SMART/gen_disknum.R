#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: gen_disknum.R
#
# Description: generate number of disks for each server by two ways.
# 1. Counting the number of disk sn[we do not consider sn with only one disk]
# 2. Summerise the working days of each disk of a server as A and counting the total working days for the server as B. use A/B 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-05-02 20:02:39
#
# Last   modified: 2017-05-02 20:02:42
#
#
#
rm(list = ls());setwd('~/Code/R/SMART/');source('~/rhead')
load(file.path(dir_data,'locate_fail_disk.Rda'))

# 1.counting the number of disk sn
sta_svrid <- melt(table(sta_ss$svrid));names(sta_svrid) <- c('svrid','countSN')

# 2.main
dayS <- as.p('2014-07-10');dayE <- as.p('2014-08-01');seqDay <- seq.Date(as.Date(dayS),as.Date(dayE),by = 1)
col_day <- intersect(names(sta_date),as.character(seqDay))
sta_date_multidisks <- subsetX(sta_date,sn %in% sta_ss$sn[sta_ss$svrid %in% sta_svrid$svrid[sta_svrid$countSN > 1]],c('sn','svrid','fn',col_day))
sta_date_multidisks[,col_day][sta_date_multidisks[,col_day] > 1] <- 1
splitSDM <- get_split_index(sta_date_multidisks$svrid)

cnt <- 1
r <- lapply(splitSDM,function(s){
  cat(sprintf('%d\n',cnt));cnt <<- 1 + cnt
  df <- subset(sta_date_multidisks[s,])
  colsum_day <- colSums(df[,col_day])
  valid_cd <- colsum_day[colsum_day > 0]
  r <- data.frame(svrid = fct2ori(df$svrid[1]),
                  countSN = nrow(df),
                  validDays = sum(colsum_day > 0),
                  meanDaySN = mean(valid_cd),
                  sdDaySN = sd(valid_cd),
                  maxDaySN = max(valid_cd))
  r$validSN <- sum(colsum_day)/r$validDays
  r
})
sta_svrid_smart <- do.call(rbind,r);row.names(sta_svrid_smart) <- NULL
sta_svrid_smart <- subsetX(sta_svrid_smart,validDays != 0)
sta_svrid_smart$dutydisk <- 1
sta_svrid_smart$dutydisk[sta_svrid_smart$countSN != sta_svrid_smart$validSN] <- 0
save(sta_svrid_smart,file = file.path(dir_data,'gen_disknum.Rda'))

###### COMPARISON ######
load(file.path(dir_dataLD15,'partIO.Rda'))
load(file.path(dir_dataCF,'merge_svrnum_svrid_1406_1409.Rda'))
sta_svrid_IO <- partSvrid[,c('svr_id','maxLenIOPS')];names(sta_svrid_IO) <- c('svrnum','countIOPS');sta_svrid_IO$countIOPS <- sta_svrid_IO$countIOPS/2
sta_svrid_IO$svrid <- FR15M$svrid[match(sta_svrid_IO$svrnum,FR15M$svrnum)];sta_svrid_IO <- subsetX(sta_svrid_IO,!is.na(svrid))
sta_svrid <- merge(sta_svrid_IO,sta_svrid_smart[c('svrid','countSN','validSN','dutydisk')],by = 'svrid')

sta_svrid_duty <- subsetX(sta_svrid,dutydisk == 1)
sta_svrid_noduty <- subsetX(sta_svrid,dutydisk == 0)
table_duty <- melt_table(sta_svrid_duty$countIOPS,sta_svrid_duty$countSN);names(table_duty) <- c('countIOPS','countSN','count')
table_noduty <- melt_table(sta_svrid_noduty$countIOPS,sta_svrid_noduty$countSN);names(table_noduty) <- c('countIOPS','countSN','count')
ggplot(sta_svrid,aes(x = factor(countIOPS),y = countSN)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2,alpha = 0.1)

# for disk of sn 3
sta_svrid_3i <- subsetX(sta_svrid,countIOPS == 3)
sta_ss_3i <- subsetX(sta_ss,svrid %in% sta_svrid_3i$svrid)

sta_ss$prefix <- substr(sta_ss$sn,1,3)
table_prefix <- melt(table(sta_ss$prefix))
table_model_prefix <- melt_table(sta_ss$model,sta_ss$prefix)
