#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: find_rules_fail_disks.R
#
# Description: I wanna find the rules to decide a server being replaced disks.
# I give each svrid features and use decision tree to predict failed disks(f2014_06_09) in order to increase the FDR when FAR is 0
# Then I parse the rules established by the decision tree.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-25 15:25:45
#
# Last   modified: 2017-04-25 15:25:46
#
#
#

rm(list = ls());setwd('~/Code/R/SMART/');source('~/rhead')
source('~/Code/R/Load_Data_Config_Failure/loadFunc.R')
source('~/Code/R/SMART/Statistic_SMART/locate_fail_diskFunc.R')
source('~/Code/R/SMART/Statistic_SMART/find_rules_fail_disksFunc.R')
load(file.path(dir_data,'locate_fail_disk.Rda'))
load(file.path(dir_dataCF,'uwork2014_06_09.Rda'))
# load(file.path(dir_dataCF,'serverInfo.Rda'))

# statistics of svrid
sta_svrid <- melt(table(sta_ss$svrid));names(sta_svrid) <- c('svrid','countSN')

# Failure record
dayS <- as.p('2014-07-10');dayE <- as.p('2014-08-01')
FR_all <- factorX(f2014_06_09[!check_vm(f2014_06_09$svrid),])
FR_all <- check_disk_replacement(FR_all,valid = 1)
FR <- subsetX(FR_all,ftime > dayS & ftime <= dayE & svrid %in% sta_ss$svrid & svrid %in% sta_svrid$svrid[sta_svrid$countSN > 1])
col_day <- intersect(names(sta_date),as.character(seq.Date(as.Date(dayS),as.Date(dayE),by = 1)))

# get smart statistic data
# list[SS,SD,splitSD] <- get_smart_statistic_data(sta_date,dayS,dayE,col_day,NULL)
# splitSD_F <- splitSD[names(splitSD) %in% FR$svrid]
# save(col_day,SS,SD,splitSD,splitSD_F,file  = file.path(dir_data,'find_rules_fail_disks_data.Rda'))
load(file.path(dir_data,'find_rules_fail_disks_data.Rda'))

# We use the xor operation to check if there is a disk being a replacement of a failed one when the smart of the failed one disappear
cnt <- 1
r <- lapply(splitSD,function(df){
  cat(sprintf('[%s]\t%d\t%s\n',date(),cnt,df$svrid[1]));cnt <<- cnt+1
  cur_col_day <- col_day
  
  # remove invalid day and get data
  list[df,cur_col_day] <- remove_invalid_day(df,cur_col_day) # remove the invalid day for all rows
  df <- subset(df,count_valid_day != length(cur_col_day));if(nrow(df) == 0)return(0) # remove row of all duty. All dudy of all disks means no replacement
  list[df,cur_col_day] <- remove_invalid_day(df,cur_col_day) # remove the preparing days for failed rows.
  
  # seperate failed disk and disk candidate for replacement
  df_failed <- df[df[cur_col_day[length(cur_col_day)]] == 0,];if(nrow(df_failed) == 0)return(1) # define failed disk if the number of its SMART in the last day is zero.
  df_candi <- subset(df,!(sn %in% df_failed$sn));if(nrow(df_candi) == 0)return(2)

  
  r1 <- lapply(seq_len(nrow(df_failed)),function(i)find_replacement(i,cur_col_day,df_failed,df_candi))
  return(do.call(rbind,r1))
})

list[rst_svrid,rst_R,rst_N] <- get_result_replacement(r)
rst_N$ftag <- 'normal';rst_N$ftag[rst_N$svrid %in% FR$svrid] <- 'failure'
rst_svrid$ftag <- 'normal';rst_svrid$ftag[rst_svrid$svrid %in% FR$svrid] <- 'failure';rst_svrid$errorID <- as.numeric(rst_svrid$num_replaced_disk > 0)
save(rst_svrid,rst_R,rst_N,file = file.path(dir_data,'find_rules_fail_disks.Rda'))

table(rst_N$ftag,rst_N$errorID)
table(rst_svrid$ftag,rst_svrid$errorID)

###### ANALYSIS ######
SD$svrid <- factor(SD$svrid);SD$sn <- factor(SD$sn)
SD_integrated_fail <- subsetX(SD,svrid %in% rst_N$svrid[rst_N$ftag == 'failure' & rst_N$errorID == 1])
SD_replacing_fail <- subsetX(SD,svrid %in% rst_svrid$svrid[rst_svrid$ftag == 'failure' & rst_svrid$errorID == 1])
SD_replacing_fail_svrid <- melt(table(SD_replacing_fail$svrid))
names(SD_replacing_fail_svrid) <- c('svrid','count')
save(SD_replacing_fail_svrid,SD_replacing_fail,file = file.path(dir_data,'SD_replacing_fail_svrid.Rda'))
