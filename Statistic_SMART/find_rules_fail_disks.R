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
table(rst_N$ftag,rst_N$errorID)
table(rst_svrid$ftag,rst_svrid$errorID)
# For SS
# splitSSF <- split(SS,SS$svrid)
# r <- lapply(splitSSF,function(df){
#   df <- splitSSF[[sample(seq_len(length(splitSSF)),1)]]
#   sid_df <- fct2ori(df$svrid[1]
#                     )
#   len_df <- nrow(df)
#   len_valid <- sum(df$count_valid_day == mode_value)
#   df_invalid <- subset(df,count_valid_day != mode_value)
#   
#   if(len_valid/len_df < 0.5){
#     return(sid_df)
#   }else{
#     return(df_invalid)
#   }
# })
# r_char<- lapplyX(r,function(x)if(is.character(x))return(x))
# r_valid <- lapplyX(r,function(x)if(!is.character(x))return(x))

# For SD, we check replacement of disk by comparison bewtween average number of working disks in each day and the number of disks recorded.
# mode_value <- as.numeric(names(sort(-table(SS$count_valid_day)))[1])
# server_replacement <- data.frame(svrid = levels(SS$svrid),
#                                  num_disks_recorded = as.numeric(tapply(SS$svrid,SS$svrid,length)),
#                                  num_disk_all_duty = as.numeric(by(SS$count_valid_day,SS$svrid,function(x)sum(x == mode_value))))
# server_replacement <- subsetX(server_replacement,num_disk_all_duty > 0)
# 
# # server_replacement$mode_days <- as.numeric(tapply(SS$count_valid_day,SS$svrid,mode_num))
# # server_replacement <- subsetX(server_replacement,mode_days >= 22)
# 
# 
# sta_days <- list2df(lapply(splitSSD, function(df){
#   num_working_disks_days <- colSums(df[,col_day])
#   roundX(mean(num_working_disks_days))
# }),n = c('mean_num_working_disks','svrid'))
# sta_days$digit <- sta_days$mean_num_working_disks - floor(sta_days$mean_num_working_disks)
# sta_days$integer <- sta_days$mean_num_working_disks - sta_days$digit
# sta_days$pred_num_working_disks <- sta_days$mean_num_working_disks
# sta_days$pred_num_working_disks[sta_days$digit > 0.95] <- sta_days$integer[sta_days$digit > 0.95] + 1
# sta_days$pred_num_working_disks[sta_days$digit < 0.05] <- sta_days$integer[sta_days$digit < 0.05]
# server_replacement$pred_num_working_disks <- sta_days$pred_num_working_disks[match(server_replacement$svrid,sta_days$svrid)]

# aggreate result of SD
# SS <- factorX(sta_sn_count(SD,dayS,dayE,T));save(SS,file = file.path(dir_data,'find_rules_fail_disks_SS.Rda'))
# load(file.path(dir_data,'find_rules_fail_disks_SS.Rda'))
# sn_in_two_server <- melt(table(sta_ss$sn));names(sn_in_two_server) <- c('sn','num_server')
# SS$num_server <- sn_in_two_server$num_server[match(SS$sn,sn_in_two_server$sn)]

# a example
# sid <- 'TYSV09123015';a <- subset(SS,svrid == sid);b <- subset(SD,svrid == sid);c <- subset(f2014_06_09,svrid == sid);d <- subset(cmdb,svr_asset_id == sid)

# if(nrow(df_candi) == 0){
#   tmp <- colSums(df[,cur_col_day])
#   idx_last_day <- which.max(tmp != 0)
#   df <- subset(df,count_valid_day != idx_last_day);if(nrow(df) == 0)return(2) # All dudy of all disks means no replacement
#   df_failed <- df[df[cur_col_day[idx_last_day]] == 0,];if(nrow(df_failed) == 0)return(3) # define failed disk if the number of its SMART in the last day is zero.
#   df_candi <- subset(df,!(sn %in% df_failed$sn));if(nrow(df_candi) == 0)return(4)
# }