#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: locate_fail_disk_analysis.R
#
# Description: 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-25 09:54:44
#
# Last   modified: 2017-04-25 09:54:45
#
#
#

rm(list = ls());setwd('~/Code/R/SMART/');source('~/rhead')
source('~/Code/R/Load_Data_Config_Failure/loadFunc.R')
source('~/Code/R/SMART/Statistic_SMART/locate_fail_diskFunc.R')

# Failure record
load(file.path(dir_data,'locate_fail_disk.Rda'))
load(file.path(dir_dataCF,'uwork2014_06_09.Rda'))
dayS <- as.p('2014-07-10');dayE <- as.p('2014-08-01')
FR_all <- factorX(f2014_06_09[!check_vm(f2014_06_09$svrid),])
FR_all <- check_disk_replacement(FR_all,valid = 1)
FR <- subsetX(FR_all,ftime > dayS & ftime <= dayE)

# statistic for each day of sn
SD <- factorX(sta_date)

# aggreate result of SD
SS <- factorX(sta_sn_count(SD,dayS,dayE))

###### MAIN ######
# Method1: examine server with more than one disks + one disks has different number of days of valid data with other disks.
# sta_svrid <- list2df(tapply(sta_sn$counSDay,sta_sn$svrid,length),n = c('countSN','svrid'))
sta_replace_svrid <- list2df(tapply(SS$countDay,SS$svrid,summary),n = c('min','Q1','median','mean','Q3','max','svrid'))
sta_sd <- list2df(tapply(SS$counSDay,SS$svrid,sd),n = c('sd','svrid'))
sta_replace_svrid$sd <- sta_sd$sd[match(sta_replace_svrid$svrid,sta_sd$svrid)]
srs_3 <- subsetX(sta_replace_svrid,sd > 3)

r_3 <- locate_evaluate(SS,FR,srs_3)

# Method2: examine number of days when valid data of smart is collected.
ss10 <- subsetX(SS,countDay < 20 & countDay > 0)
r10 <- locate_evaluate(SS,FR,ss10)

# we find that one sn can be insert into more than one servers.
sta_sn_count <- melt(table(sta_ss$sn));names(sta_sn_count) <- c('sn','count')
multi_server_sn <- subset(sta_sn_count,count > 1)
sta_ss_multi_server <- subsetX(sta_ss,sn %in% multi_server_sn$sn)
SD_multi_server <- subset(SD,sn %in% multi_server_sn$sn)
FR_all_multi_server <- subsetX(FR_all,svrid %in% sta_ss_multi_server$svrid)
