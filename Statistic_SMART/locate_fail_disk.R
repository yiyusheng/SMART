#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: locate_fail_disk.R
#
# Description: Try to locate failed disk by changing of sn. 
# If there is a failed disk, datacenter replace a new disk for it.
# Then the sn of disk will change. I would like to search the changed sn(sn not last to last)
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-24 20:36:34
#
# Last   modified: 2017-04-24 20:36:36
#
#
#
rm(list = ls());setwd('~/Code/R/SMART/');source('~/rhead')
source('~/Code/R/SMART/Statistic_SMART/locate_fail_diskFunc.R')

###### MAIN ######
fname <- list.files(dir_dataSMART14)
idx <- seq_len(length(fname))
r <- foreachX(idx,'locate_fail_disk')
sta_ss <- lapplyX(r,'[[',1)
sta_date <- rbind_sta_date(r)
sta_sn <- sta_sn_count(sta_date,as.p('2014-05-01'),as.p('2014-08-01'),T)
save(sta_ss,sta_date,sta_sn,file = file.path(dir_data,'locate_fail_disk.Rda'))
