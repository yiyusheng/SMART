#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: readSmart2016.R
#
# Description: Read original smart data which is extracted from MySQL database.
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-04-19 16:47:49
#
# Last   modified: 2016-10-25 12:56:25
#
#
#
rm(list = ls())
source('head.R')

names_smart <- c('id','sn','time','ftime','ip','device','modelNum',
                  'Raw_Read_Error_Rate_Value','Spin_Up_Time_Value',
                  'Reallocated_Sector_Ct_Value','Reallocated_Sector_Ct_Raw',
                  'Seek_Error_Rate_Value','Spin_Retry_Count_Value',
                  'Calibration_Retries_Value','Unsafe_Shutdown_Count_Value',
                  'Power_Cycle_Count_Value','PowerOnHours_Count_Value',
                  'Offline_Uncorrectable_Value','Temperature_Celsius_Value',
                  'Udma_CRC_Error_Count_Value','Current_Pending_Sector_Value',
                  'Current_Pending_Sector_Raw')

fname <- list.files(file.path(dir_data,'split'))
# fname <- fname[1:2]

read_smart <- function(i){
  smart <- read.table(file.path(dir_data,'split',fname[i]),header = F,fill = T,
                      sep=',',col.names = names_smart,stringsAsFactors = F,fileEncoding = 'ASCII')
  
  save(smart,file = file.path(dir_data,'splitRda',paste(fname[i],'.Rda',sep='')))
  return(1)
}

para <- 1:length(fname)
require(doParallel)
ck <- makeCluster(min(40,length(para)), outfile = '')
registerDoParallel(ck)
r <- foreach(i = para,
             .combine = rbind,.export = c('fct2ori','fct2oriX'),
             .verbose = T) %dopar% read_smart(i)
stopCluster(ck)

