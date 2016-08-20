#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: readSmart2016.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-04-19 16:47:49
#
# Last   modified: 2016-04-19 17:28:53
#
#
#
rm(list = ls())
source('/home/yiyusheng/Code/R/SMART/head.R')
smart <- read.table(file.path(dir_data,'smart_all_2016'),
                    header = F,fill = T,sep='\t')
names(smart) <- c('id','sn','time','svr_ip','device','modelNum',
                  'Raw_Read_Error_Rate_Value','Spin_Up_Time_Value',
                  'Reallocated_Sector_Ct_Value','Reallocated_Sector_Ct_Raw',
                  'Seek_Error_Rate_Value','Spin_Retry_Count_Value',
                  'Calibration_Retries_Value','Unsafe_Shutdown_Count_Value',
                  'Power_Cycle_Count_Value','PowerOnHours_Count_Value',
                  'Offline_Uncorrectable_Value','Temperature_Celsius_Value',
                  'Udma_CRC_Error_Count_Value','Current_Pending_Sector_Value',
                  'Current_Pending_Sector_Raw')

save(smart,file = file.path(dir_data,'smart_all_2016.Rda'))
