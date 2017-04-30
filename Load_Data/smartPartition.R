#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: smartPartition.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-04-14 08:52:56
#
# Last   modified: 2016-04-19 11:16:24
#
#
#

rm(list = ls())
source('head.R')
stts <- list()

stts$t1 <- Sys.time()
load(file.path(dir_data,'AllSMART_20161025_clean.Rda'))
load(file.path(dir_dataSource,'load_ftr_attrid.Rda'))

stts$t2 <- Sys.time()
cat(sprintf('load data done\t elapse:%.2f mins\n',
            as.numeric(difftime(stts$t2,stts$t1,units = 'mins'))))

smart <- r
smart$svrid <- cmdb$svr_asset_id[match(smart$ip,cmdb$ip)]
smart <- subset(smart,!(is.na(svrid)))
smart$svrid <- factor(smart$svrid)

names_smart <- c('id','sn','time','ftime','ip','svrid','device','modelNum',
                 'Raw_Read_Error_Rate_Value','Spin_Up_Time_Value',
                 'Reallocated_Sector_Ct_Value','Reallocated_Sector_Ct_Raw',
                 'Seek_Error_Rate_Value','Spin_Retry_Count_Value',
                 'Calibration_Retries_Value','Unsafe_Shutdown_Count_Value',
                 'Power_Cycle_Count_Value','PowerOnHours_Count_Value',
                 'Offline_Uncorrectable_Value','Temperature_Celsius_Value',
                 'Udma_CRC_Error_Count_Value','Current_Pending_Sector_Value',
                 'Current_Pending_Sector_Raw')
smart <- smart[,names_smart]

stts$t3 <- Sys.time()
cat(sprintf('add svrid done\t elapse:%.2f mins\n',
            as.numeric(difftime(stts$t3,stts$t2,units = 'mins'))))

svrid <- sort(levels(smart$svrid))
chunk <- split(svrid,ceiling(seq_along(svrid)/5000))
len_chunk <- length(chunk)
stts$t4 <- Sys.time()
cat(sprintf('split svrid done\t elapse:%.2f mins\n',
            as.numeric(difftime(stts$t4,stts$t3,units = 'mins'))))

smart_merge <- smart
split_smart <- function(i){
    smart <- subset(smart_merge,svrid %in% chunk[[i]])
    smart <- factorX(smart[order(smart$svrid,smart$sn,smart$time),])
    save(smart,file = file.path(dir_data,'smart5k',paste('smart_',i,'.Rda',sep='')))
    cat(sprintf('split_smart [%d] done\t time:%s\n',i,Sys.time()))
    return(0)
}

x <- lapply(1:length(chunk),split_smart)

# para <- 1:length(chunk)
# require(doParallel)
# ck <- makeCluster(min(8,length(para)), outfile = '')
# registerDoParallel(ck)
# r <- foreach(i = para,.combine = rbind,.verbose = T) %dopar% split_smart(i)
# stopCluster(ck)

# smart$Raw_Read_Error_Rate_Value <- levels(smart$Raw_Read_Error_Rate_Value)[smart$Raw_Read_Error_Rate_Value]
# smart <- subset(smart,,c('svrid','ip','sn','time','model','device',names(smart)[c(8:10,12:22,11)]))
# smart$Raw_Read_Error_Rate_Value <- as.integer(smart$Raw_Read_Error_Rate_Value,na.rm = T)
# smart$Current_Pending_Sector_Raw <- as.integer(smart$Current_Pending_Sector_Raw,na.rm = T)
# smart$time <- as.POSIXct(smart$time)








#@@@ CONFIGURE @@@#
#rm(list = ls())
#source('head.R')
#library('doParallel')
#fname <- paste('smart_',seq(1,31),'.Rda',sep='')
#dir_data5kA <- file.path(dir_data,'smart5kA')

###############
#ck <- makeCluster(34, type = 'FORK', outfile = '')
#registerDoParallel(ck)
###############

#@@@ Function for parallel
#Funio <- function(fname){
#    print(fname)
#    load(file.path(dir_data5k,fname))
    #smart <- subset(smart,svrid %in% sample(levels(smart$svrid),10))

#    smart <- subset(smart,,c('svrid','ip','sn','time','model','device',
#                             names(smart)[c(8:10,12:22,11)]))
#    smart$Raw_Read_Error_Rate_Value <- as.integer(smart$Raw_Read_Error_Rate_Value,na.rm = T)
#    smart$Current_Pending_Sector_Raw <- as.integer(smart$Current_Pending_Sector_Raw,na.rm = T)
#    smart$time <- as.POSIXct(smart$time)
#    save(smart,file = file.path(dir_data5kA,fname))

#    1
    #tmp
#}

#ioReturn <- foreach(i = fname,.combine = rbind,.verbose = T) %dopar% Funio(i)
#ioReturn <- factorX(ioReturn)

#save(ioReturn,file = file.path(dir_data,'a.Rda'))
#stopCluster(ck)    
