#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: smartOrder.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-04-14 08:52:56
#
# Last   modified: 2016-04-19 11:15:45
#
#
#

#@@@ CONFIGURE @@@#
rm(list = ls())
source('head.R')
library('doParallel')
fname <- paste('smart_',seq(1,31),'.Rda',sep='')
dir_data5kA <- file.path(dir_data,'smart5kA')

###############
ck <- makeCluster(34, type = 'FORK', outfile = '')
registerDoParallel(ck)
###############

#@@@ Function for parallel
Funio <- function(fname){
    print(fname)
    load(file.path(dir_data5k,fname))
    #smart <- subset(smart,svrid %in% sample(levels(smart$svrid),10))

    #smart <- subset(smart,,c('svrid','ip','sn','time','model','device',
     #                        names(smart)[c(8:10,12:22,11)]))
    #smart$Raw_Read_Error_Rate_Value <- as.integer(smart$Raw_Read_Error_Rate_Value,na.rm = T)
    #smart$Current_Pending_Sector_Raw <- as.integer(smart$Current_Pending_Sector_Raw,na.rm = T)
    #smart$time <- as.POSIXct(smart$time)
    smart <- smart[order(smart$svrid,smart$sn,smart$time),]
    save(smart,file = file.path(dir_data5kA,fname))
    1
}

ioReturn <- foreach(i = fname,.combine = rbind,.verbose = T) %dopar% Funio(i)
