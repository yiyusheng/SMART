#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: diskInfoTime.R
#
# Description: 
#
# Copyright (c) 2015, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2015-12-14 15:58:54
#
# Last   modified: 2016-06-07 18:22:10
#
#
#

#@@@ CONFIGURE @@@#
rm(list = ls())
source('/home/yiyusheng/Code/R/SMART/head.R')
library('doParallel')
fname <- paste('smart_',seq(1,31),'.Rda',sep='')
dir_data5k <- file.path(dir_data,'smart5k')

###############
ck <- makeCluster(34, type = 'FORK', outfile = '')
registerDoParallel(ck)
###############

#@@@ Function for parallel
Funio <- function(fname){
    print(fname)
    load(file.path(dir_data5k,fname))
    #smart <- factorX(subset(smart,svrid %in% sample(levels(smart$svrid),10)))
    smart$time <- as.POSIXct(smart$time,tz = 'UTC')
    tmp <- tapply(smart$time,smart$sn,function(data){
                  data.frame(min = min(data),max = max(data),count = length(data))
    })
    tmp <- do.call(rbind,tmp)
    tmp$sn <- levels(smart$sn)
    tmp <- tmp[,c('sn','min','max','count')]
    row.names(tmp) <- NULL
    tmp
}

r <- foreach(i = fname,.combine = rbind,.verbose = T) %dopar% Funio(i)
diskInfoTime <- factorX(r)

save(diskInfoTime,file = file.path(dir_data,'diskInfoTime.Rda'))
stopCluster(ck)
