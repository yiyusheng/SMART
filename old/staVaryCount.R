#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: staVaryCount.R
#
# Description: 
#
# Copyright (c) 2015, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2015-12-14 15:58:54
#
# Last   modified: 2016-04-19 12:39:38
#
#
#

#@@@ CONFIGURE @@@#
rm(list = ls())
source('/home/yiyusheng/Code/R/SMART/head.R')
library('reshape2')
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
    #smart <- subset(smart,svrid %in% sample(levels(smart$svrid),10))
    #smart$id <- factor(paste('smart$svrid','smart$sn','smart$device',sep='_'))
    b <- by(smart[,names(smart)[7:21]],smart$sn,function(x){
       apply(x,2,function(xx){length(unique(xx))})
    })
    r <- data.frame(sn = levels(smart$sn),
                    matrix(unlist(b),byrow = T, nrow = length(b)))
    names(r)[2:16] <- names(smart)[7:21]
    r
}

r <- foreach(i = fname,.combine = rbind,.verbose = T) %dopar% Funio(i)
staVaryCount <- factorX(r)

save(staVaryCount,file = file.path(dir_data,'staVaryCount.Rda'))
stopCluster(ck)
