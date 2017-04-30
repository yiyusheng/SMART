#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: template.R
#
# Description: 
#
# Copyright (c) 2015, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2015-12-14 15:58:54
#
# Last   modified: 2016-06-16 17:50:42
#
#
#

#@@@ CONFIGURE @@@#
rm(list = ls())
source('/home/yiyusheng/Code/R/SMART/head.R')
library('doParallel')
fname <- paste('smart_',seq(1,31),'.Rda',sep='')
dir_data5k <- file.path(dir_data,'smart5k')

#@@@ Function for parallel
Funio <- function(fname){
    print(fname)
    load(file.path(dir_data5k,fname))
    #smart <- factorX(subset(smart,svrid %in% sample(levels(smart$svrid),10)))
    tmp
}

#@@@ Parallel configuration and Running
require(doParallel)
ck <- makeCluster(min(40,length(fname)), outfile = '')
registerDoParallel(ck)
r <- foreach(i = fname,.combine = rbind,.verbose = T) %dopar% Funio(i)
r <- factorX(r)

save(r,file = file.path(dir_data,'a.Rda'))
stopCluster(ck)
