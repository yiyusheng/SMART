#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: extractSMART.R
#
# Description: 
#
# Copyright (c) 2015, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2015-12-14 15:58:54
#
# Last   modified: 2016-05-04 12:49:03
#
#
#

#@@@ CONFIGURE @@@#
rm(list = ls())
source('/home/yiyusheng/Code/R/SMART/head.R')
library('doParallel')
load(file.path(dir_data,'diskInfoValid.Rda'))
goodSN <- as.character(diskInfoValid$sn[diskInfoValid$class == 'Normal'])
badSN <- as.character(diskInfoValid$sn[diskInfoValid$class == 'Failure'])
goodSN <- sample(goodSN,min(2000,length(goodSN)))
badSN <- sample(badSN,min(2000,length(badSN)))
diskInfoSmp <- factorX(subset(diskInfoValid,sn %in% goodSN | sn %in% badSN))

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
    tmp <- subset(smart,sn %in% diskInfoSmp$sn)
    tmp
}

r <- foreach(i = fname,.combine = rbind,.verbose = T) %dopar% Funio(i)
smartNeed <- factorX(r)

save(smartNeed,diskInfoSmp,file = file.path(dir_data,'extractSMART.Rda'))
stopCluster(ck)
