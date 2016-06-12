#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: staDown.R
#
# Description: 
#
# Copyright (c) 2015, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2015-12-14 15:58:54
#
# Last   modified: 2016-04-21 11:02:42
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
# DV = Down Value; DT = Down Time; DTI = Down Time Interval
Funio <- function(fname){
    print(fname)
    load(file.path(dir_data5k,fname))
    attrNames <- names(smart)[7:21]
    #smart <- subset(smart,svrid %in% sample(levels(smart$svrid),10))
    b <- by(smart[,c('sn','time',attrNames)],factor(smart$sn),function(x){
            a <- apply(x[attrNames],2,function(xx){
                  xx[xx > 100000] <- 0
                  deltaXX <- xx[1:(length(xx)-1)] - xx[2:length(xx)] 
                  deltaXXNoZero <- deltaXX[deltaXX != 0]
                  deltaXXIdx <- which(deltaXX != 0)

                  if(length(deltaXXIdx) == 0){
                      deltaTime <- -1
                      deltaXXNoZero <- -1
                      deltaXXIdx <- 1
                  } else {
                      DT <- x$time[deltaXXIdx]
                      deltaTime <- DT[2:length(DT)] - DT[1:(length(DT)-1)]
                      units(deltaTime) <- 'hours'
                      deltaTime <- as.numeric(deltaTime)
                  }

                  d <- data.frame(count = length(xx),
                                  max = max(xx),
                                  min = min(xx),
                                  onlineTime = min(x$time),
                                  offlineTime = max(x$time),

                                  countDV = length(deltaXXNoZero),
                                  maxDV = max(deltaXXNoZero),
                                  minDV = min(deltaXXNoZero),
                                  avgDV = mean(deltaXXNoZero),
                                  firstDT = x$time[deltaXXIdx[1]],
                                  lastDT = x$time[tail(deltaXXIdx,1)],
                                  
                                  maxDTI = max(deltaTime),
                                  minDTI = min(deltaTime),
                                  avgDTI = mean(deltaTime))
            })
            dd <- data.frame(matrix(unlist(a),byrow = T,nrow = length(a)))
            dd$sn <- x$sn[1]
            dd$attr <- attrNames
            dd
    })
    b <- do.call('rbind',b)
    b
}

r <- foreach(i = fname,.combine = rbind,.verbose = T) %dopar% Funio(i)
row.names(r) <- NULL
len_r <- length(r)

rr <- r[,c(15,16,1:14)]
names(rr) <- c('sn','attr','count','max','min','onlineTime','offlineTime',
              'countDV','maxDV','minDV','avgDV','firstDT','lastDT',
              'maxDTI','minDTI','avgDTI')

staDown <- factorX(rr)

save(staDown,file = file.path(dir_data,'staDown.Rda'))
stopCluster(ck)
