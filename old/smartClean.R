#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: smartClean.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-04-14 08:52:56
#
# Last   modified: 2016-04-15 14:50:18
#
#
#

rm(list = ls())
source('head.R')
load(file.path(dir_data,'AllSMART.Rda'))
load(file.path(dir_data,'load_ftr_attrid.Rda'))
Sys.setlocale('LC_ALL','C') 

grepExChar <- function(x){
    x[grepl("\\\\0",x)] <- ''
    x <- factor(x)
}
replaceExChar <- function(smart,colNeed){
    for(i in 1:length(colNeed)){
        smart[[colNeed[i]]] <- grepExChar(smart[[colNeed[i]]])
    }
    smart
}
colNeed <- c('sn','time','ftime','ip',                                 
            'device','model','Raw_Read_Error_Rate_Value')
smart$model <- factor(gsub(' .*$','',smart$modelNum))
smart <- replaceExChar(smart,colNeed)

len.sn <- length(levels(smart$sn))
len.time <- length(levels(smart$time))
len.ftime <- length(levels(smart$ftime))
len.ip <- length(levels(smart$ip))
len.device <- length(levels(smart$device))
len.model <- length(levels(smart$model))
len.rr <- length(levels(smart$Raw_Read_Error_Rate_Value))

smartClean <- subset(smart, sn %in% levels(smart$sn)[4:len.sn] &
                     time %in% levels(smart$time)[3:len.time] &
                     ftime %in% levels(smart$ftime)[3:len.ftime] &
                     ip %in% levels(smart$ip)[382:(len.ip-71)] &
                     device %in% levels(smart$device)[365:389] &
                     model %in% levels(smart$model)[5:len.model] &
                     Raw_Read_Error_Rate_Value %in% levels(smart$Raw_Read_Error_Rate_Value)[2:len.rr])

smartClean <- factorX(smartClean)
save(smartClean,file = file.path(dir_data,'smartClean.Rda'))
