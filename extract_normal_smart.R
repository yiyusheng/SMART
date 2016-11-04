#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: extract_normal_smart.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-26 16:14:10
#
# Last   modified: 2016-10-26 16:14:12

rm(list = ls())
source('head.R')
load(file.path(dir_data,'diskInfo.Rda'))
load(file.path(dir_data,'ykliu_smart_6months.Rda'))
load(file.path(dir_data,'..','attrid','dataPrepareAFR10-15.Rda'))

extract_normal <- function(i){
  load(file.path(dir_data,'smart5k',fname[i]))
  extract.svrid <- setdiff(levels(smart$svrid),filter.svrid)
  extract.svrid <- extract.svrid[sample(1:length(extract.svrid),200)]
  smart <- subset(smart,svrid %in% extract.svrid)
}

# some positive sn are not filtered
filter.svrid <- c(levels(smart$svrid),
                  unique(tmp.f$svr_id[tmp.f$f_time > as.p('2014-03-01') & 
                                        tmp.f$f_time <as.p('2015-01-01')]))
fname <- list.files(file.path(dir_data,'smart5k'))

para <- 1:length(fname)
require(doParallel)
ck <- makeCluster(min(40,length(para)), outfile = '')
registerDoParallel(ck)
r <- foreach(i = para,.combine = rbind,.verbose = T) %dopar% extract_normal(i)
stopCluster(ck)

smart <- factorX(r)
col.attr <- names(smart)[8:22]
smart <- smart[,c('sn','device','modelNum','time','ftime','ip','svrid',col.attr)]

# clean
col.attr.noraw <- col.attr[!grepl('Raw$',col.attr)]
idx.attr <- apply(smart[col.attr.noraw],2,function(x)x >= 0 & x < 253)
smart[,col.attr.noraw][!idx.attr] <- -1L

# save
save(smart,file = file.path(dir_data,'york_smart_normal.Rda'))