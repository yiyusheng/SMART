#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: expand_smart_ykliu.R
#
# Description: expand smart of ykliu(201407-201410) with york's data(201405-201407)
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-26 14:53:28
#
# Last   modified: 2016-10-26 14:53:31

rm(list = ls())
source('head.R')
load(file.path(dir_data,'diskInfo.Rda'))
load(file.path(dir_data,'ykliu_smart.Rda'))

expand_ykliu <- function(i){
  load(file.path(dir_data,'smart5k',fname[i]))
  smart <- subset(smart,sn %in% sn.ykliu)
}

sn.ykliu <- levels(smart$sn)
fname <- list.files(file.path(dir_data,'smart5k'))
para <- 1:length(fname)
require(doParallel)
ck <- makeCluster(min(40,length(para)), outfile = '')
registerDoParallel(ck)
r <- foreach(i = para,.combine = rbind,.verbose = T) %dopar% expand_ykliu(i)
stopCluster(ck)

# extract failure time in ykliu's smart
table.ftime <- smart[!duplicated(smart[,c('sn','failed_time')]),c('sn','failed_time')]
table.ftime <- table.ftime[order(table.ftime$sn,table.ftime$failed_time),]
table.ftime <- table.ftime[!duplicated(table.ftime$sn),]
r$ftime <- table.ftime$failed_time[match(r$sn,table.ftime$sn)]

# rbind
# attr.name <- names(r)[9:23]
attr.name <- names(r)[8:22]
smart.york <- r[,c('sn','modelNum','ftime','time',attr.name)]
smart.york <- subset(smart.york,time < as.p('2014-07-01'))
names(smart) <- names(smart.york)
smartNew <- rbind(smart.york,smart)

smart <- smartNew
smart <- mchAttr(smart,r,'sn','sn',c('ip','svrid','device'))
smart <- factorX(smart[,c('sn','device','modelNum','time','ftime','ip','svrid',attr.name)])

# clean
col.attr.noraw <- attr.name[!grepl('Raw$',attr.name)]
# filter of attributes
idx.attr <- apply(smart[col.attr.noraw],2,function(x)x >= 0 & x < 253)
smart[,col.attr.noraw][!idx.attr] <- -1L

# save
save(smart,file = file.path(dir_data,'ykliu_smart_6months.Rda'))
