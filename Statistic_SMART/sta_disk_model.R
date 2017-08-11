#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_disk_model.R
#
# Description: We statistic disk model for each server.
# 1. statistic pattern of disk combination for each server.
# 2. extract the main model and group server by number of disks of each main models.
# 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-05-23 11:10:44
#
# Last   modified: 2017-05-23 11:10:45
#
#
#

rm(list = ls());setwd('~/Code/R/SMART/');source('~/rhead')
source('~/Code/R/SMART/Statistic_SMART/locate_fail_diskFunc.R')
load(file.path(dir_data,'locate_fail_disk.Rda'))
load(file.path(dir_dataCF,'serverInfo.Rda'))
# load(file.path(dir_dataCF,'num_model.Rda'))

# S1. generate unified disk model based on the first three chars of sn. 
# We select 15 char3s cover 94.82% disks

# S1.1 get the first 3 chars
sta_ss$char3 <- substr(sta_ss$sn,1,3)
tb_char3 <- setNames(melt_table(sta_ss$char3,sta_ss$model),c('char3','model','count'))
char3_need <- c(paste('Z1',c('K','M','N','P','X'),sep=''),'Z29',
                paste(rep(c('9W','9Q'),each = 3),rep(c('J','K','M'),2),sep=''),'9SF','9XH',
                '5QD') 
tb_char3 <- subsetX(tb_char3,char3 %in% char3_need)

# S1.2 summary
sum_char3 <- setNames(melt(tapply(tb_char3$count,tb_char3$char3,sum)),c('char3','sum'))
sum_char3$rate <- array_rate(sum_char3$sum)
sum_char3 <- sum_char3[order(sum_char3$rate,decreasing = T),]
sum_char3$cumsum <- cumsum(sum_char3$rate)
row.names(sum_char3) <- NULL

# S1.3 generate char3-model pair
pair <- data.frame(char3 = levels(tb_char3$char3),
                   model = as.character(by(tb_char3,tb_char3$char3,function(df){
                     idx <- substr(df$model,1,2) == 'ST'
                     fct2ori(df$model[idx][which.max(df$count[idx])])
                     })),stringsAsFactors = F)
pair$model[grepl('ST2000NM0033',pair$model)] <- 'ST2000NM0033'

# S2 add unified_model for each disk and generate the main model

# S2.1 add unified_model for statistic of server + sn
sta_ss$unified_model <- pair$model[match(sta_ss$char3,pair$char3)]
sta_ss$unified_model[is.na(sta_ss$unified_model)] <- 'None'
sta_ss$unified_model <- factor(sta_ss$unified_model)

# S2.2. statistic model for each svrid
sta_model <- setNames(melt_table(sta_ss$svrid,sta_ss$unified_model),c('svrid','model','count'))
sta_model <- dcast(svrid~model,data = sta_model,value.var = 'count')
sta_model <- replace_value(sta_model)
col_model <- names(sta_model)[-1]

tmp <- data.frame(t(apply(sta_model[,col_model],1,function(x)sort(x/sum(x),decreasing = T)[1:2])))
names(tmp) <- c('firstPerc','secondPerc')
sta_model <- cbind(sta_model,tmp)
sta_model$numDisk <- rowSums(sta_model[,col_model])

# S2.3 statistic pattern of difference model combination
require(plyr)
model_pattern <- count(sta_model,names(sta_model)[grepl('ST',names(sta_model))])
model_pattern$numDisk <- rowSums(model_pattern[names(model_pattern)[grepl('ST',names(model_pattern))]])
model_pattern$rate <- array_rate(model_pattern$freq)
model_pattern <- model_pattern[order(model_pattern$rate,decreasing = T),]
model_pattern$cumsum <- cumsum(model_pattern$rate)

# S2.4. add tag for svrid
model_single <- c('ST3250310NS','ST3500514NS','ST500NM0011','ST1000NM0011','ST31000524NS')
model_12d <- c('ST1000NM0011','ST31000524NS','ST2000NM0011','ST32000645NS')
sta_model$mainModel <- 'None'

for(n in model_12d){
  idx <- sta_model[[n]]/sta_model$numDisk > 0.5 & sta_model$numDisk > 1
  sta_model$mainModel[idx] <- n
}

for (n in model_single){
  idx <- sta_model[[n]] == 1 & sta_model$numDisk == 1
  sta_model$mainModel[idx] <- n
}

# SAVE
save(sta_model,sta_ss,model_pattern,file = file.path(dir_data,'sta_disk_model.Rda'))
