#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_disk_model.R
#
# Description: We statistic disk model for each server.
# we extract the main model and group server by number of disks of each main models.
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
load(file.path(dir_dataCF,'num_model.Rda'))

# S1. generate model based on the first three chars of sn. We select 15 char3s cover 94.82% disks
sta_ss$char3 <- substr(sta_ss$sn,1,3)
tb_char3 <- setNames(melt_table(sta_ss$char3,sta_ss$model),c('char3','model','count'))
char3_need <- c(paste('Z1',c('K','M','N','P','X'),sep=''),'Z29',
                paste(rep(c('9W','9Q'),each = 3),rep(c('J','K','M'),2),sep=''),'9SF','9XH',
                '5QD') 
tb_char3 <- subsetX(tb_char3,char3 %in% char3_need)

# S2. summary
sum_char3 <- setNames(melt(tapply(tb_char3$count,tb_char3$char3,sum)),c('char3','sum'))
sum_char3$rate <- array_rate(sum_char3$sum)
sum_char3 <- sum_char3[order(sum_char3$rate,decreasing = T),]
sum_char3$cumsum <- cumsum(sum_char3$rate)
row.names(sum_char3) <- NULL

# S3. generate char3-model pair
pair <- data.frame(char3 = levels(tb_char3$char3),
                   model = as.character(by(tb_char3,tb_char3$char3,function(df){
                     idx <- substr(df$model,1,2) == 'ST'
                     fct2ori(df$model[idx][which.max(df$count[idx])])
                     })),stringsAsFactors = F)
pair$model[grepl('ST2000NM0033',pair$model)] <- 'ST2000NM0033'
sta_ss$unified_model <- pair$model[match(sta_ss$char3,pair$char3)]
sta_ss$unified_model[is.na(sta_ss$unified_model)] <- 'None'
sta_ss$unified_model <- factor(sta_ss$unified_model)


# S4. statistic model for each svrid
sta_model <- setNames(melt_table(sta_ss$svrid,sta_ss$unified_model),c('svrid','model','count'))
sta_model <- dcast(svrid~model,data = sta_model,value.var = 'count')
sta_model <- replace_value(sta_model)
col_model <- names(sta_model)[-1]
tmp <- data.frame(t(apply(sta_model[,col_model],1,function(x)sort(x/sum(x),decreasing = T)[1:2])))
names(tmp) <- c('firstPerc','secondPerc')
sta_model <- cbind(sta_model,tmp)
sta_model$numDisk <- rowSums(sta_model[,col_model])

require(plyr)
count_row <- count(sta_model,names(sta_model)[grepl('ST',names(sta_model))])
count_row$numDisk <- rowSums(count_row[names(count_row)[grepl('ST',names(count_row))]])
count_row$rate <- array_rate(count_row$freq)
count_row <- count_row[order(count_row$rate,decreasing = T),]
count_row$cumsum <- cumsum(count_row$rate)

# S5. add tag for svrid
model_single <- c('ST3250310NS','ST3500514NS','ST500NM0011','ST1000NM0011','ST31000524NS')
model_12d <- c('ST1000NM0011','ST31000524NS','ST2000NM0011','ST32000645NS')

sta_model$tag <- 'None'
sta_model$numTagModify <- 0

for(n in model_12d){
  idx <- sta_model[[n]]/sta_model$numDisk > 0.4
  sta_model$tag[idx] <- n
  sta_model$numTagModify[idx] <- sta_model$numTagModify[idx] + 1
}

###
a <- subset(sta_model,secondPerc > 0)
b <- subset(sta_model,secondPerc == 0)
counta <- count(a,col_model)
countb <- count(b,col_model)

load(file.path(dir_dataCF,'serverInfo.Rda'))
