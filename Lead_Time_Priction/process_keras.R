#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: process_keras.R
#
# Description: process smart data for deep learning on keras
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-11-24 19:55:12
#
# Last   modified: 2016-11-24 19:55:31
#
#
#
rm(list = ls())
source('head.R')
source('dirFunc.R')
source('gen_smart.R')
source('plot_smart.R')

source('model_df.R')
source('gen_result_df.R')

require('e1071')
require('ggplot2')

####################################
# S1. Generate smart based on number of folders and rate of positive disks
# gen_smart_ten(5,0.1)
# load(file.path(dir_data,'smart_5folds_0.1posRate.Rda'))
# 
# S2. remove items and left one items for a disk in one day.
# list[smart.pos,smart.neg,col.smart] <- remove_column(smart.pos,smart.neg,col.smart)
# 
# list[smart.pos,smart.neg] <- list(deduplcate_smart(smart.pos),deduplcate_smart(smart.neg))
# 
# smart.backup <- list(smart.pos,smart.neg)
# save(smart.backup,id.pos,id.neg,file = file.path(dir_data,'process_keras_smart_backup.Rda'))

# load(file.path(dir_data,'process_keras_smart_backup.Rda'))
# list[smart.pos,smart.neg] <- smart.backup
# col.smart <- names(smart.pos)[6:18]

# S3. generate multi-days feature
# smart.pos <- gen_multi_days_smart(smart.pos,7)
# smart.neg <- gen_multi_days_smart(smart.neg,7)
# smart.pos$ftime <- id.pos$ftime[match(smart.pos$sn,id.pos$sn)]
# smart.neg$ftime <- id.neg$ftime[match(smart.neg$sn,id.neg$sn)]
# smart.pos$dist.fail <- as.numeric(difftime(smart.pos$ftime,smart.pos$time,units = 'days'))
# smart.neg$dist.fail <- as.numeric(difftime(smart.neg$ftime,smart.neg$time,units = 'days'))
# save(smart.pos,smart.neg,id.pos,id.neg,col.smart,file = file.path(dir_data,'process_keras_data.Rda'))
load(file.path(dir_data,'process_keras_data.Rda'))


# S4. get label
list[smart.pos,smart.neg] <- gen_weekly_pred(smart.pos,smart.neg,3,7)

# S5. extract train and test based on the group.
list[train,test] <- extract_train_test(smart.pos,smart.neg,id.pos,id.neg,1)

# S6. the last item removement to set time window and avoid too much items of negative
train <- limit_smart(train,10,20)
test <- limit_smart(test,1e5,20)

# S7. sort columns
col.attr.multi <- names(train)[grepl('a[0-9]',names(train))]
train <- sort_col(train,n = col.attr.multi)
test <- sort_col(test,n = col.attr.multi)
save(train,test,id.pos,id.neg,col.attr.multi,file = file.path(dir_data,'process_keras_output.RDa'))


# S3. remove items and left items N days before the failed day. 
# For neg, we define the failed day is the last day when it updates data in our data set
# smart.pos <- factorX(subset(smart.pos, dist.fail >= 0))
# sta.pos <- list2df(tapply(smart.pos$dist.fail,smart.pos$sn,function(x)list(length(x),max(x),min(x))))
# sta.pos$diff <- ceiling(sta.pos$X2) - floor(sta.pos$X3)
# sta.pos$miss <- sta.pos$X1 - sta.pos$diff
# smart.pos <- factorX(subset(smart.pos,sn %in% sta.pos$item[sta.pos$X1 >=  60] ))
# 
# 
# smart.neg <- factorX(subset(smart.neg,time > as.p('2014-07-03') & dist.fail <= 20))
# sta.neg <- list2df(tapply(smart.neg$dist.fail,smart.neg$sn,function(x)list(length(x),max(x),min(x))))
# sta.neg$diff <- ceiling(sta.neg$X2) - floor(sta.neg$X3)
# sta.neg$miss <- sta.neg$X1 - sta.neg$diff
# smart.neg <- factorX(subset(smart.neg,sn %in% sta.neg$item[sta.neg$miss >= -1]))