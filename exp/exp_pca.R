#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: exp_pca.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-11-11 19:26:26
#
# Last   modified: 2016-11-11 19:26:27

source('head_data.R')
source('../gen_smart.R')
library(psych)

list[nw1,nw2,use.neg,use.af,use.res,group] <- list(0,15,1,1,1,4)
list[smart.pos.nw,smart.neg.nw] <- gen_weekly_pred(smart.pos,smart.neg,nw1,nw2,use.neg,use.af,use.res)
list[train,test] <- extract_train_test(smart.pos.nw,smart.neg.nw,id.pos,id.neg,group)

# fa.parallel(smp_df(train[,col.smart],0.01),fa="PC",
#             n.iter=100,show.legend=FALSE,
#             main="Screen plot with parallel analysis")  


cor.train <- cor(train[,col.smart])
pca.train <- principal(cor.train,nfactors = 5)
