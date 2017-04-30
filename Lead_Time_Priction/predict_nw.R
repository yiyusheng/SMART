#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: predict_nw.R
#
# Description: predict if failure will happen in next week
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-26 16:55:09
#
# Last   modified: 2016-11-04 20:12:50

rm(list = ls())
source('head.R')
source('dirFunc.R')
source('gen_smart.R')
source('model_nw.R')
source('gen_result_nw.R')

require('e1071')
require('pROC')
require('ggplot2')
require('grid')

load(file.path(dir_data,'col.smart.Rda'))
####################################
# S1. Generate smart
# gen_smart_ten(5,0.1)
load(file.path(dir_data,'smart_5folds_0.1posRate.Rda'))
list[smart.pos,smart.neg] <- list(deduplcate_smart(smart.pos),deduplcate_smart(smart.neg))


# S2. Parameter setting
funcNames <- c('logic.regression','support.vector.machine')
para <- expand.grid(1:5,funcNames[c(1,2)],c(20),c(1),
                    c(3),c(10),c(99),c(5),
                    c(1),c(1),c(1))
names(para) <- c('group','func','collect.time','units.perday',
                 'nw1','nw2','pos.tw','neg.count',
                 'use.neg','use.af','use.res')
para$func <- fct2ori(para$func)
attr(para,'out.attrs') <- NULL
list[group,func,collect.time,units.perday,nw1,nw2,pos.tw,neg.count,use.neg,use.af,use.res] <- as.list(para[3,])

# S3. Fit model and generate result
require(doParallel)
idx <- seq_len(nrow(para))
ck <- makeCluster(min(40,length(idx)),type = 'FORK',outfile = 'out_nw_ten')
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = F) %dopar%
  group.model(smart.pos,smart.neg,id.pos,id.neg,
              group = para$group[i],func = para$func[i],collect.time = para$collect.time[i],units.perday = 1,
              nw1 = para$nw1[i], nw2 = para$nw2[i],pos.tw = para$pos.tw[i],neg.count = para$neg.count[i],
              use.neg = para$use.neg[i],use.af = para$use.af[i],use.res = para$use.res[i])
stopCluster(ck)
save(r,para,file = file.path(dir_data,'predict_nw_ten.Rda'))