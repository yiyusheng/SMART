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
source('eval_result.R')
source('model_nw.R')
source('gen_result_nw.R')

require('e1071')
require('pROC')
require('ggplot2')
require('grid')

load(file.path(dir_data,'col.smart.Rda'))
####################################
# S1. Merge smart from two dataset and generate smart.neg and smart.pos
# Add label of positive/negative and groups for k-folds cross validate

# gen_smart()
# load(file.path(dir_data,'smart.Rda'))
# list[smart.pos,smart.neg,id.pos,id.neg] <- gen_label_group(smart.pos,smart.neg,id.pos,id.neg,5,0.25)
# save(smart.pos,smart.neg,id.pos,id.neg,file = file.path(dir_data,'smart_5folder_0.25posRate.Rda'))
load(file.path(dir_data,'smart_5folder_0.25posRate.Rda'))


# S2. Parameter setting
funcNames <- c('logic.regression','support.vector.machine','naive.bayes'
               ,'hidden.markov','mahal.distance','classify.regression.tree')
para <- expand.grid(4,funcNames[c(1,2)],c(1,3),c(3,10),c(0,1),c(0,1),c(0,1))
names(para) <- c('group','func','nw1','nw2','use.neg','use.af','use.res')
para$func <- fct2ori(para$func)
attr(para,'out.attrs') <- NULL
list[group,func,nw1,nw2,use.neg,use.af,use.res] <- as.list(para[64,])

# S3. Fit model and generate result
require(doParallel)
idx <- seq_len(nrow(para))
ck <- makeCluster(min(40,length(idx)),type = 'FORK',outfile = 'predict_nw.paraout')
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = F) %dopar%
  group.model(smart.pos,smart.neg,id.pos,id.neg,
              group = para$group[i],func = para$func[i],nw1 = para$nw1[i], nw2 = para$nw2[i],
              use.neg = para$use.neg[i],use.af = para$use.af[i],use.res = para$use.res[i])
stopCluster(ck)
# save(r,para,file = file.path(dir_data,'predict_nw.Rda'))