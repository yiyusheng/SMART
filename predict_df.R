#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: predict_leadtime.R
#
# Description: predict failure/normal and observe failed one's lead time
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-26 16:55:09
#
# Last   modified: 2016-10-26 16:55:11

rm(list = ls())
source('head.R')
source('dirFunc.R')
source('model_diskfailure.R')
source('gen_smart.R')
source('gen_result.R')
source('eval_result.R')

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
group <- 1;tw <- 10;func <- 'logic.regression'

# S2. fit models
funcNames <- c('logic.regression','support.vector.machine','naive.bayes'
               ,'hidden.markov','mahal.distance','classify.regression.tree')
para <- expand.grid(1:5,funcNames[c(1,2,3)])
names(para) <- c('group','func')
para$func <- fct2ori(para$func)

require(doParallel)
idx <- seq_len(nrow(para))
ck <- makeCluster(min(40,length(idx)),type = 'FORK',outfile = 'predict_diskfailure.paroutput')
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = F) %dopar%
  group.model(smart.pos,smart.neg,id.pos,id.neg,
              group = para$group[i],tw = 10,func = para$func[i],rate.pos = 0.25)
stopCluster(ck)
