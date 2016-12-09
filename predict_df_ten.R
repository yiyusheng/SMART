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
source('gen_smart.R')
source('plot_smart.R')

source('model_df.R')
source('gen_result_df.R')

require('e1071')
require('ggplot2')

####################################
# S1. Generate smart
# gen_smart_ten(5,0.1)
load(file.path(dir_data,'smart_5folds_0.1posRate.Rda'))
list[smart.pos,smart.neg] <- list(deduplcate_smart(smart.pos),deduplcate_smart(smart.neg))


# S2. fit models
funcNames <- c('logic.regression','support.vector.machine')
para <- expand.grid(funcNames[c(1)],c(5:90),c(1:5),c(5),c(5),1,'pdf_lt_ten')
names(para) <- c('func','collect.time','group','pos.tw','neg.count','units.perday','dir_plot')
para$func <- fct2ori(para$func);attr(para,'out.attrs') <- NULL
para <- factorX(subset(para,pos.tw <= collect.time))
i <- 3; list[func,collect.time,group,pos.tw,neg.count,units.perday,dir_plot] <- as.list(para[i,])

# S3. Fit model and generate result
require(doParallel)
idx <- seq_len(nrow(para))
ck <- makeCluster(min(40,length(idx)),type = 'FORK',outfile = 'out_df_ten')
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = F) %dopar%
  group.model(smart.pos,smart.neg,id.pos,id.neg,para$func[i],
              para$collect.time[i],para$group[i],
              para$pos.tw[i],para$neg.count[i],
              para$units.perday[i],para$dir_plot[i])
stopCluster(ck)

# S4. Save
# save(r,para,file = file.path(dir_data,'predict_df_ten_20_5_5.Rda'))

# S5. Save patb
name.patb <- c('FDR','FAR','F1','thre','rate.allpos','func','collect.time','group','pos.tw','neg.count')
patb <- list2df(lapply(r,'[[','patb'),n = name.patb)
save(patb,file = file.path(dir_data,'patb_ten_5-90_5_5.Rda'))
