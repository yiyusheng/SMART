#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: myDL.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-12-08 11:13:27
#
# Last   modified: 2016-12-08 11:13:28
#
#
#

rm(list = ls())
#@@@ CONFIGURE @@@#
library(e1071)
library(ggplot2)
require(mxnet)

source('head.R')
source('gen_smart.R')
source('dirFunc.R')
source('DLFunc.R')

# S1. Data load
load(file.path(dir_data,'gen_smart_baidu.Rda'))
units.perday <- 24;collect.time <- 15;pos.tw <- 5;neg.count <- 5;group <- 2
smart.pos <- subset(smart.pos,dist.fail <= collect.time*units.perday)
smart.neg <- subset(smart.neg,dist.fail <= collect.time*units.perday)
list[train,test] <- extract_train_test(smart.pos,smart.neg,id.pos,id.neg,group)
train <- limit_smart(train,pos.tw*units.perday,neg.count)
test <- limit_smart(test,1e5,neg.count)
list[train_data,train_label,test_data,test_label] <- nor_sep(train,test)

# S2.generate parameters
hidden_node_para <- list(12,9,6,3,c(12,9),c(12,6),c(12,3),c(9,6),c(9,3),c(6,3))
learning.rate_para <- c(0.01,0.03,0.05,0.1,0.2,0.3)
para <- expand.grid(hn = hidden_node_para,lr = learning.rate_para)
type <- 'type1'

# S3.parallel 
require(doParallel)
idx <- seq_len(nrow(para))
ck <- makeCluster(min(30,length(idx)),type = 'FORK',outfile = 'out_dl_observ')
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = F) %dopar% {
  mx.set.seed(0)
  costSet <- list()
  cat(sprintf('[%s]START\thidden layout:%s\tlearning.rate:%.4f\n',Sys.time(),paste(para$hn[[i]],collapse = '-'),para$lr[[i]]))
  model <- mx.mlp(train_data, train_label, hidden_node=para$hn[[i]], out_node=1,
                  out_activation="logistic", num.round=10, 
                  array.batch.size=length(train_label), learning.rate=para$lr[[i]], 
                  momentum=0.9, eval.metric = obsv_clr)
  
  pred_test <- predict(model,test_data)
  pred_test <- bipred(t(pred_test))
  cost_test <- cost_disk(test_label,pred_test,test$sn)
  cat(sprintf('[%s]END\thidden layout:%s\tlearning.rate:%.4f\tCost is %.4f\n',
              Sys.time(),paste(para$hn[[i]],collapse = '-'),para$lr[[i]],cost_test))
  list(para$hn[i],para$lr[i],cost_test,costSet)
}
stopCluster(ck)

# S4.save
save(r,file = file.path(dir_data,'myDL.Rda'))
# r1 <- sapply(r,'[[',1)
# r2 <- data.frame(lr = sapply(r,'[[',2),
#                  cost = sapply(r,'[[',3),
#                  id = 1:length(r),
#                  class = rep(1:10,6))

