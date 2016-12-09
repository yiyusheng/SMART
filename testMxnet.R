#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: testMxnet.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-12-09 10:47:40
#
# Last   modified: 2016-12-09 10:47:42
#
#
#

require(mlbench)
require(mxnet)
data(Sonar, package="mlbench")

Sonar[,61] = as.numeric(Sonar[,61])-1
train.ind = c(1:50, 100:150)
train.x = data.matrix(Sonar[train.ind, 1:60])
train.y = Sonar[train.ind, 61]
test.x = data.matrix(Sonar[-train.ind, 1:60])
test.y = Sonar[-train.ind, 61]

  
mx.set.seed(0)
obsv_clr <- mx.metric.custom('obsv_clr',function(label,pred){
  pred <- bipred(t(pred))
  return(mean(pred == label))
})

model <- mx.mlp(train.x, train.y, hidden_node=10, out_node=1, out_activation="logistic",
                num.round=50, array.batch.size=15, learning.rate=0.07, momentum=0.9, 
                eval.metric=obsv_clr)
