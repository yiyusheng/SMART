#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: exp3_rate_pos.R
#
# Description: plot relation between period length of data collection and the rate of all positive disks being predicted.
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-11-18 17:25:02
#
# Last   modified: 2016-11-18 17:25:04
rm(list = ls())
source('head.R')
source('dirFunc.R')
source('plot_smart.R')

require('ggplot2')

gen_errorbar <- function(patb){
  patb$rate.allpos <- fct2num(patb$rate.allpos)
  patb$collect.time <- fct2num(patb$collect.time)
  
  patb.errbar <- list2df(tapply(patb$rate.allpos,patb$collect.time,function(x)list(mean(x),sd(x),sd(x)/sqrt(length(x)))),
                       n = c('mean','sd','sem','period_coll'))
  patb.errbar$period_coll <- as.numeric(patb.errbar$period_coll)
  
  patb.errbar
  
}

plot_patb <- function(patb.ten,patb.baidu){
  patb.errbar.ten <- gen_errorbar(patb.ten)
  patb.errbar.ten$class <- 'Tencent'
  patb.errbar.baidu <- gen_errorbar(patb.baidu)
  patb.errbar.baidu$class <- 'Baidu'
  patb.errbar <- rbind(patb.errbar.baidu,patb.errbar.ten)
  
  pd <- position_dodge(0.2)
  p1 <- ggplot(patb.errbar,aes(x = period_coll,y = mean,color = class,group = class)) + 
    geom_errorbar(aes(ymin = mean - 2*sem,ymax = mean + 2*sem),color = 'black',width = .1,position = pd) +
    geom_line(position = pd) +
    geom_point(position = pd,size = 3,shape = 21,fill = 'white') + 
    xlab('Period of data collection (days)') + ylab('Fraction of disks with all positive(%)') +
    guides(color = guide_legend(title = 'data sets')) + 
    theme_bw() +
    theme(legend.justification = c(.95,.95),
          legend.position = c(.95,.95))
  ggsave(file=file.path(dir_data,'figure','exp3','exp3_rate_allpos.jpg'), 
         plot=p1, width = 8, height = 6, dpi = 100)
  p1
}


# S1. lead time distribution of baidu
load(file.path(dir_data,'patb_baidu_5-20_5_5.Rda'))
patb.baidu <- patb

# S2. lead time distribution of Ten
load(file.path(dir_data,'patb_ten_5-90_5_5.Rda'))
patb.ten<- patb

# S3. plot and save
p <- plot_patb(patb.ten,patb.baidu)