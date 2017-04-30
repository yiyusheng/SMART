#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: exp2_lt_dist.R
#
# Description: Lead time distribution and rate of all positive disk
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-11-18 17:08:18
#
# Last   modified: 2016-11-18 17:08:20

rm(list = ls())
source('head.R')
source('dirFunc.R')
source('plot_smart.R')

require('ggplot2')

# S1. lead time distribution of baidu
load(file.path(dir_data,'predict_df_baidu_20_5_5.Rda'))
list[pat.bd,patb.bd,pdt.bd,plt.bd] <- r[[1]]

pdt.bd$leadTime <- pdt.bd$leadTime/24
plt.bd <- plot.pdf_leadTime(pdt.bd,1)

plt.bd <- plt.bd + xlab('Lead Time(days)') + ylab('Number of disks') + 
  guides(fill = guide_legend(title = 'is.all.positive'))+
  theme(legend.position = c(0,0.95),
        legend.justification = c(0,1),
        legend.background = element_rect(fill = alpha('grey',0.5)))
ggsave(file=file.path(dir_data,'figure','exp2','exp2_baidu_lt_dist.jpg'), 
       plot=plt.bd, width = 8, height = 6, dpi = 100)

# S2. lead time distribution of ten
load(file.path(dir_data,'predict_df_ten_20_5_5.Rda'))
list[pat.bd,patb.bd,pdt.bd,plt.bd] <- r[[1]]

plt.bd <- plt.bd + xlab('Lead Time(days)') + ylab('Number of disks') + 
  guides(fill = guide_legend(title = 'is.all.positive'))+
  theme(legend.position = c(0,0.95),
        legend.justification = c(0,1),
        legend.background = element_rect(fill = alpha('grey',0.5)))
ggsave(file=file.path(dir_data,'figure','exp2','exp2_ten_lt_dist.jpg'), 
       plot=plt.bd, width = 8, height = 6, dpi = 100)