#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: plot_sample.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-11-17 20:35:49
#
# Last   modified: 2016-11-17 20:35:51

# F1.plot PDF of lead time filled by all positive rate
plot.pdf_leadTime <- function(df,units.perday){
  df <- factorX(subset(df,overThre == 1 & bilabel == 1))
  df$all.pos <- factor(as.numeric(df$pos.rate == 1))
  p <- ggplot(df,aes(x = leadTime,fill = all.pos)) + geom_histogram(binwidth = units.perday)
}