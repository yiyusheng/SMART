#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_base.R
#
# Description: 
#
# Copyright (c) 2018, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2018-01-17 17:31:23
#
# Last   modified: 2018-01-17 17:31:24
#
#
#

rm(list = ls());setwd('~/Code/R/SMART/backblaze');source('~/rhead')

dir.bb <- file.path(dir_d,'backblaze')
list.files(dir.bb)

load(file.path(dir.bb,'group_sn.Rda'))
load(file.path(dir.bb,'read_sn.Rda'))
load(file.path(dir.bb,'smart_name.Rda'))
load(file.path(dir.bb,'A1.Rda'))

smart_len <- apply(DT[,grepl('smart',names(DT))],2,function(arr)length(unique(arr)))
