#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: test.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-04-19 14:34:03
#
# Last   modified: 2016-04-19 14:35:10
#
#
#
rm(list = ls())
source('/home/yiyusheng/Code/R/SMART/head.R')
library('reshape2')
library('doParallel')
fname <- paste('smart_',seq(1,31),'.Rda',sep='')
dir_data5k <- file.path(dir_data,'smart5k')
load(file.path(dir_data5k,fname[31]))
