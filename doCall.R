#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: doCall.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-05-17 21:06:28
#
# Last   modified: 2016-05-27 18:55:29
#
#
#
source('head.R')
load(file.path(dir_data,'SMARTMerge_IntListB.Rda'))
smartInt <- do.call(rbind,smart)
save(smartInt,file = file.path(dir_data,'SMARTMerge_Int_300k2h.Rda'))
