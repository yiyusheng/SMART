#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: head.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-05-27 15:36:07
#
# Last   modified: 2016-10-25 12:47:35
#
#
#
osFlag = Sys.info()[1] == 'Windows' 
dirName <- 'SMART'
dir_old <- 'old20161025'
if (osFlag){
  dir_code <- paste('D:/Git/',dirName,sep='')
  dir_data <- paste('D:/Data/',dirName,sep='')
  dir_dataSource <- 'D:/Data/dataLoadforDiskAnalysis'
  source('D:/Git/R_Function/Rfun.R')
}else{
  dir_code <- paste('/home/yiyusheng/Code/R/',dirName,sep='')
  dir_data <- paste('/home/yiyusheng/Data/',dirName,sep='')
  dir_dataSource <- '/home/yiyusheng/Data/dataLoadforDiskAnalysis'
  source('/home/yiyusheng/Code/R/R_Function/Rfun.R')
  # options('width' = 150)
}

# require('caret')
# require('e1071')
#require('ggplot2')
#require('scales')
#require('reshape2')
#require('grid')
