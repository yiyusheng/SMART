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
# Last   modified: 2016-06-07 09:32:44
#
#
#
osFlag = Sys.info()[1] == 'Windows'                                          
if (osFlag){
      source('configWindows.R')
}else{
      source('/home/yiyusheng/cs/configLinux.R')
}
