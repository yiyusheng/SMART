#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: dirFunc.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-26 17:31:27
#
# Last   modified: 2016-10-26 17:31:30



# F2. sort col of df. extract all columns before attr.
sort_col <- function(df,n = col.smart){
  df <- df[,c(setdiff(names(df),n),n)]
}

remove_smart <- function(df,n = col.smart){
  df <- df[,-which(names(df) %in% col.smart)]
}