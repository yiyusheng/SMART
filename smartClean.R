#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: smartClean.R
#
# Description: Clean SMART data (AllSMART.Rda -> smartClean.Rda)
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-04-14 08:52:56
#
# Last   modified: 2016-04-15 14:50:18
#
#
#

rm(list = ls())
source('head.R')

####################################
smart_clean <- function(i){
  load(file.path(dir_data,'splitRda',fname[i]))
  col.attr <- names(smart)[8:(8+14)]
  col.attr.noraw <- col.attr[!grepl('Raw$',col.attr)]
  smart$time <- as.p(smart$time)
  smart$ftime <- as.p(smart$ftime)
  # filter of identity
  reg.ip <- '(\\d+\\.){3}\\d+'
  smart <- subset(smart,sn != '' & modelNum != '' & 
                    grepl(reg.ip,ip) & 
                    grepl('^sd[a-z]$',device))
  
  # filter of attributes
  idx.attr <- apply(smart[col.attr.noraw],2,function(x)x >= 0 & x < 253)
  smart[,col.attr.noraw][!idx.attr] <- -1L
  cat(sprintf('[smartClean::smart_clean]file:%s\n',fname[i]))
  smart
}

fname <- list.files(file.path(dir_data,'splitRda'))
para <- 1:length(fname)
require(doParallel)
ck <- makeCluster(min(40,length(para)), outfile = '')
registerDoParallel(ck)
r <- foreach(i = para,
             .combine = rbind,
             .verbose = T) %dopar% smart_clean(i)
stopCluster(ck)

r$sn <- factor(r$sn)
r$ip <- factor(r$ip)
r$device <- factor(r$device)
r$modelNum <- factor(r$modelNum)
save(r,file = file.path(dir_data,'AllSMART_20161025_clean.Rda'))































# ####################################
# grepExChar <- function(x){
#     x[grepl("\\\\0",x)] <- ''
#     x <- factor(x)
# }
# replaceExChar <- function(smart,colNeed){
#     for(i in 1:length(colNeed)){
#         smart[[colNeed[i]]] <- grepExChar(smart[[colNeed[i]]])
#     }
#     smart
# }
# 
# ####################################
# colNeed <- c('sn','time','f_time','svr_ip',                                 
#             'device','model','Raw_Read_Error_Rate_Value')
# smart$model <- factor(gsub(' .*$','',smart$modelNum))
# # smart <- replaceExChar(smart,colNeed)
# 
# len.sn <- length(levels(smart$sn))
# len.time <- length(levels(smart$time))
# len.ftime <- length(levels(smart$ftime))
# len.ip <- length(levels(smart$ip))
# len.device <- length(levels(smart$device))
# len.model <- length(levels(smart$model))
# len.rr <- length(levels(smart$Raw_Read_Error_Rate_Value))
# 
# lev.sn <- levels(smart$sn)
# lev.time <- levels(smart$time)
# lev.ftime <- levels(smart$ftime)
# lev.ip <- levels(smart$ip)
# lev.device <- levels(smart$device)
# lev.model <- levels(smart$model)
# lev.rr <- levels(smart$Raw_Read_Error_Rate_Value)
# 
# nc.sn <- nchar(levels(smart$sn))
# nc.time <- nchar(levels(smart$time))
# nc.ftime <- nchar(levels(smart$ftime))
# nc.ip <- nchar(levels(smart$ip))
# nc.device <- nchar(levels(smart$device))
# nc.model <- nchar(levels(smart$model))
# nc.rr <- nchar(levels(smart$Raw_Read_Error_Rate_Value))
# 
# # filter error data
# smartClean <- subset(smart, sn %in% levels(smart$sn)[4:len.sn] &
#                      time %in% levels(smart$time)[3:len.time] &
#                      ftime %in% levels(smart$ftime)[3:len.ftime] &
#                      ip %in% levels(smart$ip)[382:(len.ip-71)] &
#                      device %in% levels(smart$device)[365:389] &
#                      model %in% levels(smart$model)[5:len.model] &
#                      Raw_Read_Error_Rate_Value %in% levels(smart$Raw_Read_Error_Rate_Value)[2:len.rr])
# 
# smartClean <- factorX(smartClean)
# save(smartClean,file = file.path(dir_data,'smartClean.Rda'))
