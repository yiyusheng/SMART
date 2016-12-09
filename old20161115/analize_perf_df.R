#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: analysize_perf_df.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-11-03 16:42:40
#
# Last   modified: 2016-11-03 16:42:43
rm(list = ls())
source('head.R')
source('dirFunc.R')
source('eval_result.R')

require('e1071')
require('pROC')
require('ggplot2')
require('grid')

load(file.path(dir_data,'col.smart.Rda'))
load(file.path(dir_data,'smart_5folder_0.25posRate.Rda'))
####################################


fetch_result <- function(group,func){
  fn <- paste('Group',group,'_',func,sep='')
  load(file.path(dir_data,paste(fn,'.Rda',sep='')))
  perf.all$tag1 <- paste('group',group,sep='');perf.disk$tag1 <- paste('group',group,sep='')
  perf.all$tag2 <- func;perf.disk$tag2 <- func
  perf.all$tag3 <- paste(perf.all$tag1,perf.all$tag2,sep='_');perf.disk$tag3 <- paste(perf.disk$tag1,perf.disk$tag2,sep='_')
  perf.disk <- subset(perf.disk,overThre == 1 & bilabel == 1)
  list(pa = perf.all,pd = perf.disk)
}

####################################
# S1.prepare
funcNames <- c('logic.regression','support.vector.machine','naive.bayes'
               ,'hidden.markov','mahal.distance','classify.regression.tree')
para <- expand.grid(1:5,funcNames[c(1,2)])
names(para) <- c('group','func')
para$func <- fct2ori(para$func)
attr(para,'out.attrs') <- NULL

# S2.extract result
all.result <- mapply(fetch_result,para$group,para$func,SIMPLIFY = F)
all.result <- list(group = para$group,func = para$func,
                   perf.all = lapply(all.result,'[[','pa'),
                   perf.disk = lapply(all.result,'[[','pd'))

# S3.evaluate performance of disk failure prediction
pa.all <- do.call(rbind,all.result$perf.all)
plot_ROC_all(pa.all)

# S4.evaluate lead time of different model
pd.all <- do.call(rbind,all.result$perf.disk)
row.names(pd.all) <- NULL
plot_dist_leadTime_all(subset(pd.all,rate.chaos.pos == -1))

sta.rate.pos <- lapply(all.result$perf.disk,function(df)
  list(sum(df$rate.chaos.pos == 1),sum(df$rate.chaos.pos == -1),sum(df$rate.chaos.pos != 0)))
sta.rate.pos <- list2df(sta.rate.pos,n = c('num.allPos','num.divided','num.chaos'))
sta.rate.pos$num.chaos <- sta.rate.pos$num.chaos - sta.rate.pos$num.allPos - sta.rate.pos$num.divided
sta.rate.pos <- cbind(sta.rate.pos,para)

tag.f <- 'logic.regression'
plot_dist_leadTime(subset(pd.all,rate.chaos.pos == -1 & tag2 == tag.f))
plot_dist_leadTime(subset(pd.all,rate.chaos.pos == 1 & tag2 == tag.f))
plot_dist_leadTime(subset(pd.all,rate.chaos.pos != -1 & rate.chaos.pos != 1 & tag2 == tag.f))

# S5.evaluate lead time of each disks
split.group.pd.all <- split(pd.all,pd.all$tag1)
pd.group <- lapply(split.group.pd.all, function(df){
  g <- df$tag1[1]
  df <- dcast(df[,c('sn','leadTime','tag2')],sn ~ tag2,value.var = 'leadTime')
  df <- subset(df,!is.na(df$logic.regression) & !is.na(df$support.vector.machine))
  df$diff <- df$logic.regression - df$support.vector.machine
  df$group <- g
  df
})
pd.group <- do.call(rbind,pd.group)
ggplot(subset(pd.group, diff != 0 & logic.regression < 10),aes(x = support.vector.machine)) + 
  geom_histogram()
tapply(pd.group$diff,pd.group$group,function(x)tableX(x == 0))
