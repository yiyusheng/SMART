#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-12-15 11:10:36
#
# Last   modified: 2016-12-15 11:10:38
#
#
#
rm(list = ls())
source('head.R')
fname <- paste('smart_',1:31,'.Rda',sep='')
require(doParallel)
idx <- seq_len(length(fname))
ck <- makeCluster(min(40,length(idx)),type='FORK',outfile = 'out_sta')
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = F) %dopar% {
  load(file.path(dir_data,'smart5k',fname[i]))
  return(list(fn = fname[i],numItem = nrow(smart),
              numDisk = length(levels(smart$sn)),numServer = length(levels(smart$svrid))))
}
stopCluster(ck)
save(r,file = file.path(dir_data,'sta.Rda'))
ni <- sum(as.numeric(sapply(r,'[[','numItem')))
nu <- sum(as.numeric(sapply(r,'[[','numDisk')))
nz <- sum(as.numeric(sapply(r,'[[','numServer')))
cat(sprintf('numItems: %.0f\tnumUnits: %.0f\tnumServers:%.0f',ni,nu,nz))
