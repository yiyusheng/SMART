#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: locate_fail_diskFunc.R
#
# Description: 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-25 11:05:35
#
# Last   modified: 2017-04-25 11:05:37
#
#
#

revise_model <- function(DT){
  table_model <- melt(tapply(DT$modelNum,DT$sn,function(x)length(unique(fct2ori(x)))))
  bad_sn <- factorX(subset(table_model,value > 1))
  DT_bad_sn <- factorX(subset(DT,sn %in% bad_sn$Var1))
  r <- melt(tapply(DT_bad_sn$modelNum,DT_bad_sn$sn,function(x){
    x <- fct2ori(unique(x))
    x[which.max(nchar(x))[1]]
  }))
  DT$modelNum[DT$sn %in% DT_bad_sn$sn] <- r$value[match(DT$sn[DT$sn %in% DT_bad_sn$sn],r$Var1)]
  return(DT)
}

locate_fail_disk <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\tfile:%s\tSTART!!!\n',date(),fn))
  load(file.path(dir_dataSMART14,fn))
  DT <- smart[,setdiff(names(smart),col_smart)]
  DT$time <- factor(as.Date(DT$time))
  DT <- revise_model(DT)
  DT <- factorX(DT)
  
  sta_ss <- melt_table(DT$svrid,DT$sn)
  names(sta_ss) <- c('svrid','sn','count')
  sta_ss$model <- DT$modelNum[match(sta_ss$sn,DT$sn)]
  sta_ss$devid <- DT$device[match(sta_ss$sn,DT$sn)]
  sta_ss$fn <- fn
  
  sta_date <- tapply(DT$time,paste(DT$sn,DT$svrid,sep='@'),table)
  sta_date <- list2df(sta_date,n = c(levels(DT$time),'sn'))
  splitSN <- strsplit(sta_date$sn,'@')
  sta_date$svrid <- factor(lapplyX(splitSN,'[[',2))
  sta_date$sn <- factor(lapplyX(splitSN,'[[',1))
  sta_date$fn <- fn

  cat(sprintf('[%s]\tfile:%s\tEND!!!\n',date(),fn))
  list(sta_ss, sta_date)
}

sta_sn_count <- function(df,dayS,dayE,ftr = F){
  col_day <- intersect(names(df),as.character(seq.Date(as.Date(dayS),as.Date(dayE),by = 1)))
  
  DT <- df[,c('sn','svrid',col_day)];
  DT$count_items <- apply(DT[,col_day],1,sum)
  DT$count_valid_day <- apply(DT[,col_day],1,function(x)sum(x > 0))
  if(ftr){
    DT$sd_all_day <- apply(DT[,col_day],1,sd)
    DT$mean_valid_day <- apply(DT[,col_day],1,function(x)mean(x[x != 0]))
    DT$sd_valid_day <- apply(DT[,col_day],1,function(x)sd(x[x != 0]))
    DT$min_date <- apply(DT[,col_day],1,function(x)col_day[which.min(x != 0)])
    DT$max_date <- apply(DT[,col_day],1,function(x)col_day[which.max(x != 0)])
  }
  DT <- factorX(DT)
  return(DT[,names(DT)[!(names(DT) %in% col_day)]])
}

rbind_sta_date <- function(r){
  col_sd <- names(r[[2]][[2]])
  sta_date <- lapply(r,'[[',2)
  sta_date <- lapply(sta_date,function(df){
    if(ncol(df) == length(col_sd)){
      return(df)
    }else{
      df[,setdiff(col_sd,names(df))] <- 0
      df <- df[,col_sd]
      return(df)
    }
  })
  sta_date <- do.call(rbind,sta_date)
}

locate_evaluate <- function(SS,FR,LC){
  SSvrid <- data.frame(svrid = levels(SS$svrid))
  SSvrid$real <- 0
  SSvrid$real[SSvrid$svrid %in% FR$svrid] <- 1
  SSvrid$pred <- 0
  SSvrid$pred[SSvrid$svrid %in% LC$svrid] <- 1
  return(base_classification_eval(SSvrid$real,SSvrid$pred))
}