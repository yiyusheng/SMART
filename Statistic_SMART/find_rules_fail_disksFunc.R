#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: find_rules_fail_disks.R
#
# Description: I wanna find the rules to decide a server being replaced disks.
# I give each svrid features and use decision tree to predict failed disks(f2014_06_09) in order to increase the FDR when FAR is 0
# Then I parse the rules established by the decision tree.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-25 15:25:45
#
# Last   modified: 2017-04-25 15:25:46
#
#
#

get_smart_statistic_data <- function(sta_date,dayS,dayE,col_day,FR = NULL){
  if(is.null(FR))FR <- data.frame(svrid = levels(sta_date$svrid))
  
  SS <- subsetX(sta_sn_count(sta_date,dayS,dayE,F),svrid %in% FR$svrid)
  #remove sn without valid day and remove svrid with only one disk which will never been checked as replaced svrid
  SS <- subsetX(SS,count_valid_day > 0 & svrid %in% sta_svrid$svrid[sta_svrid$countSN > 1]) 
  
  SD <- subsetX(sta_date, sn %in% SS$sn & svrid %in% SS$svrid,c('svrid','sn',col_day))
  SD[,col_day][SD[,col_day] > 0] <- 1
  SD$count_valid_day <- rowSums(SD[,col_day])
  SD <- subsetX(SD,count_valid_day > 0)
  SD$sn <- fct2ori(SD$sn)
  SD$svrid <- fct2ori(SD$svrid)
  
  splitSD <- split(SD,SD$svrid)
  
  return(list(SS,SD,splitSD))
}

remove_invalid_day <- function(df,cur_col_day){
  colSum_day <- colSums(df[,cur_col_day])
  idx_reserve <- which(colSum_day != 0)
  df <- df[,c(setdiff(names(df),cur_col_day[-idx_reserve]))]
  cur_col_day <- cur_col_day[idx_reserve]
  list(df,cur_col_day)
}

find_replacement <- function(i,cur_col_day,df_failed,df_candi){
  ccd <- cur_col_day
  cur_df <- df_failed[i,];cur_sn <- cur_df$sn;cur_svrid <- cur_df$svrid
  idx_last_day <- max(which(cur_df[,ccd] == 1))
  newPattern1 <- c(0,1,1);newPattern2 <- c(0,0,1)
  if(idx_last_day == 1){
    flag_candi <- unlist(lapply(seq_len(nrow(df_candi)),function(j){
      any(all(df_candi[j,ccd][(idx_last_day):(idx_last_day + 1)] == newPattern1[-1]),
          all(df_candi[j,ccd][(idx_last_day):(idx_last_day + 1)] == newPattern2[-1]))
    }))
  }else if(idx_last_day == length(cur_col_day)){
    flag_candi <- unlist(lapply(seq_len(nrow(df_candi)),function(j){
      any(all(df_candi[j,ccd][(idx_last_day-1):(idx_last_day)] == newPattern1[-3]),
          all(df_candi[j,ccd][(idx_last_day-1):(idx_last_day)] == newPattern2[-3]))
    }))
  }else{
    flag_candi <- unlist(lapply(seq_len(nrow(df_candi)),function(j){
      any(all(df_candi[j,ccd][(idx_last_day-1):(idx_last_day + 1)] == newPattern1),
          all(df_candi[j,ccd][(idx_last_day-1):(idx_last_day + 1)] == newPattern2))
    }))
  }
  
  if(any(flag_candi)){
    return(data.frame(svrid = cur_svrid,sn = cur_sn,num_replaced_disk = sum(flag_candi),sn_new_disk = paste(df_candi$sn[flag_candi],collapse = '-')))
  }else{
    return(data.frame(svrid = cur_svrid,sn = cur_sn,num_replaced_disk = 0,sn_new_disk = 'NO'))
  }
}

get_result_replacement <- function(r){
  rst_N <- r[sapply(r,is.numeric)]
  rst_N <- melt(rst_N)
  names(rst_N) <- c('errorID','svrid')
  rst_N$svrid <- factor(rst_N$svrid)
  
  rst_R <- r[!sapply(r,is.numeric)]
  rst_R <- do.call(rbind,rst_R)
  
  rst_svrid <- data.frame(svrid = levels(rst_R$svrid),
                            num_failed_disk = as.numeric(tapply(rst_R$sn,rst_R$svrid,length)),
                            num_replaced_disk = as.numeric(tapply(rst_R$num_replaced_disk,rst_R$svrid,function(x)sum(x != 0))))
  
  return(list(rst_svrid,rst_R,rst_N))
}

