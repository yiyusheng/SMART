# clean SMART: modify the column name, remove pos disk without enough pos data
clean_DT <- function(DT,min_pos_days=10){
  DT$date <- as.Date(DT$date)
  DT$serial_number <- factor(DT$serial_number)
  DT$model <- factor(DT$model)
  names(DT) <- gsub('\\_raw','r',gsub('smart\\_','s',names(DT)))
  names(DT) <- gsub('\\_normalized','n',gsub('smart\\_','s',names(DT)))
  
  DT.list <- split(DT,DT$serial_number)
  failed_sn <- factor(unique(DT$serial_number[DT$failure==1]))
  DT.list <- lapply(DT.list,function(df){
    if(df$serial_number[1] %in% failed_sn){
      if(sum(df$failure==0) < min_pos_days){
        return(NULL)
      }else{
        return(df)
      }
    }else{
      return(df)
    }
  })
  
  return(DT.list)
}

# get x_pos items of smart before failure as pos sample and x_neg items of neg smart
get_sample <- function(DT.list,failed_sn,x_pos = 20,x_neg = 20,random_neg = T,reserveTIA=0){
  DT.list.pos <- DT.list[names(DT.list) %in% failed_sn]
  DT.list.pos.smp <- lapply(DT.list.pos,function(df){
    df <- subset(df,failure==0)
    df$label <- 1
    df[(max(1,length(df)-x_pos+1)):length(df)-reserveTIA,]
  })
  
  DT.list.neg <- DT.list[!(names(DT.list) %in% failed_sn)]
  DT.list.neg.smp <- lapply(DT.list.neg,function(df){
    if(random_neg){
      df <- smp_df(df,min(nrow(df),x_neg))
    }else{
      df <- df[max(1,(length(df)-x_neg)):length(df),]
    }
    
    df$label <- 0
    return(df)
  })
  
  return(list(DT.list.pos.smp,DT.list.neg.smp))
}

# get trainset and testset based on the sample
get_train_test <- function(DT.list.pos.smp,DT.list.neg.smp,rate=0.7){
  DT.pos.smp <- do.call(rbind,DT.list.pos.smp)
  DT.neg.smp <- do.call(rbind,DT.list.neg.smp)
  index_pos <- sample(seq_len(nrow(DT.pos.smp)),nrow(DT.pos.smp)*0.7,replace = F)
  index_neg <- sample(seq_len(nrow(DT.neg.smp)),nrow(DT.neg.smp)*0.7,replace = F)
  DT.train <- rbind(DT.pos.smp[index_pos,],DT.neg.smp[index_neg,])
  DT.test <- rbind(DT.pos.smp[-index_pos,],DT.neg.smp[-index_neg,])
  return(list(DT.train,DT.test))
}


# add feature diff for SMART attributes *r
add_diff <- function(df){
  if(nrow(df)>1){
    raw_diff <- as.data.frame.matrix(apply(df[,grepl('^s\\d.*r$',names(df))],2,function(x)c(0,diff(x))))
    names(raw_diff) <- gsub('r','d',names(raw_diff))
    df <- cbind(df,raw_diff)
  }else{
    return(NULL)
  }
}

# F1.1 Generate smart from york andk ykliu dataset----
gen_smart_ten <- function(numfolds,rate.pos){
  # load(file.path(dir_data,'diskInfo.Rda'))
  
  # all are positive sn
  load(file.path(dir_data,'ykliu_smart_6months.Rda'))
  list[id.pos,smart.pos] <- sep_smart(smart)
  
  # almost are negative sn
  load(file.path(dir_data,'york_smart_normal.Rda'))
  list[id.neg,smart.neg] <- sep_smart(smart)
  
  # Set ftime of negative as the time of collecting the last smart item
  max.time.neg <- list2df(tapply(smart.neg$time,smart.neg$sn,max),n = c('ftime','sn'))
  max.time.neg$ftime <- as.POSIXct(max.time.neg$ftime,tz = 'UTC',origin = '1970-01-01')
  id.neg$ftime <- max.time.neg$ftime[match(id.neg$sn,max.time.neg$sn)]
  
  # filter in negative set(id and smart)
  inter <- intersect(id.neg$sn,id.pos$sn)
  id.neg <- subset(id.neg,!(sn %in% inter))
  smart.neg <- subset(smart.neg,!(sn %in% inter))
  
  id.neg <- factorX(id.neg)
  id.pos <- factorX(id.pos)
  smart.neg <- factorX(smart.neg)
  smart.pos <- factorX(smart.pos)
  
  col.smart <- names(smart.pos)[3:17]
  save(id.neg,id.pos,smart.neg,smart.pos,col.smart,file = file.path(dir_data,'smart.Rda'))
  # load(file.path(dir_data,'smart.Rda'))
  
  list[smart.pos,smart.neg,id.pos,id.neg] <- gen_label_group(smart.pos,smart.neg,id.pos,id.neg,numfolds,rate.pos)
  fn <- paste('smart_',numfolds,'folds_',rate.pos,'posRate.Rda',sep='')
  save(smart.pos,smart.neg,id.pos,id.neg,col.smart,file = file.path(dir_data,fn))
}

# F1.2 seperate the smart into data part and identity part. Dcast the identity part.
sep_smart <- function(df){
  col.id <- c('sn','device','modelNum','time','ftime','ip','svrid')
  col.data <- setdiff(names(df),col.id)
  
  # generate id.*
  tmp1 <- df[,setdiff(col.id,'time')]
  tmp1 <- tmp1[!duplicated(tmp1[,c('sn','ftime')]),]
  
  # generate smart.*
  tmp2 <- df[,c('sn','time',col.data)]
  tmp2 <- tmp2[order(tmp2$sn,tmp2$time),]
  
  list(tmp1,tmp2)
}

# F1.3.1 generate label and group with rate of train/test and rate of pos/neg
gen_label_group <- function(smart.pos,smart.neg,
                            id.pos,id.neg,
                            numfolds = 5,rate.pos = 0.5){
  
  list[id.pos,smart.pos] <- add_label(id.pos,smart.pos)
  list[id.neg,smart.neg] <- add_label(id.neg,smart.neg)
  list[id.neg,smart.neg] <- filter_neg(id.neg,smart.neg,id.pos,rate.pos)
  
  id.pos$group <- add_group(nrow(id.pos),numfolds)
  id.neg$group <- add_group(nrow(id.neg),numfolds)
  
  list(factorX(smart.pos),factorX(smart.neg),factorX(id.pos),factorX(id.neg))
}

# F1.3.2 add some info for id and dist.fail and bilabel for smart
add_label <- function(id,smart){
  # reserve the last failure for id
  id <- id[order(id$sn,id$ftime,decreasing = T),]
  id <- factorX(id[!duplicated(id[,c('sn')]),])
  
  # add count,period and max/min time for id(each disk)
  tmp <- tapply(smart$time,smart$sn,function(x){
    data.frame(count = length(x),len = as.numeric(difftime(max(x),min(x),units = 'days')),
               min.time = min(x),max.time = max(x))
  })
  tmp <- do.call(rbind,tmp)
  tmp$sn <- levels(smart$sn)
  id <- merge(id,tmp,by = 'sn')
  
  # add for smart
  smart$ftime <- id$ftime[match(smart$sn,id$sn)]
  smart$dist.fail <- round(as.numeric(difftime(smart$ftime,smart$time,units = 'days')),digits = 3)
  smart$bilabel <- 1
  smart$bilabel[smart$sn %in% id.neg$sn] <- 0
  
  smart <- smart[,c(setdiff(names(smart),col.smart),col.smart)]
  list(factorX(id),factorX(smart))
}

# F1.3.3 filter some negative items to reach spercial rate.pos
filter_neg <- function(id.neg,smart.neg,id.pos,rate.pos){
  sn.neg <- levels(id.neg$sn)
  len.neg <- length(sn.neg)
  len.pos <- length(levels(id.pos$sn))
  
  idx.neg <- sample(1:len.neg,len.pos/rate.pos*(1-rate.pos))
  
  id.neg <- subset(id.neg, sn %in% sn.neg[idx.neg])
  smart.neg <- subset(smart.neg, sn %in% sn.neg[idx.neg])
  
  list(factorX(id.neg),factorX(smart.neg))
}

# F1.3.4 add a number indicating group for k-folds cross validation
add_group <- function(n,numfolds){
  numeach <- floor(n/numfolds)
  rest <- max(0,n - numeach*numfolds)
  tmp <- c(rep(seq_len(numfolds),numeach),rep(numfolds,rest))
  tmp <- sample(tmp,length(tmp))
}

# F2. extract train and test based on id.pos/id.neg and group
extract_train_test <- function(smart.pos,smart.neg,
                               id.pos,id.neg,group){
  train <- rbind(subset(smart.pos,sn %in% id.pos$sn[id.pos$group != group]),
                 subset(smart.neg,sn %in% id.neg$sn[id.neg$group != group]))
  
  test <- rbind(subset(smart.pos,sn %in% id.pos$sn[id.pos$group == group]),
                subset(smart.neg,sn %in% id.neg$sn[id.neg$group == group]))
  
  list(factorX(train),factorX(test))
}

# F3. add label for type3(failed in next N days). nw1 is rested recovering period, nw2 is N.
gen_weekly_pred <- function(smart.pos,smart.neg,nw1 = 3,nw2 = 7,use.neg = 1,use.af = 1,use.res = 0){
  smart.neg$weeklabel <- 0
  smart.pos$weeklabel <- 0
  smart.pos$weeklabel[smart.pos$dist.fail <= (nw1 + nw2)] <- 1
  
  
  if(use.neg == 0)smart.neg <- subset(smart.neg,bilabel == -99)
  if(use.af == 0)smart.pos <- subset(smart.pos,dist.fail > 0)
  if(use.res == 0)smart.pos <- subset(smart.pos,dist.fail > nw1)
  
  list(factorX(smart.pos),factorX(smart.neg))
}

# F5.filter train and test
filter_sn <- function(df,rate){
  sn.df <- levels(df$sn)
  len.sn <- length(sn.df)
  sn.df <- sn.df[sample(seq_len(len.sn),len.sn*rate)]
  df <- factorX(subset(df,sn %in% sn.df))
}

# F6.change label based on time window
change_label_tw <- function(train,test,id.pos,id.neg,tw,posAhead = 0){
  # We filter part of 0 of positive disks in train
  train.pos <- subset(train,sn %in% id.pos$sn)
  train.pos$dist.fail <- train.pos$dist.fail - posAhead
  train.pos$bilabel[train.pos$dist.fail < tw] <- 1
  train <- rbind(train.pos[train.pos$bilabel == 1,],subset(train,!(sn %in% id.pos$sn)))
  
  test$bilabel[test$sn %in% id.pos$sn] <- 1
  
  list(train,test)
}

# F7.plot predict value for disks
plot_pred_value <- function(sid,df = perf.smart){
  df <- subset(df,sn == sid)
  p <- ggplot(df,aes(x = time,y = result)) + geom_line() + geom_point()
  print(p)
  p
}

# F8.remove duplicated smart item by day for each disk.
deduplcate_smart <- function(df){
  df$date <- as.Date(df$time)
  df <- df[!duplicated(df[,c('sn','date')]),]
  df$date <- NULL
  df
}

# F9.set time window for positive items and sample several negative items for train
limit_smart <- function(df,pos.tw,neg.count){
  df.pos <- factorX(subset(df,sn %in% id.pos$sn & dist.fail <= pos.tw))
  
  df.neg <- factorX(subset(df,sn %in% id.neg$sn))
  df.neg$id <- seq_len(nrow(df.neg))
  idx.need <- unlist(tapply(df.neg$id,df.neg$sn,function(x)sample(x,min(length(x),neg.count))))
  idx.need <- as.numeric(idx.need)
  df.neg$id <- NULL
  df.neg <- factorX(df.neg[idx.need,])
  
  df.merge <- factorX(rbind(df.neg,df.pos))
}

# F10. Generate smart from baidu: modify label, add group, generate id.pos/id.neg and save
gen_smart_baidu <- function(smart){
  col.smart <- names(smart[,3:14])
  
  smart$bilabel <- fct2num(smart$label)
  smart$bilabel[smart$bilabel == 1] <- 0
  smart$bilabel[smart$bilabel == -1] <- 1
  
  smart$dist.fail <- unlist(tapply(smart$sn,smart$sn,function(x)length(x):1))
  smart <- smart[,c('sn','bilabel','dist.fail',col.smart)]
  
  # Extract sn and label for data partition
  snLabel <- data.frame(sn = levels(smart$sn),bilabel = as.numeric(tapply(smart$bilabel,smart$sn,unique)))
  snLabel$group <- add_group(nrow(snLabel),5)
  id.pos <- factorX(subset(snLabel,bilabel == 1))
  id.neg <- factorX(subset(snLabel,bilabel == 0))
  smart.pos <- factorX(subset(smart,sn %in% id.pos$sn))
  smart.neg <- factorX(subset(smart,sn %in% id.neg$sn))
  
  save(smart.pos,smart.neg,id.pos,id.neg,col.smart,file = file.path(dir_data,'gen_smart_baidu.Rda'))
}

# F11. Remove error column and items any of its value equals to -1
remove_column <- function(smart.pos,smart.neg,col.smart){
  col_remove <- c('Calibration_Retries_Value','Unsafe_Shutdown_Count_Value')
  smart.pos <- smart.pos[,-which(names(smart.pos) %in% col_remove)]
  smart.neg <- smart.neg[,-which(names(smart.neg) %in% col_remove)]
  col.smart <- setdiff(col.smart,col_remove)
  
  idx.invalid.pos <- rowSums(smart.pos[,col.smart] == -1)
  idx.invalid.neg <- rowSums(smart.neg[,col.smart] == -1)
  
  smart.pos <- factorX(smart.pos[idx.invalid.pos == 0,])
  smart.neg <- factorX(smart.neg[idx.invalid.neg == 0,])
  
  list(smart.pos,smart.neg,col.smart)
}

# F12. Generate feature using multi-days smart
gen_multi_days_smart <- function(df,numDays){
  # S1.add necessary col
  col.attr <- paste('a',seq_len(length(col.smart)),sep='')
  len.attr <- length(col.attr)
  df$date <- as.Date(df$time)
  df <- sort_col(df,col.smart)
  names(df) <- c('sn','time','ftime','dist.fail','bilabel','date',col.attr)
  
  # S2.find sn whos data is less than numDays and remove it
  sta_df_time <- list2df(tapply(df$date,df$sn,function(x)list(length(x),max(x),min(x))))
  names(sta_df_time) <- c('count','max','min','sn')
  sta_df_time$max <- as.Date(sta_df_time$max,origin = '1970-01-01')
  sta_df_time$min <- as.Date(sta_df_time$min,origin = '1970-01-01')
  sta_df_time$count_days <- as.numeric(difftime(sta_df_time$max,sta_df_time$min,units = 'days')) + 1
  sta_df_time$diff <- sta_df_time$count_days - sta_df_time$count
  
  # S3.generate data for missed date
  intergrated_sn <- sta_df_time$sn[sta_df_time$diff == 0]
  missing_data <- by(df,df$sn,function(x){
    if(x$sn[1] %in% intergrated_sn)tmp <- NULL
    else{
      sta_df_time_sn <- subset(sta_df_time,sn == x$sn[1])
      missed_date <- as.Date(setdiff(seq.Date(sta_df_time_sn$min,sta_df_time_sn$max,by = 1),x$date),origin = '1970-01-01')
      
      tmp <- data.frame(x$sn[1],as.POSIXct.Date(missed_date),missed_date)
      tmp <- cbind(tmp,matrix(0,nrow(tmp),length(col.attr)))
    }
    tmp
  })
  missing_data <- do.call(rbind,missing_data)
  names(missing_data) <- c('sn','time','date',col.attr)
  missing_data$time <- as.POSIXct(as.numeric(missing_data$time),tz = 'UTC',origin = '1970-01-01')
  missing_data$ftime <- as.p('1970-01-01')
  missing_data$dist.fail <- 300
  missing_data$bilabel <- 0
  missing_data <- missing_data[,names(df)]
  
  # S4.rbind original data and the missing data and sort them
  df <- rbind(df,missing_data)
  df <- df[order(df$sn,df$date),]
  
  # S5.cbind for numDays
  df.multidays <- df
  for (i in 1:(numDays-1)){
    tmp <- df[1:(nrow(df)-i),col.attr]
    names(tmp) <- paste(names(tmp),'-lag',i,sep='')
    # remove the first row without valid data
    df.multidays <- cbind(df.multidays[2:nrow(df.multidays),],tmp)
  }
  
  # S6.find the first numdays items of each sn, set their lagN data to 0 to remove the comtaminated data
  col.attr.multi <- names(df.multidays)[grepl('a[0-9]',names(df.multidays))]
  df.multidays$contaminated <- !duplicated(df.multidays$sn)
  df.multidays <- sort_col(df.multidays,n = col.attr.multi)
  df.multidays$contaminated[1] <- F
  for (i in 1:(numDays - 1)){
    idx <- which(df.multidays$contaminated == T) + i - 1
    col.zero <- col.attr.multi[(i*length(col.attr.multi)/numDays + 1):length(col.attr.multi)]
    df.multidays[idx,col.zero] <- 0
  }
  
  # S7.generate number of valid days
  df.valid <- data.frame(sn = df.multidays$sn,date = df.multidays$date)
  for (i in 1:numDays){
    tmp <- data.frame(apply(df.multidays[,col.attr.multi[((i-1)*len.attr + 1):(i*len.attr)]],1,function(x)any(x != 0)))
    names(tmp) <- paste('lag',i-1,sep='')
    df.valid <- cbind(df.valid,tmp)
  }
  df.valid$num_valid_days <- apply(df.valid[,3:ncol(df.valid)],1,function(x){
    tmp <- which(x == F)
    ifelse(length(tmp) == 0,tmp <- numDays,tmp <- min(tmp) - 1)
    tmp})
  df.multidays$validDays <- df.valid$num_valid_days
  
  df.multidays$contaminated <- NULL
  df.multidays <- sort_col(df.multidays,col.attr.multi)
}
