# Generate SMART, trainset, testset
# plot data

# F1. seperate the smart into data part and identity part. Dcast the identity part.
sep_smart <- function(df){
  col.id <- c('sn','device','modelNum','time','ftime','ip','svrid')
  col.data <- setdiff(names(df),col.id)
  
  tmp1 <- df[,setdiff(col.id,'time')]
  tmp1 <- tmp1[!duplicated(tmp1[,c('sn','ftime')]),]
  
  tmp2 <- df[,c('sn','time',col.data)]
  tmp2 <- tmp2[order(tmp2$sn,tmp2$time),]
  
  list(tmp1,tmp2)
}

# F2. Generate smart from york andk ykliu dataset
gen_smart <- function(){
  load(file.path(dir_data,'diskInfo.Rda'))
  
  load(file.path(dir_data,'ykliu_smart_6months.Rda'))
  list[id.pos,smart.pos] <- sep_smart(smart)
  
  load(file.path(dir_data,'york_smart_normal.Rda'))
  list[id.neg,smart.neg] <- sep_smart(smart)
  
  inter <- intersect(id.neg$sn,id.pos$sn)
  id.neg <- subset(id.neg,!(sn %in% inter))
  smart.neg <- subset(smart.neg,!(sn %in% inter))

  id.neg <- factorX(id.neg)
  id.pos <- factorX(id.pos)
  smart.neg <- factorX(smart.neg)
  smart.pos <- factorX(smart.pos)
  
  save(id.neg,id.pos,smart.neg,smart.pos,file = file.path(dir_data,'smart.Rda'))
}

####################################

# F3. add label for smart
add_label <- function(id,smart){
  id <- id[order(id$sn,id$ftime,decreasing = T),]
  id <- factorX(id[!duplicated(id[,c('sn')]),])
  
  # add for id
  tmp <- tapply(smart$time,smart$sn,function(x){
    data.frame(count = length(x),len = difftime(max(x),min(x),units = 'days'),
               min.time = min(x),max.time = max(x))
  })
  tmp <- do.call(rbind,tmp)
  tmp$sn <- levels(smart$sn)
  id <- merge(id,tmp,by = 'sn')
  
  # add for smart
  smart$ftime <- id$ftime[match(smart$sn,id$sn)]
  smart$dist.fail <- round(as.numeric(difftime(smart$ftime,smart$time,units = 'days')),digits = 3)
  smart$bilabel <- as.numeric(smart$dist.fail <= 0)
  
  smart <- smart[,c(setdiff(names(smart),col.smart),col.smart)]
  list(factorX(id),factorX(smart))
}

filter_neg <- function(id.neg,smart.neg,id.pos,rate.pos){
  sn.neg <- levels(id.neg$sn)
  len.neg <- length(sn.neg)
  len.pos <- length(levels(id.pos$sn))
  
  idx.neg <- sample(1:len.neg,len.pos/rate.pos*(1-rate.pos))
  id.neg <- subset(id.neg, sn %in% sn.neg[idx.neg])
  smart.neg <- subset(smart.neg, sn %in% sn.neg[idx.neg])
  
  list(factorX(id.neg),factorX(smart.neg))
}

add_group <- function(n,numfolds){
  numeach <- floor(n/numfolds)
  rest <- max(0,n - numeach*numfolds)
  tmp <- c(rep(seq_len(numfolds),numeach),rep(numfolds,rest))
  tmp <- sample(tmp,length(tmp))
}

# F4.generate label and group with rate of train/test and rate of pos/neg
gen_label_group <- function(smart.pos,smart.neg,
                            id.pos,id.neg,
                            numfolds = 5,rate.pos = 0.5){
  id.neg$ftime <- as.p('2017-01-01')
  list[id.pos,smart.pos] <- add_label(id.pos,smart.pos)
  list[id.neg,smart.neg] <- add_label(id.neg,smart.neg)
  list[id.neg,smart.neg] <- filter_neg(id.neg,smart.neg,id.pos,rate.pos)
  
  id.pos$group <- add_group(nrow(id.pos),numfolds)
  id.neg$group <- add_group(nrow(id.neg),numfolds)
  
  list(factorX(smart.pos),factorX(smart.neg),factorX(id.pos),factorX(id.neg))
}

extract_train_test <- function(smart.pos,smart.neg,
                               id.pos,id.neg,
                               groupid){
  train <- rbind(subset(smart.pos,sn %in% id.pos$sn[id.pos$group != groupid]),
                   subset(smart.neg,sn %in% id.neg$sn[id.neg$group != groupid]))
  
  test <- rbind(subset(smart.pos,sn %in% id.pos$sn[id.pos$group == groupid]),
                   subset(smart.neg,sn %in% id.neg$sn[id.neg$group == groupid]))
  
  list(factorX(train),factorX(test))
}

# add label for failure in next week prediction
gen_weekly_pred <- function(smart.pos,smart.neg,nw = 7,use.neg = 0,use.af = 0){
  smart.pos$weeklabel <- 0
  smart.neg$weeklabel <- 0
  smart.neg$weeklabel[smart.neg$dist.fail <= nw] <- 1
  
  if(use.neg == 0)smart.neg = NULL
  if(use.af == 0)smart.pos <- subset(smart.pos,dist.fail > 0)
  
  list(smart.pos,smart.neg)
}
####################################

# F5.filter train and test
filter_sn <- function(df,rate){
  sn.df <- levels(df$sn)
  len.sn <- length(sn.df)
  sn.df <- sn.df[sample(seq_len(len.sn),len.sn*rate)]
  df <- factorX(subset(df,sn %in% sn.df))
}

# F6.change label based on time window
change_label_tw <- function(train,test,id.pos,id.neg,tw,posAhead = 0){
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

