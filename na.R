# S4 exp
ido.pos <- id.pos
ido.neg <- id.neg
perf.all <- list()
perf.disk <- list()
perf.smart <- list()
idp <- list()
idn <- list()
p <- list()
for(i in 1:100){
  cat(sprintf('No.%d loop\n',i))
  list[train,test,id.pos,id.neg] <- gen_train_test(smart.pos,smart.neg,ido.pos,ido.neg,0.7,0.5)
  list[train,test] <- change_label_tw(train,test,id.pos,id.neg,10)
  list[perf.all[[i]],perf.disk[[i]],perf.smart[[i]]] <- group.model(train,test,'logic.regression',use.exist = 0)
  p[[i]] <- eval_disk(perf.disk[[i]])
  idp[[i]] <- id.pos
  idn[[i]] <- id.neg
}
save(perf.all,perf.disk,p,idp,idn,file = file.path(dir_data,'result_1101.Rda'))
save(perf.smart,file = file.path(dir_data,'result_1101.smart.Rda'))
# eval_result(perf.all,perf.disk,perf.smart)

idx.21 <- c(5,30,33,47,67,71,90)
idx.76 <- idx.21 + 1

perf.allx <- perf.all[idx.21];perf.ally <- perf.all[idx.76]
perf.diskx <- perf.disk[idx.21];perf.disky <- perf.disk[idx.76]
perf.smartx <- perf.smart[idx.21];perf.smarty <- perf.smart[idx.76]
idpx <- idp[idx.21];idpy <- idp[idx.76]
idnx <- idp[idx.21];idny <- idn[idx.76]
px <- p[idx.21];py <- p[idx.76]

list[x,pd1] <- eval_disk(perf.disky[[1]]);pd1 <- subset(pd1,cut == '(76,77]')
list[x,pd2] <- eval_disk(perf.disky[[2]]);pd2 <- subset(pd2,cut == '(76,77]')
list[x,pd3] <- eval_disk(perf.disky[[3]]);pd3 <- subset(pd3,cut == '(76,77]')

tmp <- sapply(perf.disk,function(x){
  fct2ori(x$sn[round(x$leadTime/1440) == 21])
})
tmp <- unique(unlist(tmp))
tmp.76 <- factor(tmp)
tmp.21 <- factor(tmp)
id.pos.76 <- subset(id.pos,sn %in% tmp.76)
id.pos.21 <- subset(id.pos,sn %in% tmp.21)

id.pos.tmp <- id.pos
tmp <- sapply(perf.disk,function(x){
  a <- eval_lt(x)
  round(a$leadTime[match(id.pos.tmp$sn,a$sn)]/1440,digits = 2)
})
tmp1 <- apply(tmp,1,summary)
id.pos.tmp <- cbind(id.pos.tmp,t(tmp1))
id.pos.tmp$cut <- cut(id.pos.tmp$Mean,seq(0,150,5),include.lowest = T)
id.pos.tmp$diff <- round(id.pos.tmp$Max. - id.pos.tmp$Min.)
ggplot(id.pos.tmp,aes(Mean)) + geom_histogram(binwidth = 5)
ggplot(id.pos.tmp,aes(diff)) + geom_histogram(binwidth = 5)

# F4.generate train and test with rate of train/test and rate of pos/neg
gen_train_test <- function(smart.pos,smart.neg,
                           id.pos,id.neg,
                           rate.traintest,rate.pos){
  id.neg$ftime <- as.p('2017-01-01')
  list[id.pos,smart.pos] <- add_label(id.pos,smart.pos)
  list[id.neg,smart.neg] <- add_label(id.neg,smart.neg)
  
  sn.pos <- levels(smart.pos$sn)
  sn.neg <- levels(smart.neg$sn)
  len.pos <- length(sn.pos)
  len.neg <- length(sn.neg)
  
  idx.pos <- sample(1:len.pos,rate.traintest*len.pos)
  idx.neg1 <- sample(1:len.neg,len.pos/rate.pos*(1-rate.pos))
  idx.neg2 <- sample(idx.neg1,rate.traintest*length(idx.neg1))
  idx.neg3 <- setdiff(idx.neg1,idx.neg2)
  train <- rbind(smart.pos[smart.pos$sn %in% sn.pos[idx.pos],],
                 smart.neg[smart.neg$sn %in% sn.neg[idx.neg2],])
  test <- rbind(smart.pos[smart.pos$sn %in% sn.pos[-idx.pos],],
                smart.neg[smart.neg$sn %in% sn.neg[idx.neg3],])
  
  id.pos$set <- 'Free';id.neg$set <- 'Free'
  id.pos$set[id.pos$sn %in% sn.pos[idx.pos]] <- 'train'
  id.neg$set[id.neg$sn %in% sn.neg[idx.neg2]] <- 'train'
  id.pos$set[id.pos$sn %in% sn.pos[-idx.pos]] <- 'test'
  id.neg$set[id.neg$sn %in% sn.neg[idx.neg3]] <- 'test'
  
  save(train,test,id.neg,id.pos,file = file.path(dir_data,'traintest.Rda'))
  load(file.path(dir_data,'traintest.Rda'))
  list(factorX(train),factorX(test),factorX(id.pos),factorX(id.neg))
}


gen_threSet_onSMARTperf <- function(df){
  tmp <- smp_df(df,0.1)
  roc <- roc(tmp$bilabel,tmp$result)
  tmp <- data.frame(sum = roc$sensitivities + roc$specificities,roc$thresholds)
  tmp <- tmp[order(tmp$sum,decreasing = T),]
  tmp <- tmp[1:100,]
  
  opt.thre <- tmp$roc.thresholds[1]
  min.thre <- min(tmp$roc.thresholds);max.thre <- max(tmp$roc.thresholds)
  dist <- max(abs(min.thre - opt.thre),abs(max.thre-opt.thre))
  p <- opt.thre - dist;q <- opt.thre + dist
  thre.predict <- seq(p,q,length.out = 1000)
}

list[trainF,testF] <- change_label_tw(train,test,id.pos,id.neg,tw,posAhead = 0)
list[trainF1,testF1] <- change_label_tw(train,test,id.pos,id.neg,tw,posAhead = 10)
list[trainF2,testF2] <- change_label_tw(train,test,id.pos,id.neg,tw,posAhead = 20)
list[trainF3,testF3] <- change_label_tw(train,test,id.pos,id.neg,tw,posAhead = 30)

t0 <- subset(trainF,sn %in% id.pos$sn[id.pos$group != 1][2])
t1 <- subset(trainF1,sn %in% id.pos$sn[id.pos$group != 1][2])
t2 <- subset(trainF2,sn %in% id.pos$sn[id.pos$group != 1][2])
t3 <- subset(trainF3,sn %in% id.pos$sn[id.pos$group != 1][2])


# Train 
trainF <- filter_sn(trainF,1)
model <- glm(bilabel ~ .,data = trainF[,c('bilabel',col.smart)],family = 'binomial')
model1 <- glm(bilabel ~ .,data = trainF1[,c('bilabel',col.smart)],family = 'binomial')
model2 <- glm(bilabel ~ .,data = trainF2[,c('bilabel',col.smart)],family = 'binomial')
model3 <- glm(bilabel ~ .,data = trainF3[,c('bilabel',col.smart)],family = 'binomial')

# Predict
pred <- predict(model,testF[,col.smart],type = 'response')
testF$result <- pred
list[perf.all,perf.disk,perf.smart,thre] <- gen_result(testF)
perf.disk <- subset(perf.disk,overThre == 1 & bilabel == 1)
# testF

pred1 <- predict(model1,testF1[,col.smart],type = 'response')
testF1$result <- pred1
list[perf.all1,perf.disk1,perf.smart1,thre1] <- gen_result(testF1)
perf.disk1 <- subset(perf.disk1,overThre == 1 & bilabel == 1)

pred2 <- predict(model2,testF2[,col.smart],type = 'response')
testF2$result <- pred2
list[perf.all2,perf.disk2,perf.smart2,thre2] <- gen_result(testF2)
perf.disk2 <- subset(perf.disk2,overThre == 1 & bilabel == 1)

pred3 <- predict(model3,testF3[,col.smart],type = 'response')
testF3$result <- pred3
list[perf.all3,perf.disk3,perf.smart3,thre3] <- gen_result(testF3)
perf.disk3 <- subset(perf.disk3,overThre == 1 & bilabel == 1)

perf.disk <- perf.disk[,c('sn','leadTime')]
names(perf.disk) <- c('sn','leadTime0')
perf.disk1 <- perf.disk1[,c('sn','leadTime')]
names(perf.disk1) <- c('sn','leadTime1')
perf.disk2 <- perf.disk2[,c('sn','leadTime')]
ames(perf.disk2) <- c('sn','leadTime2')
perf.disk3 <- perf.disk3[,c('sn','leadTime')]
names(perf.disk3) <- c('sn','leadTime3')


a <- merge(merge(merge(perf.disk,perf.disk1,by = 'sn'),perf.disk2,by = 'sn'),perf.disk3,by = 'sn')
perf.all$class <- 'noAhead';perf.all1$class <- 'Ahead10';
perf.all2$class <- 'Ahead20';perf.all3$class <- 'Ahead30';
b <- rbind(perf.all,perf.all1,perf.all2,perf.all3)
ggplot(b,aes(x = FAR,y = FDR,group = class,color = class)) + geom_line() 

# result of positive disks
split.df <- split(df.result,df.result$sn)
r <- lapply(threSet,function(x)gen_result_disk(split.df,x))

perf.disk <- data.frame(t(sapply(r,'[[','result')))
names(perf.disk) <- c('neg.acc','pos.acc','pos.FDR','pos.FAR','thre')
perf.disk <- do.call(rbind,lapply(r,'[[','df'))
p <- plot_result_nw(perf.disk)

# F2. generate result of each disk
# gen_result_disk <- function(split.df,thre){
#   # evaluate for each disk
#   r <- lapply(split.df,function(x){
#     x$pred <- as.numeric(x$result > thre)
#     x.base <- base_eval(x$weeklabel,x$pred)
#   })
#   perf.disk <- list2df(r,n = c('TP','FN','TN','FP','P','N','FDR','FAR','acc','sn'))
#   perf.disk$class <- 1
#   perf.disk$class[perf.disk$sn %in% id.neg$sn] <- 0
#   perf.disk$thre <- thre
#   
#   # generate result for each disk
#   perf.disk.pos <- factorX(subset(perf.disk,class == 1))
#   perf.disk.neg <- factorX(subset(perf.disk,class == 0))
#   perf.disk.neg$FAR[is.na(perf.disk.neg$FAR)] <- 0
#   perf.disk.neg$FDR[is.na(perf.disk.neg$FDR)] <- 1
#   perf.disk.pos$FAR[is.na(perf.disk.pos$FAR)] <- 0
#   perf.disk.pos$FDR[is.na(perf.disk.pos$FDR)] <- 1
#   perf.disk.pos$acc <- perf.disk.pos$FDR + perf.disk.pos$FAR
#   
#   # generate result for all
#   neg.acc <- sum(perf.disk.neg$acc == 1)/nrow(perf.disk.neg)
#   pos.acc <- sum(perf.disk.pos$acc != 0,na.rm = T)/nrow(perf.disk.pos)
#   pos.FDR <- sum(perf.disk.pos$FDR != 0,na.rm = T)/nrow(perf.disk.pos)
#   pos.FAR <- sum(perf.disk.pos$FAR != 0,na.rm = T)/nrow(perf.disk.pos)
#   
#   list(result = round(c(neg.acc,pos.acc,pos.FDR,pos.FAR,thre),digits = 4),
#        df = perf.disk)
# }

# F2.extract a roc curve and other information related to prediction performance from roc object
# smp_roc <- function(df){
#   roc <- roc(df$weeklabel,df$result,algorithm = 2)
#   
#   tbl.response <- melt(table(roc$response))
#   num.pos <- tbl.response$value[tbl.response$Var1 == '1']
#   num.neg <- tbl.response$value[tbl.response$Var1 == '0']
#   
#   roc.new <- data.frame(FDR = round(roc$sensitivities,digits = 3),
#                         FAR = round(1 - roc$specificities,digits = 3),
#                         thre = roc$thresholds,
#                         TP = num.pos*roc$sensitivities,
#                         FN = num.pos*(1 - roc$sensitivities),
#                         TN = num.neg*roc$specificities,
#                         FP = num.neg*(1 - roc$specificities))
#   roc.new$F <- (roc.new$FN*num.neg + roc.new$FP*num.pos)/(num.neg + num.pos)
#   roc.new <- roc.new[!duplicated(roc.new$FDR),]
#   roc.new
# }


# F3.Analize result
# sta_result <- function(df){
#   df <- df[df$dist.fail > 0,-which(names(df) %in% col.smart)]
#   df.pos <- factorX(subset(df,sn %in% id.pos$sn))
#   df.neg <- factorX(subset(df,sn %in% id.neg$sn))
#   df.roc <- roc(df$weeklabel,df$result,algorithm = 2)
#   df.roc.simple <- smp_roc(df.roc)
#   
#   min.thre.pos <- list2df(by(df.pos,df.pos$sn,function(x){min(x$result[x$weeklabel == 1])}),n = c('thre','sn'))
#   min.thre.neg <- list2df(by(df.neg,df.neg$sn,function(x){min(x$result[x$weeklabel == 1])}),n = c('thre','sn'))
#   
#   min.thre.pos$modelNum <- id.pos$modelNum[match(min.thre.pos$sn,id.pos$sn)]
#   min.thre.pos$svrid <- id.pos$svrid[match(min.thre.pos$sn,id.pos$sn)]
#   min.thre.pos$prefix <- substr(min.thre.pos$svrid,5,6)
#   
#   ggplot(min.thre.pos,aes(x = thre,fill = prefix)) + geom_histogram(bins = 50)
# }

# For SS
# splitSSF <- split(SS,SS$svrid)
# r <- lapply(splitSSF,function(df){
#   df <- splitSSF[[sample(seq_len(length(splitSSF)),1)]]
#   sid_df <- fct2ori(df$svrid[1]
#                     )
#   len_df <- nrow(df)
#   len_valid <- sum(df$count_valid_day == mode_value)
#   df_invalid <- subset(df,count_valid_day != mode_value)
#   
#   if(len_valid/len_df < 0.5){
#     return(sid_df)
#   }else{
#     return(df_invalid)
#   }
# })
# r_char<- lapplyX(r,function(x)if(is.character(x))return(x))
# r_valid <- lapplyX(r,function(x)if(!is.character(x))return(x))

# For SD, we check replacement of disk by comparison bewtween average number of working disks in each day and the number of disks recorded.
# mode_value <- as.numeric(names(sort(-table(SS$count_valid_day)))[1])
# server_replacement <- data.frame(svrid = levels(SS$svrid),
#                                  num_disks_recorded = as.numeric(tapply(SS$svrid,SS$svrid,length)),
#                                  num_disk_all_duty = as.numeric(by(SS$count_valid_day,SS$svrid,function(x)sum(x == mode_value))))
# server_replacement <- subsetX(server_replacement,num_disk_all_duty > 0)
# 
# # server_replacement$mode_days <- as.numeric(tapply(SS$count_valid_day,SS$svrid,mode_num))
# # server_replacement <- subsetX(server_replacement,mode_days >= 22)
# 
# 
# sta_days <- list2df(lapply(splitSSD, function(df){
#   num_working_disks_days <- colSums(df[,col_day])
#   roundX(mean(num_working_disks_days))
# }),n = c('mean_num_working_disks','svrid'))
# sta_days$digit <- sta_days$mean_num_working_disks - floor(sta_days$mean_num_working_disks)
# sta_days$integer <- sta_days$mean_num_working_disks - sta_days$digit
# sta_days$pred_num_working_disks <- sta_days$mean_num_working_disks
# sta_days$pred_num_working_disks[sta_days$digit > 0.95] <- sta_days$integer[sta_days$digit > 0.95] + 1
# sta_days$pred_num_working_disks[sta_days$digit < 0.05] <- sta_days$integer[sta_days$digit < 0.05]
# server_replacement$pred_num_working_disks <- sta_days$pred_num_working_disks[match(server_replacement$svrid,sta_days$svrid)]

# aggreate result of SD
# SS <- factorX(sta_sn_count(SD,dayS,dayE,T));save(SS,file = file.path(dir_data,'find_rules_fail_disks_SS.Rda'))
# load(file.path(dir_data,'find_rules_fail_disks_SS.Rda'))
# sn_in_two_server <- melt(table(sta_ss$sn));names(sn_in_two_server) <- c('sn','num_server')
# SS$num_server <- sn_in_two_server$num_server[match(SS$sn,sn_in_two_server$sn)]

# a example
# sid <- 'TYSV09123015';a <- subset(SS,svrid == sid);b <- subset(SD,svrid == sid);c <- subset(f2014_06_09,svrid == sid);d <- subset(cmdb,svr_asset_id == sid)

# if(nrow(df_candi) == 0){
#   tmp <- colSums(df[,cur_col_day])
#   idx_last_day <- which.max(tmp != 0)
#   df <- subset(df,count_valid_day != idx_last_day);if(nrow(df) == 0)return(2) # All dudy of all disks means no replacement
#   df_failed <- df[df[cur_col_day[idx_last_day]] == 0,];if(nrow(df_failed) == 0)return(3) # define failed disk if the number of its SMART in the last day is zero.
#   df_candi <- subset(df,!(sn %in% df_failed$sn));if(nrow(df_candi) == 0)return(4)
# }