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