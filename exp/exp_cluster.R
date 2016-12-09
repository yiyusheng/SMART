#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: exp_cluster.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-11-11 09:56:47
#
# Last   modified: 2016-11-11 09:56:49
#
#
#
source('head_data.R')
source('../gen_smart.R')
require(flexclust)
require(ggplot2)

# S1.fit cluster model and return class of items in train and test
cluster_smart <- function(df.train,df.test,N){
  system.time(model <- kmeans(df.train[,col.smart],centers = N,nstart = 3,algorithm = 'Hartigan-Wong'))
  model.kcca <- as.kcca(model,data = df.train[,col.smart])
  
  class.train <- predict(model.kcca)
  class.test <- predict(model.kcca,newdata = df.test[,col.smart])
  
  list(class.train,class.test)
}

# S2.generate failure rate of each class
eval_cluster <- function(df.train,class.train){
  id.cluster <- sort(unique(class.train))
  
  df.train$class <- factor(class.train,levels = id.cluster)
  
  # percentage of each classes
  rate <- melt(table(class.train))
  names(rate) <- c('class','rate')
  rate$rate <- rate$rate/sum(rate$rate)
  
  # mean distance to real failure of each class
  mean.dist <- melt(tapply(df.train$dist.fail,df.train$class,mean))
  
  # failure rate of each class
  fr <- melt(tapply(df.train$dist.fail,df.train$class,function(x)sum(x < 0)/length(x)))
  
  # Merge
  fr <- merge(fr,mean.dist,by = 'Var1')
  names(fr) <- c('class','fr','mean.dist')
  fr$class <- factor(fr$class)
  fr <- merge(fr,rate,by = 'class')
  
  # plot
  plot.fr <- fr[order(fr$rate),]
  plot.fr$class <- 1:nrow(plot.fr)
  p <- ggplot(plot.fr,aes(x = class,y = sort(rate))) + geom_bar(stat = 'identity') +
    xlab('classID') + ylab('failure rate') + ggtitle(paste('N = ',nrow(fr),sep=''))
  ggsave(file=file.path(dir_data,'figure','cluster',paste('N',nrow(fr),'.jpg',sep='')),
         plot=p, width = 12, height = 9, dpi = 100)
  fr
}

# data prepare
list[nw1,nw2,use.neg,use.af,use.res,group] <- list(0,15,1,1,1,4)
list[smart.pos.nw,smart.neg.nw] <- gen_weekly_pred(smart.pos,smart.neg,nw1,nw2,use.neg,use.af,use.res)
list[train,test] <- extract_train_test(smart.pos.nw,smart.neg.nw,id.pos,id.neg,group)

require(doParallel)
idx <- seq(2,15,1)
ck <- makeCluster(min(40,length(idx)),type = 'FORK',outfile = 'out_cluster')
registerDoParallel(ck)

r <- foreach(i = idx,.verbose = F) %dopar% {
  list[class.train,class.test] <- cluster_smart(train,test,i)
  cat(sprintf('[%s][%d]model finished\n',Sys.time(),i))
  fr <- eval_cluster(train,class.train)
  
  test$class <- class.test
  test$fr <- fr$fr[match(test$class,fr$class)]
  cor.test <- cor(test$fr,test$dist.fail)
  
  list(a = class.train,b = class.test,c = cor.test)
}
save(r,file = file.path(dir_data,'exp_cluster1.Rda'))
stopCluster(ck)


