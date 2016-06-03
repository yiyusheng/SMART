# Statistic data getting from ykliu
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'diskInfo.Rda'))
load(file.path(dir_data,'ykliu_smart.Rda'))   #from ykliu
load(file.path(dir_data,'extractSMART.Rda'))  #from york
load(file.path(dir_data,'load_ftr_attridOld.Rda'))
source(file.path(dir_code,'dataPrepareOld.R'))

######
# #S1. check dev_class_id and business of disk from ykliu
# smartM <- smart
# smartM <- mchAttr(smartM,diskInfo,'svrid','sn')
# smartM <- mchAttr(smartM,cmdb,'dev_class_id','svrid')
# smartM <- factorX(smartM)
# cmdbM <- factorX(subset(cmdb,svrid %in% smartM$svrid))
# #extract sn
# sti$diSn <- gsub('.*SN:(\\w{8})[,;].*','\\1',sti$hwinfo)
# 
# # S2. generate train and test set.
# # S2.1 clean smart from york
# smartNeed <- mchAttr(smartNeed,diskInfoSmp,'class','sn')
# smartN <- subset(smartNeed,1==1,c('sn','model','class','time',
#                                         names(smartNeed)[c(7:9,21,10:20)]))
# names(smartN)[5:19] <- paste('a',1:15,sep='')
# smartN$label <- 0
# smartN$label[smartN$class == 'Failure'] <- 1
# smartN <- factorX(subset(smartN,
#                        a1 >= 0 & a1 <= 253 & a2 >= 0 & a2 <= 253 & a3 >= 0 & a3 <= 253 &
#                        a5 >= 0 & a5 <= 253 & a6 >= 0 & a6 <= 253 & a9 >= 0 & a9 <= 253 &
#                        a10 >= 0 & a10 <= 253 & a11 >= 0 & a11 <= 253 & a12 >= 0 & a12 <= 253 &
#                        a13 >= 0 & a13 <= 253 & a14 >= 0 & a14 <= 253 & !is.na(a15)))
# 
# # S2.2 Add failed time to york's smart
# diskInfoSmp$f_time <- data.f$f_time[match(diskInfoSmp$svrid,data.f$svrid)]
# diskInfoSmp$f_time[is.na(diskInfoSmp$f_time)] <- as.POSIXct('1970-01-01',tz='UTC')
# smartN$class <- diskInfoSmp$f_time[match(smartN$sn,diskInfoSmp$sn)]
# names(smartN)[3] <- 'failed_time'
# smartN$group <- 'york'
# 
# # S2.3 Convert failed_time to POSIXct for ykliu's smart
# smart$failed_time <- as.POSIXct(smart$failed_time)
# smart$time <- as.POSIXct(smart$time)
# smart$group <- 'ykliu'
# smart$label <- 1
# 
# # S2.4 Choose smart records for failed disk: 
# # records 4 days before failure and records 1 day after failure are selected
# smartV1 <- subset(smart,failed_time - time < 4*86400 & failed_time - time > -1*86400)
# 
# # S2.5 Choose random 10 smart records for normal disk
# idxN <- by(smartN,smartN$sn,function(x){
#   tmp <- x[sample(1:nrow(x),min(nrow(x),60)),]
#   tmp
# })
# smartNV1 <- do.call(rbind,idxN)
# 
# # S2.6 Merge them 
# smartM <- factorX(rbind(smartV1,subset(smartNV1,label == 0)))
# 
# # S2.7 Divide data
# tmp1 <- subset(smartM,label == 0)
# tmp2 <- subset(smartM,label == 1)
# idx1tr <- sort(sample(1:nrow(tmp1),round(0.7*nrow(tmp1))))
# idx1te <- sort(setdiff(1:nrow(tmp1),idx1tr))
# idx2tr <- sort(sample(1:nrow(tmp2),round(0.7*nrow(tmp2))))
# idx2te <- sort(setdiff(1:nrow(tmp2),idx1tr))
# 
# smartMTrain <- rbind(tmp1[idx1tr,],tmp2[idx2tr,])
# smartMTest <- rbind(tmp1[idx1te,],tmp2[idx2te,])
# save(smartM,file = file.path(dir_data,'smartM.Rda'))

# S3. libsvm + caret on smartM
load(file.path(dir_data,'smartM.Rda'))
library(caret)
library(e1071)
smartM1 <- smartM[,c(5:19,21)]
ctrl <- trainControl(method = 'repeatedcv',number = 10, repeats = 3)
set.seed(100)
# mod <- train(label~.,data = smartM1[sample(1:nrow(smartM1),1000),],method = 'svmLinear',trControl = ctrl)
idx <- sample(1:nrow(smartM1),20000)
idx.train <- idx[1:10000]
idx.test <- idx[10001:20000]
mod <- svm(smartM1[idx.train,c(1:15)],smartM1$label[idx.train],
           type = 'C', kernel = 'radial', gamma = 0.1, cost = 10)
pred <- predict(mod,smartM1[idx.test,c(1:15)])
r <- table(pred,smartM1$label[idx.test])
fdr <- r[2,2]/(r[2,1] + r[2,2])
far <- r[2,1]/(r[2,1] + r[1,1])
