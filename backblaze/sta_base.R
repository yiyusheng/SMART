# Load data ------
rm(list = ls());setwd('~/Code/R/SMART/backblaze');source('~/rhead')
# source('dir_func.R')
source('test.R')
source('models.R')
require('e1071')

dir.bb <- file.path(dir_d,'backblaze')
file.list <- list.files(dir.bb)
load(file.path(dir.bb,'group_sn.Rda'))
load(file.path(dir.bb,'read_sn.Rda'))
load(file.path(dir.bb,'smart_name.Rda'))
load(file.path(dir.bb,'A1.Rda'))

# Step1 ------
#get effective attributes of SMART by checking whether there are more than 10 different values of this attributes.
# smart_len <- apply(DT[,grepl('smart',names(DT))],2,function(arr)length(unique(arr)))
# smart_len.change <- smart_len[smart_len>6]
# save(smart_len.change, file=file.path(dir_data,'key_attributes_SMART.Rda'))
load(file.path(dir_data,'key_attributes_SMART.Rda'))
col_cfg <- names(DT)[1:5]
DT <- DT[,c(col_cfg,names(smart_len.change))]

# Step2 ------
# clean data
list[DT.list,failed_sn,col_value] <- clean_DT(DT)

# divide disk into trainset and testset
list[DT.list.train,DT.list.test] <- get_train_test(DT.list,failed_sn,0.7)

# resample for trainset
DT.train <- get_sample(DT.list.train,failed_sn,x_pos=7,x_neg=20,random_neg=T)

# build model
list[model,DT.train] <- get_model(DT.train,'support.vector.machine', col_value)
with(DT.train,table(label,result))

# predict testset
DT.test <- do.call(rbind,DT.list.test)
DT.test$result <- predict(model,DT.test[,col_value])
