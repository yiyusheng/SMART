# Disk Failure Predict on Baidu's dataset
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
source('gen_smart.R')
source('dirFunc.R')
source('plot_smart.R')

source('model_df.R')
source('gen_result_df.R')

library(e1071)
library(ggplot2)

# S1. Data load
# load(file.path(dir_data,'smartBaidu.Rda'))
# gen_smart_baidu(smartBaidu)
load(file.path(dir_data,'gen_smart_baidu.Rda'))

# para set
funcNames <- c('logic.regression','support.vector.machine')
para <- expand.grid(funcNames[c(1)],c(5:20),c(1:5),c(5),c(5),24,'pdf_lt_baidu')
names(para) <- c('func','collect.time','group','pos.tw','neg.count','units.perday','dir_plot')
para$func <- fct2ori(para$func);attr(para,'out.attrs') <- NULL
para <- factorX(subset(para,pos.tw <= collect.time))
i <- 31; list[func,collect.time,group,pos.tw,neg.count,units.perday,dir_plot] <- as.list(para[i,])

# S3. Fit model and generate result
require(doParallel)
idx <- seq_len(nrow(para))
ck <- makeCluster(min(40,length(idx)),type = 'FORK',outfile = 'out_df_baidu')
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = F) %dopar%
  group.model(smart.pos,smart.neg,id.pos,id.neg,para$func[i],
              para$collect.time[i],para$group[i],
              para$pos.tw[i],para$neg.count[i],
              para$units.perday[i],para$dir_plot[i])
stopCluster(ck)

# S4. Save
# save(r,para,file = file.path(dir_data,'predict_df_baidu_20_5_5.Rda'))

# S5. Save patb
name.patb <- c('FDR','FAR','F1','thre','rate.allpos','func','collect.time','group','pos.tw','neg.count')
patb <- list2df(lapply(r,'[[','patb'),n = name.patb)
save(patb,file = file.path(dir_data,'patb_baidu_5-20_5_5.Rda'))
