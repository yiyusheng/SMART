# Disk Failure Predict on Baidu's dataset
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')

#@@@ LOAD DATA @@@#
# smartBaidu <- read.table(file.path(dir_data,'Disk_SMART_dataset.txt'))
# names(smartBaidu) <- c('sn','label','Raw Read Error Rate','Spin Up Time','Reallocated Sectors Count',
#                        'Seek Error Rate','Power On Hours','Reported Uncorrectable Errors',
#                        'High Fly Writes','Temperature Celsius','Hardware ECC Recovered',
#                        'Current Pending Sector Count','Reallocated Sectors Count Raw','Current Pending Sector Count Raw')
# for(i in 1:13){
#   smartBaidu[,i] <- as.numeric(gsub(',','',levels(smartBaidu[,i])[smartBaidu[,i]]))
# }
# save(smartBaidu,file = file.path(dir_data,'smartBaidu.Rda'))
load(file.path(dir_data,'smartBaidu.Rda'))
smartB1 <- smartBaidu
smartName <- names(smartBaidu[,3:14])
####################################
# S1.统计每个盘的数据量
# snCount <- melt(table(smartBaidu[,c('sn','label')]))
# snCount <- subset(snCount,value != 0)
# ggplot(subset(snCount,label == -1),aes(x = value,fill = factor(label))) + 
#   geom_histogram(position = 'dodge',binwidth = 10)

# S2.统计每个字段下降和上升的次数，以及上升下降的平均值
# staVar <- function(array){
#   len <- length(array)
#   diff <- array[2:len] - array[1:(len-1)]
#   diffUp <- diff[diff > 0]
#   diffDown <- diff[diff < 0]
#   r <- list(upCount = length(diffUp), upAvg = mean(diffUp),
#             downCount = length(diffDown), downAvg = mean(diffDown))
# }
# 
# smartB2 <- smartB1
# n <- names(smartBaidu[,3:14])
# b <- by(smartB2[,3:14],smartB2$sn,function(data){
#     c(sapply(n,function(nn){staVar(data[[nn]])}))
# })
# b <- matrix(unlist(b),byrow = T, nrow = length(b))
# b[is.na(b)] <- 0
# b <- data.frame(b)
# save(b,file = file.path(dir_data,'predBaidu.Rda'))
# 
# failSN <- unique(smartBaidu$sn[smartBaidu$label == '-1'])
# upC <- b[,seq(1,48,4)];upC$sn <- levels(smartB2$sn);upC$label <- 1;upC$label[1:433] <- -1
# upA <- b[,seq(2,48,4)];upA$sn <- levels(smartB2$sn);upA$label <- 1;upA$label[1:433] <- -1
# downC <- b[,seq(3,48,4)];downC$sn <- levels(smartB2$sn);downC$label <- 1;downC$label[1:433] <- -1
# downA <- b[,seq(4,48,4)];downA$sn <- levels(smartB2$sn);downA$label <- 1;downA$label[1:433] <- -1
# 
# names(upC)[1:12] <- names(smartBaidu)[3:14];upC <- melt(upC,id.vars = c('sn','label'))
# names(upA)[1:12] <- names(smartBaidu)[3:14];upA <- melt(upA,id.vars = c('sn','label'))
# names(downC)[1:12] <- names(smartBaidu)[3:14];downC <- melt(downC,id.vars = c('sn','label'))
# names(downA)[1:12] <- names(smartBaidu)[3:14];downA <- melt(downA,id.vars = c('sn','label'))
# 
# for(i in 1:12){
#   proj <- 'upC'
#   p <- ggplot(subset(upA,variable == n[i]),aes(x = value,fill = factor(label))) + 
#     geom_histogram(position = 'dodge',bins = 30) + scale_y_log10() + xlab(n[i]) + ylab(proj)
#   ggsave(p,file = file.path(dir_data,'upAndDown',paste(proj,'_',n[i],'.jpg',sep='')),
#          width = 16,height = 12,dpi=100)
# }

# S3.故障预测
library(caret)
library(e1071)
# Add time column
# t <- unlist(tapply(smartB1$sn,smartB1$sn,function(x)(1:length(x))/length(x)))
# t1 <- unlist(tapply(smartB1$sn,smartB1$sn,function(x)1:length(x)))
t2 <- unlist(tapply(smartB1$sn,smartB1$sn,function(x)length(x):1))
# smartB1$time <- t   # standard time
# smartB1$timeA <- t1 # ordered time
smartB1$timeB <- t2 # descend order
smartB1 <- smartB1[,c('sn','label','timeB',smartName)]
smartB1$label <- as.numeric(fct2ori(smartB1$label))
smartB1$label[smartB1$label == -1] <- 2
smartB1$label <- smartB1$label - 1

# Extract sn and label for data partition
snLabel <- data.frame(sn = levels(smartB1$sn),
                      label = as.numeric(tapply(smartB1$label,smartB1$sn,function(x)x[1])))

smartPred <- function(smartB1,tw = 12){
  p1 <- proc.time()
  # Data partition including time window
  inTrain <- createDataPartition(y = snLabel$label, p = .7, list = FALSE)
  training <- factorX(subset(smartB1, sn %in% snLabel$sn[inTrain] & timeB <= tw))
  testing <- factorX(subset(smartB1, sn %in% snLabel$sn[-inTrain]))
  
  # Model Training
  smp <- sample(1:nrow(training),50000)
  mod <- svm(training[smp,smartName],training$label[smp],
             type = 'C', kernel = 'radial', gamma = 0.1, cost = 10)# Model Testing
  
  
  # Model Testing
  pred <- predict(mod,testing[,smartName])
  testing$pred <- pred
  r1 <- confusionMatrix(testing$pred,testing$label)
  
  predDisk <- data.frame(sn = levels(testing$sn),
                         predDisk = as.numeric(tapply(testing$pred,testing$sn,function(x){any(x == 1)})))
  predDisk$label <- snLabel$label[match(predDisk$sn,snLabel$sn)]
  r2 <- confusionMatrix(predDisk$predDisk,predDisk$label)
  
  
  # Visualization
  predVisual <- factorX(testing[testing$sn %in% snLabel$sn[snLabel$label == 1],
                                c('sn','time','pred')])
  posInfo <- data.frame(sn = levels(predVisual$sn),
                        posRate = as.numeric(tapply(predVisual$pred,predVisual$sn,
                                                    function(x)sum(x == -1)/length(x))),
                        firstTime = as.numeric(by(predVisual[c('time','pred')],predVisual$sn,
                                                  function(x){
                                                    if (any(x$pred == -1))
                                                      min(x$time[x$pred == -1])
                                                    else 1
                                                  })))
  posInfo <- posInfo[order(posInfo$posRate,posInfo$firstTime),]
  predVisual$sn <- factor(predVisual$sn,levels = posInfo$sn)
  p <- ggplot(predVisual,aes(x = time,y = factor(sn),color = pred)) + geom_point(size = 0.1) +
    xlab('time to failure (1 = failure)') + ylab('SN')
  ggsave(p,file = file.path(dir_data,'Figure','LeadTimeDist_tw',paste('tw',tw,'.jpg',sep='')),
         width = 8,height = 6,dpi = 100)
  p2 <- proc.time()
  print(sprintf('tw: %.0f FDR: %.3f FAR: %.3f time:%fs',tw,FDR*100,FAR*100,p2[3]-p1[3]))
}
sapply(seq(12,192,12),function(x)smartPred(smartB1,x))
