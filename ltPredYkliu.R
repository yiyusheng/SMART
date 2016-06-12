# Lead time Predict on Ykliu's dataset
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
library(LiblineaR)
#@@@ LOAD DATA @@@#
train <- read.table(file.path(dir_data,'ykliuData','lrtraindata.txt'),
                    header = F,col.names = c('label',paste('V',1:12,sep='')))
testPos <- read.table(file.path(dir_data,'ykliuData','lrTrue.txt'),
                      header = F,col.names = c('label',paste('V',1:12,sep='')))
testNeg <- read.table(file.path(dir_data,'ykliuData','lrFalse.txt'),
                      header = F,col.names = c('label',paste('V',1:12,sep='')))
test <- rbind(testPos,testNeg)

# S1. Build Model
mod <- LiblineaR(data = train[,-1],target = train$label,type = 0,cost = 1,epsilon = 0.1)
mod$W <- c(-0.07852740040239534,-0.02427840667346216,0.002975267651036420,0.002196515295376795,0.1107740814221596,
           -0.01975935546637947,-0.01443466649304586,0,0.0001978329654758379,0.01591120347609452,-0.0006877470849838484,
           0,0)

# S2. Test Model
thresTest <- function(test,thres,labn,predn){
  predBool <- factorX(as.numeric(test[[predn]] > thres))
  c <- confusionMatrix(predBool,test[[labn]])
  FDR <- as.numeric(c$byClass['Specificity'])*100
  FAR <- (1 - as.numeric(c$byClass['Sensitivity']))*100
  cat(sprintf('iw:%s\tThreshold:%.2f\tFDR:%.2f\tFAR:%.2f\n',predn,thres,FDR,FAR))
  r <- list(predn,thres,FDR,FAR)
}

p <- predict(mod,test[,-1],proba = T)
p <- p$probabilities[,2]
test$pred <- p
meanP <- round(mean(p),digits = 1)
rset <- sapply(seq(max(0,meanP-0.3),min(1,meanP+0.3),0.01),
               thresTest,test = test,labn = 'label',predn = 'pred')
