# Lead time Predict on Ykliu's dataset
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
library(LiblineaR)
#@@@ LOAD DATA @@@#
train <- read.table(file.path(dir_data,'old','ykLiu_Data','lrtraindata.txt'),
                    header = F,col.names = c('label',paste('V',1:12,sep='')))
testNeg <- read.table(file.path(dir_data,'old','ykLiu_Data','lrTrue.txt'),
                      header = F,col.names = c('label',paste('V',1:12,sep='')))
testPos <- read.table(file.path(dir_data,'old','ykLiu_Data','lrFalse.txt'),
                      header = F,col.names = c('label',paste('V',1:12,sep='')))
test <- rbind(testPos,testNeg)

# S1. Build Model
mod <- LiblineaR(data = train[,-1],target = train$label,type = 0,cost = 1,epsilon = 0.01)

# S2. Test Model
thresTest <- function(test,thres,lab,pred){
  predBool <- factorX(as.numeric(test[[pred]] > thres))
  c <- confusionMatrix(predBool,test[[lab]])
  FDR <- as.numeric(c$byClass['Specificity'])*100
  FAR <- (1 - as.numeric(c$byClass['Sensitivity']))*100
  cat(sprintf('iw:%s\tThreshold:%.2f\tFDR:%.2f\tFAR:%.2f\n',pred,thres,FDR,FAR))
  r <- list(pred,thres,FDR,FAR)
}

p <- predict(mod,test[,-1],proba = T)
p <- p$probabilities[,2]
test$pred <- p
rset <- sapply(seq(0.2,0.9,0.1),thresTest,test = test,lab = 'label',pred = 'pred')


# meanP <- round(mean(p),digits = 1)
# rset <- sapply(seq(max(0,meanP-0.3),min(1,meanP+0.3),0.01),
#                thresTest,test = test,labn = 'label',predn = 'pred')
