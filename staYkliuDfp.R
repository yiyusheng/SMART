# statistic steps and data for ykliu's disk failure prediction
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
library(e1071)
library(lattice)

####################################
# S1. svmTrainData
svmTrainData <- read.table(file.path(dir_data,'old','ykLiu','svmTrainData.txt'))
names(svmTrainData) <- c('label','read_error','spin_time','re_sec_value','re_sec_raw','seek_error',
                         'spin_retry','shut_down','power_cycle','power_hour','offline','temper','pend_value')

inTrain <- createDataPartition(y = svmTrainData$label, p = .7, list = FALSE)
training <- factorX(svmTrainData[inTrain,])
testing <- factorX(svmTrainData[-inTrain,])

svmMod <- svm(x = training[,-1],
              y = training$label,
              type = 'C', kernel = 'radial')
svmPred <- predict(svmMod,testing[,-1])
r <- confusionMatrix(svmPred,testing$label)

# S2. svmGoodData
svmGoodData <- read.table(file.path(dir_data,'old','ykLiu','svmGoodData.txt'))

# S3. svmtest
svmGoodTest <- read.table(file.path(dir_data,'old','ykLiu','svmGoodTest.txt'),col.names = names(svmTrainData))
svmFalseTest00 <-read.table(file.path(dir_data,'old','ykLiu','svmFalseTest00.txt'),col.names = names(svmTrainData))
svmFalseTest01 <-read.table(file.path(dir_data,'old','ykLiu','svmFalseTest01.txt'),col.names = names(svmTrainData))
svmFalseTest02 <-read.table(file.path(dir_data,'old','ykLiu','svmFalseTest02.txt'),col.names = names(svmTrainData))
goodPred <- predict(svmMod,svmGoodTest[,-1])
FalseTest00 <- predict(svmMod,svmFalseTest00[,-1])
FalseTest01 <- predict(svmMod,svmFalseTest01[,-1])
FalseTest02 <- predict(svmMod,svmFalseTest02[,-1])
r1 <- confusionMatrix(goodPred,svmGoodTest$label)
r00 <- confusionMatrix(FalseTest00,svmFalseTest00$label)
r01 <- confusionMatrix(FalseTest01,svmFalseTest01$label)
r02 <- confusionMatrix(FalseTest02,svmFalseTest02$label)
