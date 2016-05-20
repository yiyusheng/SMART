rm(list = ls())
source('configWindows.R')
library('caret')
####################################
# PDF1
####################################
# data(segmentationData)
# segmentationData$Cell <- NULL
# segmentationData$Case <- NULL
# 
# # Spending Data
# inTraining <- createDataPartition(y = segmentationData$Class,p = .7,list = F)
# training <- segmentationData[inTraining,]
# testing <- segmentationData[-inTraining,]
# 
# # Centering and Scaling
# trainX <- training[, names(training) != c('Class')]
# preProcValues <- preProcess(trainX, method = c('center','scale'))
# scaledTrain <- predict(preProcValues,trainX)
# 
# # SVM training
# set.seed(1)
# cvCtrl <- trainControl(method = "repeatedcv", repeats = 3,
#                        summaryFunction = twoClassSummary,
#                        classProbs = TRUE)
# svmTune <- train(x = trainX,
#                  y = training$Class,
#                  method = 'svmRadial',
#                  tuneLength = 9,
#                  preProc = c('center','scale'),
#                  metric = 'ROC',
#                  trControl = cvCtrl)
# svmPred <- predict(svmTune, testing[,names(testing) != 'Class'])
# confusionMatrix(svmPred,testing$Class)
####################################
# PDF2
####################################
library(mlbench)
data("Sonar")
set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class,p = .75,list = F)
training <- Sonar[inTrain,]
testing <- Sonar[-inTrain,]

ctrl <- trainControl(method = 'repeatedcv',
                     repeats = 3,
                     classProb = T,
                     summaryFunction = twoClassSummary)

plsFit <- train(Class ~ .,
                data = training,
                method = 'pls',
                preProc = c('center','scale'),
                tuneLength = 15,
                trControl = ctrl,
                metric = 'ROC')

plsClasses <- predict(plsFit, newdata = testing[,names(testing) != 'Class'])
confusionMatrix(data = plsClasses, testing$Class)

















