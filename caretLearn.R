rm(list = ls())
library('caret')
library('mlbench')
data(Sonar)
set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class, p = .75, list = FALSE)
# str(inTrain)
training <- Sonar[inTrain,]
testing <- Sonar[-inTrain,]
plsFit <- train(Class ~ ., data = training, method = 'pls', preProcess = C('center','scale'))
