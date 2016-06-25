# Lead time Predict Function
# Date: 2016-06-23
# Author: Pezy

# F1. Threshold test. FAR FDR generation
thresTest <- function(data,thres){
  predBool <- as.numeric(data$pred > thres)
  FDR <- sum(data$label == 1 & predBool == 1)/sum(data$label == 1)*100
  FAR <- sum(data$label == -1 & predBool == 1)/sum(data$label == -1)*100
  r <- list(thres,FDR,FAR)
  r
}

# F2. model training
modTrain <- function(data,iw,attrN){
  tmp <- subset(data,restTime < iw*24*2 & restTime > iw*24*0.2)
  labN <- paste('lab',iw,sep='')
  predN <- paste('pred',iw,sep='')
  tmpMod <- tmp[,c(attrN,labN)]
  
  
  lenPos <- nrow(tmpMod[tmpMod[[labN]] == 1,])
  lenNeg <- nrow(tmpMod[tmpMod[[labN]] == -1,])

  w <- data.frame(1,lenPos/lenNeg)
  names(w) <- c(0,1)
  
  mod <- LiblineaR(data = tmpMod[,attrN],target = tmpMod[[labN]],
                   type = 0,
                   cost = 1,
                   # wi = w,
                   epsilon = 0.01)
  eval(parse(text = sprintf('list(%s = mod)',labN)))
}

# F3. model testing
modTest <- function(data,iw,attrN,mod){
  # tmp <- subset(data,restTime < iw*24*2 & restTime > iw*24*0.2)
  tmp <- data
  labN <- paste('lab',iw,sep='')
  predN <- paste('pred',iw,sep='')
  pp <- predict(mod[[labN]],tmp[,attrN],proba = T,decisionValues = T)
  p <- pp$probabilities[,1]
  # q <- pp$predictions
  # print(summary(p))
  eval(parse(text = sprintf('list(%s = p)',predN)))
}

# F4. merge training and testing
leadTimePrep <- function(at,dataTr,dataTe){
  attrN <- c(at,paste(at,'amp'),paste(at,'int'),paste(at,'cont'))
  
  # training
  mod <- sapply(iwSet,modTrain,data = dataTr,attrN = at)
  
  # testing
  testSet <- sapply(iwSet,modTest,data = dataTe,attrN = at,mod = mod)
  testR <- data.frame(do.call(cbind,testSet))
  list(t = testR,m = mod) 
}

# F5. test performance with different threshold
# dataP: data predicted|dataT: data testing
evalTestS <- function(dataP,dataT,iw){
  # tmp <- subset(dataT,restTime < iw*24*2 & restTime > iw*24*0.2)
  tmp <- dataT
  labN <- paste('lab',iw,sep='')
  predN <- paste('pred',iw,sep='')
  ttest <- data.frame(pred = dataP[[predN]],label = tmp[[labN]])
  r <- sapply(seq(0,1,0.01),thresTest,data = ttest)
  r <- data.frame(matrix(unlist(r),byrow = T,ncol = 3))
  names(r) <- c('threhold','FDR','FAR')
  r <- subset(r,FAR > 0 & FAR < 30)
  if (nrow(r) != 0){
    r$class <- labN
  }
  list(r)
}

# F6. data generation base on different combination of attributes
dataGen <- function(attrNeed,scl= T){
  attrFinal <- names(smartL)[names(smartL) %in% 
                               c(attrNeed,paste(attrNeed,'amp'),paste(attrNeed,'int'),paste(attrNeed,'cont'))]
  attrFinal <- sort(attrFinal)
  sNameFinal <- c(names(smartL)[c(1:4,20,21)],attrFinal)
  
  smartL <- subset(smartL, sn %in% diskInfoF$sn[diskInfoF$countF == 1])
  smartL$restTime <- as.numeric(difftime(smartL$failed_time,smartL$time,tz = 'UTC',units = 'hours'))
  smartL <- subset(smartL,restTime > 0)
  smartL$modelLtp <- modelLabel(smartL$model)
  smartL <- smartL[order(smartL$sn,smartL$time),]
  smartL <- factorX(smartL[,c('sn','modelLtp','label','restTime','time',attrFinal)])
  if (scl == T){
    smartL[,attrFinal] <- scale(smartL[,attrFinal])
    # scale to [0,1]
    # maxs <- apply(smartL[,attrFinal],2,max)
    # mins <- apply(smartL[,attrFinal],2,min)
    # smartL[,attrFinal] <- scale(smartL[,attrFinal],center = mins, scale = maxs - mins)
  }
  smartL
}