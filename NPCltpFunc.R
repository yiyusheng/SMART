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

# F2.A model training for different iw
modTrainIW <- function(data,attrN,iw,percidx = 'per100',percSets){
  pSet <- percSets[[percidx]]
  tmp <- subset(data,(restTime >= pSet[1]*24*iw & restTime <= pSet[2]*24*iw) |
                  (restTime >= pSet[3]*24*iw & restTime <= pSet[4]*24*iw))
  labN <- paste('lab',iw,sep='')
  predN <- paste('pred',iw,sep='')
  tmpMod <- tmp[,c(attrN,labN)]
  
  # Set weight
  lenPos <- nrow(tmpMod[tmpMod[[labN]] == 1,])
  lenNeg <- nrow(tmpMod[tmpMod[[labN]] == -1,])
  w <- data.frame(1,lenPos/lenNeg)
  names(w) <- c(-1,1)
  
  # model building
  mod <- LiblineaR(data = tmpMod[,attrN],target = tmpMod[[labN]],
                   type = 0,
                   cost = 1,
                   wi = w,
                   epsilon = 0.01)
  eval(parse(text = sprintf('list(%s = mod)',labN)))
}

# F2.B model training for different percSet
modTrainPerc <- function(data,attrN,iw = iwD,percidx = 'per100',percSets){
  pSet <- percSets[[percidx]]
  tmp <- subset(data,(restTime >= pSet[1]*24*iw & restTime <= pSet[2]*24*iw) |
                  (restTime >= pSet[3]*24*iw & restTime <= pSet[4]*24*iw))
  labN <- paste('lab',iw,sep='')
  predN <- paste('pred',iw,sep='')
  tmpMod <- tmp[,c(attrN,labN)]
  
  # Set weight
  lenPos <- nrow(tmpMod[tmpMod[[labN]] == 1,])
  lenNeg <- nrow(tmpMod[tmpMod[[labN]] == -1,])
  w <- data.frame(1,lenPos/lenNeg)
  names(w) <- c(-1,1)
  
  # model building
  mod <- LiblineaR(data = tmpMod[,attrN],target = tmpMod[[labN]],
                   type = 0,
                   cost = 1,
                   # wi = w,
                   epsilon = 0.01)
  list(mod)
  
}

# F3.A model testing for iw
modTestIW <- function(data,iw,attrN,mod){
  tmp <- data
  labN <- paste('lab',iw,sep='')
  predN <- paste('pred',iw,sep='')
  pp <- predict(mod[[labN]],tmp[,attrN],proba = T,decisionValues = T)
  p <- pp$probabilities[,1]
  # q <- pp$predictions
  # print(summary(p))
  eval(parse(text = sprintf('list(%s = p)',predN)))
}

# F3.B model testing for percSet
modTestPerc <- function(data,iw,attrN,mod,percInx = 'per100'){
  tmp <- data
  labN <- paste('lab',iw,sep='')
  predN <- paste('pred',iw,sep='')
  pp <- predict(mod[[percInx]],tmp[,attrN],proba = T,decisionValues = T)
  p <- pp$probabilities[,1]
  list(p)
}

# F4. merge training and testing
leadTimePrep <- function(at,dataTr,dataTe,flag = 'iw',percSets,iwSet){
  attrN <- c(at,paste(at,'amp'),paste(at,'int'),paste(at,'cont'))
  
  if(flag == 'iw'){
    # training and testing for iw
    mod <- sapply(iwSet,modTrainIW,data = dataTr,attrN = at,percSets = percSets)
    testSet <- sapply(iwSet,modTestIW,data = dataTe,attrN = at,mod = mod)
  }else if(flag == 'perc'){
    # training and testing for percSets
    mod <- sapply(names(percSets),modTrainPerc,data = dataTr,attrN = at,iw = iwD,percSets = percSets)
    testSet <- sapply(names(percSets),modTestPerc,data = dataTe,attrN = at,mod = mod,iw = iwD)
  }

  # send out
  testR <- data.frame(do.call(cbind,testSet))
  list(t = testR,m = mod) 
}

# F5.A test performance with different threshold
# dataP: data predicted|dataT: data testing
evalTestS <- function(dataP,dataT,iw){
  tmp <- dataT
  labN <- paste('lab',iw,sep='')
  predN <- paste('pred',iw,sep='')
  ttest <- data.frame(pred = dataP[[predN]],label = tmp[[labN]])
  r <- sapply(seq(0,1,0.01),thresTest,data = ttest)
  r <- data.frame(matrix(unlist(r),byrow = T,ncol = 3))
  names(r) <- c('threhold','FDR','FAR')
  r <- subset(r,FAR > 0 & FAR < 20)
  if (nrow(r) != 0){
    r$class <- labN
  }
  list(r)
}

# F5.B test performance with different threshold
# dataP: data predicted|dataT: data testing
evalTestSPerc <- function(dataP,dataT,iw = iwD,percInx = 'per100'){
  tmp <- dataT
  labN <- paste('lab',iw,sep='')
  predN <- paste('pred',iw,sep='')
  ttest <- data.frame(pred = dataP[[percInx]],label = tmp[[labN]])
  r <- sapply(seq(0,1,0.01),thresTest,data = ttest)
  r <- data.frame(matrix(unlist(r),byrow = T,ncol = 3))
  names(r) <- c('threhold','FDR','FAR')
  r <- subset(r,FAR > 0 & FAR < 20)
  if (nrow(r) != 0){
    r$class <- percInx
  }
  list(r)
}

# F6. data generation base on different combination of attributes
dataGen <- function(attrNeed,scl= T,data = smartL){
  # for attr
  attrFinal <- names(data)[names(data) %in% 
                               c(attrNeed,paste(attrNeed,'amp'),paste(attrNeed,'int'),paste(attrNeed,'cont'))]
  attrFinal <- sort(attrFinal)
  sNameFinal <- c(names(data)[c(1:4,20,21)],attrFinal)
  # to filter disk whose number of failure is large than 1 and whose data is less than 15 days.
  data <- subset(data, sn %in% diskInfoF$sn[diskInfoF$countF == 1 & diskInfoF$count > 8*15])
  # to generate restTime
  data$restTime <- as.numeric(difftime(data$failed_time,data$time,tz = 'UTC',units = 'hours'))
  data <- subset(data,restTime > 0)
  # to scale model name
  data$modelLtp <- modelLabel(data$model)
  # order and finish
  data <- data[order(data$sn,data$time),]
  data <- factorX(data[,c('sn','modelLtp','label','restTime','time',attrFinal)])
  if (scl == T){
    data[,attrFinal] <- scale(data[,attrFinal])
    # scale to [0,1]
    # maxs <- apply(data[,attrFinal],2,max)
    # mins <- apply(data[,attrFinal],2,min)
    # data[,attrFinal] <- scale(data[,attrFinal],center = mins, scale = maxs - mins)
  }
  data
}

# F7. Plot 
plotTestE <- function(testE){
  p <- ggplot(testE,aes(x = FAR, y = FDR, color = class, shape = class )) + 
    geom_line(size = 1) + geom_point(size = 2) + xlab('False Alarm Rate (%)') + ylab('Failure Detection Rate (%)') + 
    xlim(c(0,20)) +
    scale_x_continuous(breaks = seq(0,20,2)) +
    scale_y_continuous(breaks = seq(0,60,10)) +
    guides(shape = guide_legend(title='Iterate Window'),color = guide_legend(title = 'Iterate Window')) + 
    theme_bw() +
    theme(panel.background = element_rect(color = 'black'),
          panel.grid.minor = element_line(size = 0.4),
          panel.grid.major = element_line(colour = 'grey', linetype = 'dotted', size = 1),
          #           panel.grid.major.x = element_blank(),
          
          plot.title = element_blank(),
          axis.line = element_line(color = 'black'),
          axis.text = element_text(size = 24),
          axis.title = element_text(size = 26),
          
          legend.title = element_text(size = 20),
          legend.key.width = unit(4,units = 'line'),
          legend.key.height = unit(1.5,units = 'line'),
          legend.text = element_text(size = 20),
          legend.position = c(0.95,0.05),
          legend.justification = c(1,0),
          legend.background = element_rect(fill = alpha('grey',0.5))
    )
  print(p)
  ggsave(file=file.path(dir_data,'figure','LTPNormal.eps'), plot=p, width = 8, height = 6, dpi = 100)
}
 # 8.find best pred label based on prosibility
procIterR <- function(data,iwSet){
  r <- apply(data,MARGIN = 1,function(x){
    iwSet[which(x == max(x))]
  })
  r
}

# 9. calculate iterate accuracy
iterAccu <- function(restTime,bestPred,iwSet){
  len <- length(iwSet)
  idxB <- which(bestPred == iwSet)
  bD <- ifelse(idxB == 1,0,iwSet[idxB - 1])
  bU <- ifelse(idxB == len,2000,iwSet[idxB])
  r <- restTime >= bD & restTime <= bU
  r
}

# 10. Plot for the iterate lead time prediction
plotrRate <- function(r.rate){
  p <- ggplot(r.rate,aes(x = iterateWindow,y = Rate,fill = factor(leadTime))) + 
    geom_bar(stat = 'identity',position = 'dodge') + 
    xlab('Number of Models') + ylab('Accuracy (%)') + 
    scale_y_continuous(breaks = seq(0,70,10)) +
    guides(fill = guide_legend(title = 'Iterate Window (days)')) + 
    theme_bw() +
    theme(panel.background = element_rect(color = 'black'),
          panel.grid.minor = element_line(size = 0.4),
          panel.grid.major = element_line(colour = 'grey', linetype = 'dotted', size = 1),
          #           panel.grid.major.x = element_blank(),
          
          plot.title = element_blank(),
          axis.line = element_line(color = 'black'),
          axis.text = element_text(size = 24),
          axis.title = element_text(size = 26),
          
          legend.title = element_text(size = 20),
          legend.key.width = unit(4.5,units = 'line'),
          legend.key.height = unit(1,units = 'line'),
          legend.text = element_text(size = 20),
          legend.position = c(0.03,0.97),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5))
    )
  print(p)
  ggsave(file=file.path(dir_data,'figure','LTPIterate.eps'), plot=p, width = 8, height = 6, dpi = 100)
}