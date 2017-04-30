# Eliminate some bad disk whos lead time confuse result.
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
####################################
# S1.check restTime of failed disk
load(file.path(dir_data,'SMARTMerge_Int2h.Rda'))
smartF$restTime <- as.numeric(difftime(smartF$failed_time,smartF$time,tz = 'UTC',units = 'hours'))
longRestTime <- data.frame(sn = levels(smartF$sn),
                           lrt = as.numeric(tapply(smartF$restTime,smartF$sn,max)))
longRestTime <- subset(longRestTime,lrt > 0)

# S2 decrease machines
longRestTime$day <- round(longRestTime$lrt/24)
tabL <- melt(table(longRestTime$day))
resSN <- by(longRestTime,longRestTime$day,function(x){
  if(nrow(x) > 30){
    r <- x[sample(1:nrow(x),25),]
  }else{
    r <- x
  }
  r
})
resFSN <- do.call(rbind,resSN)
save(resFSN,file = file.path(dir_data,'resFSN.Rda'))