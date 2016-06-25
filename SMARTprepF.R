# Sequenize smart ykliu
# Because smart from ykliu are all from failed disk.
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'diskInfo.Rda'))
load(file.path(dir_data,'ykliu_smart.Rda'))   #from ykliu
load(file.path(dir_data,'smartName.Rda'))
#load(file.path(dir_data,'load_ftr_attridOld.Rda'))
#source(file.path(dir_code,'dataPrepareOld.R'))

####################################
# S1 Convert failed_time to POSIXct for ykliu's smart
# smart$failed_time <- ct$failed_time[match(smart$sn,ct$sn)]
smart$failed_time <- as.POSIXct(smart$failed_time,tz = 'UTC')
smart$time <- as.POSIXct(smart$time,tz = 'UTC')
smart$label <- 1

# S2. Adjust time interval for failure disk
int <- 2
timeseq <- seq.POSIXt(as.POSIXct('2014-06-01',tz = 'UTC'),as.POSIXct('2014-11-01',tz='UTC'),by = 3600*int)
sn <- levels(smart$sn)

adjTime <- function(data){
  t1 <- proc.time()
  maxd <- max(data$time)
  mind <- min(data$time)
  ts <- timeseq[timeseq > mind & timeseq < maxd]

  idx <- rep(0,length(ts))
  for (i in 1:length(ts)){
    tmp <- abs(difftime(ts[i],data$time,units = 'day'))
    idx[i] <- which(tmp == min(tmp))
  }
  # Filter the first and the last to short array
  idxA <- which(idx>1 & idx < max(idx))
  s <- cbind(data[idx[idxA],],seqTime = ts[idxA])
  t2 <- proc.time()
  print(sprintf('No.%f SN:%s Duration:%f nrowOri:%f nrowAdd:%f rate:%f',
                which(data$sn[1] == sn),data$sn[1],t2[3]-t1[3],nrow(data),nrow(s),nrow(s)/nrow(data)))
  s
}

smart <- by(smart,factor(smart$sn),adjTime)
smartF <- do.call(rbind,smart)
names(smartF)[5:19] <- smartName
row.names(smartF) <- NULL
save(smartF,file = file.path(dir_data,
                            paste('SMARTprepF',int,'h.Rda',sep='')))
