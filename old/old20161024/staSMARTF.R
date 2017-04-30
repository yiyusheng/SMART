# Lead time Predict on Tencent's dataset
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
#@@@ LOAD DATA @@@#
load(file.path(dir_data,'predLeadTime_smartL24h.Rda'))
load(file.path(dir_data,'SMARTprepN2w.Rda'))

smartL$sector <- smartL$Reallocated_Sector_Ct_Raw + smartL$Current_Pending_Sector_Raw
smartL$sectorDiff <- smartL$Reallocated_Sector_Ct_RawDiff + smartL$Current_Pending_Sector_RawDiff
# smartName <- c('Raw_Read_Error_Rate_Value','Spin_Up_Time_Value',
#                'Reallocated_Sector_Ct_Value','sector','Seek_Error_Rate_Value')
# smartName <- c(smartName,paste(smartName,'Diff',sep=''))
# smartL <- smartL[,c(names(smartL)[1:7],smartName)]
smartLbup <- smartL

# S1. Add label
smartL <- smartLbup
smartL <- subset(smartL,restTime/24 <= 20)
iterWin <- 1
iw <- seq(iterWin,20,iterWin)
labName <- paste('lab',iw,sep='')
for (i in 1:length(iw)){
  smartL[[labName[i]]] <- as.numeric(smartL$restTime <= iw[i]*24)
}

smartName <- names(smartL)[8:22]
names(smartL)[8:22] <- paste('a',1:15,sep='')
names(smartL)[23:37] <- paste('d',1:15,sep='')

# S2.number of zero from d1 to d15
r <- sapply(1:15,function(i){
  colname <- paste('d',i,sep='')
  list(c <- colname,nz = sum(smartL[[paste('d',i,sep='')]] == 0))
})
r <- data.frame(matrix(unlist(r),nrow = 15))
