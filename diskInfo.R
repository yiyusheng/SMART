# Test
rm(list = ls())
#@@@ CONFIGURE @@@#
source(file.path('D:/Git/SMART','SMARTConfig.R'))

#@@@ Function @@@#
source('D:/Git/R_Function/Rfun.R')
source(file.path(dir_code,'SMARTFunc.R'))

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'disk_cmdb.Rda'))
load(file.path(dir_data,'load_ftr_attridOld.Rda'))
source(file.path(dir_code,'dataPrepareOld.R'))
#########################################################

diskInfo <- diskBModel
diskInfo$svrid <- cmdb$svr_asset_id[match(diskInfo$ip,cmdb$ip)]
diskInfo <- subset(diskInfo,grepl('sd',device),c('sn','svrid','ip','device','Model_clear'))


#disk Number
diskCount <- data.frame(svrid = levels(diskInfo$svrid),
                        count = as.numeric(tapply(as.character(diskInfo$sn),diskInfo$svrid,length)))
diskInfo$diskNum <- diskCount$count[match(diskInfo$svrid,diskCount$svrid)]
# diskInfo$diskNum[is.na(diskInfo$diskNum)] <- -1

#Failure
diskInfo$class <- 'Normal'
diskInfo$class[diskInfo$svrid %in% data.f$svr_id] <- 'Failure'
diskInfo$class <- factor(diskInfo$class)

#dClass
diskInfo$dClass <- cmdb$dClass[match(diskInfo$svrid,cmdb$svr_asset_id)]
diskInfo$dClass <- factor(diskInfo$dClass)

#time
diskInfo$shTime <- cmdb$shiptimeToLeft[match(diskInfo$svrid,cmdb$svr_asset_id)]
diskInfo$fsTime <- data.f$failShiptime[match(diskInfo$svrid,data.f$svr_id)]
# diskInfo$shTime[is.na(diskInfo$shTime)] <- -1
# diskInfo$fsTime[is.na(diskInfo$fsTime)] <- -1

#save
diskInfo <- factorX(diskInfo)
save(diskInfo,file = file.path(dir_data,'diskInfo.Rda'))

diskInfoValid <- subset(diskInfo,!is.na(svrid))
save(diskInfoValid,file = file.path(dir_data,'diskInfoValid.Rda'))
