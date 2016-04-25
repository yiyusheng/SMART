# Test
rm(list = ls())
#@@@ CONFIGURE @@@#
source(file.path('D:/Git/SMART','SMARTConfig.R'))

#@@@ Function @@@#
source('D:/Git/R_Function/Rfun.R')
source(file.path(dir_code,'SMARTFunc.R'))

#@@@ LOAD DATA @@@#
# load(file.path(dir_data,'load_ftr_attridOld.Rda'))
# source(file.path(dir_code,'dataPrepareOld.R'))
#########################################################
load(file.path(dir_data,'disk_cmdb.Rda'))
