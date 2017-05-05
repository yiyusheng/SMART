###### VARIABLES ######
dirName <- 'SMART'
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#CC6666", "#9999CC", "#66CC99","#000000")
col_smart <- c('Raw_Read_Error_Rate_Value','Spin_Up_Time_Value','Reallocated_Sector_Ct_Value','Reallocated_Sector_Ct_Raw',
               'Seek_Error_Rate_Value','Spin_Retry_Count_Value','Calibration_Retries_Value','Unsafe_Shutdown_Count_Value',
               'Power_Cycle_Count_Value','PowerOnHours_Count_Value','Offline_Uncorrectable_Value','Temperature_Celsius_Value',
               'Udma_CRC_Error_Count_Value','Current_Pending_Sector_Value','Current_Pending_Sector_Raw')
dir_code <- file.path(dir_c,dirName)
dir_data <- file.path(dir_d,dirName)

if (osFlag == 'Windows'){
  source('D:/Git/R_libs_user/R_custom_lib.R')
}else{
  dir_dataSMART14 <- file.path(dir_data,'smart5k')
  source('~/Code/R/R_libs_user/R_custom_lib.R')
  # options('width' = 150)
}

###### PACKAGES ######
require('scales')
require('grid')
require('ggplot2')
require('reshape2')
require('plyr')


