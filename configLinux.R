# Config for directory
dir_code <- '/home/yiyusheng/Code/R/SMART'
dir_data <- '/home/yiyusheng/Data/SMART'
dir_data5k <- file.path(dir_data,'smart5k.Rda')
source('/home/yiyusheng/Code/R/Rfun.R')
require('caret')
require('e1071')
require('ggplot2')
require('scales')
require('reshape2')
require('grid')
#source(file.path(dir_code,'SMARTFunc.R'))

# Config for linux GUI
options('width' = 150)
