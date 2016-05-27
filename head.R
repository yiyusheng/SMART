osFlag = Sys.info()[1] == 'Windows'
if (osFlag){
  source('configWindows.R')
}else{
  source('configLinux.R')
  library(doMC)
  registerDoMC(cores = 30)
}