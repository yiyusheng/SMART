# Vibration and Decreasement of SMART attributes & Disk Failure
# Test
rm(list = ls())
#@@@ CONFIGURE @@@#
source(file.path('D:/Git/SMART','SMARTConfig.R'))

#@@@ Function @@@#
source('D:/Git/R_Function/Rfun.R')
source(file.path(dir_code,'SMARTFunc.R'))

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'diskInfoValid.Rda'))
load(file.path(dir_data,'load_ftr_attridOld.Rda'))
source(file.path(dir_code,'dataPrepareOld.R'))

#@@@ LOCAL FUNCTION @@@#
CDF_plot <- function(at,ftr){
  at <- as.character(at)
  ftr <- as.character(ftr)
  tmp <- subset(SD,attr == at,c('group',ftr,'class'))
  names(tmp) <- c('group','ft','class')
  
  p <- ggplot(tmp,aes(ft,color = factor(group),linetype = factor(group))) + 
    stat_ecdf(size = 1) + xlab(at) + ylab(ftr) + 
    scale_y_continuous(breaks = seq(0,1,0.1)) +
    guides(color = guide_legend((title = NULL)), linetype = guide_legend((title = NULL))) +
    scale_color_manual(values = c(rep('red',3),rep('blue',3))) +
    # coord_cartesian(xlim = c(max(min(tmp$ft),-1000),min(max(tmp$ft),1000))) +
    scale_x_continuous(limits = c(max(min(tmp$ft),-1000),min(max(tmp$ft),1000))) +
    theme_bw() +
    theme(panel.background = element_rect(color = 'black'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = 'grey', linetype = 'dotted'),
          
          plot.title = element_text(size = 26,vjust = 1),
          axis.line = element_line(color = 'black'),
          axis.text = element_text(size = 22),
          # axis.text.x = element_text(angle = 40,margin = margin(10)),
          axis.title = element_text(size = 24),
          
          legend.key.width = unit(4,units = 'line'),
          legend.key.height = unit(1.5,units = 'line'),
          legend.text = element_text(size = 26),
          legend.position = c(0,1),legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5))
    )
  print(p)
  ggsave(file=file.path(dir_data,'SMART_FailureA',paste(at,'_',ftr,'.jpg',sep='')), 
         plot=p, width = 16, height = 12, dpi = 300)
  
  # scale_x_continuous(breaks = seq(xl[1],xl[2],1),labels = round(2^(seq(xl[1],xl[2],1)-30),2)) +
}
###################################################
# S1.导入并清洗数据
load(file.path(dir_data,'staDown.Rda'))
SD <- subset(staDown,sn %in% diskInfoValid$sn)
SD$onlineTime <- as.POSIXct(SD$onlineTime,origin='1970-01-01')
SD$offlineTime <- as.POSIXct(SD$offlineTime,origin='1970-01-01')
SD$firstDT <- as.POSIXct(SD$firstDT,origin='1970-01-01')
SD$lastDT <- as.POSIXct(SD$lastDT,origin='1970-01-01')
names(SD)[names(SD) == 'min.1'] <- 'minDV'

# S2.添加机型和故障状态
SD$dClass <- diskInfoValid$dClass[match(SD$sn,diskInfoValid$sn)]
SD$class <- diskInfoValid$class[match(SD$sn,diskInfoValid$sn)]
SD$dClass <- as.character(SD$dClass)
SD$dClass[SD$dClass != 'TS' & SD$dClass != 'C'] <- 'O'
SD$group <- factor(paste(SD$class,SD$dClass,sep='_'))
SD$attr <- factor(SD$attr)


# S3.画CDF
attr <- levels(SD$attr)
ftr <- names(SD)[c(4,5,8:16)]
dfPara <- expand.grid(attr,ftr)
# mapply(CDF_plot,dfPara$Var1,dfPara$Var2)
