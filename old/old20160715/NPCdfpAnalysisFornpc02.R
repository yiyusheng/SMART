# Disk Failure prediction result ayalysis
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')

#@@@ LOAD DATA @@@#
# npc2 is used in plot
data <- read.table(file.path(dir_data,'npc2'),skip = 150,nrows = 300-150,sep=' ',quote = '')
# data <- read.table(file.path(dir_data,'NPCdfp02'),skip = 150,nrows = 240-150)
# data <- read.table(file.path(dir_data,'npc8'))
####################################
# S1.Clean
data$V1 <- NULL
data$V7 <- NULL
# data$V10 <- NULL
data <- data.frame(sapply(1:ncol(data),function(i){
  as.numeric(gsub('.*:','',data[,i]))
}))
# names(data) <- c('timeWindow','gamma','cost','w0','w1','countTr','countNeg','FDR','FAR')
names(data) <- c('timeWindow','gamma','cost','FDR','FAR')
data <- data[order(data$FAR),]

# S2.Plot
data <- data[order(data$timeWindow,data$cost,data$gamma),]
uniTw <- unique(data$timeWindow)
dataPlot <- subset(data,timeWindow %in% c(12,24,48,72,144,240))
dataPlot$timeWindow <- factor(dataPlot$timeWindow)
levels(dataPlot$timeWindow) <- paste(levels(dataPlot$timeWindow),'hours',sep = ' ')
p <- ggplot(dataPlot,aes(x = FAR,y = FDR,color = timeWindow,shape = timeWindow)) + 
  geom_point(size = 3) + xlab('False Alarm Rate (%)') + ylab('Failure Detection Rate (%)') + 
  scale_x_continuous(breaks = seq(0,2,0.2)) +
  scale_y_continuous(breaks = seq(50,100,10)) +
  guides(shape = guide_legend(title='Time Window'),color = guide_legend(title = 'Time Window')) + 
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
        legend.key.width = unit(1.5,units = 'line'),
        legend.key.height = unit(1.5,units = 'line'),
        legend.text = element_text(size = 20),
        legend.position = c(0.95,0.05),
        legend.justification = c(1,0),
        legend.background = element_rect(fill = alpha('grey',0.5))
  )
print(p)
# ggsave(file=file.path(dir_data,'npc16',paste('fig1.eps',sep='')), plot=p, width = 8, height = 6, dpi = 100)
# 
# 
# # S3.compare tw, gamma, cost
# compPara <- function(data,attr){
#   sta <- by(data,data[attr],function(x){
#     a <- 2*(100-x$FAR)*x$FDR
#     b <- (100-x$FAR)+x$FDR
#     mean(a/b)
#   })
# }
# 
# staTW <- compPara(data,'timeWindow')
# staga <- compPara(data,'gamma')
# staco <- compPara(data,'cost')
# stawr <- compPara(data,'weightRate')
# stact <- compPara(data,'countTr')
# 
