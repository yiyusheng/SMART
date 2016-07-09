# Statistc Lead Time
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
# load(file.path(dir_data,'NPCpredExtra.Rda'))
load(file.path(dir_data,'NPCpredDiskFailure_weight.Rda'))
load(file.path(dir_data,'resFSN.Rda'))

####################################
# S1.Data prepare
data <- data.frame(matrix(unlist(r[,1:7]),nrow = nrow(r)))
names(data) <- c('tw','gamma','cost','w0','w1','FDR','FAR')
# data <- data[order(data$FDR,decreasing = T),]
ltSet <- r[,9]

# S2.Lead time distribution
idx <- which(data$FDR > 90 & data$FAR < 1)
# idx <- 1:length(ltSet)
ltValue <- sapply(idx, function(i){
  x <- subset(ltSet[[i]], sn %in% resFSN$sn)
  faslePos <- subset(x,pred == 1 & label == 0)
  turePos <- subset(x,pred == 1 & label == 1)
  data.frame(as.numeric(fct2ori(turePos$leadTime)),as.numeric(fct2ori(turePos$posRate)),
             data$tw[i],data$w0[i],data$w1[i])
},simplify = F)
ltValue <- do.call(rbind,ltValue)
names(ltValue) <- c('leadTime','posRate','timeWindow','w0','w1')

# S3.plot
ltPlot <- subset(ltValue,leadTime >= 0)
ltPlot$timeWindow <- factor(ltPlot$timeWindow)
# For weight
ltPlot4 <- subset(ltPlot,timeWindow == 48)
ltPlot4$weight <- paste('w0:',ltPlot4$w0,'-w1:',ltPlot4$w1,sep='')
p <- ggplot(ltPlot4,aes(x = leadTime/24,group = weight,color = weight)) + stat_ecdf(size = 1) +
  xlab('Lead Time (days)') + ylab('CDF (%)') +
  scale_x_continuous(breaks = seq(0,120,10),limits = c(-5,125)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  guides(shape = guide_legend(title='Weight'),color = guide_legend(title = 'Weight')) + 
  theme_bw() +
  theme(panel.background = element_rect(color = 'black'),
        panel.grid.minor = element_line(size = 0.4),
        panel.grid.major = element_line(colour = 'grey', linetype = 'dotted', size = 1),
        #           panel.grid.major.x = element_blank(),
        
        plot.title = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 26),
        axis.text.x = element_text(angle = 40, margin = margin(10)),
        
        legend.title = element_text(size = 20),
        legend.key.width = unit(1.5,units = 'line'),
        legend.key.height = unit(1.5,units = 'line'),
        legend.text = element_text(size = 20),
        legend.position = c(0.95,0.05),
        legend.justification = c(1,0),
        legend.background = element_rect(fill = alpha('grey',0.5))
  )
print(p)
ggsave(file=file.path(dir_data,'figure','npc16',paste('fig2Weight.eps',sep='')), plot=p, width = 8, height = 6, dpi = 100)