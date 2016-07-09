# Statistc Lead Time
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
load(file.path(dir_data,'NPCpredExtra.Rda'))
# load(file.path(dir_data,'NPCpredDiskFailure_weight.Rda'))
# load(file.path(dir_data,'NPCdfp04.Rda'))
load(file.path(dir_data,'resFSN.Rda'))

####################################
# S1.Data prepare
data <- data.frame(matrix(unlist(r[,1:5]),nrow = nrow(r)))
names(data) <- c('tw','gamma','cost','FDR','FAR')
# data <- data[order(data$FDR,decreasing = T),]
ltSet <- r[,6]

# S2.Lead time distribution
# idx <- which(data$FDR > 90 & data$FAR < 1)
idx <- 1:length(ltSet)
ltValue <- sapply(idx, function(i){
  x <- subset(ltSet[[i]], sn %in% resFSN$sn)
  faslePos <- subset(x,pred == 1 & label == 0)
  turePos <- subset(x,pred == 1 & label == 1)
  data.frame(as.numeric(fct2ori(turePos$leadTime)),as.numeric(fct2ori(turePos$posRate)),
             data$tw[i],data$gamma[i],data$cost[i])
},simplify = F)
ltValue <- do.call(rbind,ltValue)
names(ltValue) <- c('leadTime','posRate','timeWindow','gamma','cost')

# S3.plot
ltPlot <- subset(ltValue,leadTime >= 0)
ltPlot$timeWindow <- factor(ltPlot$timeWindow)
ltPlot$gamma <- factor(ltPlot$gamma)
ltPlot$cost <- factor(ltPlot$cost)

# For time window
ltPlot1 <- subset(ltPlot,cost == 8 & gamma == 0.125)
levels(ltPlot1$timeWindow) <- paste(levels(ltPlot$timeWindow),'hours',sep = ' ')
p <- ggplot(ltPlot1,aes(x = leadTime/24,group = timeWindow,color = timeWindow)) + stat_ecdf(size = 1) +
  xlab('Lead Time (days)') + ylab('CDF (%)') +
  scale_x_continuous(breaks = seq(0,120,10),limits = c(-5,125)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  guides(shape = guide_legend(title='Time Window'),color = guide_legend(title = 'Time Window')) + 
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
ggsave(file=file.path(dir_data,'figure','npc16',paste('fig2TW.eps',sep='')), plot=p, width = 8, height = 6, dpi = 100)

# For cost
ltPlot2 <- subset(ltPlot,timeWindow = 48 & gamma == 0.125)
p <- ggplot(ltPlot2, aes(x = leadTime/24,group = cost,color = cost)) + stat_ecdf(size = 1) +
  xlab('Lead Time (days)') + ylab('CDF (%)') + 
  scale_x_continuous(breaks = seq(0,120,10),limits = c(-5,125)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  guides(shape = guide_legend(title='Cost'),color = guide_legend(title = 'Cost')) + 
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
ggsave(file=file.path(dir_data,'figure','npc16',paste('fig2Cost.eps',sep='')), plot=p, width = 8, height = 6, dpi = 100)

# For gamma
ltPlot3 <- subset(ltPlot,timeWindow = 48 & cost == 8)
p <- ggplot(ltPlot3, aes(x = leadTime/24,group = gamma,color = gamma)) + stat_ecdf(size = 1) +
  xlab('Lead Time (days)') + ylab('CDF (%)') + 
  scale_x_continuous(breaks = seq(0,120,10),limits = c(-5,125)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  guides(shape = guide_legend(title='Gamma'),color = guide_legend(title = 'Gamma')) + 
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
ggsave(file=file.path(dir_data,'figure','npc16',paste('fig2Gamma.eps',sep='')), plot=p, width = 8, height = 6, dpi = 100)


