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
ltSet <- r[,9]

# S2.Lead time distribution
idx <- which(data$FDR > 90 & data$FAR < 1)
# idx <- 69
ltValue <- sapply(idx, function(i){
  x <- subset(ltSet[[i]], sn %in% resFSN$sn)
  faslePos <- subset(x,pred == 1 & label == 0)
  turePos <- subset(x,pred == 1 & label == 1)
  data.frame(as.numeric(fct2ori(turePos$leadTime)),as.numeric(fct2ori(turePos$posRate)),
             data$tw[i],data$w0[i],data$w1[i])
},simplify = F)
ltValue <- do.call(rbind,ltValue)
names(ltValue) <- c('leadTime','posRate','timeWindow','w0','w1')
ltValue <- subset(ltValue,leadTime > 0)
ltValue$allPos <- 'Partial Positive'
ltValue$allPos[ltValue$posRate == 1] <- 'Entire Positive'
ltValueEntirePos <- subset(ltValue,allPos == 'Partial Positive')

# S3.plot 
#48/0.1/10/10/5 94.99%/0.74%
ltHist <- subset(ltValue,leadTime >= 0)
ltHist$ltDay <- round(ltHist$leadTime/24)
ltPlot <- melt(table(ltHist$ltDay,ltHist$allPos))
names(ltPlot) <- c('ltDay','allPos','count')
ltPlot <- ltPlot[order(ltPlot$allPos,decreasing = T),]
# ltPlot$allPos <- factor(ltPlot$allPos,levels = sort(levels(ltPlot$allPos),decreasing = T))

ltPlot$rate <- ltPlot$count/sum(ltPlot$count)
p <- ggplot(ltPlot,aes(x = ltDay,y = rate*100,fill = allPos,order = allPos)) + 
  geom_bar(stat = 'identity',position = 'stack') + 
  xlab('Lead Time (days)') + ylab('Probability Density Function (%)') +
  scale_x_continuous(breaks = seq(0,120,10)) +
  # scale_y_continuous(breaks = seq(0,1,0.1)) +
  guides(fill = guide_legend(title=NULL)) +
  theme_bw() +
  theme(panel.background = element_rect(color = 'black'),
        panel.grid.minor = element_line(size = 0.4),
        panel.grid.major = element_line(colour = 'grey', linetype = 'dotted', size = 1),
        #           panel.grid.major.x = element_blank(),

        plot.title = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 26),

        legend.title = element_text(size = 20),
        legend.key.width = unit(1.5,units = 'line'),
        legend.key.height = unit(1.5,units = 'line'),
        legend.text = element_text(size = 20),
        legend.position = c(0.95,0.95),
        legend.justification = c(1,1),
        legend.background = element_rect(fill = alpha('grey',0.5))
  )
print(p)
ggsave(file=file.path(dir_data,'npc16',paste('fig2ldDist.eps',sep='')), plot=p, width = 8, height = 6, dpi = 100)