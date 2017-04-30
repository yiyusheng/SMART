# Evaluate result of test generated from gen_result

plot_ROC <- function(perf.all,group = NULL,func = NULL){
  fn <- paste('Group',group,'_ROC_',func,sep='')
  p <- ggplot(perf.all,aes(x = FAR,y = FDR)) + geom_line() + geom_point() + 
    xlab('FAR(%)') + ylab('FDR(%)') + ggtitle(fn) + xlim(c(0,0.1)) + ylim(c(0.5,1))
  if(!is.null(group)){
    ggsave(file=file.path(dir_data,'figure',paste(fn,'.jpg',sep='')), 
           plot=p, width = 8, height = 6, dpi = 100)
    
  }
  print(p)
  p
}

plot_ROC_all <- function(perf.all,title = NULL){
  p <- ggplot(perf.all,aes(x = FAR,y = FDR,group = paste(tag1,tag2),color = tag1,linetype = tag2)) + 
    geom_line() + xlab('FAR(%)') + ylab('FDR(%)') + 
    ggtitle(title) + xlim(c(0,0.1)) + ylim(c(0.5,1))
  if(!is.null(title)){
    ggsave(file=file.path(dir_data,'figure',paste(title,'.jpg',sep='')), 
           plot=p, width = 8, height = 6, dpi = 100)
  }
  print(p)
  p
}

plot_dist_leadTime <- function(perf.disk,group = NULL,func = NULL){
  tmp <- subset(perf.disk,overThre == 1 & bilabel == 1)
  fn <- paste('Group',group,'_ltDist_',func,sep='')
  
  p <- ggplot(tmp,aes(x = leadTime,fill = factor(exist.chaos))) + geom_histogram(binwidth = 5) +
    xlab('Lead Time(days)') + ylab('Count') + ggtitle(fn) + 
    guides(fill = guide_legend(title='exist.chaos')) +
    theme(legend.position = c(0.95,0.95),
          legend.justification = c(1,1),
          legend.background = element_rect(fill = alpha('grey',0.5)))
  if(!is.null(group)){
    ggsave(file=file.path(dir_data,'figure',paste(fn,'.jpg',sep='')), 
           plot=p, width = 8, height = 6, dpi = 100)
  }
  print(p)
  p
}

plot_dist_leadTime_all <- function(perf.disk,title = NULL){
  tmp <- subset(perf.disk,overThre == 1 & bilabel == 1)
  tmp$tag3 <- factor(paste(tmp$tag1,tmp$tag2,sep='_'))
  tmp <- tmp[,c('sn','leadTime','tag1','tag2','tag3','exist.chaos','rate.chaos.pos')]
  
  p <- ggplot(tmp,aes(x = leadTime,color = tag1,linetype = tag2)) + stat_ecdf() +
    xlab('Lead Time(days)') + ylab('Percentage(%)') + ggtitle(title) + 
    guides(fill = guide_legend(title=NULL)) +
    theme(legend.position = c(0.95,0.95),
          legend.justification = c(1,1),
          legend.background = element_rect(fill = alpha('grey',0.5)))
  if(!is.null(title)){
    ggsave(file=file.path(dir_data,'figure',paste(title,'.jpg',sep='')), 
           plot=p, width = 8, height = 6, dpi = 100)
  }
  print(p)
  p
}

eval_result <- function(perf,group = NULL,func = NULL){
  list[perf.all,perf.disk,perf.smart] <- perf
  p1 <- plot_ROC(perf.all,group,func)
  p2 <- plot_dist_leadTime(perf.disk,group,func)
  multiplot(p1,p2)
}
