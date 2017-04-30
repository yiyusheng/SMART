# compare distribution of different attributes
source('head_data.R')

plot_dist_pn <- function(attr){
  tmp.neg <- data.frame(value = smart.neg[[attr]],class = 'neg')
  tmp.pos <- data.frame(value = smart.pos[[attr]][smart.pos$dist.fail < 7],class = 'pos')
  tmp <- rbind(tmp.neg,tmp.pos)
  p <- ggplot(tmp,aes(x = value,group = class,color = class)) + stat_ecdf() + ggtitle(attr)
  ggsave(file=file.path(dir_data,'figure',paste('compare_pos_neg_',attr,'.jpg',sep='')),
         plot=p, width = 8, height = 6, dpi = 100)
  return(0)
}

sapply(col.smart,plot_dist_pn)

a <- melt(table(smart.neg$Reallocated_Sector_Ct_Raw,smart.neg$Reallocated_Sector_Ct_Value))
names(a) <- c('Raw','value','count')
a <- subset(a,count != 0)
tapply(a$Raw,a$value,summary)
