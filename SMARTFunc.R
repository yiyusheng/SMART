#Function set

# F1. 单字段分区域故障率
sinAttrFR <- function(data,dataF,singlePartCount,title,units,disks){
  div <- as.numeric(quantile(data,seq(0,1,1/singlePartCount)))
  cut <- tableX(cut(data,div))
  cutF <- tableX(cut(dataF,div))
  cutM <- merge(cut,cutF,by = 'item')
  names(cutM) <- c('item','count','rateA','failure','rateB')
  cutM$rate <- cutM$failure/cutM$count
  cutM$rate <- cutM$rate/disks
  cutM$left <- as.numeric(gsub("\\(|,.*","",cutM$item))
  cutM <- cutM[order(cutM$left),]
  cutM$item <- factor(cutM$item,levels = as.character(cutM$item))
  cutM$left <- factor(cutM$left,levels = as.character(cutM$left))
  p1 <- ggplot(cutM,aes(x = left,y = rate*100))+geom_bar(stat = 'identity') + 
    xlab(paste('Partition (',units,')',sep='')) + ylab('Failure Rate (%)') + 
    ggtitle(title) +
    #     scale_color_discrete(name = 'Class') +
    #     guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          #               axis.text.x = element_text(angle = 0,hjust = 1),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          plot.title = element_text(size = 26, face = 'bold'),
          legend.position = c(0,1),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5)))
  print(p1)
  ggsave(file=file.path(dir_data,'sta_attrid',paste(title,'.png',sep='')), plot=p1, width = 16, height = 12, dpi = 100)
  return(cutM)
}

# F2. 双字段分区域故障率
dblAttrFR <- function(mean_io,doublePartCount,title,xl,yl){
  
  #F2.1 数据声明
  partA <- data.frame(matrix(0,doublePartCount,doublePartCount))
  partF <- data.frame(matrix(0,doublePartCount,doublePartCount))
  partR <- data.frame(matrix(0,doublePartCount,doublePartCount))
  
  #F2.2 划分
  cn <- names(mean_io)
  names(mean_io) <- c('svrid','colA','colB','class')
  divA <- as.numeric(quantile(mean_io$colA,seq(0,1,1/doublePartCount)))
  divA <- divA + mean(divA)*1e-20*seq(0,(length(divA)-1),1)
  divB <- as.numeric(quantile(mean_io$colB,seq(0,1,1/doublePartCount)))
  divB <- divB + mean(divB)*1e-20*seq(0,(length(divB)-1),1)
  
  #F2.3 求数量，比例
  for (i in 1:(length(divA)-1)){
    for (j in 1:(length(divB)-1)){
      tmp <- mean_io[mean_io$colA > divA[i] & mean_io$colA <= divA[i+1] &
                       mean_io$colB > divB[j] & mean_io$colB <= divB[j+1],]
      partA[i,j] <- nrow(tmp)
      partF[i,j] <- nrow(subset(tmp,class == 'Failure'))
    }
  }
  partR <- partF/partA
  
  #F2.4 预处理及融化
  #过滤数据不足的区间
  partR[is.na(partR) | partA <= nrow(mean_io_C)/doublePartCount/doublePartCount/4] <- -1
  partR <- round(partR*1e5)/1e5
  names(partR) <- divB[1:doublePartCount]
  row.names(partR) <- divA[1:doublePartCount]
  meltR <- melt(as.matrix(partR))
  names(meltR) <- c('colA','colB','rate')
  meltR$colA <- factor(round(meltR$colA*1e5)/1e5)
  meltR$colB <- factor(round(meltR$colB*1e5)/1e5)
  div_tag <- as.numeric(quantile(meltR$rate[meltR$rate >= 0],seq(0,1,1/3),na.rm = T))
  div_tag <- div_tag + mean(div_tag)*1e-10*seq(0,length(div_tag)-1,1)
  meltR$tag <- cut(meltR$rate,div_tag)
  
  p1 <- ggplot(subset(meltR, rate >= 0),
               aes(x = colA, y = colB, shape = factor(tag), colour = factor(tag))) +
    geom_point(size = 15) + scale_size_continuous(range = c(3,10)) + 
    ggtitle(title) + xlab(xl) + ylab(yl) + 
    scale_color_discrete(name = 'Failure Rate') + scale_shape_discrete(name = 'Failure Rate') +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          axis.text.x = element_text(angle = 20,hjust = 1),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          plot.title = element_text(size = 26, face = 'bold'),
          #         legend.position = c(0,1),
          #         legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5)))
  print(p1)
  ggsave(file=file.path(dir_data,'sta_attrid',paste(title,'.png',sep='')), plot=p1, width = 16, height = 12, dpi = 100)
  return(list(partA,partF,partR))
}

# F3. 单字段累积故障率
sinAttrCumFR <- function(data,attr,title){
  tmp_io <- data
  tmp_io <- tmp_io[order(tmp_io[[attr]]),]
  tmp_io$culfail <- unlist(sapply(1:nrow(tmp_io),function(x){
    sum(tmp_io$class[1:x] == 'Failure')
  }))
  tmp_io$idx <- seq(1:nrow(tmp_io))
  write.csv(tmp_io,file = file.path(dir_data,title))
}

# F4. MCF计算，未完成，放弃。
mcf <- function(data,attr){
  data <- data[order(data[[attr]]),]
  mcf <- data.frame(x = sort(unique(data[[attr]])))
  #   mcf$total
  #   mcf$fail
  mcf$pointFR <- mcf$fail/mcf$total
  #   mcf$cumu
}

# F5. 单字段人工分区域故障率
# 相比于F1,这里不再是按机器数来分区间，而是采用人工划分
# 例如对时间采取[0,5,1],[5,20,5],[20,100,40]来进行划分
sinAttrManualFR <- function(data,dataF,div,title,units,disks){
  cut <- tableX(cut(data,div))
  cutF <- tableX(cut(dataF,div))
  cutM <- merge(cut,cutF,by = 'item')
  names(cutM) <- c('item','count','rateA','failure','rateB')
  cutM$rate <- cutM$failure/cutM$count
  cutM$rate <- cutM$rate/disks
  cutM$left <- as.numeric(gsub("\\(|,.*","",cutM$item))
  cutM <- cutM[order(cutM$left),]
  cutM$item <- factor(cutM$item,levels = as.character(cutM$item))
  cutM$left <- factor(cutM$left,levels = as.character(cutM$left))
  p1 <- ggplot(cutM,aes(x = left,y = rate*100))+geom_bar(stat = 'identity') + 
    xlab(paste('Partition (',units,')',sep='')) + ylab('Failure Rate (%)') + 
    ggtitle(title) +
    #     scale_color_discrete(name = 'Class') +
    #     guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          #               axis.text.x = element_text(angle = 0,hjust = 1),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          plot.title = element_text(size = 26, face = 'bold'),
          legend.position = c(0,1),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5)))
  print(p1)
  ggsave(file=file.path(dir_data,'sta_attrid',paste(title,'.png',sep='')), plot=p1, width = 16, height = 12, dpi = 100)
  return(cutM)
}

# F6. 为多行内容进行table
colTableX <- function(data,col,decreasing = T,rm.na = F){
  colMerge <- data[[col[1]]]
  for (i in seq(2,length(col))){
    colMerge <- paste(colMerge,data[[col[i]]],sep='_')
  }
  return(tableX(colMerge,decreasing = decreasing))
}

# F7. 拆分合并之后的col，并输出data.frame
splitToDF <- function(data,split = '_',header = ''){
  r <- data.frame(matrix(unlist(strsplit(as.character(data),split)),byrow = T,nrow = length(data)))
  if (header[1] != ''){
    names(r) <- header
  }
  return(r)
}

# F8. 增长率计算
incCalc <- function(data){
  len <- length(data)
  inc <- (data[2:len] - data[1:(len-1)])/data[1:(len-1)]
  inc <- c(0,inc)
  return(inc)
}

# F9. 多图函数
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# F10.返回多个值时用list[]分别接收
# from https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R
list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
  args <- as.list(match.call())
  args <- args[-c(1:2,length(args))]
  length(value) <- length(args)
  for(i in seq(along=args)) {
    a <- args[[i]]
    if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
  }
  x
}

## F11.为了进行log2处理，把小于1大于0的值设为1,大于-1小于0的值设为-1
# 后一个函数将(-Inf,Inf)的值用log2映射到(-Inf,Inf),其中[-1,1]映射为0.
mdf4log2 <- function(x){
  x[x < 1 & x >= 0] <- 1
  x[x > -1 & x < 0] <- -1
  x
}
log4neg <- function(x){
  x <- mdf4log2(x)
  x[x > 0] <- log2(x[x > 0])
  x[x < 0] <- log2(abs(x[x < 0])) * -1
  x
}

# F12.将df的item以括号后第一个数字的顺序排序,用于方便的画图
item_order <- function(df,attr = 'item'){
  od <- as.numeric(gsub('^\\[|^\\(|,.*$','',df[[attr]]))
  df[attr] <- factor(df[[attr]],levels = df[[attr]][order(od)])
  df <- df[order(od),] 
  row.names(df) <- NULL
  df
}

# F13.AFR计算
AFR <- function(f,cm,lastYears,diskCount,dev = ''){
  if(dev != ''){
    f <- subset(f,grepl(dev,dClass))
    cm <- subset(cm,grepl(dev,dClass))
  }
  
  divAll <- c(0,(seq(1,(lastYears-1)) - 1/12),lastYears)
  divF <- seq(0,lastYears)
  cutF <- tableX(cut(f$failShiptime,divF))
  cutLeft <- tableX(cut(cm$shiptimeToLeft,divAll))
  cutF$idx <- as.numeric(gsub("\\(|,.*","",cutF$item))
  cutLeft$idx <- as.numeric(gsub("\\(|,.*","",cutLeft$item))
  
  cutLeft <- cutLeft[order(cutLeft$idx),]
  cutLeft$idx <- seq(0,(lastYears-1))
  cutMerge <- merge(cutF,cutLeft,by = 'idx')
  cutMerge$rate.x <- NULL
  cutMerge$rate.y <- NULL
  cutMerge$AFR <- cutMerge$count.x/cutMerge$count.y/diskCount
  return(cutMerge)
}

# F14.AFR画图
AFR_plot <- function(cutMerge,title,yl){ 
  if (yl == -1){
    p1 <- ggplot(cutMerge,aes(x = item,y = AFR*100*6)) + geom_bar(stat = 'identity') +
      xlab('Ship Time (years)') + ylab('Annual Failure Rate (%)') + 
      ggtitle(title) + 
      theme(axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 20),
            plot.title = element_text(size = 26, face = 'bold'),
            legend.position = c(0,1),
            legend.justification = c(0,1),
            legend.background = element_rect(fill = alpha('grey',0.5)))
  } else if (yl == -2){
    p1 <- ggplot(cutMerge,aes(x = item,y = AFR*100*6,fill = class)) + 
      geom_bar(stat = 'identity',position = 'dodge') +
      xlab('Ship Time (years)') + ylab('Annual Failure Rate (%)') + 
      ggtitle(title) + guides(fill = guide_legend(title=NULL)) + 
      #       scale_alpha(guide = F) +
      theme(axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            legend.text = element_text(size = 30),
            legend.title = element_text(size = 20),
            plot.title = element_text(size = 26, face = 'bold'),
            legend.position = c(0,1),
            legend.justification = c(0,1),
            legend.background = element_rect(fill = alpha('grey',0.5)))
  } else {
    p1 <- ggplot(cutMerge,aes(x = item,y = AFR*100*6)) + geom_bar(stat = 'identity') +
      xlab('Ship Time (years)') + ylab('Annual Failure Rate (%)') + 
      ggtitle(title) + 
      ylim(c(0,yl)) +
      theme(axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 20),
            plot.title = element_text(size = 26, face = 'bold'),
            legend.position = c(0,1),
            legend.justification = c(0,1),
            legend.background = element_rect(fill = alpha('grey',0.5)))
  }
  print(p1)
  ggsave(file=file.path(dir_data,'ship_time',paste(title,'.png',sep='')), plot=p1, width = 16, height = 12, dpi = 100)
}

# F15. AFR计算A
AFR_value <- function(p3.f,p3.cmdb,p3.io,attr,levelCount,lastYears,diskCount){
  # 求区间
  div902 <- quantile(p3.io$mean_902/diskCount,seq(0,1,1/levelCount))
  div903 <- quantile(p3.io$mean_903/diskCount,seq(0,1,1/levelCount))
  div999 <- quantile(p3.io$mean_999,seq(0,1,1/levelCount))
  divAll <- c(0,(seq(1,(lastYears-1)) - 1/12),lastYears)
  divF <- seq(0,lastYears)
  # 给每台机器添加区间
  p3.io$lv902 <- cut(p3.io$mean_902/diskCount,div902)
  p3.io$lv903 <- cut(p3.io$mean_903/diskCount,div903)
  p3.io$lv999 <- cut(p3.io$mean_999,div999)
  
  mergecol <- c('svrid','lv902','lv903','lv999')
  p3.cmdb <- merge(p3.cmdb,p3.io[,mergecol],by.x = 'svr_asset_id',by.y = 'svrid')
  p3.f <- subset(p3.f,svr_id %in% cmdbio$svr_asset_id)
  p3.f <- merge(p3.f,p3.io[,mergecol],by.x = 'svr_id',by.y = 'svrid')
  
  p3.cmdb$lvUsetime <- as.character(cut(p3.cmdb$shiptimeToLeft,divAll))
  p3.f$lvUsetime <- as.character(cut(p3.f$failShiptime,divF))
  p3.cmdb$lvUsetime[p3.cmdb$lvUsetime == '(0,0.917]'] <- '(0,1]'
  p3.cmdb$lvUsetime[p3.cmdb$lvUsetime == '(0.917,1.92]'] <- '(1,2]'
  p3.cmdb$lvUsetime[p3.cmdb$lvUsetime == '(1.92,2.92]'] <- '(2,3]'
  p3.cmdb$lvUsetime[p3.cmdb$lvUsetime == '(2.92,3.92]'] <- '(3,4]'
  p3.cmdb$lvUsetime[p3.cmdb$lvUsetime == '(3.92,4.92]'] <- '(4,5]'
  p3.cmdb$lvUsetime[p3.cmdb$lvUsetime == '(4.92,5.92]'] <- '(5,6]'
  p3.cmdb$lvUsetime[p3.cmdb$lvUsetime == '(5.92,7]'] <- '(6,7]'
  p3.cmdb$lvUsetime[p3.cmdb$lvUsetime == '(4.92,6]'] <- '(5,6]'
  p3.cmdb$lvUsetime[p3.cmdb$lvUsetime == '(3.92,5]'] <- '(4,5]'
  
  col.table <- c(paste('lv',attr,sep=''),'lvUsetime')
  cutP3f <- colTableX(p3.f,col.table)
  cutP3cmdb <- colTableX(p3.cmdb,col.table)
  cutMerge <- merge(cutP3f,cutP3cmdb,by = 'item',all = T)
  cutMerge <- cbind(cutMerge,splitToDF(cutMerge$item,header = c('value','shipTime')))
  cutMerge <- subset(cutMerge,shipTime != 'NA' & value != 'NA',c('value','shipTime','count.x','count.y'))
  cutMerge <- factorX(cutMerge)
  cutMerge$value <- factor(cutMerge$value,
                           levels = levels(cutMerge$value)[
                             order(as.numeric(gsub("\\(|,.*","",levels(cutMerge$value))))])
  
  cutMerge$AFR <- cutMerge$count.x/cutMerge$count.y/diskCount
  #   cutMerge <- cutMerge[order(cutMerge$shipTime),]
  return(cutMerge)
}



# F16.AFR计算B
#对任何一个字段，不同时间段的故障率
AFR_attr <- function(f,cmdb,attr,lastYears,diskCount,dev = '',defValue = ' 0'){
  # 求区间
  f <- factorX(f)
  cmdb <- factorX(cmdb)
  if (dev != ''){
    f <- subset(f,grepl(dev,dClass))
    cmdb <- subset(cmdb,grepl(dev,dClass))
  }
  divAll <- c(0,(seq(1,(lastYears-1)) - 1/12),lastYears)
  divF <- seq(0,lastYears)
  cmdb$lvUsetime <- as.character(cut(cmdb$shiptimeToLeft,divAll))
  f$lvUsetime <- as.character(cut(f$failShiptime,divF))
  cmdb$lvUsetime[cmdb$lvUsetime == '(0,0.917]'] <- '(0,1]'
  cmdb$lvUsetime[cmdb$lvUsetime == '(0.917,1.92]'] <- '(1,2]'
  cmdb$lvUsetime[cmdb$lvUsetime == '(1.92,2.92]'] <- '(2,3]'
  cmdb$lvUsetime[cmdb$lvUsetime == '(2.92,3.92]'] <- '(3,4]'
  cmdb$lvUsetime[cmdb$lvUsetime == '(3.92,4.92]'] <- '(4,5]'
  cmdb$lvUsetime[cmdb$lvUsetime == '(4.92,5.92]'] <- '(5,6]'
  cmdb$lvUsetime[cmdb$lvUsetime == '(5.92,7]'] <- '(6,7]'
  cmdb$lvUsetime[cmdb$lvUsetime == '(4.92,6]'] <- '(5,6]'
  cmdb$lvUsetime[cmdb$lvUsetime == '(3.92,5]'] <- '(4,5]'
  
  col.table <- c(attr,'lvUsetime')
  cutP3f <- colTableX(f,col.table)
  cutP3cmdb <- colTableX(cmdb,col.table)
  cutMerge <- merge(cutP3f,cutP3cmdb,by = 'item',all = T)
  cutMerge <- cbind(cutMerge,splitToDF(cutMerge$item,header = c('value','shipTime')))
  cutMerge <- subset(cutMerge,shipTime != 'NA' & value != 'NA',c('value','shipTime','count.x','count.y'))
  cutMerge <- factorX(cutMerge)
  cutMerge$value <- factor(cutMerge$value,
                           levels = levels(cutMerge$value)[
                             order(as.numeric(gsub("\\(|,.*","",levels(cutMerge$value))))])
  cutMerge <- subset(cutMerge,value != '')
  levels(cutMerge$value)[levels(cutMerge$value) == '-1'] <- defValue
  cutMerge$AFR <- cutMerge$count.x/cutMerge$count.y/diskCount
  cutMerge <- cutMerge[order(cutMerge$shipTim,cutMerge$value),]
  return(cutMerge)
}

#分字段处理，不处理时间
AFR_attr_notime <- function(f,io,attr1,attr2,diskCount,dev = ""){
  if(dev != ""){
    f <- subset(f,grepl(dev,dClass))
    io <- subset(io,grepl(dev,dClass))
  }
  eval(parse(text = sprintf('tio <- tableX(io$%s)',attr2)))
  eval(parse(text = sprintf('tf <- tableX(f$%s)',attr1)))
  tiof <- merge(tio,tf,by = 'item',all = T)
  names(tiof) <- c('item','count_io','rate_io','count_f','rate_f')
  tiof$AFR <- tiof$count_f/tiof$count_io/diskCount*100
  if(dev == 'C'){
    tiof$class <- 'Non-Storage Servers'
  }else if(dev == 'TS'){
    tiof$class <- 'Storage Servers'
  }else if(dev == 'TS1T'){
    tiof$class <- 'Storage Servers[1T]'
  }else if(dev == 'TS2T'){
    tiof$class <- 'Storage Servers[2T]'
  }else{
    tiof$class <- attr2
  }
  tiof <- tiof[,c('item','class','AFR','count_f','count_io','rate_f','rate_io')]
  
  item_num <- as.numeric(fct2ori(tiof$item))
  if(all(!is.na(item_num))){
    tiof$item <- item_num
    tiof <- tiof[order(tiof$item),]
    row.names(tiof) <- NULL
  }
  tiof
}

# F17. AFR画图
AFR_value_plot <- function(cutMerge,title,yl,
                           subdir = '',valueFilter = '',cylim = -1){
  plotCol <- c('value','shipTime','AFR')
  if (valueFilter[1] != ''){
    cutMerge <- subset(cutMerge,!(value %in% valueFilter))
  }
  if (cylim != -1){
    cutMerge <- subset(cutMerge,count.y >= cylim)
  }
  cutMerge <- factorX(cutMerge)
  naFill <- cbind(expand.grid(value = levels(cutMerge$value),shipTime = cutMerge$shipTime),AFR = NA)
  cutMerge <- rbind(subset(cutMerge,,plotCol),naFill)
  if (yl == -1){
    p1 <- ggplot(cutMerge,aes(x = shipTime,y = AFR*100*6,fill = factor(value))) + 
      geom_bar(stat = 'identity',position = 'dodge') + 
      xlab('Ship Time (years)') + ylab('Annual Failure Rate (%)') + 
      ggtitle(title) + guides(fill = guide_legend(title=NULL)) + 
      theme(plot.title = element_text(size = 26, face = 'bold'),
            axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            legend.text = element_text(size = 25),
            legend.title = element_text(size = 20),
            legend.position = c(0,1),
            legend.justification = c(0,1),
            legend.background = element_rect(fill = alpha('grey',0.5)))
  } else {
    p1 <- ggplot(cutMerge,aes(x = shipTime,y = AFR*100*6,fill = factor(value))) + 
      geom_bar(stat = 'identity',position = 'dodge') +
      xlab('Ship Time (years)') + ylab('Annual Failure Rate (%)') + 
      ggtitle(title) + guides(fill = guide_legend(title=NULL)) + 
      ylim(c(0,yl)) +
      theme(plot.title = element_text(size = 26, face = 'bold'),
            axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            legend.text = element_text(size = 25),
            legend.title = element_text(size = 20),
            legend.position = c(0,1),
            legend.justification = c(0,1),
            legend.background = element_rect(fill = alpha('grey',0.5)))
  }
  print(p1)
  ggsave(file=file.path(dir_data,'ship_time',subdir,paste(title,'.png',sep='')), plot=p1, width = 16, height = 12, dpi = 100)
}

# F18.画CDF图
CDF_plot <- function(data,tt,xl){
  data$shTime <- paste(data$shTime + 1,'years',sep=' ')
  data$shTime[data$shTime == '1 years'] <- '1 year'
  
  p <- ggplot(data,aes(acct_9023O,color = factor(shTime),linetype = factor(shTime))) + 
    stat_ecdf(size = 1) + xlab('Amount of I/O Workload (Terabytes)') + ylab('') + 
    coord_cartesian(xlim = xl) +
    scale_y_continuous(breaks = seq(0,1,0.1)) +
    scale_x_continuous(breaks = seq(xl[1],xl[2],1),labels = round(2^(seq(xl[1],xl[2],1)-30),2)) +
    guides(color = guide_legend(title = NULL), linetype = guide_legend((title = NULL))) +
    theme_bw() +
    theme(panel.background = element_rect(color = 'black'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = 'grey', linetype = 'dotted'),
          #           panel.grid.major.x = element_blank(),
          
          plot.title = element_text(size = 26,vjust = 1),
          axis.line = element_line(color = 'black'),
          axis.text = element_text(size = 22),
          axis.text.x = element_text(angle = 40,margin = margin(10)),
          axis.title = element_text(size = 24),
          
          legend.key.width = unit(4,units = 'line'),
          legend.key.height = unit(1.5,units = 'line'),
          legend.text = element_text(size = 26),
          legend.position = c(1,0),legend.justification = c(1,0),
          legend.background = element_rect(fill = alpha('grey',0.5))
    )
  print(p)
  ggsave(file=file.path(dir_data,'sc16',paste(tt,'.eps',sep='')), plot=p, width = 8, height = 6, dpi = 300)
} 