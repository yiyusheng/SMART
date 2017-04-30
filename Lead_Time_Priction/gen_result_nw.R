gen_result_all <- function(thre){
  df.result$pred <- as.numeric(df.result$result > thre)
  tmp <- list2df(tapply(df.result$pred,df.result$sn,function(x)as.numeric(any(x == 1))),
                 n = c('result','sn'))
  tmp$label <- 0
  tmp$label[tmp$sn %in% id.pos$sn] <- 1
  base_eval(tmp$label,tmp$result)
}

gen_result_pos <- function(thre){
  df.result.pos$pred <- as.numeric(df.result.pos$result > thre)
  count.r <- list2df(by(df.result.pos,df.result.pos$sn,function(df){
    tmp1 <- subset(df,dist.fail >= nw2 + nw1);tmp2 <- subset(df,dist.fail < nw2 + nw2)
    list(as.numeric(any(tmp1$pred == 1)),as.numeric(any(tmp2$pred == 1)))
  }))
  list(sum(count.r$X1)/nrow(count.r),sum(count.r$X2)/nrow(count.r))
}


# F1.result evaluation
gen_result <- function(df.result,nw1,nw2){
  df.result.pos <- factorX(remove_smart(subset(df.result,sn %in% id.pos$sn & dist.fail > nw1)))
  df.result.neg <- factorX(remove_smart(subset(df.result,sn %in% id.neg$sn)))
  df.result <- factorX(rbind(df.result.pos,df.result.neg))
  threSet <- unique(as.numeric(quantile(df.result$result,seq(0,1,0.001))))
  
  perf.list <- lapply(threSet,function(thre){
    list(pa = gen_result_all(thre),pp = gen_result_pos(thre))
  })
  
  # disk result to compare with type1
  # perf.all <- lapply(threSet,function(thre){
  #   df.result$pred <- as.numeric(df.result$result > thre)
  #   tmp <- list2df(tapply(df.result$pred,df.result$sn,function(x)as.numeric(any(x == 1))),
  #                  n = c('result','sn'))
  #   tmp$label <- 0
  #   tmp$label[tmp$sn %in% id.pos$sn] <- 1
  #   base_eval(tmp$label,tmp$result)
  # })
  
  
  # perf.pos <- list2df(lapply(threSet,function(thre){
  #     df.result.pos$pred <- as.numeric(df.result.pos$result > thre)
  #     count.r <- list2df(by(df.result.pos,df.result.pos$sn,function(df){
  #       tmp1 <- subset(df,dist.fail >= nw2 + nw1);tmp2 <- subset(df,dist.fail < nw2 + nw2)
  #       list(as.numeric(any(tmp1$pred == 1)),as.numeric(any(tmp2$pred == 1)))
  #     }))
  #     list(sum(count.r$X1)/nrow(count.r),
  #          sum(count.r$X2)/nrow(count.r))
  # }))

  #return
  perf.all <- list2df(lapply(perf.list,'[[','pa'),n = c('TP','FN','TN','FP','P','N','FDR','FAR','acc','F1'))
  perf.all$thre <- threSet
  
  perf.pos <- list2df(lapply(perf.list,'[[','pp'),n = c('FAR','FDR'))
  perf.pos$thre <- threSet

  
  
  list(pa = perf.all,pp = perf.pos)
}

plot_result_nw <- function(perf.all,perf.pos){
  perf.all <- perf.all[,c('FAR','FDR','thre')]
  perf.all$class <- 'Disk'
  perf.pos$class <- 'Positive Disk'
  perf.plot <- rbind(perf.all,perf.pos)
  
  ggplot(perf.plot,aes(x = FAR,y = FDR,group = class,color = class)) + 
    geom_line() + 
    geom_abline(slope = 1,intercept = 0,color = 'red',linetype = 2) + 
    guides(color = guide_legend(title = NULL)) +
    theme(axis.title = element_text(size = 26),
          axis.text = element_text(size = 24),
          legend.text = element_text(size = 22),
          legend.position = c(0.95,0),
          legend.justification = c(0.95,0),
          legend.background = element_rect(fill = alpha('grey',0.5)))
}

plot_result_disk_nw <- function(tmp,df = df.result.pos){
  if(is.numeric(tmp)){
    sid <- levels(df$sn)
    tmp <- tmp%%length(sid)
    tmp <- subset(df,sn == sid[idx])
  }else{
    tmp <- subset(df,sn == tmp)
  }
  
  tmp$id <- seq_len(nrow(tmp))
  ggplot(tmp,aes(x = id,y = result,color = factor(weeklabel),group = 1)) + geom_line()
}


# test <- function(){
#   a <- list2df(tapply(df.result.neg$result,df.result.neg$sn,function(x)round(quantile(x),digits = 4)))
#   a$class <- 'neg'
#   b <- list2df(tapply(df.result.pos$result,df.result.pos$sn,function(x)round(quantile(x),digits = 4)))
#   b$class <- 'pos'
#   c <- rbind(a,b)
#   ggplot(c,aes(x = X5,group = class,color = class)) + stat_ecdf()
# }




# # result of positive disks
# perf.disk <- lapply(threSet,function(thre){
#   df.result.pos$pred <- as.numeric(df.result.pos$result > thre)
#   df.result.pos$tag.result <- 0
#   df.result.pos$tag.result[df.result.pos$weeklabel == 1 & df.result.pos$pred == 1] <- 1
#   df.result.pos$tag.result[df.result.pos$weeklabel == 1 & df.result.pos$pred == 0] <- 2
#   df.result.pos$tag.result[df.result.pos$weeklabel == 0 & df.result.pos$pred == 1] <- 3
#   df.result.pos$tag.result[df.result.pos$weeklabel == 0 & df.result.pos$pred == 0] <- 4
#   
#   tmp <- list2df(tapply(df.result.pos$tag.result,df.result.pos$sn,function(x){
#     a <- sum(x == 1);b <- sum(x == 2)
#     c <- sum(x == 3);d <- sum(x == 4)
#     list(pos.partA.FAR = c/(c+d),
#          pos.partB.FDR = a/(a+b),
#          pos.partB.FDR.forOne = as.numeric(a > 0))
#   }),n = c('pos.partA.FAR','pos.partB.FDR','pos.partB.FDR.forOne','sn'))
#   tmp$thre <- thre
#   tmp
# })
# perf.disk <- do.call(rbind,perf.disk)
# perf.disk$pos.partA.FAR[is.na(perf.disk$pos.partA.FAR)] <- 0
# perf.disk$pos.partB.FDR[is.na(perf.disk$pos.partB.FDR)] <- 1
# 
# perf.disk$thre <- factor(perf.disk$thre)
# perf.disk$sn <- factor(perf.disk$sn)
# count.sn <- length(levels(perf.disk$sn))
# perf.disk.mean <- data.frame(thre = levels(perf.disk$thre),
#                              mean.FAR = as.numeric(tapply(perf.disk$pos.partA.FAR,perf.disk$thre,mean)),
#                              mean.FDR = as.numeric(tapply(perf.disk$pos.partB.FDR,perf.disk$thre,mean)),
#                              count.pos = as.numeric(tapply(perf.disk$pos.partB.FDR.forOne,perf.disk$thre,sum)))
# perf.disk.mean$FDR.forone <- perf.disk.mean$count.pos/count.sn
