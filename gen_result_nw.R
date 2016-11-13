# Generate result for test



# F1.result evaluation
gen_result <- function(df.result,nw1,nw2){
  df.result <- factorX(remove_smart(subset(df.result,dist.fail > nw1)))
  df.result <- factorX(subset(df.result,dist.fail < (nw1 + nw2*2) | sn %in% id.neg$sn))
  
  threSet <- as.numeric(quantile(df.result$result,seq(0,1,0.01)))
  split.df <- split(df.result,df.result$sn)
  r <- lapply(threSet,function(x)gen_result_disk(split.df,x))
  
  perf.all <- data.frame(t(sapply(r,'[[','result')))
  names(perf.all) <- c('neg.acc','pos.acc','pos.FDR','pos.FAR','thre')
  perf.disk <- do.call(rbind,lapply(r,'[[','df'))
  p <- plot_result_nw(perf.all)
  
  list(pa = perf.all,pd = perf.disk,p = p)
}

# F2.extract a roc curve and other information related to prediction performance from roc object
smp_roc <- function(df){
  roc <- roc(df$weeklabel,df$result,algorithm = 2)
  
  tbl.response <- melt(table(roc$response))
  num.pos <- tbl.response$value[tbl.response$Var1 == '1']
  num.neg <- tbl.response$value[tbl.response$Var1 == '0']
  
  roc.new <- data.frame(FDR = round(roc$sensitivities,digits = 3),
                        FAR = round(1 - roc$specificities,digits = 3),
                        thre = roc$thresholds,
                        TP = num.pos*roc$sensitivities,
                        FN = num.pos*(1 - roc$sensitivities),
                        TN = num.neg*roc$specificities,
                        FP = num.neg*(1 - roc$specificities))
  roc.new$F <- (roc.new$FN*num.neg + roc.new$FP*num.pos)/(num.neg + num.pos)
  roc.new <- roc.new[!duplicated(roc.new$FDR),]
  roc.new
}


gen_result_disk <- function(split.df,thre){
  # evaluate for each disk
  r <- lapply(split.df,function(x){
    x$pred <- as.numeric(x$result > thre)
    x.base <- base_eval(x$weeklabel,x$pred)
  })
  perf.disk <- list2df(r,n = c('TP','FN','TN','FP','P','N','FDR','FAR','acc','sn'))
  perf.disk$class <- 1
  perf.disk$class[perf.disk$sn %in% id.neg$sn] <- 0
  perf.disk$thre <- thre
  
  # generate result for each disk
  perf.disk.pos <- factorX(subset(perf.disk,class == 1))
  perf.disk.neg <- factorX(subset(perf.disk,class == 0))
  perf.disk.neg$FAR[is.na(perf.disk.neg$FAR)] <- 0
  perf.disk.neg$FDR[is.na(perf.disk.neg$FDR)] <- 1
  perf.disk.pos$FAR[is.na(perf.disk.pos$FAR)] <- 0
  perf.disk.pos$FDR[is.na(perf.disk.pos$FDR)] <- 1
  perf.disk.pos$acc <- perf.disk.pos$FDR + perf.disk.pos$FAR
  
  # generate result for all
  neg.acc <- sum(perf.disk.neg$acc == 1)/nrow(perf.disk.neg)
  pos.acc <- sum(perf.disk.pos$acc != 0,na.rm = T)/nrow(perf.disk.pos)
  pos.FDR <- sum(perf.disk.pos$FDR != 0,na.rm = T)/nrow(perf.disk.pos)
  pos.FAR <- sum(perf.disk.pos$FAR != 0,na.rm = T)/nrow(perf.disk.pos)
  
  list(result = round(c(neg.acc,pos.acc,pos.FDR,pos.FAR,thre),digits = 4),
       df = perf.disk)
}

plot_result_nw <- function(perf.all){
  melt.all <- melt(perf.all,id.vars = 'thre')
  names(melt.all) <- c('thre','metrics','value')
  p <- ggplot(melt.all,aes(x = thre,y = value,
                           group = metrics,color = metrics,linetype = metrics)) + geom_line()
  print(p)
  p
}


# F3.Analize result
# sta_result <- function(df){
#   df <- df[df$dist.fail > 0,-which(names(df) %in% col.smart)]
#   df.pos <- factorX(subset(df,sn %in% id.pos$sn))
#   df.neg <- factorX(subset(df,sn %in% id.neg$sn))
#   df.roc <- roc(df$weeklabel,df$result,algorithm = 2)
#   df.roc.simple <- smp_roc(df.roc)
#   
#   min.thre.pos <- list2df(by(df.pos,df.pos$sn,function(x){min(x$result[x$weeklabel == 1])}),n = c('thre','sn'))
#   min.thre.neg <- list2df(by(df.neg,df.neg$sn,function(x){min(x$result[x$weeklabel == 1])}),n = c('thre','sn'))
#   
#   min.thre.pos$modelNum <- id.pos$modelNum[match(min.thre.pos$sn,id.pos$sn)]
#   min.thre.pos$svrid <- id.pos$svrid[match(min.thre.pos$sn,id.pos$sn)]
#   min.thre.pos$prefix <- substr(min.thre.pos$svrid,5,6)
#   
#   ggplot(min.thre.pos,aes(x = thre,fill = prefix)) + geom_histogram(bins = 50)
# }