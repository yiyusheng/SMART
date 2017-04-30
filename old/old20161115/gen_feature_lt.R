# F1.generate diff of smart for each disk
gen_diff_disk <- function(smart,itv.time){
  t1 <- Sys.time()
  smart.diff <- lapply(1:nrow(smart),function(x){
    idx <- which.min(abs(smart$dist.fail[x] + itv.time - smart$dist.fail))
    smart[itv.time,col.smart] - smart[idx,col.smart]
    })
  smart.diff <- do.call(rbind,smart.diff)
  t2 <- Sys.time()
  cat(sprintf('[%s]:[gen_diff_disk]%s\tinterval:%d days\telps:%0.2f\n',
              t2,smart$sn[1],itv.time,difftime(t2,t1,units = 'secs')))
  smart.diff
}

gen_diff <- function(smart.pos){
  new.pos <- smart.pos
  new.pos$multilabel <- round(new.pos$dist.fail/7)
  new.pos <- sort_col(new.pos)
  split.smart.pos <- split(new.pos,new.pos$sn)
  require(doParallel)
  idx <- seq_len(length(split.smart.pos))
  ck <- makeCluster(min(25,length(idx)),type = 'FORK',outfile = 'gen_feature.paroutput')
  registerDoParallel(ck)
  r1 <- foreach(i = idx,.combine = rbind,.verbose = F) %dopar% gen_diff_disk(split.smart.pos[[i]],1)
  r2 <- foreach(i = idx,.combine = rbind,.verbose = F) %dopar% gen_diff_disk(split.smart.pos[[i]],3)
  r3 <- foreach(i = idx,.combine = rbind,.verbose = F) %dopar% gen_diff_disk(split.smart.pos[[i]],7)
  stopCluster(ck)
  names(r1) <- paste('a',1:15,'.lag1',sep='')
  names(r2) <- paste('a',1:15,'.lag3',sep='')
  names(r3) <- paste('a',1:15,'.lag7',sep='')
  col.lag <- c(names(r1),names(r2),names(r3))
  save(new.pos,r1,r2,r3,col.lag,file = file.path(dir_data,'smart_lag.Rda'))
}

# F2.generate train and test to predict if failure will take place in the next week
gen_weekly_pred <- function(smart.pos,smart.neg,id.pos,id.neg,group){
  smart.pos$weeklabel <- 0
  smart.neg$weeklabel <- 0
  smart.neg$weeklabel[smart.neg$dist.fail <= 7 & smart.neg$dist.fail >= 0] <- 1
  extract_train_test(smart.pos,smart.neg,id.pos,id.neg,group)
}

# F3.evaluate model on train
eval_train <- function(trainF,model){
  pred <- predict(model,trainF[,col.smart],type = 'response')
  
}