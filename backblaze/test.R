# clean SMART: modify the column name, remove pos disk without enough pos data
clean_data <- function(DT,min_pos_days=10){
  DT$time <- as.Date(DT$date)
  DT$id <- factor(DT$serial_number)
  DT$label <- DT$failure
  DT$model <- factor(DT$model)
  names(DT) <- gsub('\\_raw','r',gsub('smart\\_','s',names(DT)))
  names(DT) <- gsub('\\_normalized','n',gsub('smart\\_','s',names(DT)))
  
  DT.list <- split(DT,DT$id)
  failed_sn <- factor(unique(DT$id[DT$label==1]))
  DT.list <- lapply(DT.list,function(df){
    if(df$id[1] %in% failed_sn){
      if(sum(df$label==0) < min_pos_days){
        return(NULL)
      }else{
        df <- subset(df,label == 0)
        df$label <- 1
        return(df)
      }
    }else{
      df$label <- 0
      return(df)
    }
  })
  
  DT.list <- DT.list[!sapply(DT.list,is.null)]
  #add feature diff for SMART attributes *r
  DT.list <- lapply(DT.list,function(df){
    if(dim(df)[1]>1){
      raw_diff <- as.data.frame.matrix(apply(df[,grepl('^s\\d.*r$',names(df))],2,function(x)c(0,diff(x))))
      names(raw_diff) <- gsub('r','d',names(raw_diff))
      df <- cbind(df,raw_diff)
      return(df)
    }else{
      return(NULL)
    }
  })
  DT.list <- DT.list[!sapply(DT.list,is.null)]
  col_value <- names(DT.list[[1]])[grepl('^s\\d.*[n|d]$',names(DT.list[[1]]))]
  DT.list <- lapply(DT.list,function(df)df[,c('id','time','label',col_value)])
  return(list(DT.list,failed_sn,col_value))
}

# get trainset and testset based on sample
get_train_test <- function(DT.list,failed_sn,rate=0.7){
  DT.list.pos <- DT.list[names(DT.list) %in% failed_sn]
  DT.list.neg <- DT.list[!(names(DT.list) %in% failed_sn)]
  index.pos <- sample(seq_len(length(DT.list.pos)),length(DT.list.pos)*0.7,replace=F)
  index.neg <- sample(seq_len(length(DT.list.neg)),length(DT.list.neg)*0.7,replace=F)
  return(list(c(DT.list.pos[index.pos],DT.list.neg[index.pos]),
         c(DT.list.pos[-index.pos],DT.list.neg[-index.pos])))
}

# get x_pos items of smart before failure as pos sample and x_neg items of neg smart for train set
get_sample <- function(DT.list.train,failed_sn,x_pos = 20,x_neg = 20,random_neg = T,reserveTIA=0){
  
  # pos samples
  DT.list.pos <- DT.list[names(DT.list) %in% failed_sn]
  DT.list.pos.smp <- lapply(DT.list.pos,function(df){
    df[(max(1,dim(df)[1]-x_pos+1)):dim(df)[1]-reserveTIA,]
  })
 
  # neg samples
  DT.list.neg <- DT.list[!(names(DT.list) %in% failed_sn)]
  DT.list.neg.smp <- lapply(DT.list.neg,function(df){
    if(random_neg){
      df <- smp_df(df,min(dim(df)[1],x_neg))
    }else{
      df <- df[max(1,(dim(df)[1]-x_neg)):dim(df)[1],]
    }
    return(df)
  })
  
  # for training disks, we resample items to build a balanced model (make size of pos dataset equales to size of neg dataset)
  DT.train.pos <- do.call(rbind,DT.list.pos.smp)
  DT.train.neg <- do.call(rbind,DT.list.neg.smp)
  len_pos <- dim(DT.train.pos)[1]
  DT.train.neg <- DT.train.neg[sample(seq_len(dim(DT.train.neg)[1]),len_pos,replace=F),]
  DT.train <- rbind(DT.train.pos,DT.train.neg)
  
  return(DT.train)
}



# Train model
get_model <- function(DT.train,funcStr,col_value,paras=0){
  f <- get(funcStr)
  return(f(DT.train,col_value,paras=0))
}

# get time in advance
get_tia <- 