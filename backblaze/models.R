support.vector.machine <- function(DT.train,col_value,paras=0){
  model <- svm(x=DT.train[,col_value],y=DT.train$label,type="C-classification")
  DT.train$result <- predict(model,DT.train[,col_value])
  return(list(model, DT.train))
}