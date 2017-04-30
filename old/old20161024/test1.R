rm(list=ls(all=TRUE))
data(iris)
Iris_2D <- iris[,3:5]
plot(Iris_2D[1:2], pch = 21, bg = c("red", "green3", "blue")[unclass(Iris_2D$Species)])

library(rpart)
Iris_2D_rpart_model <- rpart(Species~., data = Iris_2D)
Iris_2D_rpart_model <- rpart(Species~., data = Iris_2D, minsplit=1, cp=1e-3)

Iris_2D_rpart_pred <- predict(Iris_2D_rpart_model, Iris_2D)

Iris_2D_rpart_pred_ClassN <- apply( Iris_2D_rpart_pred,1,
                                    function(one_row) return(which(one_row == max(one_row))))
Iris_2D_rpart_pred_Class <- apply( Iris_2D_rpart_pred,1,
                                   function(one_row) return(colnames(Iris_2D_rpart_pred)[which(one_row == max(one_row))]))

Iris_2D_Class_temp <- unclass(Iris_2D$Species)
Iris_2D_Class <- attr(Iris_2D_Class_temp ,"levels")[Iris_2D_Class_temp]

table(Iris_2D_rpart_pred_Class,Iris_2D_Class)

plot(Iris_2D_rpart_model)
text(Iris_2D_rpart_model)

x1 <- seq(min(Iris_2D$Petal.Length), max(Iris_2D$Petal.Length), length = 50)
x2 <- seq(min(Iris_2D$Petal.Width), max(Iris_2D$Petal.Width), length = 50)
Feature_x1_to_x2 <- expand.grid(Petal.Length = x1, Petal.Width = x2)
Feature_x1_to_x2_Class <- apply(predict(Iris_2D_rpart_model,Feature_x1_to_x2),1,
                                function(one_row) return(which(one_row == max(one_row))))

plot(Iris_2D[1:2], pch = 21, bg = c("red", "green3", "blue")[unclass(Iris_2D$Species)])
contour(x1,x2,matrix(Feature_x1_to_x2_Class,length(x1)),add = T, levels = c(1.5,2.5),labex = 0)