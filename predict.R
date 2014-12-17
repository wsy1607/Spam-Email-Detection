

  
data.test <- read.csv("",header=T)


n <- nrow(data.test)

mod.svm=svm(test.y~., data=data, kernel ="polynomial",degree=d,cost =l,scale=T)
yhat <- predict(mod.svm,data.test)
yhat <- as.numeric(yhat)-1