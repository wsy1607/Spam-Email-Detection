library("e1071")
library(MASS)

summary(Boston)
y <- sample(c(0,1),nrow(Boston),replace=T)
data <- Boston
data$y <- as.factor(y)
#test
x= matrix(rnorm (200*2) , ncol =2)
y=c(rep( 0 ,100) , rep(1 ,100) )
x[y==1 ,]= x[y==1 ,] + 1
dat =data.frame(x=x, y=as.factor(y))

cv.svm <- function(data,d,l){
  #Get the number of observations
  n <- nrow(data)
  #Define 10-fold CV error
  error <- rep(0,n)
  #Define random indices.
  random.ind <- sample(n)
  #Define the predicted values.
  yhat <- rep(0,n) 
  for (i in 1:10){
    #Divide the entire data set as training and test by indices
    ind <- random.ind[(i*n/10-n/10+1):(i*n/10)]
    #Get SVM model
    mod.svm=svm(y~., data=data[-ind,], kernel ="polynomial",degree=d,cost =l,scale=T)
    #Predict
    yhat[ind] <- predict(mod.svm,data[ind,],scale=T)
    print(i)
  }
  yhat <- as.numeric(yhat)-1
  #Compute the test error.
  error[which((data$y == yhat)==F)] <- 1
  #Print the table
  print(table(predict=yhat , truth = data$y))
  #Return the prediction accuracy.
  return(1-sum(error)/n)
}

cv.svm.r <- function(data,g,l){
  #Get the number of observations
  n <- nrow(data)
  #Define 10-fold CV error
  error <- rep(0,n)
  #Define random indices.
  random.ind <- sample(n)
  #Define the predicted values.
  yhat <- rep(0,n) 
  for (i in 1:10){
    #Divide the entire data set as training and test by indices
    ind <- random.ind[(i*n/10-n/10+1):(i*n/10)]
    #Get SVM model
    mod.svm=svm(y~., data=data[-ind,], kernel ="radial",gamma=g,cost =l,scale=T)
    #Predict
    yhat[ind] <- predict(mod.svm,data[ind,])
    print(i)
  }
  yhat <- as.numeric(yhat)-1
  #Compute the test error.
  error[which((data$y == yhat)==F)] <- 1
  #Print the table
  print(table(predict=yhat , truth = data$y))
  #Return the prediction accuracy.
  return(1-sum(error)/n)
}








cv.svm(data[-c(1,2,3,4,5,6),],1,10)
cv.svm(dat,1,10)



tune.out =tune(svm ,y~., data=dat , kernel ="linear",scale=T,
               ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
bestmod = tune.out$best.model
yhat <- predict(bestmod,dat[ind,],scale=T)