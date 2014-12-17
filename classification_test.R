#test 1
n <- nrow(data)
#Define 10-fold CV error
error <- rep(0,n)
#Define random indices.
random.ind <- sample(n)
#Define y to be true ones.
y <- data$y[random.ind]
#Define the predicted values.
phat <- rep(0,n) 
for (i in 1:10){
  #Divide the entire data set as training and test by indices
  ind <- random.ind[(i*n/10-n/10+1):(i*n/10)]
  #Get SVM model
  mod.svm=svm(y~., data=data[-ind,], kernel ="polynomial",degree=2,cost =3,scale=T,probability=T)
  #Predict
  phat[ind] <- attr(predict(mod.svm,data[ind,],probability=T),"probabilities")[,2]
}

#test 2
data(iris)
attach(iris)
## classification mode
# default with factor response:
model <- svm(Species ~ ., data = iris)
# alternatively the traditional interface:
x <- subset(iris, select = -Species)
y <- Species
model <- svm(x, y, probability = TRUE)
pred <- predict(model, x, decision.values = TRUE, probability = TRUE)

test <- class.svm(data,2,3)
plot(test$accuracy,pch=".",ylim=c(0,1),type="o",col="blue")
lines(test$PPV,pch=".",col="red")
lined(test$NPV,pch=".",col="green")

data1$y <- as.factor(data1$y)
mod.rf = randomForest(y~., data=data1, mtry =5,ntree=20)
pred <- predict(mod.rf, data1[1:500,], type="prob")
test <- class.rf(data1[1:500,],6,200)


