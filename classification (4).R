#This script is for final project part 6

#The way I would like to do is that first we selection the best tuning parameters and
#model complexity in terms of 10-fold cv error rate only for those three matrices,
#word, power and combined. 
#Then we get the best SVM and random forests for each of them. So we will 
#have 2 (one for SVM and one for random forest) models for each.
#Then we make ROC curve and get cv accuracy, PPV and NPV for each model.

#part 1 is about just defining three sets of functions.
#set 1: cv functions
#set 2: model selection, getting the best tuning parameters
#set 3: ROC curves

#part 2 is for calculation when we have real word, power and combined features.

#Maybe I don't understand this part correctly, feel free to let me know if you
#want to change anything.

#load packages
library("e1071")
library("randomForest")

#part1

#First define cv.SVM and cv.RF to select for best tuning parameters.
#SVM, where d is degree, l is cost.
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
    mod.svm=svm(test.y~., data=data[-ind,], kernel ="polynomial",degree=d,cost =l,scale=T)
    #Predict
    yhat[ind] <- predict(mod.svm,data[ind,])
    print(i)
  }
  yhat <- as.numeric(yhat)-1
  #Compute the test error.
  error[which((data$test.y == yhat)==F)] <- 1
  #Return the prediction accuracy.
  return(1-sum(error)/n)
}

#Random Forests where, m is number of predictor candidates, b is number of trees.
cv.rf <- function(data,m,b){
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
    #Get RF model
    mod.rf = randomForest(test.y~., data=data[-ind,], mtry =m,ntree=b)
    #Predict
    yhat[ind] <- predict(mod.rf,data[ind,])
    print(i)
  }
  yhat <- as.numeric(yhat)-1
  #Compute the test error.
  error[which((data$test.y == yhat)==F)] <- 1
  #Return the prediction accuracy.
  return(1-sum(error)/n)
}

#Do model selection for SVMs.
#Select for the best kernel function with cost = 1.
best.d <- function(data){
  d.choice <- c(1,2,3)
  output <- rep(0,3)
  for (i in 1:3){
    output[i] <- cv.svm(data,d.choice[i],1)
  }
  return(d.choice[which.max(output)])
}


#Select for the best cost.
best.l <- function(data){
  l.choice <- c(0.001, 0.01, 0.1, 1, 3, 5, 10, 100)
  output <- rep(0,8)
  for (i in 1:8){
    output[i] <- cv.svm(data,d.best,l.choice[i])
  }
  return(l.choice[which.max(output)])
}


#Do the same model selection for random forests.
#Select for the best number of trees, holding m = sqrt(p).
best.b <- function(data){
  b.choice <- c(50,100,200,300,400,500)
  output <- rep(0,6)
  for (i in 1:6){
    output[i] <- cv.rf(data,round(sqrt(ncol(data))),b.choice[i])
  }
  return(b.choice[which.max(output)])
}


#Select for m. Candidates are sqrt(p)-2, sqrt(p)-1, sqrt(p), sqrt(p)+1, sqrt(p)+2.
best.m <- function(data){
  m.center <- round(sqrt(ncol(data)))
  m.choice <- c(m.center-2,m.center-1,m.center,m.center+1,m.center+2)
  output <- rep(0,5)
  for (i in 1:5){
    output[i] <- cv.rf(data,m.choice[i],b.best)
  }
  return(m.choice[which.max(output)])
}

#Then plot for ROC curve and compute for accuracy, PPV and NPV.
#SVM plot
class.svm <- function(data,d,l){
  #Get the number of observations
  n <- nrow(data)
  #Define 10-fold CV error
  error <- rep(0,n)
  #Define random indices.
  random.ind <- sample(n)
  #Define y to be true ones.
  test.y <- data$test.y
  #Define the predicted values.
  phat <- rep(0,n) 
  for (i in 1:10){
    #Divide the entire data set as training and test by indices
    ind <- random.ind[(i*n/10-n/10+1):(i*n/10)]
    #Get SVM model
    mod.svm=svm(test.y~., data=data[-ind,], kernel ="polynomial",degree=d,cost =l,scale=T,probability=T)
    #Predict
    phat[ind] <- attr(predict(mod.svm,data[ind,],probability=T),"probabilities")[,2]
  }
  #To make the ROC curve, we want to define a sequence of thresholds.
  p <- seq(0,1.001,0.001)
  m <- length(p)
  #We are going to loop for each p to get a sequence of points, which
  #will be shown in the ROC plot.
  sen <- rep(0,m)
  FPR <- rep(0,m)
  PPV <- rep(0,m)
  NPV <- rep(0,m)
  accuracy <- rep(0,m) 
  for (j in 1:m){
    #Define yroc for each predicted status given p
    yroc <- rep(0,n)
    #Compute yroc by p
    yroc[which(phat <= p[j])] <- 1
    #Calculate TP, TN, FP and FN
    TP <- sum(yroc==1 & test.y==1)
    TN <- sum(yroc==0 & test.y==0)
    FP <- sum(yroc==1 & test.y==0)
    FN <- sum(yroc==0 & test.y==1)
    #Calculate sensitivity and FPR
    sen[j] <- TP/(TP + FN)
    FPR[j] <- FP/(FP + TN)
    #Calculate PPV and NPV and overall accuracy
    PPV[j] <- TP/(TP + FP)
    NPV[j] <- TN/(TN + FN)
    accuracy[j] <- (TP+TN)/(TP+TN+FP+FN)
  }
  #Plot ROC curve
  plot.ts(FPR,sen,pch=".",main="ROC curve",type="o") 
  return(data.frame(accuracy,PPV,NPV))
}

#Random Forests plot
class.rf <- function(data,m,b){
  #Get the number of observations
  n <- nrow(data)
  #Define 10-fold CV error
  error <- rep(0,n)
  #Define random indices.
  random.ind <- sample(n)
  #Define y to be true ones.
  test.y <- data$test.y
  #Define the predicted values.
  phat <- rep(0,n) 
  for (i in 1:10){
    #Divide the entire data set as training and test by indices
    ind <- random.ind[(i*n/10-n/10+1):(i*n/10)]
    #Fit random forest.
    mod.rf = randomForest(test.y~., data=data[-ind,], mtry =m,ntree=b)
    #Predict
    phat[ind] <- predict(mod.rf,data[ind,],type="prob")[,2]
  }
  #To make the ROC curve, we want to define a sequence of thresholds.
  p <- seq(0,1.001,0.001)
  m <- length(p)
  #We are going to loop for each p to get a sequence of points, which
  #will be shown in the ROC plot.
  sen <- rep(0,m)
  FPR <- rep(0,m)
  PPV <- rep(0,m)
  NPV <- rep(0,m)
  accuracy <- rep(0,m) 
  for (j in 1:m){
    #Define yroc for each predicted status given p
    yroc <- rep(0,n)
    #Compute yroc by p
    yroc[which(phat > p[j])] <- 1
    #Calculate TP, TN, FP and FN
    TP <- sum(yroc==1 & test.y==1)
    TN <- sum(yroc==0 & test.y==0)
    FP <- sum(yroc==1 & test.y==0)
    FN <- sum(yroc==0 & test.y==1)
    #Calculate sensitivity and FPR
    sen[j] <- TP/(TP + FN)
    FPR[j] <- FP/(FP + TN)
    #Calculate PPV and NPV and overall accuracy
    PPV[j] <- TP/(TP + FP)
    NPV[j] <- TN/(TN + FN)
    accuracy[j] <- (TP+TN)/(TP+TN+FP+FN)
  }
  #Plot ROC curve
  plot.ts(FPR,sen,pch=".",main="ROC curve",type="o") 
  return(data.frame(accuracy,PPV,NPV))
}

#part2

#Load data

#Word feature
wordfeature <- read.csv("select word feature.csv",header=T)
#Drop the first column, which is useless.
wordfeature <- wordfeature[,-1]
wordfeature$test.y <- as.factor(wordfeature$test.y)

#Power feature
powerfeature <- read.csv("power feature.csv",header=T)
powerfeature <- powerfeature[,-1]
powerfeature$test.y <- as.factor(powerfeature$V1)
powerfeature <- powerfeature[,-1]

#Combined feature
combinedfeature <- read.csv("word+power feature matrix.csv",header=T)
combinedfeature <- combinedfeature[,-1]
combinedfeature$test.y <- as.factor(combinedfeature$test.y)

#So for three matrices, we do model selection for each of them.
#Recall, d is degree; l is the cost; b is number of trees; m is number of predictor subset
#word feature matrix
d.best <- best.d(wordfeature)
l.best <- best.l(wordfeature)
b.best <- best.b(wordfeature)
m.best <- best.m(wordfeature)
#Get the best model for wordfeature.
best.w <- c(d.best, l.best, b.best, m.best)

#powerfeature matrix
d.best <- best.d(powerfeature)
l.best <- best.l(powerfeature)
b.best <- best.b(powerfeature)
m.best <- best.m(powerfeature)
best.p <- c(d.best, l.best, b.best, m.best)

#combined feature matrix
d.best <- best.d(combinedfeature)
l.best <- best.l(combinedfeature)
b.best <- best.b(combinedfeature)
m.best <- best.m(combinedfeature)
best.c <- c(d.best, l.best, b.best, m.best)

#Then get ROC curves.
word.svm <- class.svm(wordfeature,best.w[1],best.w[2])
word.rf <- class.rf(wordfeature,best.w[4],best.w[3])
power.svm <- class.svm(powerfeature,best.p[1],best.p[2])
power.rf <- class.rf(powerfeature,best.p[4],best.p[3])
combined.svm <- class.svm(combinedfeature,best.c[1],best.c[2])
combined.rf <- class.rf(combinedfeature,best.c[4],best.c[3])

#Pot for accuracy, PPV and NPV
p <- seq(0,1.001,0.001)
#word
plot(p,word.svm$accuracy,pch=".",ylim=c(0,1),type="o",col="blue")
lines(p,word.svm$PPV,pch=".",col="red")
lines(p,word.svm$NPV,pch=".",col="green")
plot(1-p,word.rf$accuracy,pch=".",ylim=c(0,1),type="o",col="blue")
lines(1-p,word.rf$PPV,pch=".",col="red")
lines(1-p,word.rf$NPV,pch=".",col="green")
#power
plot(p,power.svm$accuracy,pch=".",ylim=c(0,1),type="o",col="blue")
lines(p,power.svm$PPV,pch=".",col="red")
lines(p,power.svm$NPV,pch=".",col="green")
plot(1-p,power.rf$accuracy,pch=".",ylim=c(0,1),type="o",col="blue")
lines(1-p,power.rf$PPV,pch=".",col="red")
lines(1-p,power.rf$NPV,pch=".",col="green")
#combined
plot(combined.svm$accuracy,pch=".",ylim=c(0,1),type="o",col="blue")
lines(combined.svm$PPV,pch=".",col="red")
lines(combined.svm$NPV,pch=".",col="green")
plot(1-p,combined.rf$accuracy,pch=".",ylim=c(0,1),type="o",col="blue")
lines(1-p,combined.rf$PPV,pch=".",col="red")
lines(1-p,combined.rf$NPV,pch=".",col="green")


