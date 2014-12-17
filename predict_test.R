data.test <- read.csv("power feature.csv",header=T)
data.test <- data.test[1:1000,]
m=3
b=50

#This script is for final project part 10 following "classification.R".

#Part 1 is to lock the best model.

#Part 2 is to predict the labels on the test set. 

#Part 1
#Lock the model we need for prediction.
#From the previous results, we pick random forest for power feature matrix.
#Assign best tuning parameters obtained from classification part.
m <- best.p[4]
b <- best.p[3]

#part 2
#Get test data.
data.test <- read.csv("test.csv",header=T)
#Drop the first column, which is useless.
data.test <- data.test[,-1]
data.test$test.y <- as.factor(data.test$V1)
data.test <- data.test[,-1]

n <- nrow(data.test)
y <- data.test$test.y
#Get the feature and best tuning parameters.
data.train <- powerfeature

#Fit model on
mod = randomForest(test.y~., data=data.train, mtry =m,ntree=b)
#Predict
phat <- predict(mod,data.test,type="prob")[,2]
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
  TP <- sum(yroc==1 & y==1)
  TN <- sum(yroc==0 & y==0)
  FP <- sum(yroc==1 & y==0)
  FN <- sum(yroc==0 & y==1)
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
#Plot for accuracy, PPV and NPV.
plot(accuracy,pch=".",ylim=c(0,1),type="o",col="blue")
lines(PPV,pch=".",col="red")
lines(NPV,pch=".",col="green")

#Find the best threshold.
j <- which.max(accuracy)
#Predict
yhat <- rep(0,n)
yhat[which(phat > p[j])] <- 1
#Preduce the 2 X 2 table.
print(table(predict=yhat,true=data.test$test.y))
#Print accuracy, PPV and NPV
results <- matrix(c(accuracy[j],PPV[j],NPV[j]),1,3)
colnames(results) <- c("accuracy","PPV","NPV")
rownames(results) <- c("results")
print(results)


