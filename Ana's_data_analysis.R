library(tree)
library(caret)
library(lattice)
library(ggplot2)
library (randomForest)
set.seed(1)

# Read the training data that has all 1s
#vtrainYes <- read.csv(file="train_yes.csv", header=FALSE, sep=",")

# Read one partition of the training data that has all 0s
# trainNo <- read.csv(file="train_55.csv", header=FALSE, sep=",")

# Combine and shuffle the two sets to create a balanced data 
# trainData <- rbind(trainYes, trainNo) 
#trainData <- read.csv(file="sub_0.csv", header=FALSE, sep=",")
# trainData <- trainData[sample(nrow(trainData)), ] 

trainData <- read.csv(file="sub_5.csv", header=TRUE, sep=",")



# Re-assign the labels originally removed during partitioning
names(trainData) <- c('ip', 'app', 'device', 'os', 'channel', 'click_time', 'attributed_time', 'is_attributed')
trainData$attributed_time <- NULL
trainData$day = as.POSIXlt(trainData$click_time)$wday
trainData$click_time <- NULL


# Set  whether as factors or numerical
trainData$is_attributed = as.factor(trainData$is_attributed)
trainData$app = as.numeric(trainData$ap)
trainData$os = as.numeric(trainData$os)
trainData$channel = as.numeric(trainData$channel)
trainData$click_time = as.numeric(trainData$click_time)
trainData$device = as.numeric(trainData$device)
trainData$day = as.numeric(trainData$day)

#### Normalization ############
trainData$app = (trainData$app - mean(trainData$app))/sd(trainData$app)
trainData$ip = (trainData$ip - mean(trainData$ip))/sd(trainData$ip)
trainData$channel = (trainData$channel - mean(trainData$channel))/sd(trainData$channel)
trainData$device = (trainData$device - mean(trainData$device))/sd(trainData$device)
trainData$os = (trainData$os - mean(trainData$os))/sd(trainData$os)
trainData$day = (trainData$day - mean(trainData$day))/sd(trainData$day)

# Split the data to training and test set
Idx <- sample(floor(nrow(trainData) * 0.8))
trainSet <- trainData[Idx,]
testSet <- trainData[-Idx,]


#testSet$is_attributed = as.numeric(testSet$is_attributed)
testSet$is_attributed = as.factor(testSet$is_attributed)

tree_=tree(is_attributed ~ ., data = trainSet)

summary(tree_)
par(mfrow = c(1, 1))
plot(tree_)
par(ps = 8, cex = 1, cex.main = 1)  #Set the size of the font = 8
text(tree_,pretty=0)
pred=predict(tree_, newdata = testSet)
results = seq(1, dim(pred)[1])
for (i in 1:dim(pred)[1]) {
  results[i] = c(0, 1)[which.max(pred[i,])]
}
pred = results

mean((pred - testSet$is_attributed)^2)  
u = union(pred, testSet$is_attributed)
t = table(factor(pred, u), factor(testSet$is_attributed, u))
confusionMatrix(t)

############## K Fold Cross Validation #################
#Randomly shuffle the data
trainSet<-trainSet[sample(nrow(trainSet)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(trainSet)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:1){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData_k <- trainSet[testIndexes, ]
  trainData_k <- trainSet[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  tree1=tree(is_attributed ~ ., data = trainData_k)
  pred1=predict(tree_, newdata = testData_k)
  results1 = seq(1, dim(pred1)[1])
  for (i in 1:dim(pred1)[1]) {
    results1[i] = c(0, 1)[which.max(pred1[i,])]
  }
  pred1 = results1
  u = union(pred1, testData_k$is_attributed)
  t = table(factor(pred1, u), factor(testData_k$is_attributed, u))
  print(confusionMatrix(t))
  print("======================================================")
}

##### Logisitic Regression on identified predictors #################
logitMod <- glm(is_attributed ~ app + channel + ip, family="binomial", data = trainSet)
predictedY <- predict(logitMod, testSet, type="response") 

table(testSet$is_attributed, predictedY > 0.5)

for (i in 1:dim(predictedY)[1]) {
  results_l[i] = c(0, 1)[which.max(predictedY[i,])]
}
predictedY = results_l

mean((predictedY - testSet$is_attributed)^2)  
u = union(predictedY, testSet$is_attributed)
t = table(factor(predictedY, u), factor(testSet$is_attributed, u))
print("Cnfusion Matrix for Logisitc Regression")
confusionMatrix(t)

######## Boosting Algorithms #########
# Stochastic Gradient Boosting
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)
set.seed(33)
gbmFit1 <- train(is_attributed ~ ., data = trainSet, method = "gbm", trControl = fitControl,verbose = FALSE)
yhat.boost <- predict(gbmFit1 , newdata=testSet, n.trees=100)
u = union(yhat.boost, testSet$is_attributed)
t = table(factor(yhat.boost, u), factor(testSet$is_attributed, u))
print("Cnfusion Matrix for Logisitc Regression")
confusionMatrix(t)

### Random Forests #############
model <- randomForest(is_attributed ~ . - is_attributed, data = trainSet, ntree=100, importance = TRUE)
model
varImpPlot(model)
predict_rf = predict(model, testSet)
testSet$rightPred <- predict_rf == testSet$is_attributed
t <- table(predict_rf,  testSet$is_attributed)
print(t)
accuracy <- sum(testSet$rightPred)/nrow(testSet)
print(accuracy)