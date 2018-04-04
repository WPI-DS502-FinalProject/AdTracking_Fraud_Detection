library(tree)
library(caret)
library (randomForest)
set.seed(1)

# Read the training data that has all 1s
trainYes <- read.csv(file="train_yes.csv", header=FALSE, sep=",")

# Read one partition of the training data that has all 0s
trainNo <- read.csv(file="train_55.csv", header=FALSE, sep=",")

# Combine and shuffle the two sets to create a balanced data 
trainData <- rbind(trainYes, trainNo) 
trainData <- trainData[sample(nrow(trainData)), ] 

# Re-assign the labels originally removed during partitioning
names(trainData) <- c('ip', 'app', 'device', 'os', 'channel', 'click_time', 'attributed_time', 'is_attributed')
trainData$attributed_time <- NULL
trainData$ip
trainData$click_time <- NULL

# Set  whether as factors or numerical
print(trainData)
print(length(levels(trainData$is_attributed)))
print(length(unique(trainData$channel)))
print(length(unique(trainData$device)))
print(length(unique(trainData$os)))
print(length(unique(trainData$app)))
print(length(unique(trainData$attributed_time)))
trainData$is_attributed = as.factor(trainData$is_attributed)
trainData$app = as.numeric(trainData$ap)
trainData$os = as.numeric(trainData$os)
trainData$channel = as.numeric(trainData$channel)
trainData$click_time = as.numeric(trainData$click_time)
trainData$device = as.numeric(trainData$device)


# Split the data to training and test set
Idx <- sample(floor(nrow(trainData) * 0.8))
trainSet <- trainData[Idx,]
testSet <- trainData[-Idx,]

testSet$is_attributed = as.numeric(testSet$is_attributed)

set.seed(1)


summary(trainData)  
summary(trainSet)


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

  ##################### DO NOT RUN THIS PART OF THE CODE ############################
  
  # STILL WORK ON THIS PART
  # c. Use cross-validation in order to determine the optimal level of tree complexity.
  cv.tree_=cv.tree(tree_)
  plot(cv.tree_$size, cv.tree_$dev, type = "b")
  tree_.min=which.min(cv.is_attributed$dev)
  points(tree.min, cv.carseats$dev[tree_.min], col = "red", cex = 2, pch = 20)
  
  prune.carseats=prune.tree(tree.carseats, best = 8)
  plot(prune.carseats)
  text(prune.carseats, pretty = 0)  
  
  pred.prune=predict(prune.carseats, newdata = Carseats.test)
  mean((pred.prune - Carseats.test$Sales)^2)  
  
  # d.	Use the bagging approach in order to analyze this data
  bag.df3 <- randomForest(is_attributed ~ ., data = trainSet, mtry = 10,ntree = 500, importance = TRUE)
  
  pred.rf=predict(bag.df3, newdata = testSet)
  results = seq(1, dim(pred.rf)[1])
  for (i in 1:dim(pred)[1]) {
    results[i] = c(0, 1)[which.max(pred.rf[i,])]
  }
  pred.rf = results
  mean((pred.rf - testSet$is_attributed)^2)  
  importance(bag.df3)
  
  # e. Use random forests to analyze this data. What test MSE do you obtain?
  rf.df3 <- randomForest(is_attributed ~ ., data = testSet, mtry = 1, ntree = 500, importance = TRUE)
  pred.rf1 <- predict(rf.df3, newdata = testSet)
  plot(rf.df3)
  par(ps = 8, cex = 1, cex.main = 1)  #Set the size of the font = 8
  text(tree_,pretty=0)
  mean((pred.rf1 - df3.test$is_attributed)^2)
  importance(rf.df3)
  
  results = seq(1, dim(pred)[1])
  for (i in 1:dim(pred)[1]) {
    results[i] = c(0, 1)[which.max(pred[i,])]
  }
    