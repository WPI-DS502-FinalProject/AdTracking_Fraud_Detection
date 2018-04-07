library(tree)
library(caret)
library (randomForest)
set.seed(1)

# Read the training data that has all 1s
trainData <- read.csv(file="sub_0.csv", header=FALSE, sep=",")

# Read one partition of the training data that has all 0s
#trainNo <- read.csv(file="train_55.csv", header=FALSE, sep=",")

# Combine and shuffle the two sets to create a balanced data 
#trainData <- rbind(trainYes, trainNo) 
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

#u = union(pred, testSet$is_attributed)
#t = table(factor(pred, u), factor(testSet$is_attributed, u))
#confusionMatrix(t)


#cv.tree(tree_) # Gives the size of tree to be 3
#tree2 = prune.misclass(tree_, best=3)
##summary(tree2)
#par(mfrow = c(1, 1))
#plot(tree2)
#par(ps = 8, cex = 1, cex.main = 1)  #Set the size of the font = 8
#text(tree2,pretty=0)

#pred2=predict(tree2, newdata = testSet)
#results = seq(1, dim(pred2)[1])
#for (i in 1:dim(pred2)[1]) {
 # results[i] = c(0, 1)[which.max(pred2[i,])]
#}
#pred2 = results

#mean((pred2 - testSet$is_attributed)^2)  
#u = union(pred2, testSet$is_attributed)
#t = table(factor(pred2, u), factor(testSet$is_attributed, u))
#confusionMatrix(t)