library(tree)
library(caret)
library (randomForest)
#set.seed(1)

# Read the training data that has all 1s
#trainYes <- read.csv(file="train_yes.csv", header=FALSE, sep=",")

# Read one partition of the training data that has all 0s
#trainNo <- read.csv(file="train_55.csv", header=FALSE, sep=",")

# Combine and shuffle the two sets to create a balanced data 
#trainData <- rbind(trainYes, trainNo) 
#trainData <- trainData[sample(nrow(trainData)), ] 

trainData <- read.csv(file="/home/daniel/Desktop/AdTracking_Fraud_Detection/data/t1p60_subsamples/sub_0.csv", header=TRUE, sep=",")



# Re-assign the labels originally removed during partitioning
names(trainData) <- c('ip', 'app', 'device', 'os', 'channel', 'click_time', 'attributed_time', 'is_attributed')
trainData$attributed_time <- NULL
trainData$click_time <- NULL

# Set  whether as factors or numerical
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

tree_=tree(is_attributed ~ ., data = trainSet)

summary(tree_)
par(mfrow = c(1, 1))
plot(tree_)
par(ps = 8, cex = 1, cex.main = 1)  #Set the size of the font = 8
text(tree_,pretty=0)