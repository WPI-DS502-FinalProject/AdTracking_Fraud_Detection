library(MASS)
trainSample = read.csv("./train_sample.csv")

# Read the training data that has all 1s
trainYes <- read.csv(file="partitioned/train_yes.csv", header=FALSE, sep=",")
# Read one partition of the training data that has all 0s
trainNo <- read.csv(file="partitioned/train_50.csv", header=FALSE, sep=",")
# Combine and shuffle the two sets to create a balanced data set
trainData <- rbind(trainYes, trainNo) 
trainData <- trainData[sample(nrow(trainData)), ] 
# Re-assign the labels originally removed during partitioning
names(trainData) <- c('ip', 'app', 'device', 'os', 'channel','click_time', 'attributed_time', 'is_attributed')
# Subset selection
targetData <- trainData[sample(10000), ]
# Convert the datetime to numeric value
targetData$click_time <- as.numeric(targetData$click_time)  
# Remove the attributed_time
targetData <- targetData[-7]
# Split the data to training and test set
Idx <- sample(floor(nrow(targetData) * 0.8))
trainSet <- targetData[Idx,]
testSet <- targetData[-Idx,]

trainSample=trainSet
summary(trainSample)

#Logistic Regression
logReg = glm(is_attributed ~ ip+app+device+os+channel+click_time, data = trainSample)
summary(logReg)

logReg = glm(is_attributed ~ ip+app+device+os+channel, data = trainSample)
summary(logReg)

#LDA
modelLDA = lda(is_attributed ~ ip+app+channel, data = trainSample)

pred = predict(modelLDA, trainSample)$class

#Conf Matrix
confMatrix=table(pred, !trainSample$is_attributed)
confMatrix
mean(pred == trainSample$is_attributed)

#F1
TP=confMatrix[1,1]
FP=confMatrix[1,2]
FN=confMatrix[2,1]
TN=confMatrix[2,2]

f1=(2*TP)/(2*TP+FP+FN)
f1
