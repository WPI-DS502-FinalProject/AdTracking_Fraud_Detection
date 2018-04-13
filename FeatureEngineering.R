# Load a subsmaple
trainData <- read.csv(file="data/t1p60_subsamples/sub_3.csv", header=TRUE, sep=",")

# Re-assign the labels originally removed during partitioning
names(trainData) <- c('ip', 'app', 'device', 'os', 'channel', 'click_time', 'attributed_time', 'is_attributed')

# Extract hour and day component of click_time
trainData$attributed_time <- NULL
trainData$day <- as.numeric(as.POSIXlt(trainData$click_time)$wday)
trainData$hour <- as.numeric(format(as.POSIXct(trainData$click_time) ,format = "%H"))
trainData$click_time <- NULL

# Set data type
trainData$ip <- as.numeric(trainData$ip)
trainData$app <- as.numeric(trainData$app)
trainData$channel <- as.numeric(trainData$channel)
trainData$os <- as.numeric(trainData$os)
trainData$device <- as.numeric(trainData$device)
trainData$is_attributed <- as.factor(trainData$is_attributed)

# Split the data to training and test set
Idx <- sample(floor(nrow(trainData) * 0.8))
trainSet <- trainData[Idx,]
testSet <- trainData[-Idx,]


### Random Forests #############
library (randomForest)
t1 <- Sys.time()
model <- randomForest(is_attributed ~ . - is_attributed, data = trainSet, ntree=100, importance = TRUE)
Sys.time() - t1
varImpPlot(model)


# Combine top features and remove day as feature
trainData$ip_app <- (trainData$ip + 1) * trainData$app
trainData$channel_app <- (trainData$channel + 1) * trainData$app
trainData$channel_ip <- (trainData$channel + 1) * trainData$ip
trainData$channel_ip_app <- (trainData$channel_ip + 1) * trainData$app
trainData$day <- NULL

### Decision Tree
library (tree)
tree_ <- tree(is_attributed ~ ., data = trainData)
par(mfrow <- c(1, 1))
plot(tree_)
par(ps = 8, cex = 1.5, cex.main = 1)  #Set the size of the font = 8
text(tree_, pretty=1)

### Subset selection
regfit <- regsubsets(is_attributed ~ ., data = trainData)
plot(regfit, scale = 'adjr2')
plot(regfit, scale = 'bic')

