

# Load the required libraries
library(class)
library(readr)
library(MASS)
set.seed(1)



# Read the training data that has all 1s
trainYes <- read.csv(file="tempdata/partitioned/train_yes.csv", header=FALSE, sep=",")

# Read one partition of the training data that has all 0s
trainNo <- read.csv(file="tempdata/partitioned/train_1.csv", header=FALSE, sep=",")

# Combine and shuffle the two sets to create a balanced data set
data <- rbind(trainYes, trainNo) 
data <- data[sample(nrow(data)), ] 


# Re-assign the labels originally removed during partitioning
names(data) <- c('ip', 'app', 'device', 'os', 'channel','click_time', 'attributed_time', 'is_attributed')

# Subset selection
targetData <- data[sample(10000), ]

# Convert the datetime to numeric value
targetData$click_time <- as.numeric(targetData$click_time)  

# Remove the attributed_time
targetData <- targetData[-7]

#Split the data set into a training set and a testing set
trainN <- sample(1:nrow(data), nrow(data)*0.8)
train <- targetData[trainN,]
test <- targetData[-trainN,]

# Visualize the data
pairs(train)
summary(train)

#---> QDA

#---> Device
#Train
qdaFit = qda(is_attributed~device, data = train)
#Test
pred = predict(qdaFit, test)$class
#Output
table(pred, !test$is_attributed)
mean(pred == test$is_attributed)


#---> OS
#Train
qdaFit = qda(is_attributed~os, data = train)
#Test
pred = predict(qdaFit, test)$class
#Output
table(pred, !test$is_attributed)
mean(pred == test$is_attributed)


#---> IP
#Train
qdaFit = qda(is_attributed~ip, data = train)
#Test
pred = predict(qdaFit, test)$class
#Output
table(pred, !test$is_attributed)
mean(pred == test$is_attributed)



#---> App
#Train
qdaFit = qda(is_attributed~app, data = train)
#Test
pred = predict(qdaFit, test)$class
#Output
table(pred, !test$is_attributed)
mean(pred == test$is_attributed)


#---> Channel
#Train
qdaFit = qda(is_attributed~channel, data = train)
#Test
pred = predict(qdaFit, test)$class
#Output
table(pred, !test$is_attributed)
mean(pred == test$is_attributed)


#---> Channel + App + IP
#Train
qdaFit = qda(is_attributed~channel+app+ip, data = train)
#Test
pred = predict(qdaFit, test)$class
#Output
table(pred, !test$is_attributed)
mean(pred == test$is_attributed)




