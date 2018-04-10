# Load data and preparing the data frame
setwd("C:/Users/Personal/Dropbox/HP-Spectre/WPI/Courses/Statistical Learning for Data Sciences/DS502/AdTracking_Fraud_Detection")

dataOri=data.frame(read.csv('./tempdata/subsamples/sub_0.csv'))
colnames(dataOri)=c("ip","app","device","os","channel","click_time","attributed_time","is_attributed")

# Thresholds to test the models
data=dataOri[dataOri$app>18.5,]   # Spliting data using APP predictor (Threshod=18.5)
data=data[data$app>19.5,]         # Spliting data using APP predictor (Threshod=19.5)
#data=data[data$channel>114.5,]
#data=dataOri


# Model Fitting

# i.	Split the sample set into a training set and a validation set.
  nSize=round(dim(data)[1]*0.7); #0.7 is the 70% of the data
  TrainData=sample(dim(data)[1],nSize,replace=FALSE);

# ii.	Train the model
  #fit.data1 = glm(is_attributed~ip+app+os+channel+ip*os+ip*channel+app*os+app*channel+os*channel+ip*app*channel, data=data, family = "binomial", subset = TrainData)
  fit.data1 = glm(is_attributed~ip+os+app+channel, data=data, family = "binomial", subset = TrainData)
  summary(fit.data1)

#iii. Obtain a prediction 
  data1.Test=data[-TrainData,]
  data1.probs=predict(fit.data1, newdata=data1.Test, type="response")

  pred.data1 = rep(1, length(data1.probs))  #Error
  pred.data1[data1.probs > 0.5] = 0
  mean(pred.data1 != data1.Test$is_attributed)

  