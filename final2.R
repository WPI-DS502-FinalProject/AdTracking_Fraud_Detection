# Load data and preparing the data frame
setwd("C:/Users/Personal/Dropbox/HP-Spectre/WPI/Courses/Statistical Learning for Data Sciences/DS502/AdTracking_Fraud_Detection")

dataOri=data.frame(read.csv('./tempdata/subsamples/sub_0.csv'))
colnames(dataOri)=c("ip","app","device","os","channel","click_time","attributed_time","is_attributed")

data=dataOri[dataOri$app>18.5,]
data=data[data$app>19.5,]
#data=data[data$channel>114.5,]
#data=dataOri

# Model Fit

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

  
# testing with the sample file
  sData=data.frame(read.csv('train_sample.csv'))
  Pred.sData=predict(fit.data1, newdata=sData, type="response")
  Pre.sData = rep(1, length(Pred.sData))
  Pre.sData[Pred.sData > 0.5] = 0
  mean(Pre.sData != sData$is_attributed)
  
  
# Subset model selection
  bestModel=regsubsets(is_attributed~ip+app+os+channel+ip*app+ip*os+ip*channel+app*os+app*channel+os*channel, data = data, nvmax = 10)  #sugested in pag 115
  best.summary=summary(bestModel)
  par(mfrow = c(1, 3))
  plot(best.summary$cp, xlab = "Number of variables", ylab = "C_p", type = "l")  #pag.246
  points(which.min(best.summary$cp), best.summary$cp[which.min(best.summary$cp)], col = "red", cex = 2, pch = 20)
  plot(best.summary$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
  points(which.min(best.summary$bic), best.summary$bic[which.min(best.summary$bic)], col = "red", cex = 2, pch = 20)
  plot(best.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = "l")
  points(which.max(best.summary$adjr2), best.summary$adjr2[which.max(best.summary$adjr2)], col = "red", cex = 2, pch = 20)
  coef(bestModel, which.max(best.summary$adjr2))
  