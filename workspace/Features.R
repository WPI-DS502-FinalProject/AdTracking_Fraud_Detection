# Features Engineering

# Load data and preparing the data frame
setwd("C:/Users/Personal/Dropbox/HP-Spectre/WPI/Courses/Statistical Learning for Data Sciences/DS502/AdTracking_Fraud_Detection")

dataOri=data.frame(read.csv('./tempdata/subsamples/sub_0.csv'))
colnames(dataOri)=c("ip","app","device","os","channel","click_time","attributed_time","is_attributed")

# Predictor Analisys

dataZero=dataOri[dataOri$is_attributed<1,]
dataOne=dataOri[dataOri$is_attributed>0,]

par(mfrow=c(2,2))
hist(dataZero$channel,200)
grid()
hist(dataOne$channel,200)
grid()
hist(log10(dataZero$channel+1),200)
grid()
hist(log10(dataOne$channel+1),200)
grid()



hist(dataN,200, xlim=c(range(dataN)))
grid()
hist(dataY,200, xlim=c(range(dataN)))
grid()
