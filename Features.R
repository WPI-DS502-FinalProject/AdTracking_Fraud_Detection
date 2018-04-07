# Features Engineering

# Load data and preparing the data frame
setwd("C:/Users/Personal/Dropbox/HP-Spectre/WPI/Courses/Statistical Learning for Data Sciences/Project")

dataOne=data.frame(read.csv('train_yes.csv'))
colnames(dataOne)=c("ip","app","device","os","channel","click_time","attributed_time","is_attributed")

dataZero=data.frame(read.csv('zerobs_0.csv'))
colnames(dataZero)=c("ip","app","device","os","channel","click_time","attributed_time","is_attributed")

# Preparing final data frame
data=rbind(dataOne,dataZero)  
attach(data)

# Predictor Analisys
par(mfrow=c(2,2))
#hist(log(dataZero$ip))
#hist(log(dataOne$ip))

dataN=(dataZero$device)
dataY=(dataOne$device)

hist(dataN,200)
hist(dataY,200)
hist(log(dataN),200)
hist(log(dataY),200)




hist(dataN,200, xlim=c(range(dataN)))
grid()
hist(dataY,200, xlim=c(range(dataN)))
grid()
