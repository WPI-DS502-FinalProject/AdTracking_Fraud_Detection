#Load Data
origData=read.csv(file="./tempdata/subsamples/sub_0.csv")
names(origData) = c('ip', 'app', 'device', 'os', 'channel','click_time', 'attributed_time', 'is_attributed')

#Create Branches:
branch_1=origData[((origData$app< 18.5) & (origData$channel< 114.5) & (origData$channel< 112)),]
branch_2=origData[((origData$app< 18.5) & (origData$channel< 114.5) & (origData$channel>=112)),]
branch_3=origData[((origData$app< 18.5) & (origData$channel>=114.5) & (origData$ip< 213413)),]
branch_4=origData[((origData$app< 18.5) & (origData$channel>=114.5) & (origData$ip>=213413)),]
branch_5=origData[((origData$app>=18.5) & (origData$app< 19.5)),]
branch_6=origData[((origData$app>=18.5) & (origData$app>=19.5) & (origData$app< 28.5)),]
branch_7=origData[((origData$app>=18.5) & (origData$app>=19.5) & (origData$app>=28.5) & (origData$channel< 345)),]
branch_8=origData[((origData$app>=18.5) & (origData$app>=19.5) & (origData$app>=28.5) & (origData$channel>=345)),]

branch=data.frame(branch_1, branch_2)

#Split data: training and testing set
PER = 0.8
branch_1_index = sample(floor(nrow(branch_1) * PER))
branch_1_train = branch_1[branch_1_index,]
branch_1_test  = branch_1[-branch_1_index,]

branch_2_index = sample(floor(nrow(branch_2) * PER))
branch_2_train = branch_2[branch_2_index,]
branch_2_test  = branch_2[-branch_2_index,]

branch_3_index = sample(floor(nrow(branch_3) * PER))
branch_3_train = branch_3[branch_3_index,]
branch_3_test  = branch_3[-branch_3_index,]

branch_4_index = sample(floor(nrow(branch_4) * PER))
branch_4_train = branch_4[branch_4_index,]
branch_4_test  = branch_4[-branch_4_index,]

branch_5_index = sample(floor(nrow(branch_5) * PER))
branch_5_train = branch_5[branch_5_index,]
branch_5_test  = branch_5[-branch_5_index,]

branch_6_index = sample(floor(nrow(branch_6) * PER))
branch_6_train = branch_6[branch_6_index,]
branch_6_test  = branch_6[-branch_6_index,]

branch_7_index = sample(floor(nrow(branch_7) * PER))
branch_7_train = branch_7[branch_7_index,]
branch_7_test  = branch_7[-branch_7_index,]

branch_8_index = sample(floor(nrow(branch_8) * PER))
branch_8_train = branch_8[branch_8_index,]
branch_8_test  = branch_8[-branch_8_index,]

#Train Model: Logistic Regression
branch_1_logreg = glm(is_attributed~ip+os+app+channel, data=branch_1, family = "binomial", subset = branch_1_index)
branch_2_logreg = glm(is_attributed~ip+os+app+channel, data=branch_2, family = "binomial", subset = branch_2_index)
branch_3_logreg = glm(is_attributed~ip+os+app+channel, data=branch_3, family = "binomial", subset = branch_3_index)
branch_4_logreg = glm(is_attributed~ip+os+app+channel, data=branch_4, family = "binomial", subset = branch_4_index)
branch_5_logreg = glm(is_attributed~ip+os+app+channel, data=branch_5, family = "binomial", subset = branch_5_index)
branch_6_logreg = glm(is_attributed~ip+os+app+channel, data=branch_6, family = "binomial", subset = branch_6_index)
branch_7_logreg = glm(is_attributed~ip+os+app+channel, data=branch_7, family = "binomial", subset = branch_7_index)
branch_8_logreg = glm(is_attributed~ip+os+app+channel, data=branch_8, family = "binomial", subset = branch_8_index)

#Test Model: Logistic Regression
branch_1_probs=predict(branch_1_logreg, newdata=branch_1_test, type="response")
branch_1_pred =rep(1, length(branch_1_probs))  #Error
branch_1_pred[branch_1_probs > 0.5] = 0
branch_1_mean=mean(branch_1_pred != branch_1_test$is_attributed)

branch_2_probs=predict(branch_2_logreg, newdata=branch_2_test, type="response")
branch_2_pred =rep(1, length(branch_2_probs))  #Error
branch_2_pred[branch_2_probs > 0.5] = 0
branch_2_mean=mean(branch_2_pred != branch_2_test$is_attributed)

branch_3_probs=predict(branch_3_logreg, newdata=branch_3_test, type="response")
branch_3_pred =rep(1, length(branch_3_probs))  #Error
branch_3_pred[branch_3_probs > 0.5] = 0
branch_3_mean=mean(branch_3_pred != branch_3_test$is_attributed)

branch_4_probs=predict(branch_4_logreg, newdata=branch_4_test, type="response")
branch_4_pred =rep(1, length(branch_4_probs))  #Error
branch_4_pred[branch_4_probs > 0.5] = 0
branch_4_mean=mean(branch_4_pred != branch_4_test$is_attributed)

branch_5_probs=predict(branch_5_logreg, newdata=branch_5_test, type="response")
branch_5_pred =rep(1, length(branch_5_probs))  #Error
branch_5_pred[branch_5_probs > 0.5] = 0
branch_5_mean=mean(branch_5_pred != branch_5_test$is_attributed)

branch_6_probs=predict(branch_6_logreg, newdata=branch_6_test, type="response")
branch_6_pred =rep(1, length(branch_6_probs))  #Error
branch_6_pred[branch_6_probs > 0.5] = 0
branch_6_mean=mean(branch_6_pred != branch_6_test$is_attributed)

branch_7_probs=predict(branch_7_logreg, newdata=branch_7_test, type="response")
branch_7_pred =rep(1, length(branch_7_probs))  #Error
branch_7_pred[branch_7_probs > 0.5] = 0
branch_7_mean=mean(branch_7_pred != branch_7_test$is_attributed)

branch_8_probs=predict(branch_8_logreg, newdata=branch_8_test, type="response")
branch_8_pred =rep(1, length(branch_8_probs))  #Error
branch_8_pred[branch_8_probs > 0.5] = 0
branch_8_mean=mean(branch_8_pred != branch_8_test$is_attributed)

mean_logreg=c(branch_1_mean, branch_2_mean, branch_3_mean, branch_4_mean, branch_5_mean, branch_6_mean, branch_7_mean, branch_8_mean)
mean_logreg
