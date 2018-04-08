#Load Data
origData=read.csv(file="./tempdata/subsamples/sub_5.csv")
names(origData) = c('ip', 'app', 'device', 'os', 'channel','click_time', 'attributed_time', 'is_attributed')

branch_mean=1;
for(branch_num in c(1:8)){
  #Create Branches:
  branch_data = switch(
    branch_num,
    origData[((origData$app< 18.5) & (origData$channel< 114.5) & (origData$channel< 112)),],
    origData[((origData$app< 18.5) & (origData$channel< 114.5) & (origData$channel>=112)),],
    origData[((origData$app< 18.5) & (origData$channel>=114.5) & (origData$ip< 213413)),],
    origData[((origData$app< 18.5) & (origData$channel>=114.5) & (origData$ip>=213413)),],
    origData[((origData$app>=18.5) & (origData$app< 19.5)),],
    origData[((origData$app>=18.5) & (origData$app>=19.5) & (origData$app< 28.5)),],
    origData[((origData$app>=18.5) & (origData$app>=19.5) & (origData$app>=28.5) & (origData$channel< 345)),],
    origData[((origData$app>=18.5) & (origData$app>=19.5) & (origData$app>=28.5) & (origData$channel>=345)),])

  #Split data: training and testing set
  PER = 0.8
  branch_index = sample(floor(nrow(branch_data) * PER))
  branch_train = branch_data[branch_index,]
  branch_test  = branch_data[-branch_index,]

  #Train Model: Logistic Regression
  branch_logreg = glm(is_attributed~ip+os+app+channel, data=branch_data, family = "binomial", subset = branch_index)

  #Test Model: Logistic Regression
  branch_probs=predict(branch_logreg, newdata=branch_test, type="response")
  branch_pred =rep(1, length(branch_probs))#Error
  branch_pred[branch_probs > 0.5] = 0
  branch_mean=c(branch_mean, mean(branch_pred != branch_test$is_attributed))
  
}
