#Results table: Initialize
results_table=data.frame(file=numeric(), branch=numeric(), model=character(), predictors=character(), accuracy=numeric(), stringsAsFactors=FALSE)

for(file_num in c(0:1)){
  #Load Data
  origData=read.csv(file=paste("./data/t1p60_subsamples/sub_", file_num,".csv", sep=""))
  names(origData) = c('ip', 'app', 'device', 'os', 'channel','click_time', 'attributed_time', 'is_attributed')

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
  
    #Logistic Regression: Train Model
    branch_logreg = glm(is_attributed~ip+os+app+channel, data=branch_data, family = "binomial", subset = branch_index)
  
    #Logistic Regression: Test Model
    branch_probs=predict(branch_logreg, newdata=branch_test, type="response")
    branch_pred =rep(1, length(branch_probs))#Error
    branch_pred[branch_probs > 0.5] = 0
    branch_mean=mean(branch_pred != branch_test$is_attributed)
    
    #Logistic Regression: Save Result
    results_table=rbind(results_table, data.frame(file=file_num, branch=branch_num, model="Logistic Regression",predictors="ip+os+app+channel", accuracy=branch_mean))
  }
}
results_table
