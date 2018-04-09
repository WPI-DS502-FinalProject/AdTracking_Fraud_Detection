#Constants
NUM_FILES=1         #Number of files to use
PER = 0.8           #Percent for training/testing set
IP_THRESHOLD=213413 #IP threshold

pred_list=c("is_attributed~ip", "is_attributed~ip+os", "is_attributed~ip+os+app", "is_attributed~ip+os+app+channel")

#Results Table: Initialize
results_table=data.frame(file=numeric(), branch=numeric(), model=character(), predictors=character(), accuracy=numeric(), stringsAsFactors=FALSE)

for(file_num in c(0:NUM_FILES)){
  #Load Data
  origData=read.csv(file=paste("./data/t1p60_subsamples/sub_", file_num,".csv", sep=""))
  names(origData) = c('ip', 'app', 'device', 'os', 'channel','click_time', 'attributed_time', 'is_attributed')

  for(branch_num in c(1:8)){
    #Create Branches:
    branch_data = switch(
      branch_num,
      origData[((origData$app< 18.5) & (origData$channel< 114.5) & (origData$channel< 112)),],
      origData[((origData$app< 18.5) & (origData$channel< 114.5) & (origData$channel>=112)),],
      origData[((origData$app< 18.5) & (origData$channel>=114.5) & (origData$ip< IP_THRESHOLD)),],
      origData[((origData$app< 18.5) & (origData$channel>=114.5) & (origData$ip>=IP_THRESHOLD)),],
      origData[((origData$app>=18.5) & (origData$app< 19.5)),],
      origData[((origData$app>=18.5) & (origData$app>=19.5) & (origData$app< 28.5)),],
      origData[((origData$app>=18.5) & (origData$app>=19.5) & (origData$app>=28.5) & (origData$channel< 345)),],
      origData[((origData$app>=18.5) & (origData$app>=19.5) & (origData$app>=28.5) & (origData$channel>=345)),])
  
    #Split data: training and testing set
    branch_index = sample(floor(nrow(branch_data) * PER))
    branch_train = branch_data[branch_index,]
    branch_test  = branch_data[-branch_index,]
  
    for(i in c(1:length(pred_list))){
      #Logistic Regression: Train Model
      branch_logreg = glm(pred_list[i], data=branch_data, family = "binomial", subset = branch_index)
    
      #Logistic Regression: Test Model
      branch_probs=predict(branch_logreg, newdata=branch_test, type="response")
      branch_pred =rep(1, length(branch_probs))#Error
      branch_pred[branch_probs > 0.5] = 0
      branch_mean=mean(branch_pred != branch_test$is_attributed)
      
      #Logistic Regression: Save Result
      results_table=rbind(results_table, data.frame(file=file_num, branch=branch_num, model="Logistic Regression",predictors=substring(pred_list[i], 15), accuracy=branch_mean))
    }
  }
}
results_table
