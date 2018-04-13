#Constants
NUM_FILES = 1    #Number of files to use
BRANCH_TOTAL = 7 #Number of branches

#Best model per branch Table: Initialize
model_per_branch_table=data.frame(file=numeric(), branch=numeric(), model=character(), predictors=character(), accuracy=numeric(), stringsAsFactors=FALSE)

extractFeature <- function(origData){
  origData$attributed_time <- NULL
  origData$hour <- as.numeric(format(as.POSIXct(origData$click_time) ,format = "%H"))
  origData$ip_app <- (origData$ip + 1) * origData$app
  origData$channel_app <- (origData$channel + 1) * origData$app
  origData$channel_ip <- (origData$channel + 1) * origData$ip
  origData$channel_ip_app <- (origData$channel_ip + 1) * origData$app
  origData$click_time <- NULL
  return (origData)
}

for(file_num in c(1:NUM_FILES)){
  #Load Data
  if(file_num=1)  origData=read.csv(file="./data/t2p20/ones.csv")
  else            origData=read.csv(file=paste("./data/t2p20/zeros_", file_num,".csv", sep=""))
  
  names(origData) = c('ip', 'app', 'device', 'os', 'channel','click_time', 'attributed_time', 'is_attributed')
  origData <- extractFeature(origData) 
  
  for(branch_num in c(1:BRANCH_TOTAL)){
    #Create Branches:
    branch_data = switch(
      branch_num,
      origData[((origData$app< 18.5) & (origData$channel< 114.5) & (origData$channel< 112)),],
      origData[((origData$app< 18.5) & (origData$channel< 114.5) & (origData$channel>=112)),],
      origData[((origData$app< 18.5) & (origData$channel>=114.5)),],
      origData[((origData$app>=18.5) & (origData$app< 19.5)),],
      origData[((origData$app>=18.5) & (origData$app>=19.5) & (origData$app< 28.5)),],
      origData[((origData$app>=18.5) & (origData$app>=19.5) & (origData$app>=28.5) & (origData$channel< 345)),],
      origData[((origData$app>=18.5) & (origData$app>=19.5) & (origData$app>=28.5) & (origData$channel>=345)),])
    
    #Training and Testing Models: using subset selection predictor list
    for(i in c(1:length(pred_list))){
      #Logistic Regression:
      msg=sprintf("Training and testing -> Branch: %d -> Predictor %d: %s -> Model: %s",branch_num, i, pred_list[i], "Logistic Regression")
      message(msg)
      
      #Logistic Regression: Train Model
      branch_logreg = glm(pred_list[i], data=branch_data, family = "binomial", subset = branch_index)
      
      #Logistic Regression: Test Model
      branch_probs=predict(branch_logreg, newdata=branch_test, type="response")
      branch_pred =rep(1, length(branch_probs))#Error
      branch_pred[branch_probs > 0.5] = 0
      branch_mean=mean(branch_pred != branch_test$is_attributed)
      
      #Logistic Regression: Save Result
      results_table=rbind(results_table, data.frame(file=file_num, branch=branch_num, model="Logistic Regression",predictors=substring(pred_list[i], 15), accuracy=branch_mean))
      
      #LDA:
      msg=sprintf("Training and testing -> Branch: %d -> Predictor %d: %s -> Model: %s",branch_num, i, pred_list[i], "LDA")
      message(msg)
      
      #LDA: Train Model
      branch_LDA=lda(as.formula(pred_list[i]), data=branch_data, subset=branch_index)
      
      #LDA: Test Model
      branch_probs=predict(branch_LDA, newdata=branch_test)
      branch_pred =rep(1, length(branch_probs$posterior))#Error
      branch_pred[branch_probs$posterior[,2] > 0.5] = 0
      branch_mean=mean(branch_pred != branch_test$is_attributed)
      
      #LDA: Save Result
      results_table=rbind(results_table, data.frame(file=file_num, branch=branch_num, model="LDA",predictors=substring(pred_list[i], 15), accuracy=branch_mean))
      
      #QDA:
      msg=sprintf("Training and testing -> Branch: %d -> Predictor %d: %s -> Model: %s",branch_num, i, pred_list[i], "QDA")
      message(msg)
      
      #QDA: Train Model
      branch_QDA=qda(as.formula(pred_list[i]), data=branch_data, subset=branch_index)
      
      #QDA: Test Model
      branch_probs=predict(branch_QDA, newdata=branch_test)
      branch_pred =rep(1, length(branch_probs$posterior))#Error
      branch_pred[branch_probs$posterior[,2] > 0.5] = 0
      branch_mean=mean(branch_pred != branch_test$is_attributed)
      
      #QDA: Save Result
      results_table=rbind(results_table, data.frame(file=file_num, branch=branch_num, model="QDA",predictors=substring(pred_list[i], 15), accuracy=branch_mean))
      
      # KNN
      #      msg=sprintf("Training and testing -> Branch: %d -> Predictor %d: %s -> Model: %s",branch_num, i, pred_list[i], "KNN")
      #      message(msg)
      # How do we can select the K in this case? # This is a special case. We need to build a good framework
      #      train.Attributed=cbind(branch_data$is_attributed[branch_index])
      #      branch_knn = knn(branch_train, branch_test, train.Attributed, k = 1)
      #      branch_mean=mean(branch_knn == branch_test$is_attributed)
      
      #KNN: Save Result
      #      results_table=rbind(results_table, data.frame(file=file_num, branch=branch_num, model="KNN",predictors=substring(pred_list[i], 15), accuracy=branch_mean))
      
    }
  }
}



#Pick Models
for(file_num in c(1:NUM_FILES)){
  for(branch_num in c(1:BRANCH_TOTAL)){
      temp_table=pred_per_model_table[(pred_per_model_table==file_num)&(pred_per_model_table$branch==branch_num),]
      model_per_branch_table=rbind(model_per_branch_table,temp_table[which.max(temp_table$accuracy),])
  }
}
model_per_branch_table
