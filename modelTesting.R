library(leaps)
library(MASS)
library(class)

#Constants
NUM_FILES = 5    #Number of files to use
MODEL_TOTAL = 5  #Number of models per branch
BRANCH_TOTAL = 7 #Number of branches

#Results Table: Initialize
final_results_table=data.frame(branch=numeric(), model=character(), predictors=character(), accuracy=numeric(), stringsAsFactors=FALSE)

#Balance Table: Initialize
final_balance_table=data.frame(file=numeric(), branch=numeric(), human=numeric(), bot=numeric(), total=numeric(), humanPer=numeric(), botPer=numeric(), totalPer=numeric(), stringsAsFactors=FALSE)

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

#Load Data
origData=read.csv(file="./data/t3p20/ones.csv")
names(origData) = c('ip', 'app', 'device', 'os', 'channel','click_time', 'attributed_time', 'is_attributed')
for(file_num in c(0:NUM_FILES)){
  newData=read.csv(file=paste("./data/t3p20/zeros_", file_num,".csv", sep=""))
  names(newData) = c('ip', 'app', 'device', 'os', 'channel','click_time', 'attributed_time', 'is_attributed')
  origData=rbind(origData,newData)
}
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

  #Blance data
  human=nrow(branch_data[branch_data$is_attributed==1,])
  bot=nrow(branch_data[branch_data$is_attributed==0,])
  total=human+bot
  humanPer=human/total
  botPer=bot/total

  final_balance_table=rbind(final_balance_table, data.frame(file=file_num, branch=branch_num, human=human, bot=bot, total=total, humanPer=humanPer, botPer=botPer, totalPer=0))
  
  #Logistic Regression:
  if(model_per_branch_table[branch_num,]$model=="Logistic Regression"){
    #Logistic Regression: Assign Predictor
    pred=pred_per_model_table[(pred_per_model_table$model=="Logistic Regression")&(pred_per_model_table$file==BEST_FILE)&(pred_per_model_table$branch==branch_num),]$predictors
    
    msg=sprintf("Testing -> Branch: %d -> Predictor: %s -> Model: %s",branch_num, pred, "Logistic Regression")
    message(msg)
    
    #Logistic Regression: Test Model
    branch_probs=predict(testing_models[[branch_num]], newdata=branch_data, type="response")
    branch_pred =rep(1, length(branch_probs))#Error
    branch_pred[branch_probs > 0.5] = 0
    branch_mean=mean(branch_pred != branch_data$is_attributed)
    
    #Logistic Regression: Save Result
    final_results_table=rbind(final_results_table, data.frame(branch=branch_num, model="Logistic Regression", predictors=pred, accuracy=branch_mean))

  }else if(model_per_branch_table[branch_num,]$model=="LDA"){
    #LDA:
    #LDA: Assign Predictor
    pred=pred_per_model_table[(pred_per_model_table$model=="LDA")&(pred_per_model_table$file==BEST_FILE)&(pred_per_model_table$branch==branch_num),]$predictors
    
    msg=sprintf("Testing -> Branch: %d -> Predictor: %s -> Model: %s",branch_num, pred, "LDA")
    message(msg)
    
    #LDA: Test Model
    branch_probs=predict(testing_models[[branch_num]], newdata=branch_data)
    branch_pred =rep(1, length(branch_probs$posterior))#Error
    branch_pred[branch_probs$posterior[,2] > 0.5] = 0
    branch_mean=mean(branch_pred != branch_data$is_attributed)
    
    #LDA: Save Result
    final_results_table=rbind(final_results_table, data.frame(branch=branch_num, model="LDA",predictors=pred, accuracy=branch_mean))

  }else if(model_per_branch_table[branch_num,]$model=="QDA"){
    #QDA:
    #QDA: Assign Predictor
    pred=pred_per_model_table[(pred_per_model_table$model=="QDA")&(pred_per_model_table$file==BEST_FILE)&(pred_per_model_table$branch==branch_num),]$predictors
    
    msg=sprintf("Testing -> Branch: %d -> Predictor: %s -> Model: %s",branch_num, pred, "QDA")
    message(msg)
    
    #QDA: Test Model
    branch_probs=predict(testing_models[[branch_num]], newdata=branch_data)
    branch_pred =rep(1, length(branch_probs$posterior))#Error
    branch_pred[branch_probs$posterior[,2] > 0.5] = 0
    branch_mean=mean(branch_pred != branch_data$is_attributed)
    
    #QDA: Save Result
    final_results_table=rbind(final_results_table, data.frame(branch=branch_num, model="QDA",predictors=pred, accuracy=branch_mean))

  }else if(model_per_branch_table[branch_num,]$model=="SVM"){
    #SVM:
    #SVM: Assign Predictor
    pred=pred_per_model_table[(pred_per_model_table$model=="SVM")&(pred_per_model_table$file==BEST_FILE)&(pred_per_model_table$branch==branch_num),]$predictors
    
    msg=sprintf("Training and testing -> Branch: %d -> Predictor: %s -> Model: %s",branch_num, pred, "SVM")
    message(msg)
    
    #SVM: Test Model
    branch_probs=predict(testing_models[[branch_num]], newdata=branch_data)
    branch_pred =rep(1, length(branch_probs))#Error
    branch_pred[branch_probs > 0.5] = 0
    branch_mean=mean(branch_pred != branch_test$is_attributed)
    
    #SVM: Save Result
    final_results_table=rbind(final_results_table, data.frame(branch=branch_num, model="SVM",predictors=pred, accuracy=branch_mean))
  
  }else if(model_per_branch_table[branch_num,]$model=="NaiveBayes"){
    #NaiveBayes:
    #NaiveBayes: Assign Predictor
    pred=pred_per_model_table[(pred_per_model_table$model=="NaiveBayes")&(pred_per_model_table$file==BEST_FILE)&(pred_per_model_table$branch==branch_num),]$predictors
    
    msg=sprintf("Training and testing -> Branch: %d -> Predictor: %s -> Model: %s",branch_num, pred, "NaiveBayes")
    message(msg)
    
    #NaiveBayes: Test Model
    branch_probs=predict(testing_models[[branch_num]], newdata=branch_data)
    branch_pred =rep(1, length(branch_probs))#Error
    branch_mean=mean(branch_pred != branch_test$is_attributed)
    
    #NaiveBayes: Save Result
    final_results_table=rbind(final_results_table, data.frame(branch=branch_num, model="NaiveBayes",predictors=pred, accuracy=branch_mean))
  }else{
    final_results_table=rbind(final_results_table, data.frame(branch=branch_num, model="ERROR",predictors=pred, accuracy=0))
  }
}
final_balance_table$totalPer=(final_balance_table$human + final_balance_table$bot)/sum(final_balance_table$human + final_balance_table$bot)

#Total Accuracy
total_accuracy=sum(final_balance_table$totalPer * final_results_table$accuracy)
total_accuracy