library(leaps)
library(MASS)
library(class)

#Constants
NUM_FILES = 5    #Number of files to use
MODEL_TOTAL = 5  #Number of models per branch
BRANCH_TOTAL = 7 #Number of branches

#Results Table: Initialize
test_results_table=data.frame(branch=numeric(), model=character(), predictors=character(), accuracy=numeric(), TP=numeric(), FN=numeric(), FP=numeric(), TN=numeric(), accuracy_cMatrix=numeric(), stringsAsFactors=FALSE)

#Best model per branch Table: Initialize
model_per_branch_table=data.frame(file=numeric(), branch=numeric(), model=character(), predictors=character(), accuracy=numeric(), TP=numeric(), FN=numeric(), FP=numeric(), TN=numeric(), accuracy_cMatrix=numeric(), stringsAsFactors=FALSE)

testing_models=list()

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
origData=read.csv(file="./data/t2p20/ones.csv")
names(origData) = c('ip', 'app', 'device', 'os', 'channel','click_time', 'attributed_time', 'is_attributed')
for(file_num in c(0:NUM_FILES)){
  newData=read.csv(file=paste("./data/t2p20/zeros_", file_num,".csv", sep=""))
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
  
  #Logistic Regression:
  #Logistic Regression: Assign Predictor
  pred=pred_per_model_table[(pred_per_model_table$model=="Logistic Regression")&(pred_per_model_table$file==BEST_FILE)&(pred_per_model_table$branch==branch_num),]$predictors
  
  msg=sprintf("Testing -> Branch: %d -> Predictor: %s -> Model: %s",branch_num, pred, "Logistic Regression")
  message(msg)
  
  #Logistic Regression: Test Model
  branch_probs=predict(training_models[[(branch_num-1)*MODEL_TOTAL+1]], newdata=branch_data, type="response")
  branch_pred =rep(0, length(branch_probs))#Error
  branch_pred[branch_probs > 0.5] = 1
  branch_mean=mean(branch_pred == branch_data$is_attributed)
  branch_cMatrix=confusionMatrix(branch_pred, branch_data$is_attributed)
  
  #Logistic Regression: Save Result
  test_results_table=rbind(test_results_table, data.frame(branch=branch_num, model="Logistic Regression", predictors=pred, accuracy=branch_mean, TP=branch_cMatrix$table[1], FN=branch_cMatrix$table[2], FP=branch_cMatrix$table[3], TN=branch_cMatrix$table[4], accuracy_cMatrix=as.numeric(branch_cMatrix$overall["Accuracy"])))
  
  #LDA:
  #LDA: Assign Predictor
  pred=pred_per_model_table[(pred_per_model_table$model=="LDA")&(pred_per_model_table$file==BEST_FILE)&(pred_per_model_table$branch==branch_num),]$predictors
  
  msg=sprintf("Testing -> Branch: %d -> Predictor: %s -> Model: %s",branch_num, pred, "LDA")
  message(msg)
  
  #LDA: Test Model
  branch_probs=predict(training_models[[(branch_num-1)*MODEL_TOTAL+2]], newdata=branch_data)
  branch_pred =rep(0, nrow(branch_probs$posterior))#Error
  branch_pred[branch_probs$posterior[,2] > 0.5] = 1
  branch_mean=mean(branch_pred == branch_data$is_attributed)
  branch_cMatrix=confusionMatrix(branch_pred, branch_data$is_attributed)
  
  #LDA: Save Result
  test_results_table=rbind(test_results_table, data.frame(branch=branch_num, model="LDA",predictors=pred, accuracy=branch_mean, TP=branch_cMatrix$table[1], FN=branch_cMatrix$table[2], FP=branch_cMatrix$table[3], TN=branch_cMatrix$table[4], accuracy_cMatrix=as.numeric(branch_cMatrix$overall["Accuracy"])))
  
  #QDA:
  #QDA: Assign Predictor
  pred=pred_per_model_table[(pred_per_model_table$model=="QDA")&(pred_per_model_table$file==BEST_FILE)&(pred_per_model_table$branch==branch_num),]$predictors
  
  msg=sprintf("Testing -> Branch: %d -> Predictor: %s -> Model: %s",branch_num, pred, "QDA")
  message(msg)
  
  #QDA: Test Model
  branch_probs=predict(training_models[[(branch_num-1)*MODEL_TOTAL+3]], newdata=branch_data)
  branch_pred =rep(0, nrow(branch_probs$posterior))#Error
  branch_pred[branch_probs$posterior[,2] > 0.5] = 1
  branch_mean=mean(branch_pred == branch_data$is_attributed)
  branch_cMatrix=confusionMatrix(branch_pred, branch_data$is_attributed)
  
  #QDA: Save Result
  test_results_table=rbind(test_results_table, data.frame(branch=branch_num, model="QDA",predictors=pred, accuracy=branch_mean, TP=branch_cMatrix$table[1], FN=branch_cMatrix$table[2], FP=branch_cMatrix$table[3], TN=branch_cMatrix$table[4], accuracy_cMatrix=as.numeric(branch_cMatrix$overall["Accuracy"])))
  
  #SVM:
  #SVM: Assign Predictor
  pred=pred_per_model_table[(pred_per_model_table$model=="SVM")&(pred_per_model_table$file==BEST_FILE)&(pred_per_model_table$branch==branch_num),]$predictors
  
  msg=sprintf("Testing -> Branch: %d -> Predictor: %s -> Model: %s",branch_num, pred, "SVM")
  message(msg)
  
  #SVM: Test Model
  branch_probs=predict(training_models[[(branch_num-1)*MODEL_TOTAL+4]], newdata=branch_data)
  branch_pred =rep(0, length(branch_probs))#Error
  branch_pred[branch_probs > 0.5] = 1
  branch_mean=mean(branch_pred == branch_test$is_attributed)
  branch_cMatrix=confusionMatrix(branch_pred, branch_data$is_attributed)
  
  #SVM: Save Result
  test_results_table=rbind(test_results_table, data.frame(branch=branch_num, model="SVM",predictors=pred, accuracy=branch_mean, TP=branch_cMatrix$table[1], FN=branch_cMatrix$table[2], FP=branch_cMatrix$table[3], TN=branch_cMatrix$table[4], accuracy_cMatrix=as.numeric(branch_cMatrix$overall["Accuracy"])))
  
  #NaiveBayes:
  #NaiveBayes: Assign Predictor
  pred=pred_per_model_table[(pred_per_model_table$model=="NaiveBayes")&(pred_per_model_table$file==BEST_FILE)&(pred_per_model_table$branch==branch_num),]$predictors
  
  msg=sprintf("Testing -> Branch: %d -> Predictor: %s -> Model: %s",branch_num, pred, "NaiveBayes")
  message(msg)
  
  #NaiveBayes: Test Model
  branch_probs=predict(training_models[[(branch_num-1)*MODEL_TOTAL+5]], newdata=branch_data)
  branch_pred =branch_probs
  branch_mean=mean(branch_pred == branch_test$is_attributed)
  branch_cMatrix=confusionMatrix(branch_pred, branch_data$is_attributed)
  
  #NaiveBayes: Save Result
  test_results_table=rbind(test_results_table, data.frame(branch=branch_num, model="NaiveBayes",predictors=pred, accuracy=branch_mean, TP=branch_cMatrix$table[1], FN=branch_cMatrix$table[2], FP=branch_cMatrix$table[3], TN=branch_cMatrix$table[4], accuracy_cMatrix=as.numeric(branch_cMatrix$overall["Accuracy"])))
  
  if(FALSE){
    #KNN
    pred=pred_per_model_table[(pred_per_model_table$model=="KNN 12")&(pred_per_model_table$file==BEST_FILE)&(pred_per_model_table$branch==branch_num),]$predictors
    
    msg=sprintf("Testing -> Branch: %d -> Predictor: %s -> Model: %s",branch_num, pred, "KNN 12")
    message(msg)
    
    #KNN: Test Model
    train.Attributed=cbind(branch_data$is_attributed[branch_index])
    predix = unlist(pred_rawlist[[i]])
    branch_knn = training_models[[(branch_num-1)*4+4]]
    branch_mean=mean(branch_knn == branch_test$is_attributed)
    
    #KNN: Save Result
    test_results_table=rbind(test_results_table, data.frame(branch=branch_num, model="KNN 12", predictors=pred, accuracy=branch_mean))
  }
}

#Pick Models
for(branch_num in c(1:BRANCH_TOTAL)){
  temp_table=test_results_table[(test_results_table$branch==branch_num),]
  #model_per_branch_table=rbind(model_per_branch_table,temp_table[which.max(temp_table$accuracy_cMatrix),])
  #testing_models[[branch_num]] = training_models[[(branch_num-1)*MODEL_TOTAL+which(unique(test_results_table$model) %in% temp_table[which.max(temp_table$accuracy_cMatrix),]$model)]]
  model_per_branch_table=rbind(model_per_branch_table,temp_table[which.max(temp_table$accuracy),])
  testing_models[[branch_num]] = training_models[[(branch_num-1)*MODEL_TOTAL+which(unique(test_results_table$model) %in% temp_table[which.max(temp_table$accuracy),]$model)]]
}
model_per_branch_table

write.csv(test_results_table, file="./results/test_results_table.csv")
write.csv(model_per_branch_table, file="./results/model_per_branch_table.csv")
