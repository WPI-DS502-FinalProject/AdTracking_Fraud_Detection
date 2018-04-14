library(leaps)
library(MASS)
library(class)

#Constants
BRANCH_TOTAL = 7 #Number of branches

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
origData=read.csv(file=paste("./data/t1p60_subsamples/sub_", BEST_FILE,".csv", sep=""))
names(origData) = c('ip', 'app', 'device', 'os', 'channel','click_time', 'attributed_time', 'is_attributed')
origData <- extractFeature(origData) 

training_models=list()
pred_rawlist=list()

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
  
  #Split data: training and testing set
  branch_index = sample(floor(nrow(branch_data) * PER))
  branch_train = branch_data[branch_index,]
  
  #Logistic Regression:
  #Logistic Regression: Assign Predictor
  pred=pred_per_model_table[(pred_per_model_table$model=="Logistic Regression")&(pred_per_model_table$file==BEST_FILE)&(pred_per_model_table$branch==branch_num),]$predictors
  
  msg=sprintf("Training and testing -> Branch: %d -> Predictor: %s -> Model: %s",branch_num, pred, "Logistic Regression")
  message(msg)
  
  #Logistic Regression: Train Model
  branch_logreg = glm(paste("is_attributed~",pred,collapse=""), data=branch_data, family = "binomial", subset = branch_index)
  
  #Logistic Regression: Save Model
  training_models[[(branch_num-1)*4+1]]=branch_logreg

  #LDA:
  #LDA: Assign Predictor
  pred=pred_per_model_table[(pred_per_model_table$model=="LDA")&(pred_per_model_table$file==BEST_FILE)&(pred_per_model_table$branch==branch_num),]$predictors
  
  msg=sprintf("Training and testing -> Branch: %d -> Predictor: %s -> Model: %s",branch_num, pred, "LDA")
  message(msg)
  
  #LDA: Train Model
  branch_LDA=lda(as.formula(paste("is_attributed~",pred,collapse="")), data=branch_data, subset=branch_index)
  
  #LDA: Save Result
  training_models[[(branch_num-1)*4+2]]=branch_LDA
  
  #QDA:
  #QDA: Assign Predictor
  pred=pred_per_model_table[(pred_per_model_table$model=="QDA")&(pred_per_model_table$file==BEST_FILE)&(pred_per_model_table$branch==branch_num),]$predictors
  
  msg=sprintf("Training and testing -> Branch: %d -> Predictor: %s -> Model: %s",branch_num, pred, "QDA")
  message(msg)
  
  #QDA: Train Model
  branch_QDA=qda(as.formula(paste("is_attributed~",pred,collapse="")), data=branch_data, subset=branch_index)
  

  #QDA: Save Result
  training_models[[(branch_num-1)*4+3]]=branch_QDA
  
  # KNN
  pred=pred_per_model_table[(pred_per_model_table$model=="KNN 12")&(pred_per_model_table$file==BEST_FILE)&(pred_per_model_table$branch==branch_num),]$predictors
  
  msg=sprintf("Training and testing -> Branch: %d -> Predictor: %s -> Model: %s",branch_num, pred, "KNN 12")
  message(msg)
    
  train.Attributed=cbind(branch_data$is_attributed[branch_index])
  predix = paste(pred,collapse="")
  branch_knn = knn(branch_train[predix], branch_test[predix], train.Attributed, k = k)

  #KNN: Save Result
  training_models[[(branch_num-1)*4+4]]=branch_knn
}

