library(leaps)
library(MASS)
library(class)
library(e1071)

#Constants
NUM_FILES = 1    #Number of files to use
PER = 0.8        #Percent for training/testing set
BRANCH_TOTAL = 7 #Number of branches

#Results Table: Initialize
results_table=data.frame(file=numeric(), branch=numeric(), model=character(), predictors=character(), accuracy=numeric(), stringsAsFactors=FALSE)

#Balance Table: Initialize
balance_table=data.frame(file=numeric(), branch=numeric(), human=numeric(), bot=numeric(), total=numeric(), humanPer=numeric(), botPer=numeric(), stringsAsFactors=FALSE)

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

for(file_num in c(0:NUM_FILES)){
  #Load Data
  origData=read.csv(file=paste("./data/t1p60_subsamples/sub_", file_num,".csv", sep=""))
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
    
    #Blance data
    human=nrow(branch_data[branch_data$is_attributed==1,])
    bot=nrow(branch_data[branch_data$is_attributed==0,])
    total=human+bot
    humanPer=human/total
    botPer=bot/total
    
    balance_table=rbind(balance_table, data.frame(file=file_num, branch=branch_num, human=human, bot=bot, total=total, humanPer=humanPer, botPer=botPer))
    
    #Split data: training and testing set
    branch_index = sample(floor(nrow(branch_data) * PER))
    branch_train = branch_data[branch_index,]
    branch_test  = branch_data[-branch_index,]
    
    #Subset Selection: Determining Predictors to use
    regfit = regsubsets(is_attributed ~ ., data = branch_train)
    regfit.summary = summary(regfit)
    
    if(FALSE){
      #Subset Selection: Determining how many predictors to use
      pred_num_adjr2=which.max(regfit.summary$adjr2)
      pred_num_cp=which.min(regfit.summary$cp)
      pred_num_bic=which.min(regfit.summary$bic)
      
      pred_num = min(pred_num_adjr2, pred_num_cp, pred_num_bic)
      png(filename=paste("./img/subsetSel_F", file_num, "_B", branch_num, ".png", sep=""))
      
      par(mfrow=c(2,2))
      plot(regfit.summary$rss ,xlab="Number of Variables ",ylab="RSS",type="l")
      
      plot(regfit.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
      points(pred_num_adjr2,regfit.summary$adjr2[pred_num_adjr2], col="red",cex=2,pch=20)
      
      plot(regfit.summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
      points(pred_num_cp,regfit.summary$cp[pred_num_cp],col="red",cex=2,pch=20)
      
      plot(regfit.summary$bic ,xlab="Number of Variables ",ylab="BIC",type='l')
      points(pred_num_bic,regfit.summary$bic[pred_num_bic],col="red",cex=2,pch=20)
      
      dev.off()
    }
    
    #Subset Selection: Building list of chosen Predictors
    pred_list=character()
    pred_rawlist=list()
    for(pred_index in c(1:length(regfit.summary$outmat[, 1]))){
      inds=which(regfit.summary$outmat[pred_index,] %in% c("*"))
      pred_string=paste("is_attributed~", paste(colnames(regfit.summary$outmat)[inds], collapse = '+'), sep="")
      pred_rawlist[[pred_index]]=as.list(colnames(regfit.summary$outmat)[inds])
      pred_list=c(pred_list, pred_string)
    }
    
    #Training and Testing Models: using subset selection predictor list
    for(i in c(1:length(pred_list))){
    tryCatch({

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
      results_table=rbind(results_table, data.frame(file=file_num, branch=branch_num, model="Logistic Regression", predictors=substring(pred_list[i], 15), accuracy=branch_mean))
    }, error = function(e) {message("Logistic Failed")})
    tryCatch({
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
    }, error = function(e) {message("LDA Failed")})
    tryCatch({
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
    }, error = function(e) {message("QDA Failed")})
      
      tryCatch({
        #SVM:
        msg=sprintf("Training and testing -> Branch: %d -> Predictor %d: %s -> Model: %s",branch_num, i, pred_list[i], "SVM")
        message(msg)
        
        #SVM: Train Model
        #branch_SVM=svm(as.formula(pred_list[i]), data=branch_data, subset=branch_index)
        branch_SVM=glm(as.formula(pred_list[i]), data=branch_data, subset=branch_index)
        
        #SVM: Test Model
        branch_probs=predict(branch_SVM, newdata=branch_test)
        branch_pred =rep(1, length(branch_probs))#Error
        branch_pred[branch_probs > 0.5] = 0
        branch_mean=mean(branch_pred != branch_test$is_attributed)
        
        #SVM: Save Result
        results_table=rbind(results_table, data.frame(file=file_num, branch=branch_num, model="SVM",predictors=substring(pred_list[i], 15), accuracy=branch_mean))
      }, error = function(e) {message("SVM Failed")})
      
      tryCatch({
        #NaiveBayes:
        msg=sprintf("Training and testing -> Branch: %d -> Predictor %d: %s -> Model: %s",branch_num, i, pred_list[i], "NaiveBayes")
        message(msg)
        
        #NaiveBayes: Train Model
        branch_NB <- naiveBayes(as.factor(is_attributed)~app+device+os+channel+hour+channel_app+channel_ip+channel_ip_app, data = branch_train, subset = branch_index)
        branch_NB <- naiveBayes(paste("as.factor(is_attributed) ~ app + device + os + channel + hour + channel_app + channel_ip + channel_ip_app"), data = branch_train, subset = branch_index)
        
        #NaiveBayes: Test Model
        branch_probs=predict(branch_NB, newdata=branch_test)
        branch_pred =rep(1, length(branch_probs))#Error
        branch_pred[branch_probs > 0.5] = 0
        branch_mean=mean(branch_pred != branch_test$is_attributed)
        
        #NaiveBayes: Save Result
        results_table=rbind(results_table, data.frame(file=file_num, branch=branch_num, model="NaiveBayes",predictors=substring(pred_list[i], 15), accuracy=branch_mean))
        
      }, error = function(e) {message("NaiveBayes Failed")})  
      
      if(FALSE){
      for(k in 12:12){
      # KNN
      tryCatch({      
        msg=sprintf("Training and testing -> Branch: %d -> Predictor %d: %s -> Model: %s%d",branch_num, i, pred_list[i], "KNN", k)
        message(msg)
        
        train.Attributed=cbind(branch_data$is_attributed[branch_index])
        predix = unlist(pred_rawlist[[i]])
        branch_knn = knn(branch_train[predix], branch_test[predix], train.Attributed, k = k)
        branch_mean=mean(branch_knn == branch_test$is_attributed)
        
        #KNN: Save Result
        results_table=rbind(results_table, data.frame(file=file_num, branch=branch_num, model=paste("KNN", k),predictors=substring(pred_list[i], 15), accuracy=branch_mean))
      }, error = function(e) {message("KNN Failed")})
      }
      }
    }
  }
}
