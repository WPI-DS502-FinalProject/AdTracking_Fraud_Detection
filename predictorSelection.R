#Constants
NUM_FILES = 1    #Number of files to use
BRANCH_TOTAL = 7 #Number of branches

#Best Predictor per model Table: Initialize
pred_per_model_table=data.frame(file=numeric(), branch=numeric(), model=character(), predictors=character(), accuracy=numeric(), stringsAsFactors=FALSE)

#Pick Predictors Per File
model_list=unique(results_table$model)
for(file_num in c(1:NUM_FILES)){
  for(branch_num in c(1:BRANCH_TOTAL)){
    for(model_num in c(1:length(model_list))){
      temp_table=results_table[(results_table==file_num)&(results_table$branch==branch_num)&(results_table$model==model_list[model_num]),]
      pred_per_model_table=rbind(pred_per_model_table,temp_table[which.max(temp_table$accuracy),])
    }
  }
}

#Pick Best file
for(file_num in c(1:NUM_FILES)){
  accuracy_list=average(pred_per_model_table[pred_per_model_table$file==0,])
}