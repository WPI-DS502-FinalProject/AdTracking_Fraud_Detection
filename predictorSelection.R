#Constants
NUM_FILES = 1    #Number of files to use
BRANCH_TOTAL = 7 #Number of branches

#Best Predictor per model Table: Initialize
pred_per_model_table=data.frame(file=numeric(), branch=numeric(), model=character(), predictors=character(), accuracy=numeric(), stringsAsFactors=FALSE)

#Average accuracy per file Table: Initialize
avr_per_file_table=data.frame(file=numeric(), accuracy=numeric(), stringsAsFactors=FALSE)

#Pick Predictors Per File
model_list=unique(results_table$model)
for(file_num in c(0:NUM_FILES)){
  for(branch_num in c(1:BRANCH_TOTAL)){
    for(model_num in c(1:length(model_list))){
      temp_table=results_table[(results_table==file_num)&(results_table$branch==branch_num)&(results_table$model==model_list[model_num]),]
      pred_per_model_table=rbind(pred_per_model_table,temp_table[which.max(temp_table$accuracy),])
    }
  }
}

#Pick Best file
for(file_num in c(0:NUM_FILES)){
  avr_per_file_table=rbind(avr_per_file_table, data.frame(file=file_num, accuracy=mean(pred_per_model_table[pred_per_model_table$file==file_num,]$accuracy)))
}

BEST_FILE=which.max(avr_per_file_table$accuracy)-1

write.csv(pred_per_model_table, file="./results/pred_per_model_table.csv")
write.csv(avr_per_file_table, file="./results/avr_per_file_table.csv")
