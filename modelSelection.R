#Constants
NUM_FILES = 1    #Number of files to use
BRANCH_TOTAL = 7 #Number of branches

#Best model per branch Table: Initialize
model_per_branch_table=data.frame(file=numeric(), branch=numeric(), model=character(), predictors=character(), accuracy=numeric(), stringsAsFactors=FALSE)

#Pick Models
for(file_num in c(1:NUM_FILES)){
  for(branch_num in c(1:BRANCH_TOTAL)){
      temp_table=pred_per_model_table[(pred_per_model_table==file_num)&(pred_per_model_table$branch==branch_num),]
      model_per_branch_table=rbind(model_per_branch_table,temp_table[which.max(temp_table$accuracy),])
  }
}
model_per_branch_table
