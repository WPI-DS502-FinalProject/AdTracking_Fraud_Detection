#Output of: modelTraining.R
write.csv(results_table, file="./results/results_table.csv")
write.csv(balance_table, file="./results/balance_table.csv")

#Output of: predictorSelection.R
write.csv(pred_per_model_table, file="./results/pred_per_model_table.csv")
write.csv(avr_per_file_table, file="./results/avr_per_file_table.csv")

#Output of: modelTrainingSave.R
#write.csv(training_models, file="./results/training_models.csv")

#Output of: modelSelection.R
write.csv(test_results_table, file="./results/test_results_table.csv")
write.csv(model_per_branch_table, file="./results/model_per_branch_table.csv")
#write.csv(testing_models, file="./results/testing_models.csv")

#Output of: modelTesting.R
write.csv(final_balance_table, file="./results/final_balance_table.csv")
write.csv(final_results_table, file="./results/final_results_table.csv")

#Values
final_values=data.frame(BEST_FILE=BEST_FILE, total_accuracy=total_accuracy)
write.csv(final_values, file="./results/final_values.csv")
