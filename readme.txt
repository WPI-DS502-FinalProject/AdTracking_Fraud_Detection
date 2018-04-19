Notes:
1. Set working directory in R to file location
2. Make sure data folder is present and populated with t1p60, t2p20, t3p20
3. Execute scripts in specified order, or you have results folder is populated with appropriate tables need.

Execution:
1. modelTraining.R: Loads 400k training set, generates a table of the top 8 predictor combinations for each of the 5 models for each of the 7 branchs
2. predictorSelection.R: Generates a table of the best predictor for each of the 5 models for each of the 7 branches
3. modelTrainingSave.R: Saves the trained models to a array list
4. modelSelection.R: Loads the 20% validation set, generates a table of the best model for each of the 7 branches
5. modelTesting.R: Loads the 20% test set, generates a table of the accuracy of each model of the 7 branches
6. save_table: Save all results table in current workspace to results folder
