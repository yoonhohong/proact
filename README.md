# ALSMaster
I am studying "A model for predicting survival and loss of physical functions in ALS".


Now, there are 8 scripts.
The order of scrips is "SurvivalTable", "Datacleaning", "Imputation", "CV_cox","CV_coxboost","CV_randomforest", "Variable_ importance","Clustering".

"SurvivalTable" is for making a table for survival analysis and plotting K-M curves for 5 domains.

"Datacleaning" is for adding features.

"Imputation" is for imputation.

"CV_cox", "CV_coxboost", "CV_randomforest" are for making models and validating the performances by cross-validation. Each one uses cox, coxboost, randomforest, respectively.

"Variable_importance" is for calculating variable importance and plotting examples of predicted time-to-event curves.

"Clustering" is for integrating 5 models' results and clustering patietns based on probability at 12 month in 5 domains. It is ongoing.
