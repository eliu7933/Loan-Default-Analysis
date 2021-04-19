# Loan-Default-Analysis

The overall purpose of this project is to investigate which variables of the Loan-Default dataset are the best predictors for determining whether a loan will default or not.

This README.md file contains information about the this Loan-Default project. The LoanDefaultCleaning.R file is the file used to clean the original loan default to exclude certain variables as well as feature engineer other variables so that a prediction analysis can be run. Methods used to clean the dataset included using spearman correlation establish a threshold for which variables to exclude. Imputation was also used with random forest to replace missing values. This file was created with the help of a statistican Matt Heaton.

The LoanDefaultAnalysis.R file is the file used to run prediction analysis. Target encoding methods were used to create dummy variables for certain categorical variables. The train function was then used with the ranger method and repeated cross-validation to further the prediction analysis.

The test_v2.csv and train_v2.csv files are the initial loan default datasets with data including over 700 variables that are not specified what those variables represent. These files are not found directly on this GitHub page because of the size of the file is not supported; however, do know that these files were used as part of the analysis.
