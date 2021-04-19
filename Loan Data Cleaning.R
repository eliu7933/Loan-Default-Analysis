# Libraries
library(tidyverse)
library(vroom)
library(caret)
library(DataExplorer)

# Read in data
loan_train <- vroom::vroom("~/Documents/GitHub Projects/Loan-Default-Analysis/train_v2.csv")
loan_test <- vroom::vroom("~/Documents/GitHub Projects/Loan-Default-Analysis/test_v2.csv")
loan_data <- bind_rows(loan_train, loan_test)

# Deal with missing data
# plot_missing(loan_data)
# one_half <- loan_data[,1:350] # test with first half
loan_data[is.na(loan_data)] = 0 # sum(is.na(one_half)) counts how many NAs, there should be zero
loan_data[colSums(loan_data) == 0] # find columns where they only contain zeros
loan_data <- loan_data %>% select(-c(f33,f34,f35,f37,f38,f678,f700,f701,f702)) # remove those columns

# preProcess notes
# method: BoxCox: effective for generating power predictions; only positive
#         BoxTidwell: estimate transformations of predictor variables
#         YeoJohnson: similar to BoxCox but can accommodate zero/negative values
# method: center: subtracts the mean of predictor data from the predictor values
#         scale: divides by standard deviation
#         range: scales the data to be within rangeBounds argument
# method: corr: filter out highly correlated data
#         conditionalX: examines the distribution of each predictor conditional on the outcome. 
#         If there is only one unique value within any class, the predictor is excluded from further calculations.
#         Note that is is time consuming
# Order of operation
# Zero-variance filter, near-zero variance filter
# correlation filter
# Box-Cox/Yeo-Johnson/exponential transformation, 
# centering, scaling, range, 
# imputation, PCA, ICA then spatial sign.

my_preProcess <- preProcess(x = loan_data, method = c("corr", "YeoJohnson", "center"),
                            pcaComp = 50)

loan_data <- predict(my_preProcess, newdata = loan_data)
print(loan_data)
# dim(predictions)
corr <- cor(x = loan_data %>% select(-loss, -id, -Set),
            y = loan_data %>% select (loss),
            use = "complete.obs",
            method = "pearson") # default




##################### Dr. Heaton's Data Cleaning #####################

# preProcess(x = dataset , method = "zv"); throw out zero-variance explanatory variables (or nzv)
# look at the correlations between the difference variables
# cor(); select(-loss, -id, -Set)







