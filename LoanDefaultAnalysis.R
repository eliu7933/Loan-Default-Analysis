##### LoanDefaultAnalysis #####
library(tidyverse)
library(DataExplorer)
library(caret)
library(vroom)
library(party)

### Read in data ###
loan <- vroom::vroom("~/Documents/GitHub Projects/Loan-Default-Analysis/FullDefaultData.csv")
default <- vroom::vroom("~/Documents/GitHub Projects/Loan-Default-Analysis/default.csv")

### Feature Engineering ###
train <- default %>% filter(Set == "train") # use this in fitting the model
test <- default %>% filter(Set == "test") # use this in the predictions


#################################### FIRST SUBMISSION ################################

data <- train1

t1 <- data.frame(ncol = ncol(loan), nrow = nrow(data))
t1 <- vector(length = ncol(data))
for (i in 1:ncol(data)) {
  t1[i] <- count(unique(data[,i]))
}

colnames(t1) <- c("id", seq(from = 2, to = ncol(data), by = 1))

x %>% arrange(desc(x)) # count of uniqe variables

# 3, 13, 14, 21, 31
loan[,3]
loan[,13]
loan[,21]
loan[,31]


### Exploratory Plots ###
# histograms
# correlation plots

plot_correlation(loan, type = "continuous",
                 cor_args=list(use = "complete.obs"))


### Target encoding ###
# dummy variables
default$f13 <- lm(loss ~ f13, data = default) %>%
  predict(., newdata = default %>% select(-loss))

default$f24 <- lm(loss ~ f24, data = default) %>%
  predict(., newdata = default %>% select(-loss))

default$f36 <- lm(loss ~ f36, data = default) %>%
  predict(., newdata = default %>% select(-loss))

default$f58 <- lm(loss ~ f58, data = default) %>%
  predict(., newdata = default %>% select(-loss))

##### Fit models #####

loan.model <- train(form = loss ~ f13 + f24 + f36 + f58,
                    data = default %>% filter(Set == 'train'), # fit model with train dataset
                    method = "ranger",
                    tuneLength = 2,
                    trControl = trainControl(
                      method = "repeatedcv",
                      number = 10,
                      repeats = 2) #repeats are for repeatedcv
)

preds <- predict(train1.model, newdata = train1 %>% filter(Set == "test")) # draw predictions with test dataset
submission <- data.frame(default %>% filter(Set == "test"),
                         count = preds)

df <- submission %>% select("datetime.id", "datetime.loss")
colnames(df) <- c("id", "loss")
df$id <- as.integer(df$id)

write.csv(x = submission, file="~/Documents/GitHub Projects/Loan-Default-Analysis/Submissions.csv", row.names = FALSE) 


#################################### SECOND SUBMISSION ################################

##### Fit models #####

# Create 5 different datasets that will be fitted
train1 <- train[,c(3,773,4:153)]
train2 <- train[,c(3,773,154:304)]
train3 <- train[,c(3,773,305:455)]
train4 <- train[,c(3,773,456:606)]
train5 <- train[,c(3,773,607:772)]

test1 <- test[,c(3,773,4:153)]
test2 <- test[,c(3,773,154:304)]
test3 <- test[,c(3,773,305:455)]
test4 <- test[,c(3,773,456:606)]
test5 <- test[,c(3,773,607:772)]

# train1
tain1.model <- train(form = loss ~.,
                    data = train1, # fit model with train dataset
                    method = "rf",
                    tuneLength = 2,
                    trControl = trainControl(
                      method = "cforest",
                      mtry = 5,
                      number = 10) #repeats are for repeatedcv
)

preds <- predict(train1.model, newdata = test1) # draw predictions with test dataset
submission <- data.frame(test1, count = preds)

submission1 <- vroom::vroom("~/Documents/GitHub Projects/Loan-Default-Analysis/test1.csv")
submission2 <- vroom::vroom("~/Documents/GitHub Projects/Loan-Default-Analysis/test2.csv")
submission3 <- vroom::vroom("~/Documents/GitHub Projects/Loan-Default-Analysis/test3.csv")
submission4 <- vroom::vroom("~/Documents/GitHub Projects/Loan-Default-Analysis/test4.csv")
submission5 <- vroom::vroom("~/Documents/GitHub Projects/Loan-Default-Analysis/test5.csv")

df <- cbind(submission1$loss, submission2$loss, submission3$loss, submission4$loss, submission5$loss)
df <- as.data.frame(df)

colnames(df) <- c("col1", "col2", "col3", "col4", "col5")

df <- df %>% rowwise() %>% mutate(loss = mean(c(col1, col2, col3, col4, col5)))
df <- cbind(submission1$id, df$loss)

write.csv(x = df, file="~/Documents/GitHub Projects/Loan-Default-Analysis/Submissions.csv", row.names = FALSE) 


##### Parallel Processing #####

# cluster <- parallel::makeCluster(30) # use 30 different computers
# doParallel:: registerDoParallel(cluster)
# system.time({
#   fitTest <- train(form = loss ~ .,
#                    data = dat,
#                    method = "ranger",
#                    trControl = trainControl(
#                      method = "repeatedcsv",
#                      number = 10,
#                      repeats = 2),
#                    num.trees = 100
#   )
# })


