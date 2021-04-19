##
## Loan Default Prediction
##

## Libraries I Need
library(tidyverse)
library(DataExplorer)
library(vroom)
library(caret)

## Read in the data
#system.time(default <- read_csv("./train_v2.csv"))
train <- vroom::vroom("./train_v2.csv")
test <- vroom::vroom("./test_v2.csv")
default <- bind_rows(train=train, test=test, .id="Set")
write.csv(default, file = "~/Documents/GitHub Projects/Loan-Default-Analysis/default.csv")

## Remove zero-variance explanatory variables
throwZV <- preProcess(x=default, method="zv")
default <- predict(throwZV, newdata=default)

## Throw out covariates that have a small spearmean
## correlation with loss
cors <- cor(x=default %>% select(-loss, -id, -Set),
            y=default %>% select(loss),
            use="pairwise.complete.obs",
            method="spearman")
qplot(cors, geom="histogram")
cor.cutoff <- 0.05
kp.x <- which(abs(cors)>cor.cutoff)
default <- default %>% select(all_of(kp.x), loss, Set)

## Impute missing values sequentially using random forest
default.full <- default
pct.missing <- colMeans(is.na(default %>% select(-loss, -Set)))
full.x <- c(names(pct.missing)[pct.missing==0])
x.to.impute <- names(pct.missing[pct.missing>0] %>% sort())
pb <- txtProgressBar(min = 0, max = length(x.to.impute), style = 3)
it <- 0
for(x in x.to.impute){
  it <- it + 1
  df <- default.full %>% select(all_of(full.x),x)
  pred.rows <- which(!complete.cases(df))
  rf.imputer <- ranger::ranger(as.formula(paste0(x,"~.")),
                               data=df %>% slice(-pred.rows),
                               num.trees=100,
                               verbose=FALSE)
  default.full[pred.rows, x] <- predict(rf.imputer, 
                                        data=as.data.frame(df[pred.rows,]))$predictions
  full.x <- c(full.x, x)
  setTxtProgressBar(pb, it)
}
close(pb)

## Write out the filled in dataset to disk
vroom_write(x=default.full, 
            path="./FullDefaultData.csv",
            delim=",")
