#Set the working directory:
setwd("/Users/Kianamon/R/kaggle")
rm(list=ls())
#####################################################################################
#libraries in use:
library(knitr)
library(httr)
library(readr)
library(dplyr)
library(tidyr)
library(XML)
library(ggplot2)
library(stringr)
library(lubridate)
library(grid)
library(caret)
library(glmnet)
library(ranger)
library(e1071)
library(Metrics)
library(rpart)  
library(rpart.plot)
library(ModelMetrics)   
library(ipred)  
library(randomForest)
library(gbm)  
library(ROCR)
library(mlr)
library(xgboost)
library(tidyverse)
library(magrittr)
library(data.table)
library(mosaic)
library(Ckmeans.1d.dp)
library(archdata)
#####################################################################################
#check for missing packages and install them:
list.of.packages <- c("knitr", "httr", "readr", "dplyr", "tidyr", "XML",
                      "ggplot2", "stringr", "lubridate", "grid", "caret", 
                      "rpart", "Metrics", "e1071", "ranger", "glmnet", 
                      "randomForest", "ROCR", "gbm", "ipred", "ModelMetrics", 
                      "rpart.plot", "xgboost", "tidyverse", "magrittr", "mosaic",
                      "Ckmeans.1d.dp", "archdata")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
#####################################################################################
#downloading the two main data sets:
df_train <- read_csv("train.csv")
df_test <- read_csv("test.csv")
#####################################################################################
head(df_train)
colSums(is.na(df_train))
unique(df_train$Activity)
#####################################################################################
#ranger
#modranger <- ranger(Activity ~ .-Id, df_train)
#modranger$confusion.matrix
#print(1-modranger$prediction.error)
#acc_ranger <- 1-modranger$prediction.error
#predranger <- predict(modranger, df_test)
#####################################################################################
#set.seed(1)
#assignment <- sample(1:3, size = nrow(df_train), prob =c(0.7, 0.15, 0.15), replace = TRUE)
#df_train1 <- df_train[assignment == 1, ]   # subset valid
#df_valid <- df_train[assignment == 2, ]   # subset valid
#df_test1 <- df_train[assignment == 3, ]   # subset test   
#####################################################################################
#set.seed(1234)  
#grid <-  expand.grid(mtry = c(3,4), splitrule = "gini", min.node.size = 10)
#df_train1 <- df_train %>%
#  select(-Id)
#fitControl <- trainControl(method = "CV",
#                           number = 5,
#                           verboseIter = TRUE)
#modmod <- caret::train(y = df_train$Activity, 
#                       x = df_train[, colnames(df_train) != "Activity"], 
#                       trControl = fitControl, method = "ranger",                          
#                       num.trees = 200,
#                       tuneGrid = grid)
#print(modmod)
#pred2 <- predict(modmod, df_test)
#####################################################################################
smp_size <- floor(0.80 * nrow(df_train))
# set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df_train)), size = smp_size)
train <- df_train[train_ind, ]
test <- df_train[-train_ind, ]
#convert data frame to data table
setDT(train) 
setDT(test)
df_train_table <- df_train
setDT(df_train_table)
#check missing values 
table(is.na(df_train_table))
#using one hot encoding 
labels1 <- factor(train$Activity)
ts_label <- factor(test$Activity)
new_tr <- model.matrix(~.+0,data = train[,-c("Activity"),with=F]) 
new_ts <- model.matrix(~.+0,data = test[,-c("Activity"),with=F])
newrealtest <- model.matrix(~.+0,data = df_test)
#convert factor to numeric 
labels <- as.numeric(labels1)-1
ts_label <- as.numeric(ts_label)-1
ts_label
#preparing matrix 
dtrain <- xgb.DMatrix(data = new_tr,label = labels) 
dtest <- xgb.DMatrix(data = new_ts,label=ts_label)
drealtest <- xgb.DMatrix(data = newrealtest)
#testdmat <- xgb.DMatrix(data = new_realtest,label=ts_label)
numberOfClasses <- length(unique(df_train$Activity))

xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)
nround    <- 50 # number of XGBoost rounds
cv.nfold  <- 5

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = dtrain, 
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = FALSE,
                   prediction = TRUE)
OOF_prediction <- data.frame(cv_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = labels + 1)
head(OOF_prediction)
caret::confusionMatrix(factor(OOF_prediction$max_prob),
                factor(OOF_prediction$label),
                mode = "everything")

bst_model <- xgb.train(params = xgb_params,
                       data = dtrain,
                       nrounds = nround)

# Predict hold-out test set
test_pred <- predict(bst_model, newdata = dtest)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = ts_label + 1,
         max_prob = max.col(., "last"))
# confusion matrix of test set
head(test_prediction)
caret::confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")
#predict the real test set:
realtest_pred <- predict(bst_model, newdata = drealtest)
realtest_prediction <- matrix(realtest_pred, nrow = numberOfClasses,
                          ncol=length(realtest_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(max_prob = max.col(., "last"))
unique(realtest_prediction$max_prob)
realtest_prediction2 <- realtest_prediction %>%
  mutate(Activity = derivedFactor(
    "sitting" = (max_prob==1),
    "sittingdown" = (max_prob==2),
    "standing" = (max_prob==3),
    "standingup" = (max_prob==4),
    "walking" = (max_prob==5),
    .method = "first",
    .default = 0
  ))
#####################################################################################
#submission
submit <- data.frame(Id=df_test$Id, Activity=realtest_prediction2$Activity, 
                     stringsAsFactors = TRUE)
head(submit)
write.csv(submit, file = "submission.csv", row.names=F)