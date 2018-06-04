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
modranger <- ranger(Activity ~ .-Id, df_train)
modranger$confusion.matrix
print(1-modranger$prediction.error)
acc_ranger <- 1-modranger$prediction.error
predranger <- predict(modranger, df_test)
#####################################################################################
#submission
submit <- data.frame(Id=df_test$Id, Activity=predranger$predictions, 
                     stringsAsFactors = TRUE)
head(submit)
write.csv(submit, file = "submission.csv", row.names=F)