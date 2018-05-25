#Set the working directory:
setwd("/Users/Kianamon/R/assignment4")
rm(list=ls())
#####################################################################################
#libraries in use:
library(httr)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(caret)
library(glmnet)
#####################################################################################
#check for missing packages and install them:
list.of.packages <- c("knitr", "httr", "readr", "dplyr", "tidyr", "XML",
                      "ggplot2", "stringr", "lubridate", "grid", "caret", 
                      "rpart", "Metrics", "e1071", "ranger", "glmnet", 
                      "randomForest", "ROCR", "gbm", "ipred", "ModelMetrics", 
                      "rpart.plot", "xgboost", "tidyverse", "magrittr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
#####################################################################################
#downloading the main data sets:
GET("https://raw.githubusercontent.com/maherharb/MATE-T580/master/Datasets/college.csv", 
    write_disk("college.csv", overwrite = TRUE))
df_college <- read_csv("college.csv")
head(df_college)
names(df_college)
colSums(is.na(df_college))
#####################################################################################
########For all the models we are monitoring the Adjusted R Squared metric###########
########################################Linear=0.9276################################
linearModAll <- lm(Apps ~ .-X1, df_college)
linearModAll
summary(linearModAll)
summary(linearModAll)$adj.r.squared
########################################Selective Linear=0.9262######################
linearModSome <- lm(Apps ~ Accept + Enroll + 
                      Top10perc + Top25perc + 
                      F.Undergrad + P.Undergrad +
                      S.F.Ratio + Expend +
                      Outstate + Room.Board +
                      Grad.Rate, df_college)
linearModSome
summary(linearModSome)
summary(linearModSome)$adj.r.squared
########################################Lasso=0.9206#################################
set.seed(1234)
trControl <- trainControl(method = "cv", number = 5)
grid <- expand.grid(alpha = 1, lambda = seq(0, 120, length = 301))
linearModLasso <- caret::train(Apps ~ .-X1, data = df_college,
                        method = "glmnet", tuneGrid = grid, 
                        trControl = trControl, metric = "RMSE",
                        preProcess = c("center", "scale"))
par(mar = c(4, 4, 0, 0))
plot(linearModLasso)
Beta <- coef(linearModLasso$finalModel, 38)
Beta
R2 <- linearModLasso$results$Rsquared[which(grid$lambda == 38)]
adjRR <- 1 - (1 - R2) * (nrow(df_college) - 1)/(nrow(df_college) - sum(Beta != 0) - 1)
adjRR
#####################################################################################