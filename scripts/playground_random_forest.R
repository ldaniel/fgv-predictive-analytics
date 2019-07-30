#################### INI TEMP ####################

# clear everything before starting rf playground --------------------------------------

cat("\014")
rm(list=ls())
invisible(gc())

# loading required libraries ----------------------------------------------------------
library(caret)
library(corrplot)
library(fastDummies)
library(feather)
library(forcats)
library(ggcorrplot)
library(ggplot2)
library(ggthemes)
library(hmeasure)
library(knitr)
library(lubridate)
library(MASS)
library(mctest)
library(pROC)
library(readr)
library(rms)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(adabag)
library(VIM)
library(reshape2)
library(ggpubr)
library(randomForest)

# loading other scripts do be used here ----------------------------------------------
source("./scripts/step_00_config_environment.R")
source("./scripts/step_01_create_functions.R")
source("./scripts/step_02_data_ingestion.R")
source("./scripts/step_03_data_cleaning.R")
source("./scripts/step_04_label_translation.R")
source("./scripts/step_05_data_enhancement.R")

#################### END TEMP ####################

# data prep ---------------------------------------------------------------------------

loan_dataset_rf <- DataPrep() %>% 
  mutate(y_loan_defaulter = as.factor(y_loan_defaulter)) %>% 
  select(-x_district_name_Usti_nad_Labem, -x_prop_old_age_pension)

# sampling ----------------------------------------------------------------------------

set.seed(12345)
index <- caret::createDataPartition(loan_dataset_rf$y_loan_defaulter, 
                                    p= 0.7,list = FALSE)

data.train_rf <- loan_dataset_rf[index, ]
data.test_rf  <- loan_dataset_rf[-index,]

event_proportion <- bind_rows(prop.table(table(loan_dataset_rf$y_loan_defaulter)),
                              prop.table(table(data.train_rf$y_loan_defaulter)),
                              prop.table(table(data.test_rf$y_loan_defaulter)))

event_proportion$scope = ''
event_proportion$scope[1] = 'full dataset'
event_proportion$scope[2] = 'train dataset'
event_proportion$scope[3] = 'test dataset'

event_proportion <- select(event_proportion, scope, everything())

kable(event_proportion)

# extent caret to allow ntree and mtry param at once ----------------------------------

customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# fit the random forest model using caret customized train function -------------------

control <- trainControl(method="repeatedcv", number=5, repeats=3, verboseIter = TRUE, allowParallel = TRUE)
tuneparam <- expand.grid(.mtry=c(5, 25, 50, 75, 85, 100, 115, 125, 150, 175, 200),
                         .ntree=c(1000, 3000, 5000, 7000, 9000, 10000))
evalmetric <- "Accuracy"

set.seed(12345)

ini <- Sys.time()
cat(paste0("\nStarted RF training at: ", ini, " ...\n\n"))

rf.full <- train(y_loan_defaulter ~ .,
                 data=data.train_rf,
                 method=customRF,
                 preProcess=c("center", "scale"),
                 metric=evalmetric,
                 tuneGrid=tuneparam,
                 trControl=control,
                 importance=TRUE)

elapsedTime <- difftime(Sys.time(), ini, units = "auto")
cat(paste0("\n\nFinished RF training. Total time taken: ", round(elapsedTime, 2), " ", units(elapsedTime)))

summary(rf.full)
plot(rf.full)

# Best selected parameters: mtry = 100 and ntree = 5000 (time taken: ~2 hours)

saveRDS(rf.full, "./models/random_forest.rds")

# to save time, only load fitted model ------------------------------------------------
rf.full <- readRDS("./models/random_forest.rds")

# generate predicted columns ----------------------------------------------------------

data.test_rf$y_loan_defaulter_predicted <- predict(rf.full, newdata = data.test_rf, type = "prob")
data.train_rf$y_loan_defaulter_predicted <- predict(rf.full, newdata = data.train_rf, type = "prob")
loan_dataset_rf$y_loan_defaulter_predicted <- predict(rf.full, newdata = loan_dataset_rf, type = "prob")

# # calculate TNR and TPR for multi-cuts for RF -----------------------------------------
# 
# metricsByCutoff <- modelMetrics(data.train_rf$y_loan_defaulter, data.train_rf$y_loan_defaulter_predicted)
# p <- plot_ly(x = ~metricsByCutoff$Cut, y = ~metricsByCutoff$TNR, name = 'TNR', type = 'scatter', mode = 'lines')
# p <- p %>% add_trace(x = ~metricsByCutoff$Cut, y = ~metricsByCutoff$TPR, name = 'TPR', type = 'scatter', mode = 'lines')
# p %>% layout(xaxis = list(title = "Cutoff Value"),
#              yaxis = list(title = "True Ratio (%)"))
# 
# # TBD: cutoff
# # Optimized cut-off selected parameter: Z
# 
# # calculate metrics for selected parameters in train/test/full dataset ----------------
# 
# kable(event_proportion)

