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

# loading other scripts do be used here ----------------------------------------------
source("./scripts/step_00_config_environment.R")
source("./scripts/step_01_create_functions.R")
source("./scripts/step_02_data_ingestion.R")
source("./scripts/step_03_data_cleaning.R")
source("./scripts/step_04_label_translation.R")
source("./scripts/step_05_data_enhancement.R")

# modeling ---------------------------------------------------------------------------

source('scripts/playground_logistic_regression.R')
source('scripts/playground_decision_tree.R')

# model evaluation -------------------------------------------------------------------

## making preditions -----------------------------------------------------------------

logistic.full.prob.train <- predict(logistic.full, type = "response")
logistic.full.prob.test <- predict(logistic.full, 
                                   newdata = data.test, type= "response")

logistic.step.prob.train <- predict(logistic.step, type = "response")
logistic.step.prob.test <- predict(logistic.step, 
                                   newdata = data.test, type= "response")

decision.tree.prob.train <- predict(tree.full, type = "prob")[,2]
decision.tree.prob.test  <- predict(tree.full, 
                                    newdata = data.test, type = "prob")[,2]

## getting measures -----------------------------------------------------------------
measures.logistic.full.train <- HMeasure(data.train$y_loan_defaulter, 
                                         logistic.full.prob.train, 
                                         threshold = 0.5)

measures.logistic.full.test  <- HMeasure(data.test$y_loan_defaulter, 
                                         logistic.full.prob.test, 
                                         threshold = 0.5)

measures.logistic.step.train <- HMeasure(data.train$y_loan_defaulter, 
                                         logistic.step.prob.train, 
                                         threshold = 0.5)

measures.logistic.step.test <- HMeasure(data.test$y_loan_defaulter, 
                                        logistic.step.prob.test, 
                                        threshold = 0.5)

measures.decition.tree.train <- HMeasure(data.train$y_loan_defaulter, 
                                         decision.tree.prob.train, 
                                         threshold = 0.5)

measures.decition.tree.test <- HMeasure(data.test$y_loan_defaulter, 
                                        decision.tree.prob.test, 
                                        threshold = 0.5)

measures <- t(bind_rows(measures.logistic.full.train$metrics,
                        measures.logistic.full.test$metrics,
                        measures.logistic.step.train$metrics,
                        measures.logistic.step.test$metrics,
                        measures.decition.tree.train$metrics,
                        measures.decition.tree.test$metrics
                        )) %>% as_tibble(., rownames = NA)

colnames(measures) <- c('logistic.full - train','logistic.full - test',
                        'logistic.step - train','logistic.step - test',
                        'decision.tree - train','decision.tree - test')

measures$metric = rownames(measures)

measures <- dplyr::select(measures, metric, everything())

kable(measures, row.names = FALSE)

## boxplot -------------------------------------------------------------------------
boxplot(logistic.full.prob.test ~ data.test$y_loan_defaulter,
        col= c("red", "green"), 
        horizontal= T,
        xlab = 'Probability Prediction',
        ylab = 'Loan Defaulter')

boxplot(logistic.step.prob.test ~ data.test$y_loan_defaulter,
        col= c("red", "green"), 
        horizontal= T,
        xlab = 'Probability Prediction',
        ylab = 'Loan Defaulter')

boxplot(decision.tree.prob.test ~ data.test$y_loan_defaulter,
        col= c("red", "green"), 
        horizontal= T,
        xlab = 'Probability Prediction',
        ylab = 'Loan Defaulter')

## ROC Curve ----------------------------------------------------------------------
roc_logistic.full <- roc(data.test$y_loan_defaulter, 
                         logistic.full.prob.test)

roc_logistic.step <- roc(data.test$y_loan_defaulter, 
                         logistic.step.prob.test)

roc_decision.tree <- roc(data.test$y_loan_defaulter, 
                         decision.tree.prob.test)

y1 <- roc_logistic.full$sensitivities
x1 <- 1 - roc_logistic.full$specificities

y2 <- roc_logistic.step$sensitivities
x2 <- 1 - roc_logistic.step$specificities

y3 <- roc_decision.tree$sensitivities
x3 <- 1 - roc_decision.tree$specificities

plot(x1, y1,  type="n",
     xlab = "False Positive Rate (Specificities)", 
     ylab = "True Positive Rate (Sensitivities)")

lines(x1, y1, lwd = 3, lty = 1, col="red") 
lines(x2, y2, lwd = 3, lty = 1, col="blue")
lines(x3, y3, lwd = 3, lty = 1, col="green")

abline(0, 1, lty = 2)


# accuracy metrics ---------------------------------------------------------------

accuracy <- function(x, threshold = 0.5) {
  
  fitted.results <- ifelse(x > threshold ,1 ,0)
  
  misClasificError <- mean(fitted.results != data.test$y_loan_defaulter)
  
  misClassCount <- misclassCounts(fitted.results, data.test$y_loan_defaulter)

  print(kable(misClassCount$conf.matrix))

  print('--------------------------------------------------------------')
  print(paste('Model General Accuracy of: ', 
              round((1 - misClassCount$metrics['ER']) * 100, 2), '%', sep = ''))
  print(paste('True Positive Rate of    : ', 
              round(misClassCount$metrics['TPR'] * 100, 2), '%', sep = ''))
}

accuracy(logistic.full.prob.test, 0.1)
accuracy(logistic.step.prob.test, 0.1)
accuracy(decision.tree.prob.test, 0.1)
