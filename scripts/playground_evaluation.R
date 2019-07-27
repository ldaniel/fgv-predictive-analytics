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
source('scripts/playground_boosting.R')

# model evaluation -------------------------------------------------------------------

## making preditions -----------------------------------------------------------------

logistic.prob.train <- predict(logistic.step, type = "response")
logistic.prob.test <- predict(logistic.step,
                              newdata = data.test_logistic, type= "response")

decision.tree.prob.train <- predict(tree.full, type = "prob")[,2]
decision.tree.prob.test  <- predict(tree.full,
                                    newdata = data.test_DT, type = "prob")[,2]

boosting.prob.train <- predict.boosting(boost, data.train_boost)$prob[,2]
boosting.prob.test  <- predict.boosting(boost, data.test_boost)$prob[,2]

## getting measures -----------------------------------------------------------------

measures.logistic.train <- HMeasure(data.train_logistic$y_loan_defaulter, 
                                         logistic.prob.train, 
                                         threshold = 0.5)

measures.logistic.test <- HMeasure(data.test_logistic$y_loan_defaulter, 
                                        logistic.prob.test, 
                                        threshold = 0.5)

measures.decision.tree.train <- HMeasure(data.train_DT$y_loan_defaulter, 
                                         decision.tree.prob.train, 
                                         threshold = 0.5)

measures.decision.tree.test <- HMeasure(data.test_DT$y_loan_defaulter, 
                                        decision.tree.prob.test, 
                                        threshold = 0.5)

measures.boosting.train <- HMeasure(data.train_boost$y_loan_defaulter, 
                                 boost.prob.train,
                                 threshold = 0.5)

measures.boosting.test  <- HMeasure(data.test_boost$y_loan_defaulter, 
                                 boost.prob.test,
                                 threshold = 0.5)


measures <- t(bind_rows(measures.logistic.train$metrics,
                        measures.logistic.test$metrics,
                        measures.decision.tree.train$metrics,
                        measures.decision.tree.test$metrics,
                        measures.boosting.train$metrics,
                        measures.boosting.test$metrics,
                        )) %>% as_tibble(., rownames = NA)

colnames(measures) <- c('logistic - train', 'logistic - test',
                        'decision.tree - train', 'decision.tree - test',
                        'boosting - train', 'boosting - test')

measures$metric = rownames(measures)

measures <- dplyr::select(measures, metric, everything())

kable(measures, row.names = FALSE)

## boxplot -------------------------------------------------------------------------

boxplot(logistic.prob.test ~ data.test_logistic$y_loan_defaulter,
        col= c("green", "red"), 
        horizontal= T,
        xlab = 'Probability Prediction',
        ylab = 'Loan Defaulter')

boxplot(decision.tree.prob.test ~ data.test_DT$y_loan_defaulter,
        col= c("green", "red"), 
        horizontal= T,
        xlab = 'Probability Prediction',
        ylab = 'Loan Defaulter')

boxplot(boosting.prob.test ~ data.test_boost$y_loan_defaulter
        ,col= c("green", "red"),
        horizontal= T,
        xlab = 'Probability Prediction',
        ylab = 'Loan Defaulter')

## ROC Curve ----------------------------------------------------------------------

roc_logistic <- roc(data.test_logistic$y_loan_defaulter,
                    logistic.prob.test)

roc_decision.tree <- roc(data.test_DT$y_loan_defaulter, 
                         decision.tree.prob.test)

roc_boosting <- roc(data.test_boost$y_loan_defaulter,
                    boosting.prob.test)

y1 <- roc_logistic$sensitivities
x1 <- 1 - roc_logistic$specificities

y2 <- roc_decision.tree$sensitivities
x2 <- 1 - roc_decision.tree$specificities

y3 <- roc_boosting$sensitivities
x3 <- 1 - roc_boosting$specificities

plot(x1, y1,  type="n",
     xlab = "False Positive Rate (Specificities)", 
     ylab = "True Positive Rate (Sensitivities)")

lines(x1, y1, lwd = 3, lty = 1, col="red") 
lines(x2, y2, lwd = 3, lty = 1, col="blue")
lines(x3, y3, lwd = 3, lty = 1, col="green")

legend("bottomright", c('Logistic', 'Decision Tree', 'Boosting'), 
       lty = 1, col = c('red', 'blue', 'green'))

abline(0, 1, lty = 2)

# accuracy metrics ---------------------------------------------------------------

accuracy <- function(score, actual, threshold = 0.5) {
  
  fitted.results <- ifelse(score > threshold ,1 ,0)
  
  misClasificError <- mean(fitted.results != actual)
  
  misClassCount <- misclassCounts(fitted.results, actual)

  print(kable(misClassCount$conf.matrix))

  print('--------------------------------------------------------------')
  print(paste('Model General Accuracy of: ', 
              round((1 - misClassCount$metrics['ER']) * 100, 2), '%', sep = ''))
  print(paste('True Positive Rate of    : ', 
              round(misClassCount$metrics['TPR'] * 100, 2), '%', sep = ''))
}

accuracy(score = logistic.prob.test, 
         actual = data.test_logistic$y_loan_defaulter, 
         threshold = 0.1)

accuracy(score = decision.tree.prob.test, 
         actual = data.test_DT$y_loan_defaulter, 
         threshold = 0.1)

accuracy(score = boosting.prob.test, 
         actual = data.test_boost$y_loan_defaulter, 
         threshold = 0.4)
