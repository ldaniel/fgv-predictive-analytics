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
library(gmodels)
library(plotly)

# loading other scripts do be used here ----------------------------------------------
source("./scripts/step_00_config_environment.R")
source("./scripts/step_01_create_functions.R")
source("./scripts/step_02_data_ingestion.R")
source("./scripts/step_03_data_cleaning.R")
source("./scripts/step_04_label_translation.R")
source("./scripts/step_05_data_enhancement.R")
source("./scripts/step_06_dataset_preparation.R")

# modeling ---------------------------------------------------------------------------

source('scripts/playground_logistic_regression.R')
source('scripts/playground_decision_tree.R')
source('scripts/playground_boosting.R')
source('scripts/playground_random_forest.R')

# model evaluation -------------------------------------------------------------------

## making preditions -----------------------------------------------------------------

logistic.prob.train <- predict(logistic.step, type = "response")
logistic.prob.test <- predict(logistic.step,newdata = data.test_logistic, type= "response")

decision.tree.prob.train <- predict(tree.full, type = "prob")[, 2]
decision.tree.prob.test  <- predict(tree.full,newdata = data.test_DT, type = "prob")[, 2]

boosting.prob.train <- predict.boosting(boost, data.train_boost)$prob[, 2]
boosting.prob.test  <- predict.boosting(boost, data.test_boost)$prob[, 2]

random.forest.prob.train <- predict(rf.full, newdata = data.train_rf, type = "prob")[,2]
random.forest.prob.test <- predict(rf.full, newdata = data.test_rf, type = "prob")[,2]

## getting measures -----------------------------------------------------------------

# logistic regression

measures.logistic.train <- HMeasure(data.train_logistic$y_loan_defaulter, logistic.prob.train, threshold = 0.1)
measures.logistic.test <- HMeasure(data.test_logistic$y_loan_defaulter, logistic.prob.test, threshold = 0.1)

# decision tree

measures.decision.tree.train <- HMeasure(data.train_DT$y_loan_defaulter, decision.tree.prob.train, threshold = 0.1)
measures.decision.tree.test <- HMeasure(data.test_DT$y_loan_defaulter, decision.tree.prob.test, threshold = 0.1)

# boosting

measures.boosting.train <- HMeasure(data.train_boost$y_loan_defaulter, boost.prob.train,threshold = 0.4)
measures.boosting.test  <- HMeasure(data.test_boost$y_loan_defaulter, boost.prob.test,threshold = 0.4)

# random forest

measures.random.forest.train <- HMeasure(data.train_rf$y_loan_defaulter, random.forest.prob.train, threshold = 0.1)
measures.random.forest.test  <- HMeasure(data.test_rf$y_loan_defaulter, random.forest.prob.test, threshold = 0.1)

measures <- t(bind_rows(measures.logistic.train$metrics,
                        measures.logistic.test$metrics,
                        measures.decision.tree.train$metrics,
                        measures.decision.tree.test$metrics,
                        measures.boosting.train$metrics,
                        measures.boosting.test$metrics,
                        measures.random.forest.train$metrics,
                        measures.random.forest.test$metrics
                        )) %>% as_tibble(., rownames = NA)

colnames(measures) <- c('logistic - train', 'logistic - test',
                        'decision.tree - train', 'decision.tree - test',
                        'boosting - train', 'boosting - test',
                        'random forest - train', 'random forest - test')

measures$metric = rownames(measures)

measures <- dplyr::select(measures, metric, everything())

kable(measures, row.names = FALSE)

## boxplot -------------------------------------------------------------------------

# logistic regression

boxplot(logistic.prob.test ~ data.test_logistic$y_loan_defaulter,
        col= c("green", "red"), 
        horizontal= T,
        xlab = 'Probability Prediction',
        ylab = 'Loan Defaulter')

# decision tree

boxplot(decision.tree.prob.test ~ data.test_DT$y_loan_defaulter,
        col= c("green", "red"), 
        horizontal= T,
        xlab = 'Probability Prediction',
        ylab = 'Loan Defaulter')

# boosting

boxplot(boosting.prob.test ~ data.test_boost$y_loan_defaulter
        ,col= c("green", "red"),
        horizontal= T,
        xlab = 'Probability Prediction',
        ylab = 'Loan Defaulter')

# random forest

boxplot(random.forest.prob.test ~ data.test_rf$y_loan_defaulter
        ,col= c("green", "red"),
        horizontal= T,
        xlab = 'Probability Prediction',
        ylab = 'Loan Defaulter')

## ROC Curve ----------------------------------------------------------------------

# logistic regression

roc_logistic <- roc(data.test_logistic$y_loan_defaulter,
                    logistic.prob.test)

# decision tree

roc_decision.tree <- roc(data.test_DT$y_loan_defaulter, 
                         decision.tree.prob.test)

# boosting

roc_boosting <- roc(data.test_boost$y_loan_defaulter,
                    boosting.prob.test)

# random forest

roc_random.forest <- roc(data.test_rf$y_loan_defaulter,
                    random.forest.prob.test)

# logistic regression

y1 <- roc_logistic$sensitivities
x1 <- 1 - roc_logistic$specificities

# decision tree

y2 <- roc_decision.tree$sensitivities
x2 <- 1 - roc_decision.tree$specificities

# boosting

y3 <- roc_boosting$sensitivities
x3 <- 1 - roc_boosting$specificities

# random.forest

y4 <- roc_random.forest$sensitivities
x4 <- 1 - roc_random.forest$specificities

plot(x1, y1,  type="n",
     xlab = "False Positive Rate (Specificities)", 
     ylab = "True Positive Rate (Sensitivities)")

lines(x1, y1, lwd = 3, lty = 1, col="red") 
lines(x2, y2, lwd = 3, lty = 1, col="blue")
lines(x3, y3, lwd = 3, lty = 1, col="green")
lines(x4, y4, lwd = 3, lty = 1, col="purple")

legend("bottomright", c('Logistic', 'Decision Tree', 'Boosting', 'Random Forest'), 
       lty = 1, col = c('red', 'blue', 'green', 'purple'))

abline(0, 1, lty = 2)

# accuracy metrics ---------------------------------------------------------------

accuracy <- function(score, actual, threshold = 0.5) {
  
  fitted.results <- ifelse(score > threshold ,1 ,0)
  
  misClasificError <- mean(fitted.results != actual)
  
  misClassCount <- misclassCounts(fitted.results, actual)

  print(kable(misClassCount$conf.matrix))

  print('--------------------------------------------------------------')
  print(paste('Model General Accuracy of: ', 
              round((1 - misClassCount$metrics['ER']) * 100, 2), '%', 
              sep = ''))
  print(paste('True Positive Rate of    : ', 
              round(misClassCount$metrics['TPR'] * 100, 2), '%',
              sep = ''))
}

# logistic regression

accuracy(score = logistic.prob.test, 
         actual = data.test_logistic$y_loan_defaulter, 
         threshold = 0.08)

# decision tree

accuracy(score = decision.tree.prob.test, 
         actual = data.test_DT$y_loan_defaulter, 
         threshold = 0.1)

# boosting

accuracy(score = boosting.prob.test, 
         actual = data.test_boost$y_loan_defaulter, 
         threshold = 0.41)

# random forest

accuracy(score = random.forest.prob.test, 
         actual = data.test_rf$y_loan_defaulter, 
         threshold = 0.12)

