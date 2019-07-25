# loading required libraries --------------------------------------------------
library(dplyr)
library(tidyr)
library(fastDummies)
library(lubridate)
library(forcats)
library(feather)
library(rpart)
library(rpart.plot)
library(caret)
library(hmeasure)
library(pROC)
library(ggplot2)
library(ggthemes)
library(ggcorrplot)
library(knitr)

# loading other scripts do be used here ---------------------------------------
source("./scripts/step_00_config_environment.R")
source("./scripts/step_01_create_functions.R")
source("./scripts/step_02_data_ingestion.R")
source("./scripts/step_03_data_cleaning.R")
source("./scripts/step_04_label_translation.R")
source("./scripts/step_05_data_enhancement.R")

# data prep -------------------------------------------------------------------

loan_dataset <- DataPrep()

kable(names(loan_dataset))

# sampling ----------------------------------------------------------------------------

set.seed(12345)
index <- caret::createDataPartition(loan_dataset$y_loan_defaulter, 
                                    p= 0.7,list = FALSE)

data.train <- loan_dataset[index, ]
data.test  <- loan_dataset[-index,]


event_proportion <- bind_rows(prop.table(table(loan_dataset$y_loan_defaulter)),
                              prop.table(table(data.train$y_loan_defaulter)),
                              prop.table(table(data.test$y_loan_defaulter)))

event_proportion$scope = ''
event_proportion$scope[1] = 'full dataset'
event_proportion$scope[2] = 'train dataset'
event_proportion$scope[3] = 'test dataset'

event_proportion <- select(event_proportion, scope, everything())

kable(event_proportion)

# fit the decision tree model -------------------------------------------------------------

names  <- names(loan_dataset)
f_full <- paste("y_loan_defaulter ~",
                           paste(names[!names %in% "y_loan_defaulter"], collapse = " + "))


tree.full <- rpart(data= data.train, y_loan_defaulter ~ .,
                   control = rpart.control(minbucket=20),
                   method = "class")

tree.full
summary(tree.full)
rpart.plot(tree.full)

printcp(tree.full)
plotcp(tree.full)

tree.prune <- prune(tree.full, cp= tree.full$cptable[which.min(tree.full$cptable[,"xerror"]),"CP"])

plotcp(tree.prune)
rpart.plot(tree.prune)

# model evaluation -------------------------------------------------------------------

## making preditions -----------------------------------------------------------------

prob.train <- predict(tree.full, type = "prob")[,2]
prob.test  <- predict(tree.full, newdata = data.test, type = "prob")[,2]

## getting measures ------------------------------------------------------------------
train <- HMeasure(data.train$y_loan_defaulter, prob.train, threshold = 0.5)
test  <- HMeasure(data.test$y_loan_defaulter, prob.test, threshold = 0.5)

measures <- t(bind_rows(train$metrics,
                      test$metrics)) %>% as_tibble(., rownames = NA)

colnames(measures) <- c('train','test')

measures$metric = rownames(measures)

measures <- select(measures, metric, everything())

kable(measures, row.names = FALSE)

## boxplot ---------------------------------------------------------------------------
boxplot(prob.test ~ data.test$y_loan_defaulter,
        col= c("red", "green"), 
        horizontal= T,
        xlab = 'Probability Prediction',
        ylab = 'Loan Defaulter')

## ROC Curve -------------------------------------------------------------------------
roc_1 <- roc(data.test$y_loan_defaulter, prob.test)

y1 <- roc_1$sensitivities
x1 <- 1 - roc_1$specificities

plot(x1, y1,  type="n",
     xlab = "False Positive Rate (Specificities)", 
     ylab= "True Positive Rate (Sensitivities)")

lines(x1, y1,lwd=3,lty=1, col="red") 

abline(0,1, lty=2)

# accuracy metrics -------------------------------------------------------------------

threshold <- 0.1

fitted.results <- ifelse(prob.test > threshold ,1 ,0)

misClasificError <- mean(fitted.results != data.test$y_loan_defaulter)

misClassCount <- misclassCounts(fitted.results, data.test$y_loan_defaulter)

paste('Model General Accuracy of: ', round((1 - misClassCount$metrics['ER']) * 100, 2), '%', sep = '')
paste('True Positive Rate of    : ', round(misClassCount$metrics['TPR'] * 100, 2), '%', sep = '')

kable(misClassCount$conf.matrix)
