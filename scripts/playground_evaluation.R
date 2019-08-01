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

# clean global environment
rm(account, account_balance, account_transaction_pattern,
   client, creditcard, disposition, district, loan,
   permanent_order, transaction, czech_regions_coords,
   lat, long, region, transaction_empty_cols,
   transaction_na_cols, ConvertToDate, GetAgeFromBirthnumber,
   GetBirthdateFromBirthnumber, GetGenderFromBirthnumber)
invisible(gc)

# modeling ---------------------------------------------------------------------------

source('scripts/playground_logistic_regression.R')
rm(clean, clean_dataset, cor_mtx_clean, cor_mtx_full, cor_mtx_high_VIF,
   dummy_variables, high_VIF_dataset, high_VIF_correlogram_before, 
   logistic.full, SplitDataset, vars.quant, VIF, VIF_Table_After, 
   VIF_Table_Before, full, correl_threshold, dummy_variables_high,
   dummy_variables_low, high_VIF, low_VIF, prop_variables, reject_variables_vector)
invisible(gc)

source('scripts/playground_decision_tree.R')
rm(SplitDataset, tree.full)
invisible(gc)

source('scripts/playground_boosting.R')
rm(SplitDataset, boost.prob.test, boost.prob.train, f_full, names, var_importance)
invisible(gc())

source('scripts/playground_random_forest.R')
rm(customRF, DistinctCounts, FitResults, metricsByCutoff.full, metricsByCutoff.test,
   metricsByCutoff.train, SplitDataset)
invisible(gc())

# model evaluation -------------------------------------------------------------------

## making preditions for each model and consilidating in a single data frame

prob.full = list()
prob.train = list()
prob.test = list()

prob.full$logistic.actual         <- loan_dataset_logistic$y_loan_defaulter
prob.full$logistic.predicted      <- predict(logistic.step, type = "response", newdata = loan_dataset_logistic)
prob.full$decision.tree.actual    <- loan_dataset_DT$y_loan_defaulter
prob.full$decision.tree.predicted <- predict(tree.prune, type = "prob", newdata = loan_dataset_DT)[, 2]
prob.full$boosting.actual         <- loan_dataset_boost$y_loan_defaulter
prob.full$boosting.predicted      <- predict.boosting(boost, loan_dataset_boost)$prob[, 2]
prob.full$random.forest.actual    <- loan_dataset_rf$y_loan_defaulter
prob.full$random.forest.predicted <- predict(rf.full, type = "prob", newdata = loan_dataset_rf)[, 2]

prob.train$logistic.actual         <- data.train_logistic$y_loan_defaulter
prob.train$logistic.predicted      <- predict(logistic.step, type = "response", newdata = data.train_logistic)
prob.train$decision.tree.actual    <- data.train_DT$y_loan_defaulter
prob.train$decision.tree.predicted <- predict(tree.prune, type = "prob", newdata = data.train_DT)[, 2]
prob.train$boosting.actual         <- data.train_boost$y_loan_defaulter
prob.train$boosting.predicted      <- predict.boosting(boost, data.train_boost)$prob[, 2]
prob.train$random.forest.actual    <- data.train_rf$y_loan_defaulter
prob.train$random.forest.predicted <- predict(rf.full, type = "prob", newdata = data.train_rf)[, 2]

prob.test$logistic.actual         <- data.test_logistic$y_loan_defaulter
prob.test$logistic.predicted      <- predict(logistic.step, type = "response", newdata = data.test_logistic)
prob.test$decision.tree.actual    <- data.test_DT$y_loan_defaulter
prob.test$decision.tree.predicted <- predict(tree.prune, type = "prob", newdata = data.test_DT)[, 2]
prob.test$boosting.actual         <- data.test_boost$y_loan_defaulter
prob.test$boosting.predicted      <- predict.boosting(boost, data.test_boost)$prob[, 2]
prob.test$random.forest.actual    <- data.test_rf$y_loan_defaulter
prob.test$random.forest.predicted <- predict(rf.full, type = "prob", newdata = data.test_rf)[, 2]

prob.full   <- prob.full %>% as_tibble()
prob.train  <- prob.train %>% as_tibble()
prob.test   <- prob.test %>% as_tibble()

#clean global environment
rm(loan_dataset_boost, loan_dataset_DT, loan_dataset_logistic, loan_dataset_rf,
   data.test_boost, data.test_DT, data.test_logistic, data.test_rf,
   data.train_boost, data.train_DT, data.train_logistic, data.train_rf)
invisible(gc())

## getting measures -----------------------------------------------------------------

metricsByCutoff.test_log    <- modelMetrics(prob.test$logistic.actual, 
                                            prob.test$logistic.predicted, 
                                            plot_title = 'Logistic Regression')
metricsByCutoff.test_DT     <- modelMetrics(prob.test$decision.tree.actual, 
                                            prob.test$decision.tree.predicted, 
                                            plot_title = 'Decision Tree')
metricsByCutoff.test_boost  <- modelMetrics(prob.test$boosting.actual, 
                                            prob.test$boosting.predicted, 
                                            plot_title = 'Boosting')
metricsByCutoff.test_rf     <- modelMetrics(prob.test$random.forest.actual, 
                                            prob.test$random.forest.predicted, 
                                            plot_title = 'Random Forest')

cutoffs <- subplot(metricsByCutoff.test_log$Plot,
                   metricsByCutoff.test_DT$Plot,
                   metricsByCutoff.test_boost$Plot,
                   metricsByCutoff.test_rf$Plot,
                   nrows = 2) %>% hide_legend()

cutoffs

# logistic regression
measures.logistic.train <- HMeasure(prob.train$logistic.actual, 
                                    prob.train$logistic.predicted, 
                                    threshold = metricsByCutoff.test_log$BestCut['Cut'])
measures.logistic.test <- HMeasure(prob.test$logistic.actual, 
                                   prob.test$logistic.predicted, 
                                   threshold = metricsByCutoff.test_log$BestCut['Cut'])

# decision tree
measures.decision.tree.train <- HMeasure(prob.train$decision.tree.actual, 
                                         prob.train$decision.tree.predicted, 
                                         threshold = metricsByCutoff.test_DT$BestCut['Cut'])
measures.decision.tree.test <- HMeasure(prob.test$decision.tree.actual, 
                                        prob.test$decision.tree.predicted, 
                                        threshold = metricsByCutoff.test_DT$BestCut['Cut'])

# boosting
measures.boosting.train <- HMeasure(prob.train$boosting.actual, 
                                    prob.train$boosting.predicted, 
                                    threshold = metricsByCutoff.test_boost$BestCut['Cut'])
measures.boosting.test  <- HMeasure(prob.test$boosting.actual, 
                                    prob.test$boosting.predicted, 
                                    threshold = metricsByCutoff.test_boost$BestCut['Cut'])

# random forest
measures.random.forest.train <- HMeasure(prob.train$random.forest.actual, 
                                         prob.train$random.forest.predicted, 
                                         threshold = metricsByCutoff.test_rf$BestCut['Cut'])
measures.random.forest.test  <- HMeasure(prob.test$random.forest.actual, 
                                         prob.test$random.forest.predicted, 
                                         threshold = metricsByCutoff.test_rf$BestCut['Cut'])

# join measures in a single data frame
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

rm(measures.boosting.test, measures.boosting.train, 
   measures.decision.tree.test, measures.decision.tree.train,
   measures.logistic.test, measures.logistic.train,
   measures.random.forest.test, measures.random.forest.train)

invisible(gc())

kable(dplyr::select(measures, contains('train')), row.names = TRUE)
kable(dplyr::select(measures, contains('test')), row.names = TRUE)

## boxplot -------------------------------------------------------------------------

Score_Boxplot <- function(dataset, predicted, actual, title) {
  ggplot(data = dataset) +
    geom_boxplot(aes(y = predicted,
                     fill = as.factor(actual))) +
    coord_flip() +
    scale_fill_manual(values = c("0" = "#16a085", "1" = "#e74c3c")) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_economist() +
    labs(title = title,
         y = 'Score',
         fill = 'Defaulter |1 = True|') +
    theme(panel.grid = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = 0,
          plot.title = element_text(hjust = 0.5))
}

boxplots <- ggarrange(Score_Boxplot(prob.test, 
                        prob.test$logistic.predicted, 
                        prob.test$logistic.actual,
                        'Logistic Regression'),
          Score_Boxplot(prob.test, 
                        prob.test$decision.tree.predicted, 
                        prob.test$decision.tree.actual,
                        'Decision Tree'),
          Score_Boxplot(prob.test, 
                        prob.test$boosting.predicted, 
                        prob.test$boosting.actual,
                        'Boosting'),
          Score_Boxplot(prob.test, 
                        prob.test$random.forest.predicted, 
                        prob.test$random.forest.actual,
                        'Random Forest'))

boxplots <- annotate_figure(boxplots, 
                            top = text_grob("Score Boxplots of All Models", 
                                            color = "black", face = "bold", 
                                            size = 14))

boxplots

Score_Histograms <- function(dataset, predicted, actual, title) {
  ggplot(data = dataset) +
    geom_density(aes(x = predicted, fill = as.factor(actual)),
                 alpha = 0.5) +
    scale_fill_manual(values = c("0" = "#16a085", "1" = "#e74c3c")) +
    scale_x_continuous(limits = c(0, 1)) +
    theme_economist() +
    labs(title = title,
         y = 'Score',
         fill = 'Defaulter |1 = True|') +
    theme(panel.grid = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = 0,
          plot.title = element_text(hjust = 0.5))
}

density_plots <- ggarrange(Score_Histograms(prob.test, 
                        prob.test$logistic.predicted,
                        prob.test$logistic.actual,
                        'Logistic Regression'),
          Score_Histograms(prob.test, 
                        prob.test$decision.tree.predicted,
                        prob.test$decision.tree.actual,
                        'Decision Tree'),
          Score_Histograms(prob.test, 
                        prob.test$boosting.predicted,
                        prob.test$boosting.actual,
                        'Boosting'),
          Score_Histograms(prob.test, 
                        prob.test$random.forest.predicted,
                        prob.test$random.forest.actual,
                        'Random Forest'))

density_plots <- annotate_figure(density_plots, 
                            top = text_grob("Density of All Models", 
                                            color = "black", face = "bold", 
                                            size = 14))

density_plots

# KS plots ------------------------------------------------------------------------


KS_Plot <- function(zeros, ones, title) {
  group <- c(rep("Non Defaulters", length(zeros)), rep("Defauters", length(ones)))
  dat <- data.frame(KSD = c(zeros, ones), group = group)
  # create ECDF of data
  cdf1 <- ecdf(zeros) 
  cdf2 <- ecdf(ones) 
  # find min and max statistics to draw line between points of greatest distance
  minMax <- seq(min(zeros, ones), max(zeros, ones), length.out=length(zeros)) 
  x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )][1] 
  y0 <- cdf1(x0)[1]
  y1 <- cdf2(x0)[1]
  ks <- round(y0 - y1, 2)
  
  ggplot(dat, aes(x = KSD, group = group, color = group))+
    stat_ecdf(size=1) +
    geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
                 linetype = "dashed", color = "blue") +
    geom_point(aes(x = x0[1] , y = y0[1]), color="blue", size=4) +
    geom_point(aes(x = x0[1] , y = y1[1]), color="blue", size=4) +
    geom_label(aes(x = x0[1], y = y1[1] + (y0[1] - y1[1]) / 2, label = ks),
               color = 'black') +
    scale_x_continuous(limits = c(0, 1)) +
    labs(title = title,
         y = 'Acumulated Probability Distribution',
         x = 'Score') +
    theme_economist() +
    theme(legend.title = element_blank(),
          panel.grid = element_blank(),
          legend.position = 0,
          plot.title = element_text(hjust = 0.5))
}


KS_plots <- ggarrange(
  KS_Plot(prob.test$logistic.predicted[prob.test$logistic.actual == 0],
          prob.test$logistic.predicted[prob.test$logistic.actual == 1],
          'Logistic Regression'),
  KS_Plot(prob.test$decision.tree.predicted[prob.test$decision.tree.actual == 0],
          prob.test$decision.tree.predicted[prob.test$decision.tree.actual == 1],
          'Decision Tree'),
  KS_Plot(prob.test$boosting.predicted[prob.test$boosting.actual == 0],
          prob.test$boosting.predicted[prob.test$boosting.actual == 1],
          'Boosting'),
  KS_Plot(prob.test$random.forest.predicted[prob.test$random.forest.actual == 0],
          prob.test$random.forest.predicted[prob.test$random.forest.actual == 1],
          'Random Forest'))

KS_plots <- annotate_figure(KS_plots,
                            top = text_grob("KS Plots of All Models",
                                            color = "black", face = "bold",
                                            size = 14))

KS_plots

## ROC Curve ----------------------------------------------------------------------

# logistic regression

roc_logistic      <- roc(prob.test$logistic.actual,
                         prob.test$logistic.predicted)

# decision tree

roc_decision.tree <- roc(prob.test$decision.tree.actual, 
                         prob.test$decision.tree.predicted)

# boosting

roc_boosting      <- roc(prob.test$boosting.actual,
                         prob.test$boosting.predicted)

# random forest

roc_random.forest <- roc(prob.test$random.forest.actual,
                         prob.test$random.forest.predicted)


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

accuracy(score = prob.test$logistic.predicted, 
         actual = prob.test$logistic.actual, 
         threshold = 0.08)

# decision tree

accuracy(score = prob.test$decision.tree.predicted, 
         actual = prob.test$boosting.actual, 
         threshold = 0.1)

# boosting

accuracy(score = prob.test$boosting.predicted, 
         actual = prob.test$boosting.actual, 
         threshold = 0.41)

# random forest

accuracy(score = prob.test$random.forest.predicted, 
         actual = prob.test$boosting.actual, 
         threshold = 0.12)

