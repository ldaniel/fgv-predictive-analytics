# loading required libraries ----------------------------------------------------------
# libraries for data prep
library(dplyr)
library(readr)
library(magrittr)
library(forcats)
library(lubridate)
library(stringr)
library(feather)
library(fastDummies)
library(reshape2)
library(knitr)

#libraries for plots
library(ggplot2)
library(ggthemes)
library(ggcorrplot)
library(ggpubr)
library(plotly)

# libraries for data clean
library(VIM)
library(rms)
library(mctest)

# libraries for modeling
library(caret)
library(gmodels)
library(MASS)
library(rpart)
library(rpart.plot)
library(adabag)
library(randomForest)

# libraries for measures
library(hmeasure)
library(pROC)

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

source('scripts/model_01_logistic_regression.R')
rm(clean, clean_dataset, cor_mtx_clean, cor_mtx_full, cor_mtx_high_VIF,
   dummy_variables, high_VIF_dataset, high_VIF_correlogram_before, 
   logistic.full, SplitDataset, vars.quant, VIF, VIF_Table_After, 
   VIF_Table_Before, full, correl_threshold, dummy_variables_high,
   dummy_variables_low, high_VIF, low_VIF, prop_variables, reject_variables_vector)
invisible(gc)

source('scripts/model_02_decision_tree.R')
rm(SplitDataset)
invisible(gc)

source('scripts/model_03_boosting.R')
rm(SplitDataset, f_full, names, var_importance)
invisible(gc())

source('scripts/model_04_random_forest.R')
rm(customRF, DistinctCounts, FitResults, metricsByCutoff.full, metricsByCutoff.test,
   metricsByCutoff.train, SplitDataset)
invisible(gc())

# model evaluation -------------------------------------------------------------------

## making preditions for each model and consilidating in a single data frame
prob.full   <-  list()
prob.train  <-  list()
prob.test   <-  list()

# getting predicted and actual values in the full dataset
prob.full$logistic.actual         <- loan_dataset_logistic$y_loan_defaulter
prob.full$logistic.predicted      <- predict(logistic.step, type = "response", 
                                             newdata = loan_dataset_logistic)
prob.full$decision.tree.actual    <- loan_dataset_DT$y_loan_defaulter
prob.full$decision.tree.predicted <- predict(tree.prune, type = "prob", 
                                             newdata = loan_dataset_DT)[, 2]
prob.full$boosting.actual         <- loan_dataset_boost$y_loan_defaulter
prob.full$boosting.predicted      <- predict.boosting(boost, 
                                                      newdata = loan_dataset_boost)$prob[, 2]
prob.full$random.forest.actual    <- loan_dataset_rf$y_loan_defaulter
prob.full$random.forest.predicted <- predict(rf.full, type = "prob", 
                                             newdata = loan_dataset_rf)[, 2]

# getting predicted and actual values in the train dataset
prob.train$logistic.actual         <- data.train_logistic$y_loan_defaulter
prob.train$logistic.predicted      <- predict(logistic.step, type = "response", 
                                              newdata = data.train_logistic)
prob.train$decision.tree.actual    <- data.train_DT$y_loan_defaulter
prob.train$decision.tree.predicted <- predict(tree.prune, type = "prob", 
                                              newdata = data.train_DT)[, 2]
prob.train$boosting.actual         <- data.train_boost$y_loan_defaulter
prob.train$boosting.predicted      <- predict.boosting(boost, newdata = 
                                                          data.train_boost)$prob[, 2]
prob.train$random.forest.actual    <- data.train_rf$y_loan_defaulter
prob.train$random.forest.predicted <- predict(rf.full, type = "prob", 
                                              newdata = data.train_rf)[, 2]

# getting predicted and actual values in the test dataset
prob.test$logistic.actual         <- data.test_logistic$y_loan_defaulter
prob.test$logistic.predicted      <- predict(logistic.step, type = "response", 
                                             newdata = data.test_logistic)
prob.test$decision.tree.actual    <- data.test_DT$y_loan_defaulter
prob.test$decision.tree.predicted <- predict(tree.prune, type = "prob", 
                                             newdata = data.test_DT)[, 2]
prob.test$boosting.actual         <- data.test_boost$y_loan_defaulter
prob.test$boosting.predicted      <- predict.boosting(boost, newdata = 
                                                         data.test_boost)$prob[, 2]
prob.test$random.forest.actual    <- data.test_rf$y_loan_defaulter
prob.test$random.forest.predicted <- predict(rf.full, type = "prob", 
                                             newdata = data.test_rf)[, 2]

# converting lists into tibble
prob.full   <- prob.full   %>% as_tibble()
prob.train  <- prob.train  %>% as_tibble()
prob.test   <- prob.test   %>% as_tibble()

# clean global environment
rm(loan_dataset_boost, loan_dataset_DT, loan_dataset_logistic, loan_dataset_rf,
   data.test_boost, data.test_DT, data.test_logistic, data.test_rf,
   data.train_boost, data.train_DT, data.train_logistic, data.train_rf)
invisible(gc())

## getting cut off measures -----------------------------------------------------------------

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

cutoffs <- plotly::subplot(metricsByCutoff.test_log$Plot,
                   metricsByCutoff.test_DT$Plot,
                   metricsByCutoff.test_boost$Plot,
                   metricsByCutoff.test_rf$Plot,
                   nrows = 2) %>% hide_legend() %>%
                   layout(title="Comparing cut off measures of all models")
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

## density plots --------------------------------------------------------------------

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

## boxplot -------------------------------------------------------------------------

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

# KS plots ------------------------------------------------------------------------

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

ROC_test <- Plot_ROC(prob.test, smooth_opt = FALSE)

print(ROC_test)

# accuracy metrics ----------------------------------------------------------------

# logistic regression
accuracy(score = prob.test$logistic.predicted, 
         actual = prob.test$logistic.actual, 
         threshold = metricsByCutoff.test_log[["BestCut"]][["Cut"]])

# decision tree
accuracy(score = prob.test$decision.tree.predicted, 
         actual = prob.test$decision.tree.actual, 
         threshold = metricsByCutoff.test_DT[["BestCut"]][["Cut"]])

# boosting
accuracy(score = prob.test$boosting.predicted, 
         actual = prob.test$boosting.actual, 
         threshold = metricsByCutoff.test_boost[["BestCut"]][["Cut"]])

# random forest
accuracy(score = prob.test$random.forest.predicted, 
         actual = prob.test$random.forest.actual, 
         threshold = metricsByCutoff.test_rf[["BestCut"]][["Cut"]])
