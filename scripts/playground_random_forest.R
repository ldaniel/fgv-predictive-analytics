# data prep ---------------------------------------------------------------------------

loan_dataset_rf <- source_dataset

DistinctCounts <- loan_dataset_rf %>% summarise_all(n_distinct) %>% t %>% as.data.frame

loan_dataset_rf %<>% 
  mutate(y_loan_defaulter = as.factor(y_loan_defaulter)) %>% 
  select(-x_prop_old_age_pension)

# sampling ----------------------------------------------------------------------------

SplitDataset <- SplitTestTrainDataset(loan_dataset_rf)
data.train_rf <- SplitDataset$data.train
data.test_rf <- SplitDataset$data.test

kable(SplitDataset$event.proportion)

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

# # fit the random forest model using caret customized train function -------------------
# 
# control <- trainControl(method="repeatedcv", number=5, repeats=3, verboseIter = TRUE, allowParallel = TRUE)
# tuneparam <- expand.grid(.mtry=c(5, 25, 50, 75, 85, 100, 115, 125, 150, 175, 200),
#                          .ntree=c(1000, 3000, 5000, 7000, 9000, 10000))
# evalmetric <- "Accuracy"
# 
# set.seed(12345)
# 
# ini <- Sys.time()
# cat(paste0("\nStarted RF training at: ", ini, " ...\n\n"))
# 
# rf.full <- train(y_loan_defaulter ~ .,
#                  data=data.train_rf,
#                  method=customRF,
#                  metric=evalmetric,
#                  tuneGrid=tuneparam,
#                  trControl=control,
#                  importance=TRUE)
# 
# elapsedTime <- difftime(Sys.time(), ini, units = "auto")
# cat(paste0("\n\nFinished RF training. Total time taken: ", round(elapsedTime, 2), " ", units(elapsedTime)))
# 
# summary(rf.full)
# plot(rf.full)
# 
# # Best selected parameters: mtry = 85 and ntree = 3000 (time taken: ~5 hours)
# 
# saveRDS(rf.full, "./models/random_forest.rds")

# to save time, only load fitted model ------------------------------------------------
rf.full <- readRDS("./models/random_forest.rds")

# generate predicted columns ----------------------------------------------------------

data.test_rf$y_loan_defaulter_predicted <- predict(rf.full, newdata = data.test_rf, type = "prob")[,2]
data.train_rf$y_loan_defaulter_predicted <- predict(rf.full, newdata = data.train_rf, type = "prob")[,2]
loan_dataset_rf$y_loan_defaulter_predicted <- predict(rf.full, newdata = loan_dataset_rf, type = "prob")[,2]

# calculate TNR and TPR for multi-cuts for RF -----------------------------------------

metricsByCutoff <- modelMetrics(loan_dataset_rf$y_loan_defaulter, loan_dataset_rf$y_loan_defaulter_predicted)
p <- plot_ly(x = ~metricsByCutoff$Cut, y = ~metricsByCutoff$TNR, name = 'TNR', type = 'scatter', mode = 'lines')
p <- p %>% add_trace(x = ~metricsByCutoff$Cut, y = ~metricsByCutoff$TPR, name = 'TPR', type = 'scatter', mode = 'lines')
p %>% layout(xaxis = list(title = "Cutoff Value"),
             yaxis = list(title = "True Ratio (%)"))

# Optimized cut-off selected parameter: 0.15

# calculate metrics for selected parameters in train/test/full dataset ----------------

kable(event_proportion)

