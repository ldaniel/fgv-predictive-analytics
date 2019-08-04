# specific data ingestion functions -------------------------------------------

# The birth_number column is given in the form of YYMMDD for men,
# and YYMM+50DD for women. The objective of this function is to return
# the gender of the client via the birth_number.
GetGenderFromBirthnumber <- function(var_birth_number) {
  
  month <- substr(var_birth_number, 3, 4)
  result <- ifelse(as.integer(month) > 50, "female", "male")
  
  return(as.factor(result))
}

# The birth_number column is given in the form of YYMMDD for men,
# and YYMM+50DD for women. The objective of this function is to return
# the final birthday as Date.
GetBirthdateFromBirthnumber <- function(var_birth_number, var_gender) {
 
  year <- paste("19", substr(var_birth_number, 1, 2), sep="")
  month <- ifelse(var_gender == "male", substr(var_birth_number, 3, 4), as.integer(substr(var_birth_number, 3, 4)) - 50)
  day <- substr(var_birth_number, 5, 6)
  result <- as.Date(paste(year, "-", month, "-", day, sep=""), format = "%Y-%m-%d")
  
  return(result)
}

# The objective of this function is to convert the strange bank date style 
# to the regular R Date datatype.
ConvertToDate <- function(var_date) {
  
  year <- paste("19", substr(var_date, 1, 2), sep="")
  month <- substr(var_date, 3, 4)
  day <- substr(var_date, 5, 6)
  result <- as.Date(paste(year, "-", month, "-", day, sep=""), format = "%Y-%m-%d")
  
  return(result)
}

# The objective of this function is to get age given the birth_number.
GetAgeFromBirthnumber <- function(var_birth_number) {
  
  base_year <- 99 # considering 1999 as the base year for this exercise
  year <- substr(var_birth_number, 1, 2)
  result <- base_year - as.integer(year)
  
  return(result)
}

# metrics functions -----------------------------------------------------------

# The objective of this function is to calculate main metrics of model performance according to a cutoff value.
calculateModelMetrics <- function(cutData, realData, predData){
  cuttedData <- as.factor(ifelse(predData>=cutData, 1, 0))
  
  invisible(capture.output(out <- CrossTable(realData, cuttedData, 
                                             prop.c = F, prop.t = F, prop.r = T, prop.chisq = F)))
  
  out <- as.data.frame(out) %>% 
    mutate(merged=paste0(t.x, t.y)) %>% 
    dplyr::select(merged, val=t.Freq)
  
  TN <- filter(out, merged == "00")$val[1]
  FP <- filter(out, merged == "01")$val[1]
  FN <- filter(out, merged == "10")$val[1]
  TP <- filter(out, merged == "11")$val[1]
  
  return(data.frame(Cut = cutData,
                    TN = TN, 
                    FP = FP,
                    FN = FN, 
                    TP = TP,
                    TPR = TP/(TP+FN), TNR=TN/(TN+FP),
                    Error = (FP+FN)/(TP+TN+FP+FN),
                    Precision = TP/(TP+FP),
                    F1 = 2*(TP/(TP+FN))*(TP/(TP+FP))/((TP/(TP+FP)) + (TP/(TP+FN)))))
}

# The objective of this function is to calculate main metrics of model performance 
# for cutoffs from 0-1 based on given step.
modelMetrics <- function(realData, predData, stepping = 0.01, 
                         plot_title = "TPR/TNR by cutoff over full dataset"){
  probCuts <- seq(from = 0, to = 1, by = stepping)
  out <- bind_rows(lapply(probCuts, calculateModelMetrics, realData = realData, predData = predData))
  out <- out[complete.cases(out),] %>% mutate(Difference = abs(TPR-TNR))
  
  best <- out %>% arrange(Difference) %>% head(1) %>% dplyr::select(-Difference)
  
  p <- plot_ly(x = ~out$Cut, y = ~out$Difference, name = 'Abs. Diff.', type = 'bar', opacity = 0.3) %>% 
    add_trace(x = ~out$Cut, y = ~out$TPR, name = 'TPR', type = 'scatter', mode = 'lines', opacity = 1) %>% 
    add_trace(x = ~out$Cut, y = ~out$TNR, name = 'TNR', type = 'scatter', mode = 'lines', opacity = 1) %>% 
    add_text(x = best$Cut, y = best$TPR, text = best$Cut, opacity = 1) %>% 
    layout(xaxis = list(title = "Cutoff Value"),
           yaxis = list(title = "True Ratio (%)"),
           title = plot_title)
  
  return(list(TableResults = out,
              BestCut = best,
              Plot = p))
}

# accuracy ----------------------------------------------------------------
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

# data preparation functions --------------------------------------------------
# The objective of this function is to split a given dataset 
# in train and test datasets
SplitTestTrainDataset <- function(dataset) {
  set.seed(12345)
  
  dataset$y_loan_defaulter <- as.integer(dataset$y_loan_defaulter)
  
  index <- caret::createDataPartition(dataset$y_loan_defaulter, 
                                      p= 0.7, list = FALSE)
  data.train <- dataset[index, ]
  data.test  <- dataset[-index,]
  
  # checking event proportion in sample and test datasets against full dataset.
  event_proportion <- bind_rows(prop.table(table(dataset$y_loan_defaulter)),
                                prop.table(table(data.train$y_loan_defaulter)),
                                prop.table(table(data.test$y_loan_defaulter)))
  
  event_proportion$scope = ''
  event_proportion$scope[1] = 'full dataset'
  event_proportion$scope[2] = 'train dataset'
  event_proportion$scope[3] = 'test dataset'
  
  event_proportion <- select(event_proportion, scope, everything())
  
  SplitDataset <-  list()
  SplitDataset$data.train <- data.train
  SplitDataset$data.test  <- data.test
  SplitDataset$event.proportion <- event_proportion
  
  return(SplitDataset)
  
}

# plot functions --------------------------------------------------------------
# functions used in the evaluation step to compare the models.

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

KS_Plot <- function(zeros, ones, title) {
  group <- c(rep("Non Defaulters", length(zeros)), rep("Defauters", length(ones)))
  dat <- data.frame(KSD = c(zeros, ones), group = group)
  cdf1 <- ecdf(zeros) 
  cdf2 <- ecdf(ones) 
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
         y = 'Cumulative Probability Distribution',
         x = 'Score') +
    theme_economist() +
    theme(legend.title = element_blank(),
          panel.grid = element_blank(),
          legend.position = 0,
          plot.title = element_text(hjust = 0.5))
}

Plot_ROC <- function(dataset, smooth_opt = FALSE) {
  roc_logistic      <- roc(logistic.actual ~ logistic.predicted,
                           dataset,
                           smooth = smooth_opt)
  
  roc_decision.tree <- roc(decision.tree.actual ~ decision.tree.predicted,
                           dataset,
                           smooth = smooth_opt)
  
  roc_boosting      <- roc(boosting.actual ~ boosting.predicted,
                           dataset,
                           smooth = smooth_opt)
  
  roc_random.forest <- roc(random.forest.actual ~ random.forest.predicted,
                           dataset,
                           smooth = smooth_opt)
  
  p <- ggplot() +
    geom_line(aes(x = 1 - roc_logistic$specificities, 
                  y = roc_logistic$sensitivities, 
                  colour = 'Logistic Regression'), # red
              size = 1,
              linetype = 1,
              alpha = 0.7) +
    geom_line(aes(x = 1 - roc_decision.tree$specificities, 
                  y = roc_decision.tree$sensitivities,
                  colour = 'Decision Tree'), # blue
              size = 1,
              linetype = 1,
              alpha = 0.7) +
    geom_line(aes(x = 1 - roc_boosting$specificities, 
                  y = roc_boosting$sensitivities,
                  colour = 'Boosting'), # green
              size = 1,
              linetype = 1,
              alpha = 0.7) +
    geom_line(aes(x = 1 - roc_random.forest$specificities, 
                  y = roc_random.forest$sensitivities,
                  colour = 'Random Forest'), # purple
              size = 2,
              linetype = 1,
              alpha = 1) +
    geom_abline(aes(intercept = 0, slope = 1),
                linetype = 2,
                size = 1) +
    scale_colour_manual(name = NULL,
                        breaks = c('Logistic Regression', 
                                   'Decision Tree',
                                   'Boosting', 
                                   'Random Forest'),
                        labels = c('Logistic Regression', 
                                   'Decision Tree',
                                   'Boosting', 
                                   'Random Forest'),
                        values = c('#C0392B', 
                                   '#3498DB', 
                                   '#28B463', 
                                   '#9B59B6')) +
    labs(y = 'True Positive Rate',
         x = 'False Positive Rate',
         title = 'Receiver Oerating Characteristic Curve - ROC',
         subtitle = 'Random Forest is the model that best discriminate Defaulters and Non-Defaulters') +
    theme_economist() +
    theme(panel.grid = element_blank())
  
  return (p)
}