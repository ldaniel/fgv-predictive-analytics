# GetGenderFromBirthnumber ----------------------------------------------------
# The birth_number column is given in the form of YYMMDD for men,
# and YYMM+50DD for women. The objective of this function is to return
# the gender of the client via the birth_number.
GetGenderFromBirthnumber <- function(var_birth_number) {
  
  month <- substr(var_birth_number, 3, 4)
  result <- ifelse(as.integer(month) > 50, "female", "male")
  
  return(as.factor(result))
}

# GetBirthdateFromBirthnumber -------------------------------------------------
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

# ConvertToDate ---------------------------------------------------------------
# The objective of this function is to convert the strange bank date style 
# to the regular R Date datatype.
ConvertToDate <- function(var_date) {
  
  year <- paste("19", substr(var_date, 1, 2), sep="")
  month <- substr(var_date, 3, 4)
  day <- substr(var_date, 5, 6)
  result <- as.Date(paste(year, "-", month, "-", day, sep=""), format = "%Y-%m-%d")
  
  return(result)
}

# GetAgeFromBirthnumber -------------------------------------------------------
# The objective of this function is to get age given the birth_number.
GetAgeFromBirthnumber <- function(var_birth_number) {
  
  base_year <- 99 # considering 1999 as the base year for this exercise
  year <- substr(var_birth_number, 1, 2)
  result <- base_year - as.integer(year)
  
  return(result)
}

# calculateModelMetrics -------------------------------------------------------
# The objective of this function is to calculate main metrics of model performance according to a cutoff value.
calculateModelMetrics <- function(cutData, realData, predData){
  cuttedData <- as.factor(ifelse(predData>=cutData, 1, 0))
  
  invisible(capture.output(out <- CrossTable(realData, cuttedData, prop.c = F, prop.t = F, prop.r = T, prop.chisq = F)))
  
  out <- as.data.frame(out) %>% 
    mutate(merged=paste0(t.x, t.y)) %>% 
    select(merged, val=t.Freq)
  
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

# modelMetrics ----------------------------------------------------------------
# The objective of this function is to calculate main metrics of model performance 
# for cutoffs from 0-1 based on given step.
modelMetrics <- function(realData, predData, stepping = 0.01, 
                         plot_title = "TPR/TNR by cutoff over full dataset"){
  probCuts <- seq(from = 0, to = 1, by = stepping)
  out <- bind_rows(lapply(probCuts, calculateModelMetrics, realData = realData, predData = predData))
  out <- out[complete.cases(out),] %>% mutate(Difference = abs(TPR-TNR))
  
  best <- out %>% arrange(Difference) %>% head(1) %>% select(-Difference)
  
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

# SplitTestTrainDataset -------------------------------------------------------
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