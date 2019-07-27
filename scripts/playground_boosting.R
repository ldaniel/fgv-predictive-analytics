
# data prep -------------------------------------------------------------------
temp <- DataPrep()
temp <- dplyr::select(temp, -starts_with('x_district'), 
                      -x_prop_old_age_pension)

kable(tibble(variables = names(temp)))

#Analysis on base
summary(temp)
aggr(temp)
View(temp)

boxplot(temp$x_account_balance ~ temp$y_loan_defaulter)
boxplot(temp$x_avg_account_balance ~ temp$y_loan_defaulter)



# sampling ----------------------------------------------------------------------------

set.seed(12345)
index <- caret::createDataPartition(temp$y_loan_defaulter, 
                                    p= 0.7,list = FALSE)
data.train <- temp[index, ]
data.test  <- temp[-index,]

event_proportion <- bind_rows(prop.table(table(temp$y_loan_defaulter)),
                              prop.table(table(data.train$y_loan_defaulter)),
                              prop.table(table(data.test$y_loan_defaulter)))

event_proportion$scope = ''
event_proportion$scope[1] = 'full dataset'
event_proportion$scope[2] = 'train dataset'
event_proportion$scope[3] = 'test dataset'

event_proportion <- select(event_proportion, scope, everything())

kable(event_proportion)

# transforming the answer var on factor------------------------------------------------------

data.train$y_loan_defaulter <- as.factor(data.train$y_loan_defaulter)
data.test$y_loan_defaulter  <- as.factor(data.test$y_loan_defaulter)


# MODELAGEM DOS DADOS - M?TODOS DE ENSEMBLE

names  <- names(data.train) # saving the name of all vars to put on formula
f_full <- as.formula(paste("y_loan_defaulter ~",
                           paste(names[!names %in% "y_loan_defaulter"], collapse = " + ")))


# fit the decision tree model with boosting--------------------------------------------------

library(adabag)

boost <- boosting(f_full, data= data.train, mfinal= 250, 
                  coeflearn = "Freund", 
                  control = rpart.control(minbucket= 50,maxdepth = 1))


# analysing the error evolution by each interation
plot(errorevol(boost, data.train))


# Analysing var importance on model
var_importance <- boost$importance[order(boost$importance,decreasing = T)]
var_importance
importanceplot(boost)


# Applying the boosted model on data test to test
boost.prob.train <- predict.boosting(boost, data.train)$prob[,2]
boost.prob.test  <- predict.boosting(boost, data.test)$prob[,2]


# Comportamento da saida do modelo
hist(boost.prob.test, breaks = 25, col = "lightblue",xlab= "Probabilidades",
     ylab= "Frequ?ncia",main= "Boosting")


boxplot(boost.prob.test ~ data.test$y_loan_defaulter,col= c("green", "red"), horizontal= T)






