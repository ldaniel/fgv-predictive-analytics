# data prep -------------------------------------------------------------------

loan_dataset_DT <- DataPrep()
loan_dataset_DT <- dplyr::select(loan_dataset_DT, 
                      -x_prop_old_age_pension)

kable(tibble(variables = names(loan_dataset_DT)))

# sampling ----------------------------------------------------------------------------

set.seed(12345)
index <- caret::createDataPartition(loan_dataset_DT$y_loan_defaulter, 
                                    p= 0.7,list = FALSE)
data.train_DT <- loan_dataset_DT[index, ]
data.test_DT  <- loan_dataset_DT[-index,]

event_proportion <- bind_rows(prop.table(table(loan_dataset_DT$y_loan_defaulter)),
                              prop.table(table(data.train_DT$y_loan_defaulter)),
                              prop.table(table(data.test_DT$y_loan_defaulter)))

event_proportion$scope = ''
event_proportion$scope[1] = 'full dataset'
event_proportion$scope[2] = 'train dataset'
event_proportion$scope[3] = 'test dataset'

event_proportion <- select(event_proportion, scope, everything())

kable(event_proportion)

# fit the decision tree model -------------------------------------------------------------

tree.full <- rpart(data= data.train_DT, y_loan_defaulter ~ .,
                   control = rpart.control(minbucket=10),
                   method = "class")

tree.full
summary(tree.full)
rpart.plot(tree.full)

printcp(tree.full)
plotcp(tree.full)

tree.prune <- prune(tree.full, cp= tree.full$cptable[which.min(tree.full$cptable[,"xerror"]),"CP"])
tree.prune <- tree.full

plotcp(tree.prune)
rpart.plot(tree.prune)
