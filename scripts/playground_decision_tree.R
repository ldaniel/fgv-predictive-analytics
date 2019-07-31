# data prep -------------------------------------------------------------------

loan_dataset_DT <-source_dataset
loan_dataset_DT <- dplyr::select(loan_dataset_DT,-x_prop_old_age_pension)

kable(tibble(variables = names(loan_dataset_DT)))

# sampling ----------------------------------------------------------------------------

SplitDataset <- SplitTestTrainDataset(loan_dataset_DT)
data.train_DT <- SplitDataset$data.train
data.test_DT <- SplitDataset$data.test

kable(SplitDataset$event.proportion)

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
