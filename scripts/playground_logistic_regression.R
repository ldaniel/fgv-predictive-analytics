# data prep -------------------------------------------------------------------

temp <- DataPrep()
temp <- dplyr::select(temp, -starts_with('x_district'), 
                      -x_prop_old_age_pension)

kable(tibble(variables = names(temp)))

# evaluating multicolinearity of remaining variables.
vars.quant <- select_if(temp, is.numeric)
VIF <- imcdiag(vars.quant, temp$y_loan_defaulter)

VIF_Table_Before <- tibble(variable = names(VIF$idiags[,1]),
                    VIF = VIF$idiags[,1]) %>% 
             arrange(desc(VIF))

knitr::kable(VIF_Table_Before)

# taking multicolinear variables from the dataset.
low_VIF <- filter(VIF_Table_Before, VIF <= 5)$variable
high_VIF <- filter(VIF_Table_Before, VIF > 5)$variable

high_VIF_dataset <- dplyr::select(temp, high_VIF)

cor_mtx <- cor(high_VIF_dataset)

ggcorrplot(cor_mtx, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlation Matrix of Loan Dataset", 
           ggtheme=theme_bw)

loan_dataset <- dplyr::select(temp, low_VIF, 
                       x_prop_interest_credited,
                       x_average_salary,
                       x_transaction_amount)

# evaluating multicolinearity of remaining variables.
vars.quant <- select_if(loan_dataset, is.numeric)

VIF <- imcdiag(vars.quant, loan_dataset$y_loan_defaulter)

VIF_Table_After <- tibble(variable = names(VIF$idiags[,1]),
                          VIF = VIF$idiags[,1]) %>% 
  arrange(desc(VIF))

kable(VIF_Table_After)

ggplot(VIF_Table_After, aes(x = fct_reorder(variable, VIF), y = log(VIF), label = round(VIF, 2))) + 
  geom_point(stat='identity', fill="black", size=15)  +
  geom_segment(aes(y = 0, 
                   yend = log(VIF), 
                   xend = variable), 
               color = "black") +
  geom_text(color="white", size=4) +
  geom_hline(aes(yintercept = log(5)), color = 'red', size = 2) +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  coord_flip() +
  theme_economist() +
  theme(legend.position = 'none', 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(x = 'Variable',
       y = NULL,
       title = 'Variance Inflation Factor',
       subtitle="Checking for multicolinearity in X's variables.
       Variables with VIF more than 5 will be droped from the model")

cor_mtx <- cor(vars.quant)

ggcorrplot(cor_mtx, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlation Matrix of Loan Dataset", 
           ggtheme=theme_bw)


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


# fit the logistic model -------------------------------------------------------------

names  <- names(loan_dataset)
f_full <- as.formula(paste("y_loan_defaulter ~",
                           paste(names[!names %in% "y_loan_defaulter"], collapse = " + ")))

logistic.full <- glm(f_full, data= data.train, family= binomial(link='logit'))

names(logistic.full$coefficients) <- stringr::str_sub(names(logistic.full$coefficients), 1, 25)
summary(logistic.full)

#logistic.step <- stepAIC(logistic.full, direction = 'both', trace = TRUE)
logistic.step <- step(logistic.full, direction = "backward", test = "F")

names(logistic.step$coefficients) <- stringr::str_sub(names(logistic.step$coefficients), 1, 25)
summary(logistic.step)
