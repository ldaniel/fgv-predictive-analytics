# loading required libraries --------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)
library(forcats)
library(feather)
library(corrplot)
library(mctest)
library(caret)
library(hmeasure)
library(pROC)
library(rms)
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

temp <- left_join(loan, disposition, by = c('account_id', 'type')) %>% 
  left_join(client, by = 'client_id') %>%
  left_join(district, by = 'district_id') %>% 
  left_join(creditcard, by = 'disp_id') %>% 
  left_join(account_balance, by = 'account_id') %>% 
  left_join(account_transaction_pattern, by = 'account_id') %>% 
  mutate(card_age_month = (issued %--% 
                             make_date(1998, 12, 31)) / months(1),
         last_transaction_age_days = ((last_transaction_date.y %--% 
                                         make_date(1998, 12, 31)) / days(1)),
         has_card = ifelse(type.y == 'no card', 0, 1)) %>% 
  dplyr::select(c("amount.x", "duration", "payments", "status", "defaulter", 
                  "contract_status", "gender", "age", "district_name", 
                  "region", "no_of_inhabitants", 
                  "no_of_municip_inhabitants_less_499", 
                  "no_of_municip_500_to_1999", "no_of_municip_2000_to_9999", 
                  "no_of_municip_greater_10000", "no_of_cities", 
                  "ratio_of_urban_inhabitants", 
                  "average_salary", "unemploymant_rate_1995", 
                  "unemploymant_rate_1996", 
                  "no_of_enterpreneurs_per_1000_inhabitants", 
                  "no_of_commited_crimes_1995", 
                  "no_of_commited_crimes_1996", "type.y", 
                  "card_age_month","account_balance", 
                  "avg_balance","transaction_count", "amount.y", 
                  "last_transaction_age_days", "prop_old_age_pension", 
                  "prop_insurance_payment", 
                  "prop_sanction_interest","prop_household", 
                  "prop_statement", "prop_interest_credited", 
                  "prop_loan_payment", "prop_other", "has_card"))

colnames(temp) <- c("x_loan_amount", "x_loan_duration", "x_loan_payments", 
                    "x_loan_status", "y_loan_defaulter", "x_loan_contract_status",
                    "x_client_gender", "x_client_age", 
                    "x_district_name", "x_region", 
                    "x_no_of_inhabitants", "x_no_of_municip_inhabitants_less_499", 
                    "x_no_of_municip_500_to_1999", "x_no_of_municip_2000_to_9999", 
                    "x_no_of_municip_greater_10000", "x_no_of_cities", 
                    "x_ratio_of_urban_inhabitants", 
                    "x_average_salary", "x_unemploymant_rate_1995", 
                    "x_unemploymant_rate_1996", 
                    "x_no_of_enterpreneurs_per_1000_inhabitants", 
                    "x_no_of_commited_crimes_1995", 
                    "x_no_of_commited_crimes_1996", "x_card_type", 
                    "x_card_age_month","x_account_balance", 
                    "x_avg_account_balance","x_transaction_count", 
                    "x_transaction_amount", "x_last_transaction_age_days", 
                    "x_prop_old_age_pension", "x_prop_insurance_payment", 
                    "x_prop_sanction_interest","x_prop_household","x_prop_statement",
                    "x_prop_interest_credited", "x_prop_loan_payment", "x_prop_other",
                    "x_has_card")

temp <- dplyr::select(temp, y_loan_defaulter, everything())

temp$x_card_type = ifelse(is.na(temp$x_card_type), 'no card', 
                          as.character(temp$x_card_type))

temp$x_has_card = ifelse(temp$x_card_type == 'no card', 0, 1)

temp$x_card_age_month = ifelse(is.na(temp$x_card_age_month), 0, 
                               temp$x_card_age_month)

temp$y_loan_defaulter = as.numeric(temp$y_loan_defaulter)

# taking out variables from the dataset that have no variability,
# redudant or not applicable to the model.
temp <- dplyr::select(temp, -c(x_prop_old_age_pension, 
                               x_district_name, 
                               x_region, 
                               x_loan_status, 
                               x_loan_contract_status, 
                               x_prop_sanction_interest))


kable(tibble(variables = names(temp)))


# evaluating multicolinearity of remaining variables.
vars.quant <- select_if(temp, is.numeric)
VIF <- imcdiag(vars.quant, temp$y_loan_defaulter)

VIF_Table_Before <- tibble(variable = names(VIF$idiags[,1]),
                    VIF = VIF$idiags[,1]) %>% 
             arrange(desc(VIF))

knitr::kable(VIF_Table_Before)

ggplot(VIF_Table_Before, aes(x = fct_reorder(variable, VIF), y = log(VIF), label = round(VIF, 2))) + 
  geom_point(stat='identity', fill="black", size=15)  +
  geom_segment(aes(y = 0, 
                   yend = log(VIF), 
                   xend = variable), 
               color = "black") +
  geom_text(color="white", size=4) +
  geom_hline(aes(yintercept = log(4)), color = 'red', size = 2) +
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

# taking multicolinear variables from the dataset.
temp <- dplyr::select(temp, -c(x_prop_insurance_payment,
                               x_prop_household,
                               x_prop_statement,
                               x_prop_loan_payment,
                               x_prop_other,
                               x_no_of_inhabitants,
                               x_no_of_commited_crimes_1996,
                               x_transaction_amount,
                               x_transaction_count,
                               x_unemploymant_rate_1996,
                               x_unemploymant_rate_1995,
                               x_average_salary,
                               x_no_of_cities))

loan_reg_dataset <- temp

# evaluating multicolinearity of remaining variables.
vars.quant <- select_if(loan_reg_dataset, is.numeric)

VIF <- imcdiag(vars.quant, loan_reg_dataset$y_loan_defaulter)

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
  geom_hline(aes(yintercept = log(4)), color = 'red', size = 2) +
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
index <- caret::createDataPartition(loan_reg_dataset$y_loan_defaulter, 
                                    p= 0.7,list = FALSE)

data.train <- loan_reg_dataset[index, ]
data.test  <- loan_reg_dataset[-index,]


event_proportion <- bind_rows(prop.table(table(loan_reg_dataset$y_loan_defaulter)),
                              prop.table(table(data.train$y_loan_defaulter)),
                              prop.table(table(data.test$y_loan_defaulter)))

event_proportion$scope = ''
event_proportion$scope[1] = 'full dataset'
event_proportion$scope[2] = 'train dataset'
event_proportion$scope[3] = 'test dataset'

event_proportion <- select(event_proportion, scope, everything())

kable(event_proportion)

# fit the logistic model -------------------------------------------------------------

model_1 <- glm(data = data.train, formula = y_loan_defaulter ~ .,
               family= binomial(link='logit'))

names(model_1$coefficients) <- stringr::str_sub(names(model_1$coefficients), 1, 25)

summary(model_1)

model_2 <- glm(data = data.train, formula = y_loan_defaulter ~ x_loan_amount +
                 x_loan_duration + x_has_card + x_prop_interest_credited,
               family= binomial(link='logit'))

names(model_2$coefficients) <- stringr::str_sub(names(model_2$coefficients), 1, 25)

summary(model_2)

# model evaluation -------------------------------------------------------------------

## making preditions -----------------------------------------------------------------

glm.prob.train.1 <- predict(model_1, type = "response")
glm.prob.test.1 <- predict(model_1, newdata = data.test, type= "response")

glm.prob.train.2 <- predict(model_2, type = "response")
glm.prob.test.2 <- predict(model_2, newdata = data.test, type= "response")


## getting measures ------------------------------------------------------------------
glm.train.1 <- HMeasure(data.train$y_loan_defaulter, glm.prob.train.1, threshold = 0.5)
glm.test.1  <- HMeasure(data.test$y_loan_defaulter, glm.prob.test.1, threshold = 0.5)

glm.train.2 <- HMeasure(data.train$y_loan_defaulter, glm.prob.train.2, threshold = 0.5)
glm.test.2 <- HMeasure(data.test$y_loan_defaulter, glm.prob.test.2, threshold = 0.5)

measures <- t(bind_rows(glm.train.1$metrics,
                      glm.test.1$metrics,
                      glm.train.2$metrics,
                      glm.test.2$metrics)) %>% as_tibble(., rownames = NA)

colnames(measures) <- c('model 1 - train','model 1 - test',
                        'model 2 - train','model 2 - test')

measures$metric = rownames(measures)

measures <- select(measures, metric, everything())

kable(measures, row.names = FALSE)

## boxplot ---------------------------------------------------------------------------
boxplot(glm.prob.test.1 ~ data.test$y_loan_defaulter,
        col= c("red", "green"), 
        horizontal= T,
        xlab = 'Probability Prediction',
        ylab = 'Loan Defaulter')

boxplot(glm.prob.test.2 ~ data.test$y_loan_defaulter,
        col= c("red", "green"), 
        horizontal= T,
        xlab = 'Probability Prediction',
        ylab = 'Loan Defaulter')

## ROC Curve -------------------------------------------------------------------------
roc_1 <- roc(data.test$y_loan_defaulter, glm.prob.test.1)
roc_2 <- roc(data.test$y_loan_defaulter, glm.prob.test.2)

y1 <- roc_1$sensitivities
x1 <- 1-roc_1$specificities

y2 <- roc_2$sensitivities
x2 <- 1-roc_2$specificities

plot(x1, y1,  type="n",
     xlab = "False Positive Rate (Specificities)", 
     ylab= "True Positive Rate (Sensitivities)")

lines(x1, y1,lwd=3,lty=1, col="red") 
lines(x2, y2,lwd=3,lty=1, col="blue")

abline(0,1, lty=2)

# accuracy metrics -------------------------------------------------------------------

threshold <- 0.1

fitted.results.2 <- ifelse(glm.prob.test.2 > threshold ,1 ,0)

misClasificError <- mean(fitted.results.2 != data.test$y_loan_defaulter)

misClassCount <- misclassCounts(fitted.results.2, data.test$y_loan_defaulter)

paste('Model General Accuracy of: ', round((1 - misClassCount$metrics['ER']) * 100, 2), '%', sep = '')
paste('True Positive Rate of    : ', round(misClassCount$metrics['TPR'] * 100, 2), '%', sep = '')

kable(misClassCount$conf.matrix)


