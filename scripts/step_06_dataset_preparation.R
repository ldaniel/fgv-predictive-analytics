# dataset preparation ---------------------------------------------------------

# The objective of this step is to return a DataFrame to be used in predictive 
# modeling. Therefore, it will join loan, client, district, creditcard, 
# account_balance, account_balance_pattern. Finally, it will rename the variables 
# and create the appropriate dummy variables to be used in the modeling process.

# joining datasets
source_dataset <- left_join(loan, disposition, by = c('account_id', 'type')) %>% 
  left_join(client, by = 'client_id') %>%
  left_join(district, by = 'district_id') %>% 
  left_join(creditcard, by = 'disp_id') %>% 
  left_join(account_balance, by = 'account_id') %>% 
  left_join(account_transaction_pattern, by = 'account_id') %>% 
  mutate(card_age_month = (issued %--% 
                             make_date(1998, 12, 31)) / months(1), 
         last_transaction_age_days = ((last_transaction_date.y %--% 
                                         make_date(1998, 12, 31)) / days(1))) %>% 
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
                  "prop_loan_payment", "prop_other"))

# renaming variables
colnames(source_dataset) <- c("x_loan_amount", "x_loan_duration", "x_loan_payments", 
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
                       "x_prop_interest_credited", "x_prop_loan_payment", "x_prop_other")

# excluding redundant variables
source_dataset <- dplyr::select(source_dataset, -c("x_loan_status", "x_loan_contract_status", 
                                     'x_prop_sanction_interest'))

# coercing variable domains and data types
source_dataset$x_card_type = ifelse(is.na(source_dataset$x_card_type), 'no card', 
                             as.character(source_dataset$x_card_type))

source_dataset$x_card_age_month = ifelse(is.na(source_dataset$x_card_age_month), 0, 
                                  source_dataset$x_card_age_month)

source_dataset$y_loan_defaulter = as.integer(source_dataset$y_loan_defaulter)

# creating dummies
source_dataset <- fastDummies::dummy_cols(source_dataset,
                                   remove_first_dummy = TRUE,
                                   select_columns = c("x_client_gender", "x_district_name", 
                                                      "x_region", "x_card_type"))

source_dataset <- dplyr::select(source_dataset, -c("x_client_gender", "x_district_name", "x_region", 
                                     "x_card_type"))

# reordering variables
source_dataset <- source_dataset[ , order(names(source_dataset))]

source_dataset <- dplyr::select(source_dataset, y_loan_defaulter, everything())

# excluding non desirable characters in variable names
colnames(source_dataset) <- stringr::str_replace_all(names(source_dataset), ' ', '_')
colnames(source_dataset) <- stringr::str_replace_all(names(source_dataset), '_-_', '_')
colnames(source_dataset) <- trimws(names(source_dataset))

# calling function to split and create train and test databases
# this function will split the dataset into train and test data and save the sampling in disk
# to resample just delete './models/source_train_test_dataset.rds' file and rerun this script
if (file.exists('./models/source_train_test_dataset.rds')) {
  source_train_test_dataset <- readRDS('./models/source_train_test_dataset.rds')
} else {
  source_train_test_dataset <- SplitTestTrainDataset(source_dataset)
  saveRDS(source_train_test_dataset, './models/source_train_test_dataset.rds')  
}
