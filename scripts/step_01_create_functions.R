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

# DataPrep --------------------------------------------------------------------
# The objective of this function is to return a DataFrame to be used in predictive modeling.
# this function will join loan, client, district, creditcard, account_balance, account_balance_pattern
# rename the variables and create the appropriate dummy variables to be used in the modeling process.

DataPrep <- function() {

  temp <- left_join(loan, disposition, by = c('account_id', 'type')) %>% 
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
                      "x_prop_interest_credited", "x_prop_loan_payment", "x_prop_other")
  
  
  
  temp$x_card_type = ifelse(is.na(temp$x_card_type), 'no card', 
                            as.character(temp$x_card_type))
  
  temp$x_card_age_month = ifelse(is.na(temp$x_card_age_month), 0, 
                                 temp$x_card_age_month)
  
  temp$y_loan_defaulter = as.numeric(temp$y_loan_defaulter)
  
  temp <- dummy_cols(temp, 
                     remove_first_dummy = TRUE,
                     select_columns = c("x_loan_status", "x_loan_contract_status",
                                        "x_client_gender", "x_district_name", "x_region", 
                                        "x_card_type"))
  
  temp <- dplyr::select(temp, -c("x_loan_status", "x_loan_contract_status",
                          "x_client_gender", "x_district_name", "x_region", 
                          "x_card_type"))
  
  temp <- temp[ , order(names(temp))]
  
  temp <- dplyr::select(temp, y_loan_defaulter, everything())

  return(temp)

}
