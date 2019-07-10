# performing data loading -----------------------------------------------------
dataDirectory <- "data/"

client          <- read.csv2(paste(dataDirectory, "client.asc", sep = ""), stringsAsFactors = TRUE)
disposition     <- read.csv2(paste(dataDirectory, "disp.asc", sep = ""), stringsAsFactors = TRUE)
district        <- read.csv2(paste(dataDirectory, "district.asc", sep = ""), stringsAsFactors = TRUE)
creditcard      <- read.csv2(paste(dataDirectory, "card.asc", sep = ""), stringsAsFactors = TRUE)
account         <- read.csv2(paste(dataDirectory, "account.asc", sep = ""), stringsAsFactors = TRUE)
loan            <- read.csv2(paste(dataDirectory, "loan.asc", sep = ""), stringsAsFactors = TRUE)
permanent_order <- read.csv2(paste(dataDirectory, "order.asc", sep = ""), stringsAsFactors = TRUE)

# special case for (big) transaction table to speedup data ingestion
if(!file.exists('data/transaction.feather')) {
  transaction   <- read.csv2(paste(dataDirectory, "trans.asc", sep = ""), stringsAsFactors = TRUE)
  write_feather(transaction, 'data/transaction.faether')
} else {
  read_feather('data/transaction.faether')
}

# performing data casting, column renaming and small touch-ups ----------------

# renaming columns in district table 
names(district)[names(district) == "A1"] <- "district_id"
names(district)[names(district) == "A2"] <- "district_name"
names(district)[names(district) == "A3"] <- "region"
names(district)[names(district) == "A4"] <- "no_of_inhabitants"
names(district)[names(district) == "A5"] <- "no_of_municip_inhabitants_less_499"
names(district)[names(district) == "A6"] <- "no_of_municip_500_to_1999"
names(district)[names(district) == "A7"] <- "no_of_municip_2000_to_9999"
names(district)[names(district) == "A8"] <- "no_of_municip_greater_10000"
names(district)[names(district) == "A9"] <- "no_of_cities"
names(district)[names(district) == "A10"] <- "ratio_of_urban_inhabitants"
names(district)[names(district) == "A11"] <- "average_salary"
names(district)[names(district) == "A12"] <- "unemploymant_rate_1995"
names(district)[names(district) == "A13"] <- "unemploymant_rate_1996"
names(district)[names(district) == "A14"] <- "no_of_enterpreneurs_per_1000_inhabitants"
names(district)[names(district) == "A15"] <- "no_of_commited_crimes_1995"
names(district)[names(district) == "A16"] <- "no_of_commited_crimes_1996"

# casting columns with decimal or "?" values in district table
district <- district %>% 
  mutate(ratio_of_urban_inhabitants = as.double(ratio_of_urban_inhabitants)) %>% 
  mutate(average_salary = as.double(average_salary)) %>% 
  mutate(unemploymant_rate_1995 = as.double(unemploymant_rate_1995)) %>% 
  mutate(unemploymant_rate_1996 = as.double(unemploymant_rate_1996)) %>% 
  mutate(no_of_commited_crimes_1995 = as.integer(no_of_commited_crimes_1995))

# casting column issued in creditcard table from string to datetime data type
creditcard <- creditcard %>% 
  mutate(issued  = ymd_hms(issued))

# casting column date in account table from string to datetime data type
account <- account %>% 
  mutate(date = ConvertToDate(date))

# casting columns in table loan to the right data types
loan <- loan %>% 
  mutate(date = ConvertToDate(date)) %>% 
  mutate(amount = as.double(amount)) %>% 
  mutate(payments = as.double(payments))

# casting columns with decimal values in permanent order table
permanent_order <- permanent_order %>% 
  mutate(amount = as.double(amount))

# casting columns in table transaction to the right data types
transaction <- transaction %>% 
  mutate(date = ConvertToDate(date)) %>% 
  mutate(amount = as.double(amount)) %>% 
  mutate(balance = as.double(balance)) 