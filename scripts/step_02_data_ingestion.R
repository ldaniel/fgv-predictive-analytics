# performing data loading -----------------------------------------------------
dataDirectory <- "data/"

table <- read.csv2(paste(dataDirectory, "table", sep = ""), stringsAsFactors = TRUE)

# performing data casting, column renaming and small touch-ups ----------------

# renaming columns in district table 
names(table)[names(table) == "X1"] <- "column_name"

# casting columns with decimal or "?" values in district table
table <- district %>% 
  mutate(column_name = as.double(column_name)) %>% 
  mutate(column_date_time  = ymd_hms(column_date_time)) %>% 
  mutate(date = ConvertToDate(date))

# View(table)
# View(disposition