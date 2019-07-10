# analysing missing values and other strange conditions -----------------------

# fix observations in k_symbol transaction table with ' ' (one space) to empty string ('')
transaction$k_symbol = trimws(transaction$k_symbol)

# looking for NA's in any column
# sapply(client, function(x) sum(is.na(x)))
# sapply(disposition, function(x) sum(is.na(x)))
# sapply(district, function(x) sum(is.na(x)))
# sapply(creditcard, function(x) sum(is.na(x)))
# sapply(account, function(x) sum(is.na(x)))
# sapply(loan, function(x) sum(is.na(x)))
# sapply(permanent_order, function(x) sum(is.na(x)))

# only the transaction table has 760931 NA's in account column
transaction_na_cols <- sapply(transaction, function(x) sum(is.na(x)))

# looking for empty cells in any column
# sapply(client, function(x) table(as.character(x) =="")["TRUE"])
# sapply(disposition, function(x) table(as.character(x) =="")["TRUE"])
# sapply(district, function(x) table(as.character(x) =="")["TRUE"])
# sapply(creditcard, function(x) table(as.character(x) =="")["TRUE"])
# sapply(account, function(x) table(as.character(x) =="")["TRUE"])
# sapply(loan, function(x) table(as.character(x) =="")["TRUE"])
# sapply(permanent_order, function(x) table(as.character(x) =="")["TRUE"]) 

# only the transaction table has empty values, in the following columns:
#   operation = 183114 empty cells
#   k_symbol  = 535314 empty cells
#   bank      = 782812 empty cells
transaction_empty_cols <-  sapply(transaction, function(x) table(as.character(x) == "" | as.character(x) == " ")["TRUE"])

