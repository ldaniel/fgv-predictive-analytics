# analysing missing values and other strange conditions -----------------------

# looking for NA's in any column
sapply(table, function(x) sum(is.na(x)))

# looking for empty cells in any column
sapply(table, function(x) table(as.character(x) =="")["TRUE"])

# View(table)
# summary(table)
# str(table)

# matrixplot(table)
