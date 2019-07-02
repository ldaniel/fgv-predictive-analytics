# this step aims to improve the analysis by adding auxiliary information ------

# improving client data by havivng its age group
table <- mutate(table, age_bin = paste(findInterval(age, c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) * 10,'+'))