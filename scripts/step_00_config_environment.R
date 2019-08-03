# clearing everything before starting -----------------------------------------
# clear environment and memory
rm(list=ls())
invisible(gc())

# clear console screen
cat("\014")

# setting the environment -----------------------------------------------------
options(encoding = "UTF-8")

username <- Sys.info()[["user"]]
directoryPath <- dirname(rstudioapi::getSourceEditorContext()$path)
directoryPath <- stringr::str_replace(directoryPath, "/scripts", "")

setwd(directoryPath)
getwd()
