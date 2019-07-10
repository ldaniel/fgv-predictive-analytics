# setting the environment -----------------------------------------------------
options(encoding = "UTF-8")

username <- Sys.info()[["user"]]
directoryPath <- dirname(rstudioapi::getSourceEditorContext()$path)
directoryPath <- str_replace(directoryPath, "/scripts", "")
wd = directoryPath

setwd(wd)
getwd()
