# setting the environment -----------------------------------------------------
options(encoding = "UTF-8")

username <- Sys.info()[["user"]]
if(tolower(username) == "daniell") {
  wd = "M:/Leandro/FGV/02 - Analise Exploratoria de Dados/Trabalho Final"
} else if (tolower(username) == "leandro") {
  wd = "D:/OneDrive/FGV/04_Modulos/04 - Analise Preditiva/Predictive-Analytics"
} else if (tolower(username) == "rodri") {
  wd = "C:/Users/rodri/OneDrive/My R Projects/R_Bank_Berka"
} else if (tolower(username) == "a57622988") {
  wd = "C:/Users/A57622988/Desktop/Aula"
} else {
  directoryPath <- dirname(rstudioapi::getSourceEditorContext()$path)
  directoryPath <- str_replace(directoryPath, "/scripts", "")
  wd = directoryPath
}
return(getwd())

setwd(wd)
getwd()
