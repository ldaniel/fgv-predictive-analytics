# setting the environment -----------------------------------------------------
options(encoding = "UTF-8")

username <- Sys.info()[["user"]]
if(tolower(username) == "daniell") {
  wd = "M:/Leandro/FGV/02 - Analise Exploratoria de Dados/R_Bank_Berka"
} else if (tolower(username) == "leandro") {
  wd = "D:/OneDrive/FGV/04_Modulos/02 - Analise Exploratoria de Dados/R_Bank_Berka"
} else if (tolower(username) == "rodri") {
  wd = "C:/Users/rodri/OneDrive/My GIT Projects/R_Bank_Berka"
} else if (tolower(username) == "a57622988") {
  wd = "C:/Users/A57622988/Desktop/Aula"
} else if (tolower(username) == "Ygor") {
  wd = "/Users/Ygor/Documents/MBA/analise_preditiva/R_Bank_Berka"
} else {
  directoryPath <- dirname(rstudioapi::getSourceEditorContext()$path)
  directoryPath <- str_replace(directoryPath, "/scripts", "")
  wd = directoryPath
}
setwd(wd)
getwd()
