# getting the necessary directories to generate the website -------------------
scriptsDirectoryPath  <- dirname(rstudioapi::getSourceEditorContext()$path)
markdownDirectoryPath <- stringr::str_replace(scriptsDirectoryPath, "/scripts", "/markdown")
docsDirectoryPath     <- stringr::str_replace(scriptsDirectoryPath, "/scripts", "/docs")
tempSiteDirecoryPath  <- paste(markdownDirectoryPath, "/_site", sep = "")
rootDirectoryPath     <- stringr::str_replace(scriptsDirectoryPath, "/scripts", "")

# generating website from markdown files --------------------------------------
setwd(markdownDirectoryPath)

initialTime <- Sys.time() # get the initial time
rmarkdown::render_site()  # call the website generation function
finalTime <- Sys.time()   # get the final time

# showing the amount of time, in minutes, to generate the website
print(difftime(finalTime, initialTime, tz="GMT", units="mins")) 

# moving website files to docs directory --------------------------------------
if(file.exists(docsDirectoryPath))
{
  do.call(file.remove, list(list.files(docsDirectoryPath, full.names = TRUE)))
}

if(file.exists(tempSiteDirecoryPath))
{
  list_of_files <- list.files(tempSiteDirecoryPath)
  file.copy(file.path(tempSiteDirecoryPath,list_of_files), docsDirectoryPath,
            overwrite = TRUE, 
            recursive = TRUE, 
            copy.mode = TRUE)
  unlink(tempSiteDirecoryPath, recursive = TRUE)
}

# setting the working directory to the original one ---------------------------
setwd(rootDirectoryPath)
getwd()