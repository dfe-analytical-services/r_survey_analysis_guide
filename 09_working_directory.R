# 9 -  working directory(wd)  ------------------------------------------------------

#This is a path that points to folders where your data and scripts are.
#The default is your project directory.
#If you have sensitive data that must be kept in certain folders then you can change your wd to read in that data.
#We recommend you change your wd back to its default after, so that no problems are caused in the code later on.

# 9.2-Checking current working directory ----------------------------------

#check working directory path
getwd()


# 9.3-Changing working directory ------------------------------------------

#store project's wd
project_wd <- getwd()

#store wd of sensitive folders you need data from
secure_folder_wd <- "path to secure folders/ the folders must be separated with these slashes"

#set wd to sensitive folder

setwd(secure_folder_wd)

#to return to original wd,

setwd(project_wd)
