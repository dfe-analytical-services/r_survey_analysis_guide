
# 1.1 - renv --------------------------------------------------------------------

# Setting up renv helps you create reproducible environments for your R projects.
# It helps maintain the correct version of packages needed for the project.
# To install all the packages needed for this project in the correct version

# 1.1.1 - Install the renv package and compare packages installed vs needed --------


#install renv
install.packages("renv")
#compare installed and required packages
renv::restore()


# 1.2 - Loading in packages and functions -------------------------------------------------------

source("00_packages_and_functions.R")

