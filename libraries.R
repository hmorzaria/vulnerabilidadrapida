#to install rJava
#https://github.com/hannarud/r-best-practices/wiki/Installing-RJava-(Ubuntu)
#sudo add-apt-repository ppa:marutter/c2d4u3.5
#sudo apt-get update
#sudo apt-get install default-jdk
#sudo R CMD javareconf
#sudo apt-get install r-cran-rjava
#sudo apt-get install libgdal-dev libproj-dev
#Then in Rstudio install.packages("rJava")

#install.packages("testthat")

#install.packages("devtools")
#library(devtools)

.packages = c("devtools","raster","readxl",
              "sp","sf",
              "readr","rgdal","XML", 
              "stringr","data.table","ggmap","Redmonder") 

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
# Load packages into session 
lapply(.packages, require, character.only=TRUE)

