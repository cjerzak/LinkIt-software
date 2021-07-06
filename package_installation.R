##################################################
##INSTRUCTIONS FOR PACKAGE DOWNLOAD############
##################################################

package_name <- "LinkOrgs"

#Generate documentation 
{
setwd(sprintf("~/Dropbox/Directory/%s-software",package_name))
devtools::document(sprintf("./%s",package_name)) 
try(file.remove(sprintf("./%s.pdf",package_name)),T); system(sprintf("R CMD Rd2pdf %s",package_name))
}

#Install package
devtools::install_github(sprintf("cjerzak/%s-software/%s",package_name,package_name))
devtools::install_github("cjerzak/LinkOrgs-software/LinkOrgs")
?LinkOrgs::FastFuzzyMatch
?LinkOrgs::AssessMatchPerformance
library(LinkOrgs)

{
  ?LinkOrgs::LinkOrgs
  #Create synthetic data 
  x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
  y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds co")
  x <- data.frame("orgnames_x"=x_orgnames)
  y <- data.frame("orgnames_y"=y_orgnames)
  
  # Perform merge 
  linkedOrgs <- LinkOrgs(x = x, 
                         y = y, 
                         by.x = "orgnames_x", 
                         by.y = "orgnames_y")
  
  print( linkedOrgs )
  
  
}

#Load in package to environment  
eval(parse(text=sprintf("library(%s)",package_name)))

#OR 
devtools::install_github("cjerzak/LinkIt-software/LinkIt")
library(LinkIt)