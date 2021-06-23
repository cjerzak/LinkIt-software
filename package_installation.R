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


#Load in package to environment  
eval(parse(text=sprintf("library(%s)",package_name)))

#OR 
devtools::install_github("cjerzak/LinkIt-software/LinkIt")
library(LinkIt)