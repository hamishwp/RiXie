library(rmarkdown)

##-------rmd2html spotlight-------

load("/home/coleen/Documents/GitHub/RiXie/Data/codes_desc.RData")
source("./GeneralFunctions.R")
source("./AdminBoundaries.R")

country<-adm_group$iso


rmd2html<- function(iso){
  filname<-paste0(iso,"_RiX-Spotlight.html")
  country_iso <- iso
  render("./Spotlight.Rmd",
         output_format = "html_document",
         output_file = filname,
         output_dir = "/home/coleen/Documents/GRAF_files/Results/Spotlight")
  
}

lapply(country[1:28], rmd2html)


