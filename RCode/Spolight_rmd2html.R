library(rmarkdown)

##-------rmd2html spotlight-------

load("./codes_desc.RData") 
source("./GeneralFunctions.R")
source("./AdminBoundaries.R")

country<-adm_group$iso


rmd2html<- function(iso){
  filname<-paste0(iso,"_spotlight.html")
  country_iso <- iso
  render("./Spotlight.Rmd",
         output_format = "html_document",
         output_file = filname,
         output_dir = "/home/coleen/Documents/GRAF_files/Results/Spotlight")
  
}

lapply(country, rmd2html)