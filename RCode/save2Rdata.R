###---------------Generate RData for some codes and lists--------------

##-----------------ISO country codes---------
iso3 <-read.csv("/home/coleen/Documents/GitHub/ISO-3166-Countries-with-Regional-Codes/slim-3/slim-3.csv", header=TRUE)
colnames(iso3) <- c("Country","iso3","code")

  
##-------------------description for INFORM index or indicator codes----------
Inform_codes_desc <- read.csv("/home/coleen/Documents/GitHub/GRAF_files/INFORM_codes_list.csv",header = TRUE)[,4:5]


##--------------------description WorldBank indicators---------------
Wbank_codes_desc<-data.frame(wbstats::wb_indicators())





#----------------shapefile adm labels---------------------------------------
#adm_labels <-read.csv("/home/coleen/Documents/GitHub/RiXie/Data/Spatial/Field_Names.csv",header=T)

#--------------Adm sources-------------------------------------------------
#adm_sources <-read.csv("/home/coleen/Documents/GitHub/RiXie/Data/Spatial/MappingDataSources.csv",header=T)


save.image(file="codes_desc.RData")
