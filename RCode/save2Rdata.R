###---------------Generate RData for some codes and lists--------------

##-------------------description for INFORM index or indicator codes----------
Inform_codes_desc <- read.csv("/home/coleen/Documents/GRAF_files/INFORM_codes_list.csv",header = TRUE)[,4:5]


##--------------------description WorldBank indicators---------------
Wbank_codes_desc<-data.frame(wbstats::wb_indicators())





#----------------shapefile adm labels---------------------------------------
adm_labels <-read.csv("/home/coleen/Documents/Data/Spatial/Field_Names.csv",header=T)

#--------------Adm sources-------------------------------------------------
adm_sources <-read.csv("/home/coleen/Documents/Data/Spatial/MappingDataSources.csv",header=T)


save.image(file="codes_desc.RData")
