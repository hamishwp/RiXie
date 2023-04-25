dir<-getwd()
packred<-T; installer<-F

ll <- parse(file = "./RCode/GetPackages.R")

for (i in seq_along(ll)) {
  tryCatch(eval(ll[[i]]), 
           error = function(e) message("Oops!  ", as.character(e)))
}

library(readxl)
library(dplyr)
library(magrittr)
#Subnational INFORM:
#read excel that has compiled sheets from different SUBNAT files
path2files<-"./Data/Other_Global_datasets/manual_download/by_source/INFORM_subnat/0_INFORM_subnet_compiled_locally.xlsx"

# To load all sheets in a workbook, use lapply()
fil_list<-lapply(excel_sheets(path2files), read_excel, path = path2files) 

#Lazy combining - just merge into one df, thought making columns uniform, some will just have NAs.
# INFORM.df<-plyr::rbind.fill(fil_list)%>%
#   dplyr::relocate(contains(c("Country", "ISO3","ADM","COD","Divipola","NAME")))%>%
#   relocate(contains("Haz_"),.after = "ADM2CD")%>%
#   relocate("HAZARD",.before = "HAZARD & EXPOSURE") %>%
#   relocate(contains("Vul_"),.after = "HAZARD & EXPOSURE")%>%
#   relocate("VULNERABILITY",.after = contains("Vul_"))%>%
#   relocate(contains("LCC_"),.after="VULNERABILITY")


INFORM.df<-plyr::rbind.fill(fil_list)%>%
  dplyr::select(c("Country","ISO3","ADM1NM","ADM2NM","HAZARD","HAZARD & EXPOSURE","VULNERABILITY","LACK OF COPING CAPACITY",
                                             "INFORM RISK"))

#Complete ISO3 information:
INFORM.df$ISO3<-ifelse(is.na(INFORM.df$ISO3)==TRUE,convCountryIso3(INFORM.df$Country),INFORM.df$ISO3)

#Merge HAzard & EXPosure Field to HAzard only
INFORM.df$HAZARD<-ifelse(is.na(INFORM.df$HAZARD)==TRUE,INFORM.df$`HAZARD & EXPOSURE`,INFORM.df$HAZARD ) 

INFORM.df%<>%dplyr::select(-`HAZARD & EXPOSURE`)


INFORM2ADM<-function(iso3c,INFORM){
  print("Detecting if level 1 or level 2 ADMIN information is present")
  if(all(is.na(INFORM$ADM2NM))==TRUE){
    level<-1
    print(paste0("ADM level for ", iso3c, " is ", level))
  }else if(all(is.na(INFORM$ADM2NM))==FALSE){
    level<-2
    print(paste0("ADM level for ", iso3c, " is ", level))
  }

  

  ADM<-tryCatch(GetUNMaps(iso3c),error=function(e) NA)
  if(any(is.na(ADM))){
    print(paste0("Level 1 and Level 2 data of ",iso3c," have NAs in UN Maps, resorting to GADM"))
    gADM<-tryCatch(GetGADM(iso3c),error=function(e) NA)
    ADM<-gADM %>%
      rename("geometry" ="geom")
  }
  
  #Disolve level 2 ADM polygons to level1 :
  if(level == 1){
    dissADM<-ADM%>%
    group_by(ADM1NM)%>%
    summarize()%>%
    ungroup()%>%
    mutate(ISO3CD = iso3c)
    
    join<- dplyr::left_join(dissADM, INFORM,by='ADM1NM',multiple="first")
  }else{
    join<- dplyr::left_join(ADM, INFORM,by='ADM2NM',multiple="first")%>%
      rename(ADM1NM= ADM1NM.x)%>%
      dplyr::select(-ADM1NM.y)
  }
  
  return(join)
}


#Make INFORM data spatial:
iso3<-sort(unique(INFORM.df$ISO3))
INFORM.list<-split(INFORM.df,f=INFORM.df$ISO3)
ADM_INFORM<-list()
for(i in seq_along(INFORM.list)){
  ADM_INFORM[[i]]<-INFORM2ADM(iso3[[i]],INFORM.list[[i]]) 
  
}

for(i in seq_along(ADM_INFORM)){
  x<-ADM_INFORM[[i]]
  x%<>%
    dplyr::select(ISO3CD, Country, ADM1NM, ADM2NM, HAZARD, VULNERABILITY, `LACK OF COPING CAPACITY`,`INFORM RISK`,geometry)
  %>% mutate(across(5:8, round, 1))
  ADM_INFORM[[i]]<-x
}


INFORM.sf<-do.call(rbind, ADM_INFORM)
  
# INFORM.sf%<>%dplyr::select(-ADM2NMCD)%>%
#   rename_at(.vars = vars(ends_with(".x")),
#             .funs = funs(sub("[.]x$", "", .)))%>%
#   dplyr::select(-contains(".y"))%>%
#   relocate(contains(c("Code","COD","3NM","CAD","Divipola")),.after = "ISO3CD")%>%
#   .[,-c(2:9,11:19)]

dir<-"/home/coleen/Documents/GitHub/RiXie/Data/Other_Global_datasets/manual_download/by_source/INFORM_subnat/"

write_sf(INFORM.sf, paste0(dir,"INFORM_subnat.shp"),driver = "ESRI Shapefile",delete_layer = T)
write_sf(INFORM.sf, paste0(dir,"INFORM_subnat.gpkg"),delete_layer = T)
  
#INFORM LONG df format:






#