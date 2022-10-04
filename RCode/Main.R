dir<-getwd()
packred<-T; installer<-F
source("./RCode/GetPackages.R")
# Parallelise the workload
ncores<-1; if(detectCores()<ncores) stop("You don't have enough CPU threads available, reduce ncores")

# Choose the country you want to work with
ISO<-xlsx::read.xlsx(paste0(dir,"/Data/Country_Rollout_Timeline.xlsx"),
                     sheetName = "Country RIX Start Dates",as.data.frame = T)%>%
  pull(ISO3C.Code)%>%na.omit()

######################################################################
############################ SPATIAL DATA ############################
######################################################################
for (iso3c in unique(ISO)){
  print(paste0("Currently working on ",convIso3Country(iso3c)))
  # ADMIN LEVEL BOUNDARIES
  Dasher<-tryCatch(GetUNMaps(iso3c),error=function(e) NA)
  if(any(is.na(Dasher)) | nrow(Dasher@data)==0) {
    print(paste0("Error with ",iso3c," UN Maps"))
    Dasher<-tryCatch(GetGADM(iso3c),error=function(e) NA)
    if(any(is.na(Dasher))) {print(paste0("Error with ",iso3c," GADM Maps too!"));next}
    if(nrow(Dasher@data)==0) {print(paste0("Error with ",iso3c," GADM Maps too!"));next}
  }
  Dasher%<>%ADMexceptions()
  # Check if landlocked or not:
  Landlocked<-CheckLandLock(iso3c)
  # Bounding box of the country for cropping
  ext <- GetExtent(Dasher,expander=1.1)
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  #@@@@@@@@@@ EXPOSURE @@@@@@@@@@#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  # Population
  Dasher%<>%GetPop(ISO=iso3c,ncores=ncores,outsiders=T)
  # Female, Over 64 & Under 14 population
  Dasher%<>%GetDemog(ISO=iso3c)
  # Forest Cover
  # tmp<-tryCatch(GetLandCover(Dasher,ISO=iso3c,ext=ext),error=function(e) NA)
  # if(!all(is.na(tmp))) Dasher<-tmp
  # GDP-PPP Per Capita
  Dasher%<>%GetGDP(ISO=iso3c,ext=ext)
  # Built-up Surface - GHSL
  # Dasher%<>%GetInfra(ISO=iso3c)
  # Healthcare Sites
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  #@@@@@@@@@@ IMPACTS @@@@@@@@@@@#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  # Make visualisations of GDACS & ACLED data
  
  # Events from EM-DAT
  
  # Events from GDACS
  
  # Displacement from IDMC
  
  # Get INFORM indices for hazards
  
  # Conflict from ACLED. API found here:
  # https://www.acleddata.com/wp-content/uploads/dlm_uploads/2017/10/API-User-Guide.pdf
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  #@@@@@@@@@@@ HAZARD @@@@@@@@@@@#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  # Air pollution
  tmp<-tryCatch(GetAirPollution(Dasher,ISO=iso3c,ext=ext),error=function(e) NA)
  if(!all(is.na(tmp))) Dasher<-tmp
  # Tropical Cyclones
  tmp<-tryCatch(GetTropCyc(Dasher,ISO=iso3c,ext=ext),error=function(e) NA)
  if(!all(is.na(tmp))) Dasher<-tmp
  # Floods
  tmp<-tryCatch(GetFloodRisk(Dasher,ISO=iso3c,ext=ext),error=function(e) NA)
  if(!all(is.na(tmp))) Dasher<-tmp
  # Drought
  tmp<-tryCatch(GetDrought(Dasher,ISO=iso3c,ext=ext),error=function(e) NA)
  if(!all(is.na(tmp))) Dasher<-tmp
  # Earthquakes
  tmp<-tryCatch(GetEarthquakeRisk(Dasher,ISO=iso3c,ext=ext),error=function(e) NA)
  if(!all(is.na(tmp))) Dasher<-tmp
  # Landslides
  tmp<-tryCatch(GetLandslide(Dasher,ISO=iso3c,ext=ext),error=function(e) NA)
  if(!all(is.na(tmp))) Dasher<-tmp
  # Extreme Heat
  tmp<-tryCatch(GetExtremeHeat(Dasher,ISO=iso3c,ext=ext),error=function(e) NA)
  if(!all(is.na(tmp))) Dasher<-tmp
  # Volcanic Ash
  tmp<-tryCatch(GetVolcAsh(Dasher,ISO=iso3c,ext=ext),error=function(e) NA)
  if(!all(is.na(tmp))) Dasher<-tmp
  # Include coastal hazards such as tsunami or sea level rise
  if(!Landlocked){
    # Sea Level Rise data
    tmp<-tryCatch(GetSeaLevelRise(Dasher,ISO=iso3c),error=function(e) NA)
    if(!all(is.na(tmp))) Dasher<-tmp
    # Tsunami Risk
    tmp<-tryCatch(GetTsunamiRisk(Dasher,ISO=iso3c,ext=ext),error=function(e) NA)
    if(!all(is.na(tmp))) Dasher<-tmp
  }
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  #@@@@@@@@ VULNERABILITY @@@@@@@#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  # Sub-national HDI data
  tmp<-tryCatch(GetSHDI(Dasher,ISO=iso3c),error=function(e) NA)
  if(!all(is.na(tmp))) Dasher<-tmp
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  #@@@@@@@ CLIMATE CHANGE @@@@@@@#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  tmp<-tryCatch(GetCDD(Dasher,ISO=iso3c),error=function(e) NA)
  if(!all(is.na(tmp))) Dasher<-tmp
  tmp<-tryCatch(GetCWD(Dasher,ISO=iso3c),error=function(e) NA)
  if(!all(is.na(tmp))) Dasher<-tmp
  tmp<-tryCatch(GetLGS(Dasher,ISO=iso3c),error=function(e) NA)
  if(!all(is.na(tmp))) Dasher<-tmp
  tmp<-tryCatch(GetMinTemp(Dasher,ISO=iso3c),error=function(e) NA)
  if(!all(is.na(tmp))) Dasher<-tmp
  tmp<-tryCatch(GetMaxTemp(Dasher,ISO=iso3c),error=function(e) NA)
  if(!all(is.na(tmp))) Dasher<-tmp
  
  tmp<-tryCatch(GetCCPrecip(Dasher,ISO=iso3c),error=function(e) NA)
  if(!all(is.na(tmp))) {CCtable<-tmp$temporal; Dasher<-tmp$ADM}
  tmp<-tryCatch(GetCCSurfTemp(Dasher,ISO=iso3c),error=function(e) NA)
  if(!all(is.na(tmp))) {CCtable%<>%merge(tmp$temporal,by=c("ISO3C","year")); Dasher<-tmp$ADM}
  tmp<-tryCatch(GetCCTotRunoff(Dasher,ISO=iso3c),error=function(e) NA)
  if(!all(is.na(tmp))) {CCtable%<>%merge(tmp$temporal,by=c("ISO3C","year")); Dasher<-tmp$ADM; rm(tmp)}
  
  Dasher@data%<>%dplyr::select(-c(ADM1CD,ADM2CD,LONGITUDE,LATITUDE))
  
  # Create a folder for the results
  dir.create(paste0(dir,"/Results/",iso3c),showWarnings = F,recursive = T)
  rgdal::writeOGR(Dasher,
                  dsn=paste0(dir,"/Results/",iso3c,"/ADM_",iso3c),
                  layer = paste0("/ADM_",iso3c),
                  driver = "ESRI Shapefile",overwrite_layer = T)
  
  xlsx::write.xlsx(CCtable,paste0(dir,"/Results/",iso3c,"/CC_tables_",iso3c,".xlsx"))
  
}

######################################################################
######################## COMBINE SPATIAL DATA ########################
######################################################################
ISO<-list.files("./Results/"); ISO<-ISO[!ISO%in%c("CC_tables.csv","CC_tables.xlsx","Full","Full.zip","MonthlyClimate.csv","MonthlyClimateV2.csv")]
# Number 2 has the full 36 elements
iso3c<-unique(ISO[1])
file<-paste0("./Results/",iso3c,"/ADM_",iso3c,"/_ADM_",iso3c,".shp")
Fuller<-st_read(file)
for (iso3c in unique(ISO)[c(2,3:length(unique(ISO)))]){
  file<-paste0("./Results/",iso3c,"/",
               grep("shp",list.files(paste0("./Results/",iso3c),recursive = T),value=T))
  Dasher<-st_read(file)
  for(coly in names(Fuller)[!names(Fuller)%in%names(Dasher)]){
    Dasher$tmp<-NA
    names(Dasher)[ncol(Dasher)]<-coly
  }
  Dasher%<>%dplyr::select(names(Fuller))
  Fuller%<>%rbind(Dasher)
}
rgdal::writeOGR(as(Fuller,"Spatial"),
                dsn=paste0(dir,"/Results/Full"),
                layer = "/ADM_Full",
                driver = "ESRI Shapefile",overwrite_layer = T)

######################################################################
######################## YEARLY CLIMATE DATA #########################
######################################################################
CCoverall<-data.frame()
for (iso3c in unique(ISO)){
  print(paste0("Currently working on ",convIso3Country(iso3c)))
  # ADMIN LEVEL BOUNDARIES
  Dasher<-tryCatch(GetUNMaps(iso3c),error=function(e) NA)
  if(all(is.na(Dasher@data)) | nrow(Dasher@data)==0) {print(paste0("Error with ",iso3c," UN Maps")); next}
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  #@@@@@@@ CLIMATE CHANGE @@@@@@@#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  tmp<-tryCatch(GetCCPrecip(Dasher,ISO=iso3c),error=function(e) NA)
  if(any(is.na(tmp))) {print(paste0("Error with CC data in ",iso3c)); next}
  CCtable<-tmp$temporal;
  tmp<-GetCCSurfTemp(Dasher,ISO=iso3c)
  CCtable%<>%merge(tmp$temporal,by=c("ISO3C","year"))
  tmp<-GetCCTotRunoff(Dasher,ISO=iso3c)
  CCtable%<>%merge(tmp$temporal,by=c("ISO3C","year"))
  
  CCoverall%<>%rbind(CCtable)
  
  xlsx::write.xlsx(CCoverall,paste0(dir,"/Results/CC_tables.xlsx"),showNA = F)
  
}

######################################################################
######################## MONTHLY CLIMATE DATA ########################
######################################################################
RPS<-data.frame()
for (iso3c in unique(ISO))  RPS%<>%rbind(GetMonthlies(iso3c))
write_csv(RPS,"./Results/MonthlyClimate.csv")



