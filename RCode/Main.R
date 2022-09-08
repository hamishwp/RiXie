# Choose the country you want to work with
ISO<-"SDN"
dir<-getwd()
packred<-F
source("./RCode/GetPackages.R")
# Parallelise the workload
ncores<-4; if(detectCores()<ncores) stop("You don't have enough CPU threads available, reduce ncores")

#@@@@@@@@@@@@@ TO DO LIST @@@@@@@@@@@@@#
# Find a way 
for (iso3c in unique(ISO)){
  print(paste0("Currently working on ",convIso3Country(iso3c)))
  # ADMIN LEVEL BOUNDARIES
  Dasher<-GetUNMaps(iso3c)
  # Check if landlocked or not:
  Landlocked<-CheckLandLock(ISO)
  # Bounding box of the country for cropping
  ext <- GetExtent(Dasher,expander=1.1)
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  #@@@@@@@@@@ EXPOSURE @@@@@@@@@@#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  # Population
  Dasher%<>%GetPop(ISO=iso3c,ncores=ncores)
  # Female, Over 64 & Under 14 population
  Dasher%<>%GetDemog(ISO=iso3c)
  # Forest Cover
  
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
  Dasher%<>%GetAirPollution(ISO=ISO)
  # Tropical Cyclones
  Dasher%<>%GetTropCyc(ISO=ISO,ext=ext)
  # Floods
  Dasher%<>%GetFloodRisk(ISO=ISO,ext=ext)
  # Drought
  Dasher%<>%GetDrought(ISO=ISO,ext=ext)
  # Earthquakes
  Dasher%<>%GetEarthquakeRisk(ISO=ISO,ext=ext)
  # Landslides
  Dasher%<>%GetLandslide(ISO=ISO,ext=ext)
  # Extreme Heat
  Dasher%<>%GetExtremeHeat(ISO=ISO,ext=ext)
  # Volcanic Ash
  Dasher%<>%GetVolcAsh(ISO=ISO,ext=ext)
  # Include coastal hazards such as tsunami or sea level rise
  if(!Landlocked){
    # Sea Level Rise data
    Dasher%<>%GetSeaLevelRise(ISO=ISO)
    # Tsunami Risk
    Dasher%<>%GetTsunamiRisk(ISO=ISO,ext=ext)
  }
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  #@@@@@@@@ VULNERABILITY @@@@@@@#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  # Sub-national HDI data
  Dasher%<>%GetSHDI(ISO=iso3c)
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  #@@@@@@@ CLIMATE CHANGE @@@@@@@#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  
  # Create a folder for the results
  dir.create(paste0(dir,"/Results/",ISO),showWarnings = F,recursive = T)
  rgdal::writeOGR(Dasher,
                  dsn=paste0(dir,"Results/",ISO,"/",ISO),
                  layer = "map",
                  driver = "ESRI Shapefile",overwrite_layer = T)
  
}



# For each category, 