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
  Dasher<-ExtractADM(iso3c)
  # Check if landlocked or not:
  Landlocked<-CheckLandLock(ISO)
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  #@@@@@@@@@@ EXPOSURE @@@@@@@@@@#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  # Population
  Dasher%<>%GetPop(ISO=iso3c,ncores=ncores)
  # Female, Over 64 & Under 14 population
  Dasher%<>%GetDemog(ISO=iso3c)
  # Forest Cover
  
  # GDP-PPP Per Capita
  Dasher%<>%GetGDP(ISO=iso3c,ncores=ncores)
  # Built-up Surface - GHSL
  Dasher%<>%GetInfra(ISO=iso3c)
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
  # Some hazards that affect everyone: air pollution, maybe storms or floods?
  
  
  # Include coastal hazards such as tsunami or sea level rise
  if(!Landlocked){
    
    
  }
  # Some based on the impact data to find top 5 hazards
  
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  #@@@@@@@@ VULNERABILITY @@@@@@@#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  # Sub-national HDI data
  Dasher%<>%GetSHDI(ISO=iso3c)
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  #@@@@@@@ CLIMATE CHANGE @@@@@@@#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  
  
  
}



# For each category, 