# Choose the country you want to work with
ISO<-"MDV"

source("./RCode/GeneralFunctions.R")
source("./AdminBoundaries.R")
source("./GetPackages.R")
source("./MostlyHazard.R")
source("./MostlyExposure.R")
source("./MostlyVulnerability.R")
source("./MostlyClimateChange.R")
source("./MostlyImpact.R")

#@@@@@@@@@@@@@ TO DO LIST @@@@@@@@@@@@@#
# Find a way 
for (iso3c in unique(ISO)){
  # ADMIN LEVEL BOUNDARIES
  Dasher<-ExtractADM(ISO)
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  #@@@@@@@@@@ EXPOSURE @@@@@@@@@@#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  # Population
  Dasher%<>%GetPop(ISO=ISO)
  # Female, Over 64 & Under 14 population
  Dasher%<>%GetDemog(ISO=ISO)
  # Forest Cover
  
  # GDP-PPP Per Capita
  
  # Built-up Surface - GHSL
  
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
  
  # Some based on the impact data to find top 5 hazards
  
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  #@@@@@@@@ VULNERABILITY @@@@@@@#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  # Sub-national HDI data
  Dasher%<>%GetSHDI(ISO=ISO)
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  #@@@@@@@ CLIMATE CHANGE @@@@@@@#
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
  
  
  
}



# For each category, 