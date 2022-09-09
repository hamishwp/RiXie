
LoadLibraries<-function(packred){
  
  library(dplyr)
  library(magrittr)
  library(tidyverse)
  library(ggplot2)
  library(sf)
  library(ggmap)
  library(geojsonR)
  library(countrycode)
  library(stringr)
  library(pracma)
  library(parallel)
  library(doParallel)
  library(foreach)
  library(raster)
  library(ncdf4)
  library(geodist)
  library(GADMTools)  
  
  if(!packred) {
    library(codetools)
    library(osmdata)
    library(OpenStreetMap)
    library(osmdata)
  }
  
}

GetRiXiePackages<-function(packred){
  
  list.of.packages <- c("ggplot2","sf","tidyverse","openxlsx","pracma","exactextractr",
                        "geojsonR", "tiff", "gstat", "mvtnorm","raster","geodist",
                        "RColorBrewer", "geosphere","GGally", "wbstats","HiClimR",
                        "countrycode","rworldmap","rworldxtra","chron","ncdf4",
                        "GADMTools","akima","adehabitatMA","flexsurv")
  
  if(!packred) list.of.packages<-c(list.of.packages,
                                   "codetools","latex2exp",
                                   "rJava","devtools","OpenStreetMap","osmdata",
                                   "tidyRSS","geojsonR", "tiff", "gstat",
                                   "FactoMineR","factoextra","xtable",
                                   "gsubfn","mapsapi","leaflet", "ssh","RPostgres",
                                   "GADMTools")
  
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  # devtools::install_github('daroczig/fbRads')
  # if(length(list.of.packages[!("openrouteservice" %in% installed.packages()[,"Package"])])){devtools::install_github("rCarto/osrm")}
  if(length(list.of.packages[!("ggmap" %in% installed.packages()[,"Package"])])){devtools::install_github("dkahle/ggmap")}
  # if(length(list.of.packages[!("countrycodes" %in% installed.packages()[,"Package"])])){devtools::install_github("vincentarelbundock/countrycode")}
  if(length(list.of.packages[!("wbstats" %in% installed.packages()[,"Package"])])){devtools::install_github('nset-ornl/wbstats')}
  if(length(list.of.packages[!("wid" %in% installed.packages()[,"Package"])])){devtools::install_github("WIDworld/wid-r-tool")}
  
  LoadLibraries(packred)
  
}

GetRiXiePackages(packred)

source(paste0(dir,"/RCode/GeneralFunctions.R"))
source(paste0(dir,"/RCode/AdminBoundaries.R"))
source(paste0(dir,"/RCode/MostlyHazard.R"))
source(paste0(dir,"/RCode/MostlyExposure.R"))
source(paste0(dir,"/RCode/MostlyVulnerability.R"))
source(paste0(dir,"/RCode/MostlyClimateChange.R"))
source(paste0(dir,"/RCode/MostlyImpact.R"))
source(paste0(dir,"/RCode/GetDemographics.R"))
source(paste0(dir,"/RCode/GetGDACS.R"))
source(paste0(dir,"/RCode/GetWorldBank.R"))
