source("./functions/GetPackages.R")
# Parallelise the workload
ncores<-4; if(detectCores()<ncores) stop("You don't have enough CPU threads available, reduce ncores")

#################################################################
######################## DEMOGRAPHIC DATA #######################
#################################################################
WorldPopURL<-function(iso3c,year,constrained=T,kmres=T,unadj=T){
  ############### CONSTRAINED DATASETS ###############
  if(constrained & kmres & unadj){
    base_url<-"https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/"
    url<-paste0(base_url,year,"/BSGM/",str_to_upper(iso3c),
                "/",str_to_lower(iso3c),"_ppp_",year,"_UNadj_constrained.tif")  
  } else if(constrained & kmres & !unadj){
    base_url<-"https://data.worldpop.org/GIS/Population/Individual_countries/"
    url<-paste0(base_url,year,"/",str_to_upper(iso3c),
                "/",str_to_lower(iso3c),"_ppp_",year,"_constrained.tif")  
  }else if(constrained & !kmres & !unadj){
    warning("100m WorldPop constrained dataset only available for 2010 and 2015")
    stop("please go and download by hand from https://data.worldpop.org/GIS/Population/Individual_countries/")
    year<-c(2010,2015)[which.min(abs(year-c(2010,2015)))]
    base_url<-"https://data.worldpop.org/GIS/Population/Individual_countries/"
    url<-paste0(base_url,str_to_upper(iso3c),"/",
                paste0(convIso3Country(iso3c), collapse = "_"),"_100m_Population/",
                paste0(convIso3Country(iso3c), collapse = "_"),"_100m_Population",".tif")  
  } else if(constrained & !kmres & !unadj){
    warning("100m WorldPop constrained dataset only available for 2010 and 2015")
    stop("please go and download by hand from https://data.worldpop.org/GIS/Population/Individual_countries/")
    year<-c(2010,2015)[which.min(abs(year-c(2010,2015)))]
    base_url<-"https://data.worldpop.org/GIS/Population/Individual_countries/"
    url<-paste0(base_url,str_to_upper(iso3c),
                "/",str_to_lower(iso3c),"_ppp_",year,"_constrained.tif")  
    
  ############### UNCONSTRAINED DATASETS ###############
  } else if(!constrained & !kmres & unadj){    
    # UN-adjusted, 100m resolution and using the unconstrained method
    base_url<-"https://data.worldpop.org/GIS/Population/Global_2000_2020/"
    url<-paste0(base_url,year,"/",str_to_upper(iso3c),
                "/",str_to_lower(iso3c),"_ppp_",year,"_UNadj.tif")  
  } else if(!constrained & kmres & unadj){    
    # UN-adjusted, 1 km resolution and using the unconstrained method
    base_url<-"https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/"
    url<-paste0(base_url,year,"/",str_to_upper(iso3c),
                "/",str_to_lower(iso3c),"_ppp_",year,"_1km_Aggregated_UNadj.tif")
  } else if(!constrained & !kmres & !unadj){    
    # 100m resolution and using the unconstrained method
    base_url<-"https://data.worldpop.org/GIS/Population/Global_2000_2020/"
    url<-paste0(base_url,year,"/",str_to_upper(iso3c),
                "/",str_to_lower(iso3c),"_ppp_",year,".tif")  
  } else if(!constrained & kmres & !unadj){ 
    # 1 km resolution and using the unconstrained method
    base_url<-"https://data.worldpop.org/GIS/Population/Global_2000_2020_1km/"
    url<-paste0(base_url,year,"/",str_to_upper(iso3c),
                "/",str_to_lower(iso3c),"_ppp_",year,"_1km_Aggregated.tif")
  }
}
# Extract the required URL link to WorldPop and build the file names
CheckWPop_API<-function(iso3c,year,folder="./",constrained=T,kmres=T,unadj=T){
  # Main host URL
  url<-WorldPopURL(iso3c,year,constrained,kmres,unadj)
  # File name to be saved
  filer<-paste0("WorldPop_Population_UNadj_constrained_",str_to_upper(iso3c),"_",year,".tif")
  locy<-paste0(folder,filer)
  # Attempt to extract file
  checker<-tryCatch(download.file(url,locy),error = function(e) NA)
  if(is.na(checker)) {stop("Worldpop file not found, please check the WorldPop API address and your input iso3c & year")
  } else return(list(filer=filer,locy=locy))
}
#@@@@@@@@@@@@@@@@@@@@@@@@@ WORLDPOP POPULATION @@@@@@@@@@@@@@@@@@@@@@@@@@#
GetWorldPopISO3C<-function(iso3c,year=NULL,folder="./",constrained=T,kmres=T,unadj=T){
  # Try to download the most recent dataset
  if(is.null(year)) {year<-AsYear(Sys.Date()); mostrecent<-T} else mostrecent<-F
  # If we left the year blank, then let's search for the most recent CONSTRAINED dataset
  if(mostrecent){
    # Go through every year from now until 2020 until we find some data!
    yr<-year; extracter<-T
    # keep trying to extract most recent data, unless you go lower than 2020 which means WorldPop changed their API addresses
    while(extracter | yr>=2020){
      checker<-tryCatch(CheckWPop_API(iso3c,yr,folder,constrained,kmres,unadj),error = function(e) NA)
      # Check that something was returned!
      if(is.na(checker)) {yr<-yr-1; next} else extracter<-F
    }
  } else {
    checker<-tryCatch(CheckWPop_API(iso3c,year,folder,constrained,kmres,unadj),error = function(e) NA)
  }
  # If nothing happened... FML
  if(any(is.na(checker))) stop("something went wrong in extracting WorldPop data, check year, iso3c & output folder address")
  # Extract the mean hazard intensity from raster
  popy<-raster(file.path(checker$locy))
  # Convert it to how we looove it!
  popy%>%as("SpatialPixelsDataFrame")
}


#################################################################
#@@@@@@@@@@@@@@@@@ ADMIN BOUNDARY AGGREGATION @@@@@@@@@@@@@@@@@@#
#################################################################
# Load the administrative boundaries at level 2
ExtractADM<-function(ISO){
  ADM<-gadm_sp_loadCountries(
    unique(ISO),
    level = 2,
    basefile="./Data"
  )
  ADM<-ADM$spdf
  ADM@data%<>%dplyr::select(GID_0,NAME_0,GID_2,NAME_2)
  names(ADM)<-c("ISO3C","Country","ADM2_code","ADM2_name")
  # Calculate the area (in kilometres squared) of each admin boundary region
  ADM$AREA_km2<-as.numeric(st_area(st_as_sf(ADM))/1e6)
  centroids<-st_coordinates(st_centroid(st_as_sf(ADM)))
  ADM$LONGITUDE<-centroids[,1]
  ADM$LATITUDE<-centroids[,2]
  rm(centroids)
  return(ADM)
}
#################################################################
#@@@@@@@@@@@@@@@@@@@@@@@@@ EXTRACTION @@@@@@@@@@@@@@@@@@@@@@@@@@#
#################################################################

####################### GRIDDED WORLDPOP #######################
# Extract population data and put the conflict data onto the grid
GetPop<-function(ISO,ADM){
  FullADM<-data.frame()
  # Create a temporary column on sth_asia_st full of ones to count later on
  for (iso3c in unique(ISO)){
    print(paste0("Currently working on ",convIso3Country(iso3c)))
    isoframe<-data.frame()
    # Get the WorldPop data
    pop<-GetWorldPopISO3C(iso3c,2020,folder="./",constrained=T,kmres=T,unadj=T)
    names(pop)[1]<-"POPULATION"
    # Aggregate the population data to admin level 2
    popvec<-Grid2ADM(pop,
                     ADM[ADM@data$ISO3C==iso3c,],
                     sumFn="sum",
                     index = which(names(pop)=="POPULATION"),
                     ncores = cpus); rm(pop)
    # Combine into one large data.frame
    FullADM%<>%rbind(data.frame(POPULATION=popvec$all,
                                POP_POLYONLY=popvec$polyonly))
  }
  
  return(list(events=FullADM,boundaries=ADM))
  
}

GetDemog<-function(Dasher,ISO){
  # Get World Bank data
  # Gender stats
  Gend<-WBcall(AsYear(Sys.Date())-1 ,indicator="SP.POP.TOTL.FE.ZS",ISO=ISO)
  Dasher$FemalePop<-Dasher$Pop*Gend$value[1]/100
  # Under14 stats
  Und14<-WBcall(AsYear(Sys.Date())-1 ,indicator="SP.POP.0014.TO.ZS",ISO=ISO)
  Dasher$Under14Pop<-Dasher$Pop*Und14$value[1]/100
  # Over 64 stats
  Ovr64<-WBcall(AsYear(Sys.Date())-1 ,indicator="SP.POP.65UP.TO.ZS",ISO=ISO)
  Dasher$Over64Pop<-Dasher$Pop*Ovr64$value[1]/100
  
  return(Dasher)
}







