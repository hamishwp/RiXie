#################################################################
######################## DEMOGRAPHIC DATA #######################
#################################################################
WorldPopURL<-function(iso3c,year,constrained=T,kmres=T,unadj=T,BGSM=T){
  ############### CONSTRAINED DATASETS ###############
  if(constrained & kmres & unadj & BGSM){
    base_url<-"https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/"
    url<-paste0(base_url,year,"/BSGM/",str_to_upper(iso3c),
                "/",str_to_lower(iso3c),"_ppp_",year,"_UNadj_constrained.tif")  
  } else if(constrained & kmres & unadj & !BGSM){
      base_url<-"https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/"
      url<-paste0(base_url,year,"/maxar_v1/",str_to_upper(iso3c),
                  "/",str_to_lower(iso3c),"_ppp_",year,"_UNadj_constrained.tif")  
  } else if(constrained & kmres & !unadj){
    base_url<-"https://data.worldpop.org/GIS/Population/Individual_countries/"
    url<-paste0(base_url,year,"/",str_to_upper(iso3c),
                "/",str_to_lower(iso3c),"_ppp_",year,"_constrained.tif")  
  } else if(constrained & !kmres & !unadj){
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
  return(url)
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
  if(is.na(checker)) {
    print("No BGSM data for WorldPop, trying for Maxar datasets")
    # If error, try the maxar-based population data
    url<-WorldPopURL(iso3c,year,constrained,kmres,unadj,BGSM = F)
    checker<-tryCatch(download.file(url,locy),error = function(e) NA)
    if(is.na(checker)) stop("Worldpop file not found, please check the WorldPop API address and your input iso3c & year")
    
  } 
  
  return(list(filer=filer,locy=locy))
}
#@@@@@@@@@@@@@@@@@@@@@@@@@ WORLDPOP POPULATION @@@@@@@@@@@@@@@@@@@@@@@@@@#
GetWorldPopISO3C<-function(iso3c,year=NULL,folder="./Data/Exposure/PopDemo/",constrained=T,kmres=T,unadj=T){
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
#@@@@@@@@@@@@@@@@@@@@@@@@@ EXTRACTION @@@@@@@@@@@@@@@@@@@@@@@@@@#
#################################################################

####################### GRIDDED WORLDPOP #######################
# Extract population data and put the conflict data onto the grid
GetPop<-function(ISO,ADM,ncores=2){
  FullADM<-data.frame()
  # Get the WorldPop data
  if(length(list.files("./Data/Exposure/PopDemo/",ISO))==0){
    pop<-GetWorldPopISO3C(ISO,2020,folder="./Data/Exposure/PopDemo/",constrained=T,kmres=T,unadj=T)
    names(pop)[1]<-"POPULATION"
  } else {
    pop<-raster(paste0("./Data/Exposure/PopDemo/",
                       list.files("./Data/Exposure/PopDemo/",ISO)))%>%
    as("SpatialPixelsDataFrame")
    names(pop@data)[1]<-"POPULATION"
  }
  # Aggregate the population data to admin level 2
  popvec<-Grid2ADM(pop,
                   ADM[ADM@data$ISO3C==ISO,],
                   sumFn="sum",
                   index = which(names(pop)=="POPULATION"),
                   ncores = ncores)
  # Scale to make sure that the value is current
  popvec$all<-popvec$all*InterpPopWB(ISO,Sys.Date(),normdate=as.Date("2015-01-01"))$factor
  # Combine into one large data.frame
  ADM@data%<>%cbind(data.frame(POPULATION=round(popvec$all)))
  return(ADM)
}

GetDemog<-function(Dasher,ISO){
  # Get World Bank data
  # Gender stats
  Gend<-WBcall(AsYear(Sys.Date())-1 ,indicator="SP.POP.TOTL.FE.ZS",ISO=ISO)
  Dasher$FemalePop<-round(Dasher$POPULATION*Gend$value[1]/100)
  # Under14 stats
  Und14<-WBcall(AsYear(Sys.Date())-1 ,indicator="SP.POP.0014.TO.ZS",ISO=ISO)
  Dasher$Under14Pop<-round(Dasher$POPULATION*Und14$value[1]/100)
  # Over 64 stats
  Ovr64<-WBcall(AsYear(Sys.Date())-1 ,indicator="SP.POP.65UP.TO.ZS",ISO=ISO)
  Dasher$Over64Pop<-round(Dasher$POPULATION*Ovr64$value[1]/100)
  
  return(Dasher)
}







