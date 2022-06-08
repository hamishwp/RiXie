#################################################################
######################### EXPOSURE DATA #########################
#################################################################
# Exposure data is split into four main components:
#   1) Population & Demographic
#   2) Physical Infrastructural Assets
#   3) Financial/Economic Assets
#   4) Environmental & Cultural Assets
# 
#@@@@@@@@@@@@@@@@@@@@@@ Population & Demography @@@@@@@@@@@@@@@@@@@@@@#


GetWorldPopISO3C<-function(iso3c){
  
  # Try to download the most recent dataset
  year<-AsYear(Sys.Date())
  # This is the main link to the WorldPop datasets
  base_url<-"https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/"
  # Go through every year from now until 2020 until we find some data!
  yr<-year; extracter<-F
  while(extracter | yr>=2020){
    # Main host URL
    url<-paste0(base_url,yr,"/BSGM/",str_to_upper(iso3c),
                "/",str_to_lower(iso3c),"_ppp_",yr,"_UNadj_constrained.tif")
    # File name to be saved
    filer<-paste0("WorldPop_Population_UNadj_constrained_",str_to_upper(iso3c),"_",yr,".tif")
    locy<-paste0(dir,"Data/Exposure/PopDemo/",filer)
    # Go get it!
    checky<-tryCatch(download.file(url,locy),error = function(e) NULL)
    # Did it work?
    if(is.null(checky)) {yr<-yr-1; next} else {
      # Extract the mean hazard intensity from raster
      popy<-raster(file.path(locy))
      # Convert it to how we looove it!
      popy%<>%asSPDF()
      # it worked!
      extracter<-T
    }
  }
  
  if(!extracter) stop("Error in extracting population data from WorldPop")
  
  return(popy)
  
}


GetExpPopDemo<-function(iso3c){
  
  # WorldPop database: 
  
}

GetExpPop<-function(iso3c){
  
  
  
}



GetExposure<-function(iso3c){
  
  
}
