###############################################################
######################### HAZARD DATA #########################
###############################################################
# Flood hazard map (100 year return period)
# https://data.europa.eu/data/datasets/jrc-floods-floodmapgl_rp100y-tif?locale=en
GetFloodRisk<-function(ADM,ISO,RP = 100, ext){
  fil<-list.files(paste0(dir,"/Data/Hazard/Flood"), pattern = paste0("rp",RP,"y.tif"),full.names = TRUE)
  x<-terra::rast(fil)%>%
    crop(ext)%>%
    terra::extract(ADM,method='bilinear',fun=mean,na.rm=T,ID=FALSE) %>%
    unlist() %>%
    ifelse(is.nan(.) ==TRUE,NA,.)  #NaN to Na
  ADM$x<-x
  names(ADM)[names(ADM) == "x"] <- paste0("Fl_rp",RP,"y")
  return(ADM)
}

# https://datacatalog.worldbank.org/search/dataset/0037584/Global-landslide-hazard-map
GetLandslide<-function(ADM,ISO,trigger = "EQ", ext){
  if(trigger == "EQ"){
    fil<-"LS_EQ.tif"
  }else{
    fil<-"LS_RF_Mean_1980-2018.tif"
  }
  x<-terra::rast(paste0(dir,"/Data/Hazard/Landslide/",fil)) %>%
    crop(ext)%>%terra::extract(ADM,method='bilinear',fun=mean,na.rm=T,ID=FALSE)%>%
    unlist() %>%
    ifelse(is.nan(.) ==TRUE,NA,.)  #NaN to Na
  ADM$x<-x
  names(ADM)[names(ADM) == "x"] <- paste0("LS_",trigger)
  return(ADM)
}

# https://sites.wustl.edu/acag/datasets/surface-pm2-5/#V5.GL.02
# https://wustl.app.box.com/v/ACAG-V5GL02-GWRPM25/folder/148054977849?page=2
GetAirPollution<-function(ADM,ISO,ext){
  SLR<-nc_open(paste0(dir,"/Data/Hazard/AirPollution/PM2_5/Annual/V5GL02.HybridPM25.Global.202001-202012.nc"))
  # Extract variables
  RPS<-ncvar_get(SLR,"GWRPM25")%>%as.vector()
  Longitude<-ncvar_get(SLR,"lon")
  Latitude<-ncvar_get(SLR,"lat")
  nc_close(SLR)
  
  RPSout<-raster(ncol=length(Longitude), nrow=length(Latitude),
                 xmn=min(Longitude), xmx=max(Longitude),
                 ymn=min(Latitude), ymx=max(Latitude))
  values(RPSout)<-RPS
  RPSout%<>%crop(ext)%>%raster::extract(ADM,method='bilinear',na.rm=T,fun=mean)%>%as.numeric()%>%
    ifelse(is.nan(.) ==TRUE,NA,.)  #NaN to Na
  ADM$x<-RPSout
  names(ADM)[names(ADM) == "x"] <- paste0("AirPollu_PM2_5")
  return(ADM)
  
}

# https://datacatalog.worldbank.org/search/dataset/0038576/Global-earthquake-hazard
GetEarthquakeRisk<-function(ADM,ISO,RP = 475,ext){
  fil<-list.files(paste0(dir,"/Data/Hazard/EQuake"), pattern = paste0("_",RP,"y.tif"),full.names = TRUE)
  
  x<-terra::rast(fil)%>%crop(ext)%>%
    terra::extract(ADM,method='bilinear',fun=mean,na.rm=T,ID=FALSE)%>%
    mutate_all(~ifelse(is.nan(.), NA, .)) %>%
    unlist() %>%
    ifelse(is.nan(.) ==TRUE,NA,.)  #NaN to Na
  ADM$x<-x
  names(ADM)[names(ADM) == "x"] <- paste0("EQ_rp",RP,"y")
  return(ADM)
}

# Tsunami risk dataset
# Historical tsunamis can be found here: https://www.kaggle.com/datasets/andrewmvd/tsunami-dataset
# https://preview.grid.unep.ch/index.php?preview=data&events=tsunamis&evcat=2&lang=eng
# https://www.geonode-gfdrrlab.org/layers/hazard:ts_mih_rp50
# https://pubs.er.usgs.gov/publication/70196102
#https://www.earth-prints.org/bitstream/2122/11610/1/2018_Davies_etal_GSL-SP456.5.pdf
#https://datacatalog.worldbank.org/search/dataset/0040783
GetTsunamiRisk<-function(ADM,ISO,RP,ext){
  fil<-list.files(paste0(dir,"/Data/Hazard/Tsunami"), pattern = paste0("rp",RP,"_"),full.names = TRUE)
  x<-terra::rast(fil) %>%
    crop(ext)%>%
    terra::extract(ADM,method='bilinear',na.rm=T,fun=mean,ID=FALSE)%>%
    unlist() %>%
    ifelse(is.nan(.) ==TRUE,NA,.)  #NaN to Na
  ADM$x<-x
  names(ADM)[names(ADM) == "x"] <- paste0("Ts_mih_rp",RP,"y")
 
  return(ADM)
}

# Extreme sea level events
# https://doi.org/10.3389/fmars.2020.00263      data found at      https://zenodo.org/record/3660927#.Yr6OunhByV4
# This dataset should be replaced by the more recent and higher resolution European Space Agency (ESA) one (https://catalogue.ceda.ac.uk/uuid/a0a6fa39470a4a7baf847e3a1751f950?jump=related-anchor)
GetSeaLevelRise<-function(ADM,RP,ISO){
  SLR<-nc_open(paste0(dir,"/Data/Hazard/SeaLevelRise/CODEC_amax_ERA5_1979_2017_coor_mask_GUM_RPS.nc"))
  RPS<-ncvar_get(SLR,"RPS")
  rps<-ncvar_get(SLR,"return_periods")
  RPS<-RPS[rps==RP,]%>%as.vector()
  # Choose a 10-year return period (why not?)
  Long<-ncvar_get(SLR,"station_x_coordinate")
  Lat<-ncvar_get(SLR,"station_y_coordinate")
  nc_close(SLR)
 
  dd<-data.frame(Longitude=Long,Latitude=Lat,RPS=RPS)
  RPSout <- sf::st_as_sf(dd, coords = c("Longitude","Latitude")) %>%
    st_set_crs(st_crs(ADM))
  
  ADM_buf<-st_buffer(ADM,dist= 10000)  #AS an estimate, create outer buffer to polygon  to collect sufficient points.
  
  # find points within polygons
  rps_in_adm <- st_intersects(ADM_buf, RPSout) %>%
    lapply(., function(x) RPSout[unlist(x),] %>%
             st_drop_geometry() %>%
             dplyr::summarize(RPSmean = mean(RPS, na.rm=TRUE))%>%
             mutate_all(~ifelse(is.nan(.), NA, .))) %>% #NaN to Na) 
           unlist()
  
  
  ADM$x<-rps_in_adm
  names(ADM)[names(ADM) == "x"] <- paste0("SLR_rp",RP,"y")
  return(ADM)
}

# Tropical cyclone risk map 
# https://www.nature.com/articles/s41597-020-0381-2#Sec9        
# download present risk here: https://data.4tu.nl/articles/dataset/STORM_tropical_cyclone_wind_speed_return_periods/12705164
# download climate change future risk here (good luck...): https://data.4tu.nl/articles/dataset/STORM_climate_change_tropical_cyclone_wind_speed_return_periods/14510817
# REMEMBER TO ALSO EXTRACT BASIN POLYGON FILE: 'Basins of STORM IBTrACS dataset.kml' or
#Read all files for all basins then mosaic so global in extent (more or less)
GetTropCyc<-function(ADM,ISO,RP = 50, ext){
  fil<-list.files(path = paste0(dir,"/Data/Hazard/TrCyclone"), pattern = paste0(RP,"_YR"), full.names = TRUE)
  
  x<-lapply(fil, terra::rast) %>%
    do.call(mosaic, .)%>%
    terra::extract(y=ADM, method='bilinear',fun=mean,na.rm=T, ID = F) %>%
    unlist()%>%
    ifelse(is.nan(.) ==TRUE,NA,.)  #NaN to Na
  ADM$x<-x
  names(ADM)[names(ADM) == "x"] <- paste0("TCyc_rp",RP,"y")

  return(ADM)
}

# https://datacatalog.worldbank.org/search/dataset/0038605/Global-volcanic-ash-hazard
GetVolcAsh<-function(ADM,ISO,ext){
  VC<-brick(paste0(dir,"/Data/Hazard/VolcanicAsh/volc_globalproximalhazard_wgs84_dH3pTEE.tif"))
  tmp<-VC%>%
    crop(ext)%>%
    terra::extract(ADM,method='bilinear',fun=mean,na.rm=T,ID=FALSE)%>%
    unlist()
  tmp[tmp<100 | is.nan(tmp) ==TRUE]<-100; tmp[tmp>103]<-103
  ADM$VAsh<-"No Evidence"
  ADM$VAsh[tmp==100]<-"No Evidence"
  ADM$VAsh[tmp==101]<-"Low"
  ADM$VAsh[tmp==102]<-"Medium"
  ADM$VAsh[tmp==103]<-"High"
  return(ADM)
}

# https://datacatalog.worldbank.org/search/dataset/0040194/Global-extreme-heat-hazard
GetExtremeHeat<-function(ADM,ISO,RP = 20,ext){
  fil<- list.files(paste0(dir,"/Data/Hazard/ExtremeHeat"), pattern = paste0("*",RP,"y.tif"),full.names = TRUE)
  x<-terra::rast(fil)%>%crop(ext)%>%
    crop(ext)%>%
    terra::extract(ADM,method='bilinear',fun=mean,na.rm=T,ID=FALSE)%>%
    unlist()%>%
    ifelse(is.nan(.) ==TRUE,NA,.)  #NaN to Na
  ADM$x<-x
  names(ADM)[names(ADM) == "x"] <- paste0("EH_rp",RP,"y")
  return(ADM)
}

# Water scarcity - 50 year return period
# https://www.geonode-gfdrrlab.org/layers/hazard:wci_m3yrcap_current2010_ensmean_pcrglobwb_rp50
#WCI and indicator for water scarcity
GetWatScarceWCI<-function(ADM,ISO=ISO,RP= 50,ext){
  fil<- list.files(paste0(dir,"/Data/Hazard/WaterScarcity/"), pattern = paste0("*rp",RP,".tif"),full.names = TRUE)
  x<- terra::rast(fil)%>%crop(ext)%>%
    terra::extract(ADM,method='bilinear',fun=mean,na.rm=T,ID=FALSE)%>%
    unlist()%>%
    ifelse(is.nan(.) ==TRUE,NA,.)  #NaN to Na
  ADM$x<-x
  names(ADM)[names(ADM) == "x"] <- paste0("Dr-WCI_rp",RP,"y")
  return(ADM)
}

# rgdal::writeOGR(ADM,
#                 dsn="./Data/MDV_Data_2",
#                 layer = "map",
#                 driver = "ESRI Shapefile",overwrite_layer = T)


#Drought - https://data.humdata.org/dataset/global-drought-hazard
GetDroughtSPI<-function(ADM,ISO=ISO,RP=50, ext){
  fil<-list.files(path = paste0(dir,"/Data/Hazard/Drought/SPI_Humdata"),pattern =paste0(RP,"-years"),full.names = TRUE)
  spei_val<-substr(sub(".*spei", "", x=fil),2,4)  # Extract characters after pattern
  x <- raster::stack(fil)%>%
    crop(ext) %>%
    terra::extract(ADM,method='bilinear',fun=mean,na.rm=T) %>%
    as.data.frame() %>%
    mutate_all(~ifelse(is.nan(.), NA, .))  #NaN to Na
  names(x) <- paste0("Dr-SPEI_",spei_val,"_rp",RP,"y")
  ADM<-cbind(ADM,x)
}
