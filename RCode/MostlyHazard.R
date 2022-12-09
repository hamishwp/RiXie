###############################################################
######################### HAZARD DATA #########################
###############################################################
# Flood hazard map (100 year return period)
# https://data.europa.eu/data/datasets/jrc-floods-floodmapgl_rp100y-tif?locale=en
GetFloodRisk<-function(ADM,ISO,ext){
  FL<-brick(paste0(dir,"/Data/Hazard/floodMapGL_rp100y.tif"))
  projection(FL)<-"+proj=longlat +datum=WGS84 +no_defs"
  FL%<>%crop(ext)
  # Interpolate onto the population grid
  # pop%<>%InterpOn(FromDF=FL,namer="tmp",ext)
  # Interpolate onto the admin level polygons
  ADM$FL100yr<-FL%>%raster::extract(ADM,method='bilinear',fun=mean,na.rm=T)%>%as.numeric()
  return(ADM)
}

# https://datacatalog.worldbank.org/search/dataset/0037584/Global-landslide-hazard-map
GetLandslide<-function(ADM,ISO,ext){
  LS<-raster(paste0(dir,"/Data/Hazard/LS_EQ.tif"))
  projection(LS)<-"+proj=longlat +datum=WGS84 +no_defs"
  LS%<>%crop(ext)
  # Combine into one large data.frame
  ADM$LS<-LS%>%raster::extract(ADM,method='bilinear',fun=mean,na.rm=T)%>%as.numeric()
  return(ADM)
}

# https://sites.wustl.edu/acag/datasets/surface-pm2-5/#V5.GL.02
# https://wustl.app.box.com/v/ACAG-V5GL02-GWRPM25/folder/148054977849?page=2
GetAirPollution<-function(ADM,ISO,ext){
  SLR<-nc_open(paste0(dir,"/Data/Hazard/V5GL02.HybridPM25.Global.202001-202012.nc"))
  # Extract variables
  RPS<-ncvar_get(SLR,"GWRPM25")%>%as.vector()
  Longitude<-ncvar_get(SLR,"lon")
  Latitude<-ncvar_get(SLR,"lat")
  nc_close(SLR)
  
  RPSout<-raster(ncol=length(Longitude), nrow=length(Latitude),
                 xmn=min(Longitude), xmx=max(Longitude),
                 ymn=min(Latitude), ymx=max(Latitude))
  values(RPSout)<-RPS
  RPSout%<>%crop(ext)
  
  ADM$AirPollution<-RPSout%>%raster::extract(ADM,method='bilinear',na.rm=T,fun=mean)%>%as.numeric()
  
  return(ADM)
  
}

# https://datacatalog.worldbank.org/search/dataset/0038576/Global-earthquake-hazard
GetEarthquakeRisk<-function(ADM,ISO,ext){
  EQ<-raster(paste0(dir,"/Data/Hazard/pga_475y.tif"))
  projection(EQ)<-"+proj=longlat +datum=WGS84 +no_defs"
  EQ%<>%crop(ext)
  # Combine into one large data.frame
  ADM$EQ475yr<-EQ%>%raster::extract(ADM,method='bilinear',fun=mean,na.rm=T)%>%as.numeric()
  return(ADM)
}

# Tsunami risk dataset
# Historical tsunamis can be found here: https://www.kaggle.com/datasets/andrewmvd/tsunami-dataset
# https://preview.grid.unep.ch/index.php?preview=data&events=tsunamis&evcat=2&lang=eng
# https://www.geonode-gfdrrlab.org/layers/hazard:ts_mih_rp50
# https://pubs.er.usgs.gov/publication/70196102
#https://www.earth-prints.org/bitstream/2122/11610/1/2018_Davies_etal_GSL-SP456.5.pdf
#https://datacatalog.worldbank.org/search/dataset/0040783
GetTsunamiRisk<-function(ADM,ISO,ext){
  TS<-raster(paste0(dir,"/Data/Hazard/ts_frequency.tif"))
  projection(TS)<-"+proj=longlat +datum=WGS84 +no_defs"
  TS%<>%crop(ext)
  # Note we use bilinear interpolation because none of the none-zero values 
  # will be inside the admin boundaries
  ADM$Tsunami<-TS%>%raster::extract(ADM,method='bilinear',na.rm=T,fun=mean)%>%as.numeric()
  return(ADM)
}

# Extreme sea level events
# https://doi.org/10.3389/fmars.2020.00263      data found at      https://zenodo.org/record/3660927#.Yr6OunhByV4
# This dataset should be replaced by the more recent and higher resolution European Space Agency (ESA) one (https://catalogue.ceda.ac.uk/uuid/a0a6fa39470a4a7baf847e3a1751f950?jump=related-anchor)
GetSeaLevelRise<-function(ADM,ISO){
  SLR<-nc_open(paste0(dir,"/Data/Hazard/SeaLevelRise_CODEC_amax_ERA5_1979_2017_coor_mask_GUM_RPS.nc"))
  
  RPS<-ncvar_get(SLR,"RPS")
  return_periods<-ncvar_get(SLR,"return_periods")
  RPS<-RPS[return_periods==50,]%>%as.vector()
  # Choose a 10-year return period (why not?)
  Longitude<-ncvar_get(SLR,"station_x_coordinate")
  Latitude<-ncvar_get(SLR,"station_y_coordinate")
  nc_close(SLR)
  
  RPSout<-SpatialPointsDataFrame(data.frame(Longitude=Longitude,Latitude=Latitude),
                                 data.frame(RPS=RPS))
  
  ADM$ExtrSeaLevel50yr<-RPSout%>%raster::extract(ADM,method='bilinear',na.rm=T,fun=mean)%>%as.numeric()
  
  return(ADM)
}

# Tropical cyclone risk map 
# https://www.nature.com/articles/s41597-020-0381-2#Sec9        
# download present risk here: https://data.4tu.nl/articles/dataset/STORM_tropical_cyclone_wind_speed_return_periods/12705164
# download climate change future risk here (good luck...): https://data.4tu.nl/articles/dataset/STORM_climate_change_tropical_cyclone_wind_speed_return_periods/14510817
# REMEMBER TO ALSO EXTRACT BASIN POLYGON FILE: 'Basins of STORM IBTrACS dataset.kml'
GetTropCyc<-function(ADM,ISO,ext){
  TC<-raster(paste0(dir,"/Data/Hazard/STORM_FIXED_RETURN_PERIODS_NI_50_YR_RP.tif"))
  TC%<>%crop(ext)
  ADM$TC50yr<-TC%>%raster::extract(ADM,method='bilinear',fun=mean,na.rm=T)%>%as.numeric()
  return(ADM)
}

# https://datacatalog.worldbank.org/search/dataset/0038605/Global-volcanic-ash-hazard
GetVolcAsh<-function(ADM,ISO,ext){
  VC<-brick(paste0(dir,"/Data/Hazard/volc_globalproximalhazard_wgs84_dH3pTEE.tif"))
  VC%<>%crop(ext)
  tmp<-VC%>%raster::extract(ADM,method='bilinear',fun=mean,na.rm=T)%>%as.integer()
  tmp[tmp<100]<-100; tmp[tmp>103]<-103
  ADM$VC<-"No Evidence"
  ADM$VC[tmp==100]<-"No Evidence"
  ADM$VC[tmp==101]<-"Low"
  ADM$VC[tmp==102]<-"Medium"
  ADM$VC[tmp==103]<-"High"
  return(ADM)
}

# https://datacatalog.worldbank.org/search/dataset/0040194/Global-extreme-heat-hazard
GetExtremeHeat<-function(ADM,ISO,ext){
  EH<-raster(paste0(dir,"/Data/Hazard/intensity_returnperiod20y.tif"))
  EH%<>%crop(ext)
  ADM$EHeat20yr<-EH%>%raster::extract(ADM,method='bilinear',fun=mean,na.rm=T)%>%as.numeric()
  return(ADM)
}

# Drought - 50 year return period
# https://www.geonode-gfdrrlab.org/layers/hazard:wci_m3yrcap_current2010_ensmean_pcrglobwb_rp50
#WCI and indicator for water scarcity
GetDrought<-function(ADM,ISO=ISO,ext){
  DR<-raster(paste0(dir,"/Data/Hazard/wci_m3yrcap_current2010_ensmean_pcrglobwb_rp50.tif"))
  DR%<>%crop(ext)
  # Combine into one large data.frame
  ADM$DR50yr<-DR%>%raster::extract(ADM,method='bilinear',fun=mean,na.rm=T)%>%as.numeric()
  return(ADM)
}


# rgdal::writeOGR(ADM,
#                 dsn="./Data/MDV_Data_2",
#                 layer = "map",
#                 driver = "ESRI Shapefile",overwrite_layer = T)