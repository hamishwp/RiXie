###############################################################
######################### HAZARD DATA #########################
###############################################################
# Flood hazard map (100 year return period)
# https://data.europa.eu/data/datasets/jrc-floods-floodmapgl_rp100y-tif?locale=en
GetFloodRisk<-function(ADM,ISO){
  
  FL<-brick("./Data/Hazard/floodMapGL_rp100y.tif")
  projection(FL)<-"+proj=longlat +datum=WGS84 +no_defs"
  e <- as(extent(ADM@bbox[c(1,3,2,4)]), 'SpatialPolygons')
  crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
  FL%<>%crop(e)%>%as("SpatialPixelsDataFrame")
  
  # Aggregate the population data to admin level 2
  FLr<-Grid2ADM(FL,outsiders = F,
                   ADM,
                   sumFn="mean",
                   index = 1,
                   ncores = ncores)
  # Combine into one large data.frame
  ADM@data%<>%cbind(data.frame(FL100yr=FLr))
  return(ADM)
}
# Tsunami risk dataset
# Historical tsunamis can be found here: https://www.kaggle.com/datasets/andrewmvd/tsunami-dataset
# https://preview.grid.unep.ch/index.php?preview=data&events=tsunamis&evcat=2&lang=eng
GetTsunamiRisk<-function(ADM,ISO){
  
  TS<-raster("./Data/Hazard/ts_frequency.tif")
  projection(TS)<-"+proj=longlat +datum=WGS84 +no_defs"
  e <- as(extent(ADM@bbox[c(1,3,2,4)]), 'SpatialPolygons')
  crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
  TS%<>%crop(e)%>%as("SpatialPixelsDataFrame")
  
  # Aggregate the population data to admin level 2
  TSr<-Grid2ADM(TS,outsiders = F,
                   ADM,
                   sumFn="mean",
                   index = 1,
                   ncores = ncores)
  # Combine into one large data.frame
  ADM@data%<>%cbind(data.frame(Tsunami=round(TSr)))
  return(ADM)
}

TS%<>%crop(e)%>%as("SpatialPixelsDataFrame")
ADM2$Tsunami<-TS%>%raster%>%raster::extract(ADM2,method='bilinear',fun=mean)%>%as.numeric()

# Extreme sea level events
# https://doi.org/10.3389/fmars.2020.00263      data found at      https://zenodo.org/record/3660927#.Yr6OunhByV4
# This dataset should be replaced by the more recent and higher resolution European Space Agency (ESA) one (https://catalogue.ceda.ac.uk/uuid/a0a6fa39470a4a7baf847e3a1751f950?jump=related-anchor)
SLR<-nc_open("./Data/Hazard/SeaLevelRise_CODEC_amax_ERA5_1979_2017_coor_mask_GUM_RPS.nc") # varname="RPS"
RPS<-ncvar_get(SLR,"RPS")
return_periods<-ncvar_get(SLR,"return_periods")
# Choose a 10-year return period (why not?)
ESL<-data.frame(Longitude=ncvar_get(SLR,"station_x_coordinate"),
                Latitude=ncvar_get(SLR,"station_y_coordinate"),
                ESL=RPS[return_periods==10,])
nc_close(SLR)

ind<-ESL$Longitude>ADM2@bbox[1]&
  ESL$Longitude<ADM2@bbox[3]&
  ESL$Latitude>ADM2@bbox[2]&
  ESL$Latitude<ADM2@bbox[4]

# ESL<-SpatialPointsDataFrame(ESL[ind,1:2],filter(ESL,ind),bbox = ADM2@bbox)
ESL<-ESL[ind,]

centroiders<-rgeos::gCentroid(ADM2,byid=TRUE)

vals<-sapply(1:nrow(ESL), function(i){
  geodist(coordinates(centroiders),ESL[i,1:2])
})
ADM2$ExtrSeaLevel<-ESL$ESL[sapply(1:nrow(vals),function(i) which.min(vals[i,]))]%>%as.numeric()

# Tropical cyclone risk map 
# https://www.nature.com/articles/s41597-020-0381-2#Sec9        
# download present risk here: https://data.4tu.nl/articles/dataset/STORM_tropical_cyclone_wind_speed_return_periods/12705164
# download climate change future risk here (good luck...): https://data.4tu.nl/articles/dataset/STORM_climate_change_tropical_cyclone_wind_speed_return_periods/14510817
# REMEMBER TO ALSO EXTRACT BASIN POLYGON FILE: 'Basins of STORM IBTrACS dataset.kml'
TC<-raster("./Data/Hazard/STORM_FIXED_RETURN_PERIODS_NI_200_YR_RP.tif")
TC%<>%crop(e)%>%as("SpatialPixelsDataFrame")
ADM2$Cyclone<-TC%>%raster%>%raster::extract(ADM2,method='bilinear',fun=mean)%>%as.numeric()

AirTemp<-raster("~/Downloads/Maldives_TEMP_GISdata_LTAym_DailySum_GlobalSolarAtlas_GEOTIFF/TEMP.tif")
AirTemp%<>%crop(e)%>%as("SpatialPixelsDataFrame")
ADM2$AirTemp<-AirTemp%>%raster%>%raster::extract(ADM2,method='bilinear',fun=mean)%>%as.numeric()

rgdal::writeOGR(ADM2,
         dsn="./Data/MDV_Data_2",
         layer = "map",
         driver = "ESRI Shapefile",overwrite_layer = T)


