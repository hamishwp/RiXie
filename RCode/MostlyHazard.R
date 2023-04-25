#----------------------------------
#-HAZARD DATA - following HIPs -#
#--------------------------------
#Pls. arrange alphabetically per hazard type so easy to follow when have to insert a function later.


#--------------
#1. GEOHAZARDS
##--------------

# https://datacatalog.worldbank.org/search/dataset/0038576/Global-earthquake-hazard
GetEarthquakeRisk<-function(ADM,ISO,RP = 475,ext){
  fil<-list.files(paste0(dir,"/Data/Hazard/GEOHAZARDS/EQuake"), pattern = paste0("_",RP,"y.tif"),full.names = TRUE)
  
  x<-terra::rast(fil)%>%crop(ext)%>%
    terra::extract(ADM,method='bilinear',fun=mean,na.rm=T,ID=FALSE)%>%
    mutate_all(~ifelse(is.nan(.), NA, .)) %>%
    unlist() %>%
    ifelse(is.nan(.) ==TRUE,NA,.)  #NaN to Na
  ADM$x<-x
  names(ADM)[names(ADM) == "x"] <- paste0("EQ_rp",RP,"y")
  return(ADM)
}

# https://datacatalog.worldbank.org/search/dataset/0037584/Global-landslide-hazard-map
GetLandslide<-function(ADM,ISO,trigger = "EQ", ext){
  if(trigger == "EQ"){
    fil<-"LS_EQ.tif"
  }else{
    fil<-"LS_RF_Mean_1980-2018.tif"
  }
  x<-terra::rast(paste0(dir,"/Data/Hazard/GEOHAZARDS/Landslide/",fil)) %>%
    crop(ext)%>%terra::extract(ADM,method='bilinear',fun=mean,na.rm=T,ID=FALSE)%>%
    unlist() %>%
    ifelse(is.nan(.) ==TRUE,NA,.)  #NaN to Na
  # #reclassify to classes as used by ThinkHazard!
  # x<-ifelse(x<0.0001, "Very Low",ifelse(x<=0.001, "Low",ifelse(x <=0.01, "Medium", "High")))
  
  ADM$x<-x
  names(ADM)[names(ADM) == "x"] <- paste0("LS_",trigger)
  return(ADM)
}

# https://datacatalog.worldbank.org/search/dataset/0038605/Global-volcanic-ash-hazard
GetVolcAsh<-function(ADM,ISO,ext){
  VC<-brick(paste0(dir,"/Data/Hazard/GEOHAZARDS/VolcanicAsh/volc_globalproximalhazard_wgs84_dH3pTEE.tif"))
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



#-----------------------------------
#2. HYDROLOGICAL AND METEOROLOGICAL
#-----------------------------------

#Drought - https://data.humdata.org/dataset/global-drought-hazard
GetDroughtSPI<-function(ADM,ISO=ISO,RP=50, ext){
  fil<-list.files(path = paste0(dir,"/Data/Hazard/HYDROMETEO/Drought/SPI_Humdata"),pattern =paste0(RP,"-years"),full.names = TRUE)
  spei_val<-substr(sub(".*spei", "", x=fil),2,4)  # Extract characters after pattern
  x <- raster::stack(fil)%>%
    crop(ext) %>%
    terra::extract(ADM,method='bilinear',fun=mean,na.rm=T) %>%
    as.data.frame() %>%
    mutate_all(~ifelse(is.nan(.), NA, .))  #NaN to Na
  names(x) <- paste0("Dr-SPEI_",spei_val,"_rp",RP,"y")
  ADM<-cbind(ADM,x)
}


# https://datacatalog.worldbank.org/search/dataset/0040194/Global-extreme-heat-hazard
GetExtremeHeat<-function(ADM,ISO,RP = 20,ext){
  fil<- list.files(paste0(dir,"/Data/Hazard/HYDROMETEO/ExtremeHeat"), pattern = paste0("*",RP,"y.tif"),full.names = TRUE)
  x<-terra::rast(fil)%>%crop(ext)%>%
    crop(ext)%>%
    terra::extract(ADM,method='bilinear',fun=mean,na.rm=T,ID=FALSE)%>%
    unlist()%>%
    ifelse(is.nan(.) ==TRUE,NA,.)  #NaN to Na
  ADM$x<-x
  names(ADM)[names(ADM) == "x"] <- paste0("EH_rp",RP,"y")
  return(ADM)
}


# Flood hazard map (100 year return period)
# https://data.europa.eu/data/datasets/jrc-floods-floodmapgl_rp100y-tif?locale=en
GetFloodRisk<-function(ADM,ISO,RP = 100, ext){
  fil<-list.files(paste0(dir,"/Data/Hazard/HYDROMETEO/Flood"), pattern = paste0("rp",RP,"y.tif"),full.names = TRUE)
  x<-terra::rast(fil)%>%
    crop(ext)%>%
    terra::extract(ADM,method='bilinear',fun=mean,na.rm=T,ID=FALSE) %>%
    unlist() %>%
    ifelse(is.nan(.) ==TRUE,NA,.)  #NaN to Na
  ADM$x<-x
  names(ADM)[names(ADM) == "x"] <- paste0("Fl_rp",RP,"y")
  return(ADM)
}


# Tropical cyclone risk map 
# https://www.nature.com/articles/s41597-020-0381-2#Sec9        
# download present risk here: https://data.4tu.nl/articles/dataset/STORM_tropical_cyclone_wind_speed_return_periods/12705164
# download climate change future risk here (good luck...): https://data.4tu.nl/articles/dataset/STORM_climate_change_tropical_cyclone_wind_speed_return_periods/14510817
# REMEMBER TO ALSO EXTRACT BASIN POLYGON FILE: 'Basins of STORM IBTrACS dataset.kml' or
#Read all files for all basins then mosaic so global in extent (more or less)
GetTropCyc<-function(ADM,ISO,RP = 50, ext){
  fil<-list.files(path = paste0(dir,"/Data/Hazard/HYDROMETEO/TrCyclone"), pattern = paste0(RP,"_YR"), full.names = TRUE)
  
  x<-lapply(fil, terra::rast) %>%
    do.call(mosaic, .)%>%
    terra::extract(y=ADM, method='bilinear',fun=mean,na.rm=T, ID = F) %>%
    unlist()%>%
    ifelse(is.nan(.) ==TRUE,NA,.)  #NaN to Na
  ADM$x<-x
  names(ADM)[names(ADM) == "x"] <- paste0("TCyc_rp",RP,"y")

  return(ADM)
}



# Water scarcity - 50 year return period
# https://www.geonode-gfdrrlab.org/layers/hazard:wci_m3yrcap_current2010_ensmean_pcrglobwb_rp50
#WCI and indicator for water scarcity
GetWatScarceWCI<-function(ADM,ISO=ISO,RP= 50,ext){
  fil<- list.files(paste0(dir,"/Data/Hazard/HYDROMETEO/WaterScarcity/"), pattern = paste0("*rp",RP,".tif"),full.names = TRUE)
  x<- terra::rast(fil)%>%crop(ext)%>%
    terra::extract(ADM,method='bilinear',fun=mean,na.rm=T,ID=FALSE)%>%
    unlist()%>%
    ifelse(is.nan(.) ==TRUE,NA,.)  #NaN to Na
  ADM$x<-x
  names(ADM)[names(ADM) == "x"] <- paste0("Dr-WCI_rp",RP,"y")
  return(ADM)
}




#-----------------
#3. ENVIRONMENTAL 
#-----------------
# https://sites.wustl.edu/acag/datasets/surface-pm2-5/#V5.GL.02
# https://wustl.app.box.com/v/ACAG-V5GL02-GWRPM25/folder/148054977849?page=2
GetAirPollution<-function(ADM,ISO,ext){
  PM25<-nc_open(paste0(dir,"/Data/Hazard/ENVIRONMENTAL/AirPollution/PM2_5/Annual/V5GL02.HybridPM25.Global.202001-202012.nc"))
  nc_close(PM25)
  
  varn<-names(PM25$var)
  
  PM25ras<-raster(paste0(dir,"/Data/Hazard/ENVIRONMENTAL/AirPollution/PM2_5/Annual/V5GL02.HybridPM25.Global.202001-202012.nc"),varname=varn)
  PM25ras%<>%crop(ext)%>%
    raster::extract(ADM,method='bilinear',na.rm=T,fun=mean)%>%as.numeric()%>%
    ifelse(is.nan(.) ==TRUE,NA,.)  #NaN to Na
  
  ADM$x<-PM25ras
  names(ADM)[names(ADM) == "x"] <- paste0("AirPollu_PM2_5")
  return(ADM)
  
}

#Coral Bleaching database - SQLite
#https://www.nature.com/articles/s41597-022-01121-y#Sec4
GeCoralBleaching<-function(ADM,ISO,ext){
  library(RSQLite)
  filename <- paste0(dir,"/Data/Hazard/ENVIRONMENTAL/CoralBleaching/Global_Coral_Bleaching_Database_SQLite_11_24_21.db")
  sqlite.driver <- dbDriver("SQLite")
  db <- dbConnect(sqlite.driver,dbname = filename)
  ## Some operations
  #dbListTables(db)
  site<-dbReadTable(db,"Site_Info_tbl") %>%    select(Site_ID,Latitude_Degrees,Longitude_Degrees)
  samples<-dbReadTable(db,"Sample_Event_tbl")%>%    select(Sample_ID,Site_ID,Date_Day,Date_Month,Date_Year,Depth_m)
  bleach_perc <- dbReadTable(db,"Bleaching_tbl")%>%    select(Sample_ID, Bleaching_ID,Percent_Bleached)
  #Joining tables:
  site_samp<-dplyr::left_join(site,samples, by="Site_ID")
  site_samp_blea<-left_join(site_samp,bleach_perc,by="Sample_ID")
  
  pts = st_as_sf(site_samp_blea, coords = c("Longitude_Degrees", "Latitude_Degrees"), crs = 4326)
  
  if(ISO!= "FJI"){
    ADM_buf<-st_buffer(ADM,dist= 10000)  #AS an estimate, create outer buffer to polygon  to collect sufficient points.
    
    # find points within polygons
    shore_in_adm <- st_filter(pts, ADM_buf) %>%
      st_drop_geometry()%>%
      dplyr::summarize(Percent_Bleached = mean(Percent_Bleached, na.rm=TRUE))%>%
      mutate_all(function(x) ifelse(is.nan(x), NA, x))
    
    ADM$x<-shore_in_adm$Percent_Bleached
    } else{
      ADM$x<-NA
      }
  names(ADM)[names(ADM) == "x"] <- "CorlBlchPerc"
  return(ADM)
  }





#Global Shoreline change-Long Term Change from SLR_Retreat and Ambient Changes
#https://data.jrc.ec.europa.eu/dataset/18eb5f19-b916-454f-b2f5-88881931587e
#https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/LISCOAST/globalShorelineChange/LATEST/globalCoastalMorphodynamicsDb.nc
#https://climate-adapt.eea.europa.eu/en/metadata/tools/large-scale-integrated-sea-level-and-coastal-assessment-tool-liscoast

GetShorelineChanges<--function(ADM,ISO,ext,RCP="RCPC45"){
  Shore<-read.csv(paste0(dir,"/Data/Hazard/ENVIRONMENTAL/CoastalMorphodynamics/globalErosionProjections/globalErosionProjections_Long_Term_Change_RCP45_2050.csv"))
  #Each file contains 9 columns: lat, lon and values for the following percentiles: 1     5    17    50    83    95    99 
  colnames(Shore)<-c("lat", "lon", "p1", "p5","p17","p50","p83","p95","p99")
  Shore_sf<-Shore%>%
    st_as_sf(coords=c("lon","lat"), crs=4326) 
  
  if(ISO!= "FJI"){
    ADM_buf<-st_buffer(ADM,dist= 10000)  #AS an estimate, create outer buffer to polygon  to collect sufficient points.
    
    # find points within polygons
    shore_in_adm <- st_filter(Shore_sf, ADM_buf) %>%
      st_drop_geometry()%>%
       dplyr::summarize(p50 = mean(p50, na.rm=TRUE))
    
    ADM$x<-shore_in_adm$p50
  } else{
    ADM$x<-NA
  }
  names(ADM)[names(ADM) == "x"] <- paste0("ShlineRCP452050")
  return(ADM)
  
}



# Extreme sea level events
# https://doi.org/10.3389/fmars.2020.00263      data found at      https://zenodo.org/record/3660927#.Yr6OunhByV4
# This dataset should be replaced by the more recent and higher resolution European Space Agency (ESA) one (https://catalogue.ceda.ac.uk/uuid/a0a6fa39470a4a7baf847e3a1751f950?jump=related-anchor)
GetSeaLevelRise<-function(ADM,RP,ISO){
  SLR<-nc_open(paste0(dir,"/Data/Hazard/ENVIRONMENTAL/SeaLevelRise/CODEC_amax_ERA5_1979_2017_coor_mask_GUM_RPS.nc"))
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
  
  if(ISO!= "FJI"){
    ADM_buf<-st_buffer(ADM,dist= 10000)  #AS an estimate, create outer buffer to polygon  to collect sufficient points.
    
    # find points within polygons
    rps_in_adm <- st_filter(RPSout,ADM_buf) %>%
               st_drop_geometry() %>%
               dplyr::summarize(RPSmean = mean(RPS, na.rm=TRUE))%>%
               mutate_all(~ifelse(is.nan(.), NA, .))  #NaN to Na) 

    ADM$x<-rps_in_adm$RPSmean
    } else{
    ADM$x<-NA
  }
  names(ADM)[names(ADM) == "x"] <- paste0("SLR_rp",RP,"y")
  return(ADM)
}

st_filter(Shore_sf, ADM_buf) %>%
  st_drop_geometry()%>%
  dplyr::summarize(p50 = mean(p50, na.rm=TRUE))


# Soil Erosion
#GLOSEM 25km resolution
#Need to request data from https://esdac.jrc.ec.europa.eu/content/global-soil-erosion #Downloaded manually
#paper - https://www.nature.com/articles/s41467-017-02142-7


GetSoilErosion <-function(ADM,ISO,ext){
  fil<-list.files(path =paste0(dir,"/Data/Hazard/ENVIRONMENTAL/"),
                     pattern = "RUSLE_SoilLoss_v1.1_yr2012_25km.tif", recursive = TRUE,full.names = TRUE)
  
  x<-terra::rast(fil) %>%
    crop(ext)%>%
    terra::extract(ADM,method='bilinear',na.rm=T,fun=mean,ID=FALSE)%>%
    unlist() %>%
    ifelse(is.nan(.) ==TRUE,NA,.)  #NaN to Na
  ADM$x<-x
  names(ADM)[names(ADM) == "x"] <- paste0("SErode2012")
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
  fil<-list.files(paste0(dir,"/Data/Hazard/ENVIRONMENTAL/Tsunami"), pattern = paste0("rp",RP,"_"),full.names = TRUE)
  x<-terra::rast(fil) %>%
    crop(ext)%>%
    terra::extract(ADM,method='bilinear',na.rm=T,fun=mean,ID=FALSE)%>%
    unlist() %>%
    ifelse(is.nan(.) ==TRUE,NA,.)  #NaN to Na
  ADM$x<-x
  names(ADM)[names(ADM) == "x"] <- paste0("Ts_mih_rp",RP,"y")
  
  return(ADM)
}



#WetlandLoss
#https://www.nature.com/articles/s41586-022-05572-6
#https://zenodo.org/record/7293597#.ZEeAxqdBxoI
#https://github.com/etiennefluetchouinard/wetland-loss-reconstruction
GetWetlandLoss<-function(ISO,ADM,ext,year=2020){
  wLoss<-nc_open(paste0(dir,"/Data/Hazard/ENVIRONMENTAL/WetlandLoss/grid_ncdf/ensemblemean/wetland_loss_1700-2020_ensemblemean_v10.nc"))
  #varn<-names(wLoss$var) - if need to see all var names
  varn<-"wetland_loss"
  
  wLoss_ras<-raster(paste0(dir,"/Data/Hazard/ENVIRONMENTAL/WetlandLoss/grid_ncdf/ensemblemean/wetland_loss_1700-2020_ensemblemean_v10.nc"),varname=varn)
  
  x<-wLoss_ras%>%
    crop(ext)%>%
    terra::extract(ADM,method='bilinear',na.rm=T,fun=mean,ID=FALSE)%>%
    unlist() %>%
    ifelse(is.nan(.) ==TRUE,NA,.)  #NaN to Na
  ADM$x<-x
  names(ADM)[names(ADM) == "x"] <- paste0("WtndLss2010_2020_km2")
  return(ADM)
}


#Wildfire
#https://gwis.jrc.ec.europa.eu/apps/country.profile/downloads
#Monthly burned area [ha] by landcover class for years 2002-2019 for all countries 
#and sub-country administrative units (GADM level 0 and level 1 administrative units).
#The dataset is in tabular format (csv) and is derived from the MCD64A1 burned area product.
GetBurnedAreas<-function(ISO,ADM,ext){
  fil<-read.csv(paste0(dir,"/Data/Hazard/ENVIRONMENTAL/Wildfire/MCD64A1_burned_area_full_dataset_2002-2019.csv"),header=TRUE)%>%
    filter(GID_0 == ISO)%>%
    filter(Year ==max(fil$Year))
  
  #select only those in same ADM:
  burnedADM<-fil%>%
    filter(Region == ADM$ADM2NM)%>%
    group_by(Region)%>%
    summarize()
  
  
}
